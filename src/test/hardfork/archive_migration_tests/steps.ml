open Async
open Integration_test_lib
open Core
open Settings
open Temp_dir

module ReplayerOutput = struct
  type t =
    { target_epoch_ledgers_state_hash : string
    ; target_fork_state_hash : string
    ; target_genesis_ledger : Runtime_config.Ledger.t
    ; target_epoch_data : Runtime_config.Epoch_data.t
    }
  [@@deriving yojson]
end

module BerkeleyTablesAppInput = struct
  type t =
    { target_epoch_ledgers_state_hash : string option
          (* String is used for backward compatibility with mainnet until we fix hashes mistmatch*)
          [@default None]
    ; start_slot_since_genesis : int64 [@default 0L]
    ; genesis_ledger : Runtime_config.Ledger.t
    ; first_pass_ledger_hashes : Mina_base.Ledger_hash.t list [@default []]
    ; last_snarked_ledger_hash : Mina_base.Ledger_hash.t option [@default None]
    }
  [@@deriving yojson]

  let of_runtime_config_file_exn config target_epoch_ledgers_state_hash =
    let runtime_config =
      Yojson.Safe.from_file config
      |> Runtime_config.of_yojson |> Result.ok_or_failwith
    in
    { target_epoch_ledgers_state_hash
    ; start_slot_since_genesis = 0L
    ; genesis_ledger = Option.value_exn runtime_config.ledger
    ; first_pass_ledger_hashes = []
    ; last_snarked_ledger_hash = None
    }

  let to_yojson_file t output = Yojson.Safe.to_file output (to_yojson t)

  let of_ledger_file_exn ledger_file ~target_epoch_ledgers_state_hash =
    let genesis_ledger =
      Yojson.Safe.from_file ledger_file
      |> Runtime_config.Ledger.of_yojson |> Result.ok_or_failwith
    in
    { target_epoch_ledgers_state_hash
    ; start_slot_since_genesis = 0L
    ; genesis_ledger
    ; first_pass_ledger_hashes = []
    ; last_snarked_ledger_hash = None
    }
end

module BerkeleyTablesAppOutput = struct
  type t =
    { target_epoch_ledgers_state_hash : Mina_base.State_hash.t
    ; target_fork_state_hash : Mina_base.State_hash.t
    ; target_genesis_ledger : Runtime_config.Ledger.t
    ; target_epoch_data : Runtime_config.Epoch_data.t
    }
  [@@deriving yojson]

  let of_json_file_exn file =
    Yojson.Safe.from_file file |> of_yojson |> Result.ok_or_failwith
end

module HardForkSteps = struct
  type t = { env : Settings.t; temp_dir : TempDir.t }

  let create (env : Settings.t) temp_dir = { env; temp_dir }

  let unpack_random_data archive output =
    Unix.mkdir output ;
    Util.run_cmd_exn "." "tar" [ "-xf"; archive; "-C"; output ]

  let create_db t db =
    let%bind _ =
      Util.run_cmd_exn "." "psql"
        [ "-h"
        ; t.env.db.host
        ; "-p"
        ; string_of_int t.env.db.port
        ; "-U"
        ; t.env.db.user
        ; "-c"
        ; Printf.sprintf "CREATE DATABASE %s;" db
        ]
    in
    Util.run_cmd_exn "." "psql"
      [ "-h"
      ; t.env.db.host
      ; "-p"
      ; string_of_int t.env.db.port
      ; "-U"
      ; t.env.db.user
      ; "-d"
      ; db
      ; "-a"
      ; "-f"
      ; t.env.paths.create_schema_script
      ]

  let create_random_output_db t prefix =
    let db_name = Printf.sprintf "%s_%d" prefix (Random.int 1000000 + 1000) in
    let%bind _ = create_db t db_name in
    Deferred.return (Settings.connection_str_to t.env db_name)

  let run t ~args app =
    match t.env.executor with
    | Dune ->
        Util.run_cmd_exn "." "dune" ([ "exec"; app; "--" ] @ args)
    | Bash ->
        Util.run_cmd_exn "." app args

  let perform_berkeley_migration t ~batch_size ~genesis_ledger
      ~source_archive_uri ~source_blocks_bucket ~target_archive_uri
      ~end_global_slot ~berkeley_migration_app =
    let args =
      [ "--batch-size"
      ; string_of_int batch_size
      ; "--config-file"
      ; genesis_ledger
      ; "--mainnet-archive-uri"
      ; source_archive_uri
      ; "--migrated-archive-uri"
      ; target_archive_uri
      ; "--mainnet-blocks-bucket"
      ; source_blocks_bucket
      ; "--end-global-slot"
      ; string_of_int end_global_slot
      ]
    in
    run t ~args berkeley_migration_app

  let run_berkeley_account_tables_fix t ~archive_uri ~input_config
      ~interval_checkpoint ~berkeley_account_tables_app ~output_ledger =
    let args =
      [ "--archive-uri"
      ; archive_uri
      ; "--input-file"
      ; input_config
      ; "--checkpoint-interval"
      ; string_of_int interval_checkpoint
      ; "--output-file"
      ; output_ledger
      ]
    in

    run t ~args berkeley_account_tables_app

  let gather_berkeley_account_tables_checkpoint_files root =
    Sys.readdir root |> Array.to_list
    |> List.filter ~f:(fun x ->
           String.is_substring x ~substring:"berkeley-account-tables-checkpoint" )
    |> List.sort ~compare:(fun left right ->
           let number s =
             Scanf.sscanf s "berkeley-account-tables-checkpoint-%d.json"
               (fun s -> s)
           in
           let left = number left in
           let right = number right in
           left - right )

  let get_latest_state_hash uri migration_end_slot =
    let open Deferred.Let_syntax in
    let mainnet_pool = Caqti_async.connect_pool ~max_size:128 uri in

    match mainnet_pool with
    | Error _ ->
        failwithf "Failed to create Caqti pools for Postgresql to %s"
          (Uri.to_string uri) ()
    | Ok mainnet_pool ->
        let query_mainnet_db = Mina_caqti.query mainnet_pool in
        let%bind maybe_slot =
          query_mainnet_db ~f:(fun db ->
              Sql.Mainnet.latest_state_hash db migration_end_slot )
        in
        Deferred.return
          (Option.value_exn maybe_slot
             ~message:
               (Printf.sprintf "Cannot find latest state has for slot: %d"
                  migration_end_slot ) )

  let get_migration_end_slot_for_state_hash conn_str state_hash =
    let open Deferred.Let_syntax in
    let uri = Uri.of_string conn_str in
    let mainnet_pool = Caqti_async.connect_pool ~max_size:128 uri in
    match mainnet_pool with
    | Error _ ->
        failwithf "Failed to create Caqti pools for Postgresql to %s"
          (Uri.to_string uri) ()
    | Ok mainnet_pool ->
        let query_mainnet_db = Mina_caqti.query mainnet_pool in
        let%bind maybe_slot =
          query_mainnet_db ~f:(fun db ->
              Sql.Mainnet.global_slot_since_genesis_at_state_hash db state_hash )
        in
        Deferred.return
          (Option.value_exn maybe_slot
             ~message:
               (Printf.sprintf "Cannot find slot has for state hash: %s"
                  state_hash ) )

  let compare_hashes mainnet_archive_conn_str migrated_archive_conn_str
      end_global_slot =
    let mainnet_archive_uri = Uri.of_string mainnet_archive_conn_str in
    let migrated_archive_uri = Uri.of_string migrated_archive_conn_str in
    let mainnet_pool =
      Caqti_async.connect_pool ~max_size:128 mainnet_archive_uri
    in
    let migrated_pool =
      Caqti_async.connect_pool ~max_size:128 migrated_archive_uri
    in
    match (mainnet_pool, migrated_pool) with
    | Error _e, _ | _, Error _e ->
        failwith "Failed to create Caqti pools for Postgresql"
    | Ok mainnet_pool, Ok migrated_pool ->
        let query_mainnet_db = Mina_caqti.query mainnet_pool in
        let query_migrated_db = Mina_caqti.query migrated_pool in
        let compare_hashes ~fetch_data_sql ~find_element_sql name =
          let%bind expected_hashes = query_mainnet_db ~f:fetch_data_sql in
          Deferred.List.iter expected_hashes ~f:(fun hash ->
              let%bind element_id = find_element_sql hash in
              if element_id |> Option.is_none then
                failwithf "Cannot find %s hash ('%s') in migrated database" name
                  hash ()
              else Deferred.unit )
        in
        let%bind _ =
          compare_hashes
            ~fetch_data_sql:(fun db ->
              Sql.Mainnet.user_commands_hashes db end_global_slot )
            ~find_element_sql:(fun hash ->
              query_migrated_db ~f:(fun db ->
                  Sql.Berkeley.find_user_command_id_by_hash db hash ) )
            "user_commands"
        in
        let%bind _ =
          compare_hashes
            ~fetch_data_sql:(fun db ->
              Sql.Mainnet.internal_commands_hashes db end_global_slot )
            ~find_element_sql:(fun hash ->
              query_migrated_db ~f:(fun db ->
                  Sql.Berkeley.find_internal_command_id_by_hash db hash ) )
            "internal_commands"
        in
        let%bind _ =
          compare_hashes
            ~fetch_data_sql:(fun db ->
              Sql.Mainnet.block_hashes db end_global_slot )
            ~find_element_sql:(fun hash ->
              query_migrated_db ~f:(fun db ->
                  Sql.Berkeley.find_user_command_id_by_hash db hash ) )
            "block_state_hashes"
        in
        let%bind _ =
          compare_hashes
            ~fetch_data_sql:(fun db ->
              Sql.Mainnet.block_parent_hashes db end_global_slot )
            ~find_element_sql:(fun hash ->
              query_migrated_db ~f:(fun db ->
                  Sql.Berkeley.find_user_command_id_by_hash db hash ) )
            "block_parent_state_hashes"
        in
        let%bind _ =
          compare_hashes
            ~fetch_data_sql:(fun db ->
              Sql.Mainnet.ledger_hashes db end_global_slot )
            ~find_element_sql:(fun hash ->
              query_migrated_db ~f:(fun db ->
                  Sql.Berkeley.find_user_command_id_by_hash db hash ) )
            "ledger_hashes"
        in

        Deferred.unit

  let compare_replayer_outputs expected actual ~compare_auxiliary_data =
    let expected_output = BerkeleyTablesAppOutput.of_json_file_exn expected in
    let actual_output = BerkeleyTablesAppOutput.of_json_file_exn actual in

    let get_accounts (output : BerkeleyTablesAppOutput.t) file =
      match output.target_genesis_ledger.base with
      | Named _ ->
          failwithf "%s file does not have any account" file ()
      | Accounts accounts ->
          accounts
      | Hash _ ->
          failwithf "%s file does not have any account" file ()
    in

    let expected_accounts = get_accounts expected_output expected in
    let actual_accounts = get_accounts actual_output actual in

    List.iter expected_accounts ~f:(fun expected_account ->
        let get_value_or_none option = Option.value option ~default:"None" in
        let compare_balances (actual_account : Runtime_config.Accounts.Single.t)
            (expected_account : Runtime_config.Accounts.Single.t) =
          if
            Currency.Balance.( = ) expected_account.balance
              actual_account.balance
          then ()
          else
            failwithf
              "Incorrect balance for account %s when comparing replayer \
               outputs expected:(%s) vs actual(%s)"
              expected_account.pk expected actual ()
        in
        let compare_receipt (actual_account : Runtime_config.Accounts.Single.t)
            (expected_account : Runtime_config.Accounts.Single.t) =
          let expected_receipt_chain_hash =
            get_value_or_none expected_account.receipt_chain_hash
          in
          let actual_account_receipt_chain_hash =
            get_value_or_none actual_account.receipt_chain_hash
          in
          if
            String.( = ) expected_receipt_chain_hash
              actual_account_receipt_chain_hash
          then ()
          else
            failwithf
              "Incorrect receipt chain hash for account %s when comparing \
               replayer outputs expected:(%s) vs actual(%s)"
              expected_account.pk expected_receipt_chain_hash
              actual_account_receipt_chain_hash ()
        in
        let compare_delegation
            (actual_account : Runtime_config.Accounts.Single.t)
            (expected_account : Runtime_config.Accounts.Single.t) =
          let expected_delegation =
            get_value_or_none expected_account.delegate
          in
          let actual_delegation = get_value_or_none actual_account.delegate in
          if String.( = ) expected_delegation actual_delegation then ()
          else
            failwithf
              "Incorrect delegation for account %s when comparing replayer \
               outputs expected:(%s) vs actual(%s)"
              expected_account.pk expected_delegation actual_delegation ()
        in

        match
          List.find actual_accounts ~f:(fun actual_account ->
              String.( = ) expected_account.pk actual_account.pk )
        with
        | Some actual_account ->
            compare_balances actual_account expected_account ;
            if compare_auxiliary_data then
              compare_receipt actual_account expected_account ;
            compare_delegation actual_account expected_account
        | None ->
            failwithf
              "Cannot find account in actual file %s when comparing replayer \
               outputs expected:(%s) vs actual(%s)"
              expected_account.pk expected actual () )

  let get_expected_ledger t name =
    let expected_ledger_path =
      Printf.sprintf "%s/%s" t.env.paths.mainnet_expected_ledger_folder name
    in
    let expected_ledger =
      Yojson.Safe.from_file expected_ledger_path
      |> ReplayerOutput.of_yojson |> Result.ok_or_failwith
    in
    let end_migration_ledger_hash =
      expected_ledger.target_epoch_ledgers_state_hash
    in
    return (expected_ledger_path, end_migration_ledger_hash)
end
