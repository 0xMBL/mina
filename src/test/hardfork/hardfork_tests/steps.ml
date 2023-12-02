open Async
open Integration_test_lib
open Core
open Settings
open Temp_dir
open Docker_manager

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

  let of_runtime_config_file_exn config =
    let runtime_config =
      Yojson.Safe.from_file config
      |> Runtime_config.of_yojson |> Result.ok_or_failwith
    in
    { target_epoch_ledgers_state_hash = None
    ; start_slot_since_genesis = 0L
    ; genesis_ledger = Option.value_exn runtime_config.ledger
    ; first_pass_ledger_hashes = []
    ; last_snarked_ledger_hash = None
    }

  let to_yojson_file t output = Yojson.Safe.to_file output (to_yojson t)

  let of_ledger_file_exn ledger_file =
    let genesis_ledger =
      Yojson.Safe.from_file ledger_file
      |> Runtime_config.Ledger.of_yojson |> Result.ok_or_failwith
    in
    { target_epoch_ledgers_state_hash = None
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
  type t =
    { env : Settings.Env.t
    ; temp_dir : TempDir.t
    ; docker_manager : DockerManager.t
    }

  let create (env : Settings.Env.t) temp_dir =
    let network =
      match env.executor with Docker -> Some "hardfork" | _ -> None
    in
    { env
    ; temp_dir
    ; docker_manager =
        DockerManager.create network
          [ { image = env.dockers.postgres; name = "database" } ]
    }

  let get_hostname t =
    match t.env.executor with
    | Docker ->
        DockerManager.network_ip t.docker_manager
    | _ ->
        Deferred.return "localhost"

  let get_conn_str_to_mainnet_db t =
    let open Deferred.Let_syntax in
    let%bind hostname = get_hostname t in
    Deferred.return
      (Printf.sprintf "postgres://%s:%s@%s:%d/%s" t.env.db.user
         t.env.db.password hostname t.env.db.port t.env.db.input_db )

  let get_conn_str_to_migrated_db t =
    let open Deferred.Let_syntax in
    let%bind hostname = get_hostname t in
    Deferred.return
      (Printf.sprintf "postgres://%s:%s@%s:%d/%s" t.env.db.user
         t.env.db.password hostname t.env.db.port t.env.db.output_db )

  let start_postgres_docker t =
    let args =
      [ "-p"
      ; Settings.Env.postgres_port_mapping t.env
      ; "-e"
      ; Settings.Env.postgres_user_env t.env
      ; "-e"
      ; Settings.Env.postgres_pass_env t.env
      ; "-e"
      ; Settings.Env.postgres_input_db_env t.env
      ; "-d"
      ]
    in

    let poll_command = [ "pg_isready"; "-U"; t.env.db.user ] in
    let is_started (output : Process.Output.t) =
      String.is_substring output.stdout ~substring:"accepting connections"
    in
    DockerManager.start_and_wait_for_docker_init t.docker_manager
      t.env.dockers.postgres ~args ~poll_command ~is_started

  let setup t =
    let open Deferred.Let_syntax in
    let%bind _ = DockerManager.setup t.docker_manager in
    start_postgres_docker t

  let docker_of_app t app =
    if String.( = ) app t.env.paths.berkeley_migration then
      Option.value_exn t.env.dockers.archive
        ~message:"archive docker is not defined"
    else if String.( = ) app t.env.paths.berkeley_account_tables then
      Option.value_exn t.env.dockers.archive
        ~message:"archive docker is not defined"
    else if String.( = ) app t.env.paths.berkeley_migration_data_verifier then
      Option.value_exn t.env.dockers.test_suite
        ~message:"test suite docker is not defined"
    else if String.( = ) app t.env.paths.replayer then
      Option.value_exn t.env.dockers.archive
        ~message:"archive docker is not defined"
    else failwithf "cannot find docker for app '%s'" app ()

  let unpack_random_data archive output =
    Unix.mkdir output ;
    Util.run_cmd_exn "." "tar" [ "-xf"; archive; "-C"; output ]

  let import_dump t dump_file =
    Util.run_cmd_exn "." "psql"
      [ "-h"
      ; "localhost"
      ; "-p"
      ; string_of_int t.env.db.port
      ; "-U"
      ; t.env.db.user
      ; "-d"
      ; t.env.db.input_db
      ; "-f"
      ; dump_file
      ]

  let download_and_import_mainnet_dump t =
    let dump_file_archive = TempDir.path t.temp_dir "mainnet_dump.tar.gz" in
    let dump_file = TempDir.path t.temp_dir t.env.paths.mainnet_dump in
    let%bind _ =
      Util.run_cmd_exn "." "wget"
        [ "-c"
        ; Printf.sprintf
            "https://storage.googleapis.com/mina-archive-dumps/%s.tar.gz"
            t.env.paths.mainnet_dump
        ; "-O"
        ; dump_file_archive
        ]
    in
    let%bind _ =
      Util.run_cmd_exn "." "tar"
        [ "-xvf"; dump_file_archive; "-C"; t.temp_dir.root ]
    in
    import_dump t dump_file

  let create_output_db t =
    let%bind _ =
      Util.run_cmd_exn "." "psql"
        [ "-h"
        ; "localhost"
        ; "-p"
        ; string_of_int t.env.db.port
        ; "-U"
        ; t.env.db.user
        ; "-c"
        ; Printf.sprintf "CREATE DATABASE %s;" t.env.db.output_db
        ]
    in
    Util.run_cmd_exn "." "psql"
      [ "-h"
      ; "localhost"
      ; "-p"
      ; string_of_int t.env.db.port
      ; "-U"
      ; t.env.db.user
      ; "-d"
      ; t.env.db.output_db
      ; "-a"
      ; "-f"
      ; t.env.paths.create_schema_script
      ]

  let run t ~args app =
    match t.env.executor with
    | Dune ->
        Util.run_cmd_exn "." "dune" ([ "exec"; app; "--" ] @ args)
    | Docker ->
        DockerManager.run_in_docker t.docker_manager ~args app
          (docker_of_app t app)
    | Build ->
        Util.run_cmd_exn "." app args

  let run_replayer t ~input_file ~archive_uri ~output_file ~replayer_app =
    let args =
      [ "--input-file"
      ; input_file
      ; "--archive-uri"
      ; archive_uri
      ; "--output-file"
      ; output_file
      ]
    in
    run t ~args replayer_app

  let run_mainnet_replayer t ~input_file ~archive_uri ~output_file =
    let args =
      [ "--input-file"
      ; input_file
      ; "--archive-uri"
      ; archive_uri
      ; "--output-file"
      ; output_file
      ]
    in
    DockerManager.run_in_docker t.docker_manager ~args t.env.paths.replayer
      t.env.dockers.mainnet_archive

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
      ~interval_checkpoint ~berkeley_account_tables_app =
    let args =
      [ "--archive-uri"
      ; archive_uri
      ; "--input-file"
      ; input_config
      ; "--checkpoint-interval"
      ; string_of_int interval_checkpoint
      ; "--output-file"
      ; "output_ledger.json"
      ]
    in

    run t ~args berkeley_account_tables_app

  let run_berkeley_migration_verifier t ~archive_uri ~migrated_uri
      ~verification_start_slot ~verification_end_slot
      ~genesis_accounts_test_count ~genesis_ledger
      ~berkeley_migration_data_verifier_app =
    let args =
      [ "--archive-uri"
      ; archive_uri
      ; "--migrated-uri"
      ; migrated_uri
      ; "--end-global-slot-from"
      ; string_of_int verification_start_slot
      ; "--end-global-slot-to"
      ; string_of_int verification_end_slot
      ; "--genesis-accounts-test-count"
      ; string_of_int genesis_accounts_test_count
      ; "--genesis-ledger-file"
      ; genesis_ledger
      ; "--skip-commands-hashes-verifications"
        (* remove after fix for intact hashes is provided*)
      ]
    in
    run t ~args berkeley_migration_data_verifier_app

  let gather_checkpoints_files root =
    Sys.readdir root |> Array.to_list
    |> List.filter ~f:(fun x ->
           String.is_substring x ~substring:"berkeley-account-tables-checkpoint" )

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

  let compare_replayer_outputs ~expected ~actual =
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
        match
          List.find actual_accounts ~f:(fun actual_account ->
              String.( = ) expected_account.pk actual_account.pk )
        with
        | Some actual_account ->
            if
              Currency.Balance.( = ) expected_account.balance
                actual_account.balance
            then ()
            else
              failwithf
                "Incorrect balance for account %s when comparing replayer \
                 outputs expected:(%s) vs actual(%s)"
                expected_account.pk expected actual ()
        | None ->
            failwithf
              "Cannot find account in actual file %s when comparing replayer \
               outputs expected:(%s) vs actual(%s)"
              expected_account.pk expected actual () )

  let build_replayer_input_config temp_dir db_uri config_filename
      (input : BerkeleyTablesAppInput.t) (params : Settings.TestParams.t) =
    let open Deferred.Let_syntax in
    let%bind latest_state_hash =
      get_latest_state_hash (Uri.of_string db_uri) params.migration_end_slot
    in
    let input_config = TempDir.path temp_dir config_filename in

    let replayer_input =
      { input with target_epoch_ledgers_state_hash = Some latest_state_hash }
    in
    BerkeleyTablesAppInput.to_yojson_file replayer_input input_config ;
    Deferred.return input_config

  let compare_replayers_runs_from_mainnet_and_current t
      (params : Settings.TestParams.t) (input : BerkeleyTablesAppInput.t) =
    let open Deferred.Let_syntax in
    let%bind migrated_db = get_conn_str_to_migrated_db t in
    let%bind replayer_input_config =
      build_replayer_input_config t.temp_dir migrated_db
        "replayer_input_config.json" input params
    in
    let%bind mainnet_db = get_conn_str_to_mainnet_db t in

    let%bind replayer_mainnet_input_config =
      build_replayer_input_config t.temp_dir mainnet_db
        "mainnet_replayer_input_config.json" input params
    in

    let actual_replayer_output =
      TempDir.path t.temp_dir "actual_replayer_output.json"
    in
    let expected_replayer_output =
      TempDir.path t.temp_dir "expected_replayer_output.json"
    in

    let%bind _ =
      run_replayer t ~input_file:replayer_input_config
        ~output_file:actual_replayer_output ~archive_uri:migrated_db
        ~replayer_app:t.env.paths.replayer
    in

    let%bind _ =
      run_mainnet_replayer t ~input_file:replayer_mainnet_input_config
        ~output_file:expected_replayer_output ~archive_uri:mainnet_db
    in

    compare_replayer_outputs ~expected:expected_replayer_output
      ~actual:actual_replayer_output ;

    Deferred.unit
end
