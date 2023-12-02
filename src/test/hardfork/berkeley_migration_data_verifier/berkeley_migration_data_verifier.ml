open Core_kernel
open Async

let get_mainnet_reference_data ~end_global_slot ~mainnet_pool =
  let query_mainnet_db = Mina_caqti.query mainnet_pool in

  match end_global_slot with
  | Some end_global_slot ->
      query_mainnet_db ~f:(fun db ->
          Sql.Base.Mainnet.balances_before_slot db ~slot:end_global_slot )
  | None ->
      query_mainnet_db ~f:(fun db ->
          Sql.Base.Mainnet.balances_before_slot_max_slot db )

let get_actual_migrated_data ~end_global_slot ~migrated_pool =
  let query_migrated_db = Mina_caqti.query migrated_pool in

  match end_global_slot with
  | Some end_global_slot ->
      query_migrated_db ~f:(fun db ->
          Sql.Base.Berkeley.balances_before_slot db ~slot:end_global_slot )
  | None ->
      query_migrated_db ~f:(fun db ->
          Sql.Base.Berkeley.balances_before_slot_max_slot db )

let verify_updated_addresses ~mainnet_pool ~migrated_pool ~logger
    ~end_global_slot =
  let%bind mainnet_balances =
    get_mainnet_reference_data ~end_global_slot ~mainnet_pool
  in

  [%log info] "Found %d addresses as reference in mainnet archive db "
    (List.length mainnet_balances) ;

  let%bind migrated_balances =
    get_actual_migrated_data ~end_global_slot ~migrated_pool
  in

  List.iter mainnet_balances ~f:(fun mainnet_balance ->
      match
        List.find migrated_balances ~f:(fun migrated_balance ->
            Sql.Base.equal mainnet_balance migrated_balance )
      with
      | Some migrated_balance ->
          if Int.equal mainnet_balance.balance migrated_balance.balance then
            [%log info] "Balances correct."
              ~metadata:
                [ ("mainnet_address", `String mainnet_balance.address)
                ; ("migrated_address", `String migrated_balance.address)
                ]
          else
            failwithf "Balance mismatch between '%s' and '%s'"
              (Sql.Base.to_string mainnet_balance)
              (Sql.Base.to_string migrated_balance)
              ()
      | None ->
          if Int.equal mainnet_balance.balance 0 then
            [%log info] "Skipping mainnet address as it has 0 balance."
              ~metadata:[ ("mainnet_address", `String mainnet_balance.address) ]
          else
            failwithf "Failed to find balance for mainnet address: '%s'"
              mainnet_balance.address () ) ;

  Deferred.unit

let verify_genesis_address ~logger ~(account : Runtime_config.Accounts.Single.t)
    ~end_global_slot ~mainnet_pool ~migrated_pool =
  let%bind mainnet_balances =
    get_mainnet_reference_data ~end_global_slot ~mainnet_pool
  in

  let%bind migrated_balances =
    get_actual_migrated_data ~end_global_slot ~migrated_pool
  in

  let predicate (balance : Sql.Base.t) =
    String.equal balance.address account.pk
  in
  let compare (left : Sql.Base.t) (right : Sql.Base.t) =
    left.slot - right.slot
  in

  let maybe_account_mainnet_balance =
    List.filter mainnet_balances ~f:predicate |> List.max_elt ~compare
  in
  let maybe_account_migrated_balance =
    List.filter migrated_balances ~f:predicate |> List.max_elt ~compare
  in

  match (maybe_account_mainnet_balance, maybe_account_migrated_balance) with
  | Some mainnet_balance, None ->
      failwithf
        "Failed to find balance for migrated address while mainnet was \
         present: '%s'"
        (Sql.Base.to_string mainnet_balance)
        ()
  | None, Some migrated_balance ->
      failwithf
        "Failed to find balance for mainnet address while migrated was \
         present: '%s'"
        (Sql.Base.to_string migrated_balance)
        ()
  | None, None ->
      [%log info] "Both Balances were not updated" ;
      Deferred.unit
  | Some mainnet_balance, Some migrated_balance ->
      if Sql.Base.equal mainnet_balance migrated_balance then (
        [%log info] "Balances correct."
          ~metadata:
            [ ("mainnet_address", `String mainnet_balance.address)
            ; ("migrated_address", `String migrated_balance.address)
            ] ;

        Deferred.unit )
      else
        failwithf "Balance mismatch between '%s' and '%s'"
          (Sql.Base.to_string mainnet_balance)
          (Sql.Base.to_string migrated_balance)
          ()

let verify_genesis_addresses ~mainnet_pool ~migrated_pool ~logger
    ~end_global_slot ~genesis_ledger_file ~genesis_accounts_test_count =
  [%log info] "Verifing genesis addresses consistency" ;
  let config =
    Yojson.Safe.from_file genesis_ledger_file
    |> Runtime_config.of_yojson |> Result.ok_or_failwith
  in
  match (Option.value_exn config.ledger).base with
  | Runtime_config.Ledger.Accounts accounts ->
      let rec loop ~n =
        if Int.equal n 0 then Deferred.unit
        else
          let index = Random.int (List.length accounts) in
          let account = List.nth_exn accounts index in
          let%bind () =
            verify_genesis_address ~logger ~account ~end_global_slot
              ~mainnet_pool ~migrated_pool
          in
          loop ~n:(n - 1)
      in

      loop ~n:genesis_accounts_test_count
  | _ ->
      failwithf "Ledger '%s' has no accounts" genesis_ledger_file ()

let verify_transaction_and_block_hashes ~mainnet_pool ~migrated_pool ~logger =
  [%log info] "Verifing transaction hashes are intact" ;
  let query_mainnet_db = Mina_caqti.query mainnet_pool in
  let query_migrated_db = Mina_caqti.query migrated_pool in
  let%bind internal_commands_hashes =
    query_mainnet_db ~f:(fun db -> Sql.Base.Mainnet.internal_commands_hashes db)
  in
  let%bind _ =
    Deferred.List.iter internal_commands_hashes ~f:(fun hash ->
        let%bind internal_command_id =
          query_migrated_db ~f:(fun db ->
              Sql.Base.Berkeley.find_user_command_id_by_hash db hash )
        in
        if internal_command_id |> Option.is_none then
          failwithf
            "Cannot find internal command hash ('%s') in migrated database" hash
            ()
        else Deferred.unit )
  in
  let%bind user_commands_hashes =
    query_mainnet_db ~f:(fun db -> Sql.Base.Mainnet.user_commands_hashes db)
  in
  Deferred.List.iter user_commands_hashes ~f:(fun hash ->
      let%bind user_command_id =
        query_migrated_db ~f:(fun db ->
            Sql.Base.Berkeley.find_user_command_id_by_hash db hash )
      in
      if user_command_id |> Option.is_none then
        failwithf "Cannot find user command hash ('%s') in migrated database"
          hash ()
      else Deferred.unit )

let main ~end_global_slot ~mainnet_archive_uri ~migrated_archive_uri
    ~genesis_ledger_file ~genesis_accounts_test_count ~end_global_slot_from
    ~end_global_slot_to ~no_global_slot_to ~no_commands_hashes_verifications ()
    =
  let genesis_accounts_test_count =
    match genesis_accounts_test_count with
    | Some genesis_accounts_test_count ->
        genesis_accounts_test_count
    | None ->
        10
  in

  let end_global_slot =
    match (end_global_slot, no_global_slot_to) with
    | _, true ->
        None
    | Some end_global_slot, _ ->
        Some end_global_slot
    | None, _ ->
        let message =
          "Either --end-global-slot should be defined or \
           --end-global-slot-from and --end-global-slot-to arguments"
        in
        let from = Option.value_exn end_global_slot_from ~message in
        let till = Option.value_exn end_global_slot_to ~message in

        let from = Int.min from till in
        let till = Int.max from till in
        Option.some (Random.int_incl from till)
  in

  let logger = Logger.create () in

  let option_default dflt vo = match vo with None -> dflt | Some v -> v in
  [%log info] "Running verifications with parameters"
    ~metadata:
      [ ("genesis_accounts_test_count", `Int genesis_accounts_test_count)
      ; ("end_global_slot", `Int (option_default 0 end_global_slot))
      ] ;

  let mainnet_archive_uri = Uri.of_string mainnet_archive_uri in
  let migrated_archive_uri = Uri.of_string migrated_archive_uri in
  let mainnet_pool =
    Caqti_async.connect_pool ~max_size:128 mainnet_archive_uri
  in
  let migrated_pool =
    Caqti_async.connect_pool ~max_size:128 migrated_archive_uri
  in
  match (mainnet_pool, migrated_pool) with
  | Error e, _ | _, Error e ->
      [%log fatal]
        ~metadata:[ ("error", `String (Caqti_error.show e)) ]
        "Failed to create Caqti pools for Postgresql" ;
      exit 1
  | Ok mainnet_pool, Ok migrated_pool ->
      let%bind () =
        verify_updated_addresses ~mainnet_pool ~migrated_pool ~logger
          ~end_global_slot
      in
      let%bind () =
        verify_genesis_addresses ~mainnet_pool ~migrated_pool ~logger
          ~end_global_slot ~genesis_ledger_file ~genesis_accounts_test_count
      in

      if no_commands_hashes_verifications then Deferred.unit
      else
        verify_transaction_and_block_hashes ~mainnet_pool ~migrated_pool ~logger

let () =
  Command.(
    run
      (let open Let_syntax in
      Command.async
        ~summary:
          "Verifies data before and after migration of Mina archive database"
        (let%map end_global_slot =
           Param.flag "--end-global-slot" ~doc:"last slot in migrated database"
             Param.(optional int)
         and mainnet_archive_uri =
           Param.flag "--archive-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         and migrated_archive_uri =
           Param.flag "--migrated-uri"
             ~doc:
               "URI URI for connecting to the archive database (e.g., \
                postgres://$USER@localhost:5432/archiver)"
             Param.(required string)
         and genesis_ledger_file =
           Param.flag "--genesis-ledger-file"
             ~doc:"Path Path to genesis ledger json file"
             Param.(required string)
         and genesis_accounts_test_count =
           Param.flag "--genesis-accounts-test-count"
             ~doc:"Int Number of accounts from genesis to verify migration"
             Param.(optional int)
         and end_global_slot_from =
           Param.flag "--end-global-slot-from"
             ~doc:"Int Lower range of random slot"
             Param.(optional int)
         and end_global_slot_to =
           Param.flag "--end-global-slot-to"
             ~doc:"Int Upper Upper range of random slot"
             Param.(optional int)
         and no_global_slot_to =
           Param.flag "--no-end-global-slot" ~doc:"Bool No Upper range for slot"
             Param.no_arg
         and no_commands_hashes_verifications =
           Param.flag "--skip-commands-hashes-verifications"
             ~doc:"Bool Skip user and internal command hashes equality"
             Param.no_arg
         in

         main ~end_global_slot ~mainnet_archive_uri ~migrated_archive_uri
           ~genesis_ledger_file ~genesis_accounts_test_count
           ~end_global_slot_from ~end_global_slot_to ~no_global_slot_to
           ~no_commands_hashes_verifications )))
