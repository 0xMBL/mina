open Async
open Settings
open Steps
open Core
open Temp_dir

module HardForkTests = struct
  let mainnet_migration env_file =
    let env = Settings.Env.of_file_or_fail env_file in
    let params =
      let open Settings.TestParams in
      { default with
        migration_end_slot = 30
      ; minimal_verification_slot_distance = 1
      ; genesis_accounts_test_count = 200
      }
    in
    let temp_dir = TempDir.create "mainnet_migration" in
    let input_config = TempDir.path temp_dir "genesis_enhanced.json" in

    Async.Thread_safe.block_on_async_exn (fun () ->
        let steps = HardForkSteps.create env temp_dir in
        let open Deferred.Let_syntax in
        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let%bind _ = HardForkSteps.setup steps in
        let%bind conn_str_source_db =
          HardForkSteps.get_conn_str_to_mainnet_db steps
        in
        let%bind conn_str_target_db =
          HardForkSteps.get_conn_str_to_migrated_db steps
        in
        let%bind _ = HardForkSteps.download_and_import_mainnet_dump steps in
        let%bind _ = HardForkSteps.create_output_db steps in
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps
            ~batch_size:params.precomputed_blocks_download_batch_size
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:params.migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:
              params.berkeley_accounts_table_interval_checkpoint
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
        in

        let%bind _  = HardForkSteps.run_berkeley_migration_verifier steps
            ~archive_uri:conn_str_source_db ~migrated_uri:conn_str_target_db
            ~verification_start_slot:
              ( params.migration_end_slot
              - params.minimal_verification_slot_distance )
            ~verification_end_slot:params.migration_end_slot
            ~genesis_accounts_test_count:params.genesis_accounts_test_count
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~berkeley_migration_data_verifier_app:
              env.paths.berkeley_migration_data_verifier
      
    in
    Deferred.unit
    (*
     
      HardForkSteps.compare_replayers_runs_from_mainnet_and_current steps
          params input 
        *)  )

  let random_migration env_file =
    let env = Settings.Env.of_file_or_fail env_file in
    let params =
      let open Settings.TestParams in
      { default with
        migration_end_slot = 14
      ; genesis_accounts_test_count = 10
      ; precomputed_blocks_download_batch_size = 2
      }
    in
    let temp_dir = TempDir.create "random_migration" in

    Async.Thread_safe.block_on_async_exn (fun () ->
        let open Deferred.Let_syntax in
        let steps = HardForkSteps.create env temp_dir in
        let%bind _ = HardForkSteps.setup steps in
        let%bind conn_str_source_db =
          HardForkSteps.get_conn_str_to_mainnet_db steps
        in
        let%bind conn_str_target_db =
          HardForkSteps.get_conn_str_to_migrated_db steps
        in
        let random_data_folder = TempDir.path temp_dir "random_data" in
        let input_genesis_ledger_json =
          Printf.sprintf "%s/genesis_ledger.json" random_data_folder
        in
        let input_config = TempDir.path temp_dir "genesis_enhanced.json" in
        let%bind _ =
          HardForkSteps.unpack_random_data env.paths.random_data
            random_data_folder
        in

        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let%bind _ =
          HardForkSteps.import_dump steps env.paths.random_data_dump
        in
        let%bind _ = HardForkSteps.create_output_db steps in
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps
            ~batch_size:params.precomputed_blocks_download_batch_size
            ~genesis_ledger:input_genesis_ledger_json
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.random_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:params.migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:
              params.berkeley_accounts_table_interval_checkpoint
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
        in
       
        let%bind _ =  HardForkSteps.run_berkeley_migration_verifier
            ~archive_uri:conn_str_source_db ~migrated_uri:conn_str_target_db
            ~verification_start_slot:params.migration_end_slot
            ~verification_end_slot:params.migration_end_slot
            ~genesis_accounts_test_count:params.genesis_accounts_test_count
            ~genesis_ledger:env.paths.mainnet_genesis_ledger steps
            ~berkeley_migration_data_verifier_app:
              env.paths.berkeley_migration_data_verifier
     
    in
    Deferred.unit
     (*  
      HardForkSteps.compare_replayers_runs_from_mainnet_and_current steps
          params input 
        *)  )

  let checkpoint env_file =
    let env = Settings.Env.of_file_or_fail env_file in
    let params =
      let open Settings.TestParams in
      { default with
        migration_end_slot = 200
      ; genesis_accounts_test_count = 10
      ; berkeley_accounts_table_interval_checkpoint = 5
      }
    in
    let temp_dir = TempDir.create "checkpoint" in
    let input_config = TempDir.path temp_dir "genesis_enhanced.json" in

    Async.Thread_safe.block_on_async_exn (fun () ->
        let steps = HardForkSteps.create env temp_dir in
        let open Deferred.Let_syntax in
        let%bind _ = HardForkSteps.setup steps in
        let%bind conn_str_source_db =
          HardForkSteps.get_conn_str_to_mainnet_db steps
        in
        let%bind conn_str_target_db =
          HardForkSteps.get_conn_str_to_migrated_db steps
        in
        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let%bind _ = HardForkSteps.download_and_import_mainnet_dump steps in
        let%bind _ = HardForkSteps.create_output_db steps in

        (* first we migrate half of slots *)
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps
            ~batch_size:params.precomputed_blocks_download_batch_size
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:(params.migration_end_slot / 2)
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:
              params.berkeley_accounts_table_interval_checkpoint
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
        in

        let checkpoints = HardForkSteps.gather_checkpoints_files "." in

        (* then we migrate second half of slots *)
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps
            ~batch_size:params.precomputed_blocks_download_batch_size
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:params.migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db
            ~input_config:(List.last_exn checkpoints)
            ~interval_checkpoint:
              params.berkeley_accounts_table_interval_checkpoint
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
        in

        let%bind _ = HardForkSteps.run_berkeley_migration_verifier steps
            ~archive_uri:conn_str_source_db ~migrated_uri:conn_str_target_db
            ~verification_start_slot:params.migration_end_slot
            ~verification_end_slot:params.migration_end_slot
            ~genesis_accounts_test_count:params.genesis_accounts_test_count
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~berkeley_migration_data_verifier_app:
              env.paths.berkeley_migration_data_verifier


           in
          Deferred.unit
     (*
      HardForkSteps.compare_replayers_runs_from_mainnet_and_current steps
          params input 
        *)  )
end

let env =
  let doc = "Path Path to env file" in
  Cmdliner.Arg.(
    required
    & opt (some string) None
    & info [ "env" ] ~doc ~docv:"HARDFORK_TEST_ENV_FILE")

let () =
  let open Alcotest in
  run_with_args "Hardfork test suite." env
    [ ( "real_data"
      , [ test_case "Test for short global slots on mainnet data" `Quick
            HardForkTests.mainnet_migration
        ] )
    ; ( "random_data"
      , [ test_case "Test for short global slots on artificial data" `Quick
            HardForkTests.random_migration
        ] )
    ; ( "checkpoints"
      , [ test_case "Test for short global slots on artificial data" `Quick
            HardForkTests.checkpoint
        ] )
    ]
