open Async
open Settings
open Steps
open Core
open Temp_dir

module HardForkTests = struct
  let mainnet_migration env_file =
    let test_name = "mainnet_migration" in
    let env = Settings.of_file_or_fail env_file in
    let temp_dir = TempDir.create test_name in
    let input_config = TempDir.path temp_dir "runtime_config.json" in
    let actual_migrated_ledger =
      TempDir.path temp_dir "actual_migrated_ledger.json"
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        let steps = HardForkSteps.create env temp_dir in
        let open Deferred.Let_syntax in
        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let conn_str_source_db = Settings.connection_str_to_mainnet_db env in
        let%bind expected_ledger_path, end_migration_ledger_hash =
          HardForkSteps.get_expected_ledger steps "ledger_at_200.json"
        in
        let%bind conn_str_target_db =
          HardForkSteps.create_random_output_db steps test_name
        in
        let%bind migration_end_slot =
          HardForkSteps.get_migration_end_slot_for_state_hash conn_str_source_db
            end_migration_ledger_hash
        in
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps ~batch_size:2
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger (Some end_migration_ledger_hash)
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:10
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
            ~output_ledger:actual_migrated_ledger
        in

        let%bind _ =
          HardForkSteps.compare_hashes conn_str_source_db conn_str_target_db
            migration_end_slot
        in
        HardForkSteps.compare_replayer_outputs expected_ledger_path
          actual_migrated_ledger ~compare_auxiliary_data:false ;
        Deferred.unit )

  let random_migration env_file =
    let test_name = "random_migration" in
    let env = Settings.of_file_or_fail env_file in
    let temp_dir = TempDir.create test_name in
    let input_config = TempDir.path temp_dir "runtime_config.json" in
    let actual_migrated_ledger =
      TempDir.path temp_dir "actual_migrated_ledger.json"
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        let steps = HardForkSteps.create env temp_dir in
        let open Deferred.Let_syntax in
        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let conn_str_source_db =
          Settings.connection_str_to env "random_mainnet"
        in
        let%bind expected_ledger_path, end_migration_ledger_hash =
          HardForkSteps.get_expected_ledger steps "ledger_at_end.json"
        in
        let%bind conn_str_target_db =
          HardForkSteps.create_random_output_db steps test_name
        in
        let%bind migration_end_slot =
          HardForkSteps.get_migration_end_slot_for_state_hash conn_str_source_db
            end_migration_ledger_hash
        in
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps ~batch_size:2
            ~genesis_ledger:env.paths.random_data_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.random_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.random_data_ledger (Some end_migration_ledger_hash)
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:10
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
            ~output_ledger:actual_migrated_ledger
        in

        let%bind _ =
          HardForkSteps.compare_hashes conn_str_source_db conn_str_target_db
            migration_end_slot
        in
        HardForkSteps.compare_replayer_outputs expected_ledger_path
          actual_migrated_ledger ~compare_auxiliary_data:false ;
        Deferred.unit )

  let checkpoint env_file =
    let test_name = "checkpoint" in
    let env = Settings.of_file_or_fail env_file in
    let temp_dir = TempDir.create test_name in
    let input_config = TempDir.path temp_dir "runtime_config.json" in
    let actual_migrated_ledger =
      TempDir.path temp_dir "actual_migrated_ledger.json"
    in
    Async.Thread_safe.block_on_async_exn (fun () ->
        let steps = HardForkSteps.create env temp_dir in
        let open Deferred.Let_syntax in
        Unix.putenv ~key:"PGPASSWORD" ~data:env.db.password ;
        let conn_str_source_db = Settings.connection_str_to_mainnet_db env in
        let%bind expected_ledger_path, end_migration_ledger_hash =
          HardForkSteps.get_expected_ledger steps "ledger_at_end.json"
        in
        let%bind conn_str_target_db =
          HardForkSteps.create_random_output_db steps test_name
        in
        let%bind migration_end_slot =
          HardForkSteps.get_migration_end_slot_for_state_hash conn_str_source_db
            end_migration_ledger_hash
        in

        (* first we migrate half of slots *)
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps ~batch_size:2
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:(migration_end_slot / 2)
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger None
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db ~input_config
            ~interval_checkpoint:10
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
            ~output_ledger:"temp_ledger.json"
        in

        let checkpoints =
          HardForkSteps.gather_berkeley_account_tables_checkpoint_files "."
        in

        (* then we migrate second half of slots *)
        let%bind _ =
          HardForkSteps.perform_berkeley_migration steps ~batch_size:2
            ~genesis_ledger:env.paths.mainnet_genesis_ledger
            ~source_archive_uri:conn_str_source_db
            ~source_blocks_bucket:env.paths.mainnet_data_bucket
            ~target_archive_uri:conn_str_target_db
            ~end_global_slot:migration_end_slot
            ~berkeley_migration_app:env.paths.berkeley_migration
        in

        let input =
          BerkeleyTablesAppInput.of_runtime_config_file_exn
            env.paths.mainnet_genesis_ledger (Some end_migration_ledger_hash)
        in
        BerkeleyTablesAppInput.to_yojson_file input input_config ;

        let%bind _ =
          HardForkSteps.run_berkeley_account_tables_fix steps
            ~archive_uri:conn_str_target_db
            ~input_config:(List.last_exn checkpoints)
            ~interval_checkpoint:10
            ~berkeley_account_tables_app:env.paths.berkeley_account_tables
            ~output_ledger:actual_migrated_ledger
        in

        let%bind _ =
          HardForkSteps.compare_hashes conn_str_source_db conn_str_target_db
            migration_end_slot
        in
        HardForkSteps.compare_replayer_outputs expected_ledger_path
          actual_migrated_ledger ~compare_auxiliary_data:false ;
        Deferred.unit )
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
    [ ( "mainnet_migration"
      , [ test_case "Test for short global slots on mainnet data" `Quick
            HardForkTests.mainnet_migration
        ] )
    ; ( "random_migration"
      , [ test_case "Test for short global slots on artificial data" `Quick
            HardForkTests.random_migration
        ] )
    ; ( "checkpoints"
      , [ test_case "Test for checkpoint in migration process" `Quick
            HardForkTests.checkpoint
        ] )
    ]
