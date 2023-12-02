open Core

module Settings = struct
  module TestParams = struct
    type t =
      { precomputed_blocks_download_batch_size : int [@default 100]
      ; minimal_verification_slot_distance : int [@default 10]
      ; migration_end_slot : int
      ; genesis_accounts_test_count : int
      ; berkeley_accounts_table_interval_checkpoint : int [@default 100]
      }
    [@@deriving yojson]

    let default =
      { precomputed_blocks_download_batch_size = 100
      ; minimal_verification_slot_distance = 10
      ; migration_end_slot = 20
      ; genesis_accounts_test_count = 10
      ; berkeley_accounts_table_interval_checkpoint = 100
      }
  end

  module Env = struct
    type dockers =
      { mainnet_archive : string
      ; archive : string option [@default None]
      ; daemon : string option [@default None]
      ; test_suite : string option [@default None]
      ; postgres : string
      }
    [@@deriving yojson]

    type paths =
      { berkeley_migration : string
      ; berkeley_account_tables : string
      ; berkeley_migration_data_verifier : string
      ; replayer : string
      ; mainnet_genesis_ledger : string
      ; mainnet_data_bucket : string [@default "mina_network_block_data"]
      ; mainnet_dump : string
            [@default "mainnet-archive-dump-2023-11-02_0000.sql"]
      ; create_schema_script : string
      ; zkapp_tables_script : string
      ; random_data : string
      ; random_data_dump : string
      ; random_data_bucket : string
            [@default "mina_random_network_block_data_placeholder"]
      }
    [@@deriving yojson]

    type db =
      { password : string
      ; user : string
      ; input_db : string
      ; output_db : string
      ; port : int
      }
    [@@deriving yojson]

    module Executor = struct
      type t = Dune | Build | Docker [@@deriving yojson]

      let to_yojson = function
        | Dune ->
            `String "dune"
        | Build ->
            `String "build"
        | Docker ->
            `String "docker"

      let of_yojson = function
        | `String s -> (
            match String.lowercase s with
            | "dune" ->
                Ok Dune
            | "build" ->
                Ok Build
            | "docker" ->
                Ok Docker
            | _ ->
                Error (sprintf "Invalid Executor.t value: %s" s) )
        | _ ->
            Error "Settings.Env.Executor"
    end

    type t =
      { executor : Executor.t [@default Dune]
      ; dockers : dockers
      ; paths : paths
      ; db : db
      }
    [@@deriving yojson]

    let of_file_or_fail env_file =
      match Yojson.Safe.from_file env_file |> of_yojson with
      | Ok env ->
          env
      | Error err ->
          failwithf "cannot parse '%s'  due to: %s" env_file err ()

    let postgres_port_mapping t = Printf.sprintf "%d:%d" t.db.port 5432

    let postgres_user_env t = Printf.sprintf "POSTGRES_USER=%s" t.db.user

    let postgres_pass_env t =
      Printf.sprintf "POSTGRES_PASSWORD=%s" t.db.password

    let postgres_input_db_env t = Printf.sprintf "POSTGRES_DB=%s" t.db.input_db
  end
end
