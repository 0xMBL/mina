open Core

module Settings = struct
  type paths =
    { berkeley_migration : string
    ; berkeley_account_tables : string
    ; mainnet_genesis_ledger : string
    ; mainnet_data_bucket : string [@default "mina_network_block_data"]
    ; mainnet_expected_ledger_folder : string
    ; create_schema_script : string
    ; zkapp_tables_script : string
    ; random_data_ledger : string
    ; random_data_bucket : string
          [@default "mina_random_network_block_data_placeholder"]
    ; random_data_expected_ledger_folder : string
    }
  [@@deriving yojson]

  type db =
    { password : string
    ; user : string
    ; port : int
    ; host : string [@default "127.0.0.1"]
    ; mainnet_source_schema : string [@default "archive_balances_migrated"]
    }
  [@@deriving yojson]

  module Executor = struct
    type t = Dune | Bash [@@deriving yojson]

    let to_yojson = function Dune -> `String "dune" | Bash -> `String "bash"

    let of_yojson = function
      | `String s -> (
          match String.lowercase s with
          | "dune" ->
              Ok Dune
          | "bash" ->
              Ok Bash
          | _ ->
              Error (sprintf "Invalid Executor.t value: %s" s) )
      | _ ->
          Error "Settings.Env.Executor"
  end

  type t = { executor : Executor.t [@default Dune]; paths : paths; db : db }
  [@@deriving yojson]

  let of_file_or_fail env_file =
    match Yojson.Safe.from_file env_file |> of_yojson with
    | Ok env ->
        env
    | Error err ->
        failwithf "cannot parse '%s'  due to: %s" env_file err ()

  let connection_str_to env db =
    Printf.sprintf "postgres://%s:%s@%s:%d/%s" env.db.user env.db.password
      env.db.host env.db.port db

  let connection_str_to_mainnet_db env =
    connection_str_to env env.db.mainnet_source_schema
end