open Core
open Async
open Currency
open Signature_lib
open Mina_base
open Integration_test_lib

[@@@warning "-27"]

let docker_swarm_version = "3.8"

let postgres_image = "docker.io/bitnami/postgresql"

let mina_archive_schema =
  "https://raw.githubusercontent.com/MinaProtocol/mina/develop/src/app/archive/create_schema.sql"

let mina_create_schema = "create_schema.sql"

module Network_config = struct
  module Cli_inputs = Cli_inputs

  module PortManager = struct
    type t =
      { mutable available_ports : int list
      ; mutable used_ports : int list
      ; min_port : int
      ; max_port : int
      }

    let create ~min_port ~max_port =
      let available_ports = List.range min_port max_port in
      { available_ports; used_ports = []; min_port; max_port }

    let allocate_port t =
      match t.available_ports with
      | [] ->
          failwith "No available ports"
      | port :: rest ->
          t.available_ports <- rest ;
          t.used_ports <- port :: t.used_ports ;
          port |> Int.to_string

    let release_port t port =
      t.used_ports <- List.filter t.used_ports ~f:(fun p -> p <> port) ;
      t.available_ports <- port :: t.available_ports

    let get_latest_used_port t =
      match t.used_ports with
      | [] ->
          failwith "No used ports"
      | port :: _ ->
          port |> Int.to_string
  end

  type docker_volume_configs = { name : string; data : string }
  [@@deriving to_yojson]

  type block_producer_config =
    { name : string
    ; keypair : Network_keypair.t
    ; libp2p_secret : string
    ; rest_port : string
    }
  [@@deriving to_yojson]

  type snark_worker_config =
    { name : string; public_key : string; rest_port : string }
  [@@deriving to_yojson]

  type snark_coordinator_config =
    { name : string
    ; public_key : string
    ; rest_port : string
    ; snark_worker_fee : string
    ; worker_nodes : snark_worker_config list
    }
  [@@deriving to_yojson]

  type archive_node_configs =
    { name : string
    ; id : string
    ; schema : string
    ; schema_aux_files : string list
    ; rest_port : string
    }
  [@@deriving to_yojson]

  type seed_config = { name : string; rest_port : string }
  [@@deriving to_yojson]

  type docker_config =
    { docker_swarm_version : string
    ; stack_name : string
    ; mina_image : string
    ; mina_agent_image : string
    ; mina_bots_image : string
    ; mina_points_image : string
    ; mina_archive_image : string
    ; runtime_config : Yojson.Safe.t
    ; docker_volume_configs : docker_volume_configs list
    ; seed_configs : seed_config list
    ; block_producer_configs : block_producer_config list
    ; snark_coordinator_config : snark_coordinator_config option
    ; archive_node_configs : archive_node_configs list
    ; log_precomputed_blocks : bool
    ; cpu_request : int
    ; mem_request : string
    ; worker_cpu_request : int
    ; worker_mem_request : string
    }
  [@@deriving to_yojson]

  type t =
    { debug_arg : bool
    ; genesis_keypairs :
        (Network_keypair.t Core.String.Map.t
        [@to_yojson
          fun map ->
            `Assoc
              (Core.Map.fold_right ~init:[]
                 ~f:(fun ~key:k ~data:v accum ->
                   (k, Network_keypair.to_yojson v) :: accum )
                 map )] )
    ; constants : Test_config.constants
    ; docker : docker_config
    }
  [@@deriving to_yojson]

  let expand ~logger ~test_name ~(cli_inputs : Cli_inputs.t) ~(debug : bool)
      ~(test_config : Test_config.t) ~(images : Test_config.Container_images.t)
      =
    let { requires_graphql
        ; genesis_ledger
        ; epoch_data
        ; block_producers
        ; snark_coordinator
        ; snark_worker_fee
        ; num_archive_nodes
        ; log_precomputed_blocks (* ; num_plain_nodes *)
        ; proof_config
        ; Test_config.k
        ; delta
        ; slots_per_epoch
        ; slots_per_sub_window
        ; txpool_max_size
        } =
      test_config
    in
    (* TODO: Also refactor this *)
    let user_from_env = Option.value (Unix.getenv "USER") ~default:"auto" in
    let user_sanitized =
      Str.global_replace (Str.regexp "\\W|_-") "" user_from_env
    in
    let user_len = Int.min 5 (String.length user_sanitized) in
    let user = String.sub user_sanitized ~pos:0 ~len:user_len in
    let git_commit = Mina_version.commit_id_short in
    (* see ./src/app/test_executive/README.md for information regarding the namespace name format and length restrictions *)
    let stack_name = "it-" ^ user ^ "-" ^ git_commit ^ "-" ^ test_name in

    (* check to make sure the test writer hasn't accidentally created duplicate names of accounts and keys *)
    let key_names_list =
      List.map genesis_ledger ~f:(fun acct -> acct.account_name)
    in
    if List.contains_dup ~compare:String.compare key_names_list then
      failwith
        "All accounts in genesis ledger must have unique names.  Check to make \
         sure you are not using the same account_name more than once" ;
    let all_nodes_names_list =
      List.map block_producers ~f:(fun acct -> acct.node_name)
      @ match snark_coordinator with None -> [] | Some n -> [ n.node_name ]
    in
    if List.contains_dup ~compare:String.compare all_nodes_names_list then
      failwith
        "All nodes in testnet must have unique names.  Check to make sure you \
         are not using the same node_name more than once" ;
    let keypairs =
      List.take
        (List.tl_exn
           (Array.to_list (Lazy.force Key_gen.Sample_keypairs.keypairs)) )
        (List.length genesis_ledger)
    in
    let runtime_timing_of_timing = function
      | Account.Timing.Untimed ->
          None
      | Timed t ->
          Some
            { Runtime_config.Accounts.Single.Timed.initial_minimum_balance =
                t.initial_minimum_balance
            ; cliff_time = t.cliff_time
            ; cliff_amount = t.cliff_amount
            ; vesting_period = t.vesting_period
            ; vesting_increment = t.vesting_increment
            }
    in
    let add_accounts accounts_and_keypairs =
      List.map accounts_and_keypairs
        ~f:(fun
             ( { Test_config.Test_Account.balance; account_name; timing }
             , (pk, sk) )
           ->
          let timing = runtime_timing_of_timing timing in
          let default = Runtime_config.Accounts.Single.default in
          let account =
            { default with
              pk = Public_key.Compressed.to_string pk
            ; sk = Some (Private_key.to_base58_check sk)
            ; balance =
                Balance.of_mina_string_exn balance
                (* delegation currently unsupported *)
            ; delegate = None
            ; timing
            }
          in
          (account_name, account) )
    in
    let genesis_accounts_and_keys = List.zip_exn genesis_ledger keypairs in
    let genesis_ledger_accounts = add_accounts genesis_accounts_and_keys in
    (* DAEMON CONFIG *)
    let constraint_constants =
      Genesis_ledger_helper.make_constraint_constants
        ~default:Genesis_constants.Constraint_constants.compiled proof_config
    in
    let ledger_is_prefix ledger1 ledger2 =
      List.is_prefix ledger2 ~prefix:ledger1
        ~equal:(fun
                 ({ account_name = name1; _ } : Test_config.Test_Account.t)
                 ({ account_name = name2; _ } : Test_config.Test_Account.t)
               -> String.equal name1 name2 )
    in
    let runtime_config =
      { Runtime_config.daemon =
          Some
            { txpool_max_size = Some txpool_max_size
            ; peer_list_url = None
            ; zkapp_proof_update_cost = None
            ; zkapp_signed_single_update_cost = None
            ; zkapp_signed_pair_update_cost = None
            ; zkapp_transaction_cost_limit = None
            ; max_event_elements = None
            ; max_action_elements = None
            }
      ; genesis =
          Some
            { k = Some k
            ; delta = Some delta
            ; slots_per_epoch = Some slots_per_epoch
            ; slots_per_sub_window = Some slots_per_sub_window
            ; genesis_state_timestamp =
                Some Core.Time.(to_string_abs ~zone:Zone.utc (now ()))
            }
      ; proof = Some proof_config (* TODO: prebake ledger and only set hash *)
      ; ledger =
          Some
            { base =
                Accounts
                  (List.map genesis_ledger_accounts ~f:(fun (_name, acct) ->
                       acct ) )
            ; add_genesis_winner = None
            ; num_accounts = None
            ; balances = []
            ; hash = None
            ; name = None
            }
      ; epoch_data =
          (* each staking epoch ledger account must also be a genesis ledger account, though
             the balance may be different; the converse is not necessarily true, since
             an account may have been added after the last epoch ledger was taken

             each staking epoch ledger account must also be in the next epoch ledger, if provided

             if provided, each next_epoch_ledger account must be in the genesis ledger

             in all ledgers, the accounts must be in the same order, so that accounts will
             be in the same leaf order
          *)
          Option.map epoch_data ~f:(fun { staking = staking_ledger; next } ->
              let genesis_winner_account : Runtime_config.Accounts.single =
                Runtime_config.Accounts.Single.of_account
                  Mina_state.Consensus_state_hooks.genesis_winner_account
                |> Result.ok_or_failwith
              in
              let ledger_of_epoch_accounts
                  (epoch_accounts : Test_config.Test_Account.t list) =
                let epoch_ledger_accounts =
                  List.map epoch_accounts
                    ~f:(fun { account_name; balance; timing } ->
                      let balance = Balance.of_mina_string_exn balance in
                      let timing = runtime_timing_of_timing timing in
                      let genesis_account =
                        match
                          List.Assoc.find genesis_ledger_accounts account_name
                            ~equal:String.equal
                        with
                        | Some acct ->
                            acct
                        | None ->
                            failwithf
                              "Epoch ledger account %s not in genesis ledger"
                              account_name ()
                      in
                      { genesis_account with balance; timing } )
                in
                (* because we run integration tests with Proof_level = Full, the winner account
                   gets added to the genesis ledger

                   there isn't a corresponding mechanism to add the winner account to epoch
                   ledgers, so we add it explicitly here

                   `add_genesis_winner` in the record below has no effect, it's ignored in
                   Runtime_config.Epoch_data.to_yojson, which is used to create the config file
                *)
                ( { base =
                      Accounts (genesis_winner_account :: epoch_ledger_accounts)
                  ; add_genesis_winner = None (* no effect *)
                  ; num_accounts = None
                  ; balances = []
                  ; hash = None
                  ; name = None
                  }
                  : Runtime_config.Ledger.t )
              in
              let staking =
                let ({ epoch_ledger; epoch_seed }
                      : Test_config.Epoch_data.Data.t ) =
                  staking_ledger
                in
                if not (ledger_is_prefix epoch_ledger genesis_ledger) then
                  failwith "Staking epoch ledger not a prefix of genesis ledger" ;
                let ledger = ledger_of_epoch_accounts epoch_ledger in
                let seed = epoch_seed in
                ({ ledger; seed } : Runtime_config.Epoch_data.Data.t)
              in
              let next =
                Option.map next ~f:(fun { epoch_ledger; epoch_seed } ->
                    if
                      not
                        (ledger_is_prefix staking_ledger.epoch_ledger
                           epoch_ledger )
                    then
                      failwith
                        "Staking epoch ledger not a prefix of next epoch ledger" ;
                    if not (ledger_is_prefix epoch_ledger genesis_ledger) then
                      failwith
                        "Next epoch ledger not a prefix of genesis ledger" ;
                    let ledger = ledger_of_epoch_accounts epoch_ledger in
                    let seed = epoch_seed in
                    ({ ledger; seed } : Runtime_config.Epoch_data.Data.t) )
              in
              ({ staking; next } : Runtime_config.Epoch_data.t) )
      }
    in
    let genesis_constants =
      Or_error.ok_exn
        (Genesis_ledger_helper.make_genesis_constants ~logger
           ~default:Genesis_constants.compiled runtime_config )
    in
    let constants : Test_config.constants =
      { constraints = constraint_constants; genesis = genesis_constants }
    in
    let mk_net_keypair keypair_name (pk, sk) =
      let keypair =
        { Keypair.public_key = Public_key.decompress_exn pk; private_key = sk }
      in
      Network_keypair.create_network_keypair ~keypair_name ~keypair
    in
    let open Docker_node_config.Services in
    let port_manager = PortManager.create ~min_port:7000 ~max_port:7100 in
    let block_producer_config name keypair =
      let rest_port = PortManager.allocate_port port_manager in
      { name = Block_producer.name ^ "-" ^ name
      ; keypair
      ; libp2p_secret = ""
      ; rest_port
      }
    in
    let block_producer_configs =
      List.map block_producers ~f:(fun node ->
          let keypair =
            match
              List.find genesis_accounts_and_keys
                ~f:(fun ({ account_name; _ }, _keypair) ->
                  String.equal account_name node.account_name )
            with
            | Some (_acct, keypair) ->
                keypair
            | None ->
                let failstring =
                  Format.sprintf
                    "Failing because the account key of all initial block \
                     producers must be in the genesis ledger.  name of Node: \
                     %s.  name of Account which does not exist: %s"
                    node.node_name node.account_name
                in
                failwith failstring
          in
          block_producer_config node.node_name
            (mk_net_keypair node.account_name keypair) )
    in
    let mina_archive_schema = "create_schema.sql" in
    let long_commit_id =
      if String.is_substring Mina_version.commit_id ~substring:"[DIRTY]" then
        String.sub Mina_version.commit_id ~pos:7
          ~len:(String.length Mina_version.commit_id - 7)
      else Mina_version.commit_id
    in
    let mina_archive_base_url =
      "https://raw.githubusercontent.com/MinaProtocol/mina/" ^ long_commit_id
      ^ "/src/app/archive/"
    in
    let mina_archive_schema_aux_files =
      [ mina_archive_base_url ^ "create_schema.sql"
      ; mina_archive_base_url ^ "zkapp_tables.sql"
      ]
    in
    let archive_node_configs =
      (List.init num_archive_nodes) ~f:(fun index ->
          let rest_port = PortManager.allocate_port port_manager in
          { name = Archive_node.name ^ "-" ^ Int.to_string (index + 1)
          ; id = Int.to_string index
          ; schema = mina_archive_schema
          ; schema_aux_files = mina_archive_schema_aux_files
          ; rest_port
          } )
    in
    let genesis_keypairs =
      List.fold genesis_accounts_and_keys ~init:String.Map.empty
        ~f:(fun map ({ account_name; _ }, (pk, sk)) ->
          let keypair = mk_net_keypair account_name (pk, sk) in
          String.Map.add_exn map ~key:account_name ~data:keypair )
    in
    let snark_coordinator_config =
      match snark_coordinator with
      | None ->
          None
      | Some node ->
          let network_kp =
            match String.Map.find genesis_keypairs node.account_name with
            | Some acct ->
                acct
            | None ->
                let failstring =
                  Format.sprintf
                    "Failing because the account key of all initial snark \
                     coordinators must be in the genesis ledger.  name of \
                     Node: %s.  name of Account which does not exist: %s"
                    node.node_name node.account_name
                in
                failwith failstring
          in
          let public_key =
            Public_key.Compressed.to_base58_check
              (Public_key.compress network_kp.keypair.public_key)
          in
          let worker_nodes =
            List.init node.worker_nodes ~f:(fun index ->
                let rest_port = PortManager.allocate_port port_manager in
                { name = Snark_worker.name ^ "-" ^ Int.to_string (index + 1)
                ; public_key
                ; rest_port
                } )
          in
          let snark_coord_rest_port = PortManager.allocate_port port_manager in
          Some
            { name = node.node_name
            ; public_key
            ; snark_worker_fee
            ; worker_nodes
            ; rest_port = snark_coord_rest_port
            }
    in
    let seed_rest_port = PortManager.allocate_port port_manager in
    let seed_configs = [ { name = Seed.name; rest_port = seed_rest_port } ] in
    let docker_volume_configs =
      List.map block_producer_configs ~f:(fun config ->
          { name = "sk-" ^ config.name; data = config.keypair.private_key } )
      @ [ { name = Docker_node_config.Volumes.Runtime_config.name
          ; data =
              Yojson.Safe.to_string (Runtime_config.to_yojson runtime_config)
          }
        ]
    in
    let genesis_keypairs =
      List.fold genesis_accounts_and_keys ~init:String.Map.empty
        ~f:(fun map ({ account_name; _ }, (pk, sk)) ->
          let keypair = mk_net_keypair account_name (pk, sk) in
          String.Map.add_exn map ~key:account_name ~data:keypair )
    in
    (* NETWORK CONFIG *)
    { debug_arg = debug
    ; genesis_keypairs
    ; constants
    ; docker =
        { docker_swarm_version
        ; stack_name
        ; mina_image = images.mina
        ; mina_agent_image = images.user_agent
        ; mina_bots_image = images.bots
        ; mina_points_image = images.points
        ; mina_archive_image = images.archive_node
        ; runtime_config = Runtime_config.to_yojson runtime_config
        ; log_precomputed_blocks (* Node configs *)
        ; block_producer_configs
        ; seed_configs
        ; snark_coordinator_config
        ; archive_node_configs (* Resource configs *)
        ; cpu_request = 1
        ; mem_request = "12Gi"
        ; worker_cpu_request = 6
        ; worker_mem_request = "8Gi"
        ; docker_volume_configs
        }
    }

  (*
     Composes a docker_compose.json file from the network_config specification and writes to disk. This docker_compose
     file contains docker service definitions for each node in the local network. Each node service has different
     configurations which are specified as commands, environment variables, and docker bind volumes.
     We start by creating a runtime config volume to mount to each node service as a bind volume and then continue to create each
     node service. As we create each definition for a service, we specify the docker command, volume, and environment varibles to 
     be used (which are mostly defaults).
  *)
  let to_docker network_config =
    let open Docker_compose.Compose in
    let open Docker_node_config in
    (* RUNTIME CONFIG DOCKER BIND VOLUME *)
    let runtime_config =
      Service.Volume.create Volumes.Runtime_config.name
        Volumes.Runtime_config.container_mount_target
    in
    let entrypoint_script =
      Service.Volume.create "entrypoint.sh" "/root/entrypoint.sh"
    in
    (* BLOCK PRODUCER DOCKER SERVICE *)
    let block_producer_map =
      List.map network_config.docker.block_producer_configs ~f:(fun config ->
          (* BP KEYPAIR CONFIG DOCKER BIND VOLUME *)
          let private_key_config_name = "sk-" ^ config.name in
          let private_key_config =
            Service.Volume.create private_key_config_name
              ("/root/" ^ private_key_config_name)
          in
          let cmd =
            Cmd.(
              Block_producer
                (Block_producer.default
                   ~private_key_config:private_key_config.target ))
          in
          ( config.name
          , { Service.image = network_config.docker.mina_image
            ; volumes =
                [ private_key_config; runtime_config; entrypoint_script ]
            ; command =
                Cmd.create_cmd cmd ~config_file:runtime_config.target
                  ?docker_dns_name:(Some config.name)
            ; entrypoint = Some Services.Block_producer.entrypoint
            ; ports = [ config.rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            ; dns = Service.Dns.default
            ; networks = Service.Network.default
            } ) )
      |> StringMap.of_alist_exn
    in
    (* SNARK COORD/WORKER DOCKER SERVICE *)
    let snark_worker_map =
      match network_config.docker.snark_coordinator_config with
      | None ->
          StringMap.empty
      | Some snark_coordinator_config ->
          let cmd =
            Cmd.(
              Snark_coordinator
                (Snark_coordinator.default
                   ~snark_coordinator_key:snark_coordinator_config.public_key
                   ~snark_worker_fee:snark_coordinator_config.snark_worker_fee ))
          in
          let command =
            Cmd.create_cmd cmd ~config_file:runtime_config.target
              ?docker_dns_name:None
          in
          let environment =
            Service.Environment.create
              (Envs.snark_coord_envs
                 ~snark_coordinator_key:snark_coordinator_config.public_key
                 ~snark_worker_fee:snark_coordinator_config.snark_worker_fee )
          in
          let coordinator =
            ( snark_coordinator_config.name
            , { Service.image = network_config.docker.mina_image
              ; volumes = [ runtime_config; entrypoint_script ]
              ; command
              ; entrypoint = Some Services.Snark_coordinator.entrypoint
              ; ports = [ snark_coordinator_config.rest_port ]
              ; environment
              ; networks = Service.Network.default
              ; dns = Service.Dns.default
              } )
          in
          List.map snark_coordinator_config.worker_nodes
            ~f:(fun worker_config ->
              (* Assign different command and environment configs depending on coordinator or worker *)
              let daemon_address = Services.Snark_coordinator.name in
              let daemon_port = Services.Snark_coordinator.default_port in
              let cmd =
                Cmd.(
                  Snark_worker
                    (Snark_worker.default ~daemon_address ~daemon_port))
              in
              let command =
                Cmd.create_cmd cmd ~config_file:runtime_config.target
                  ?docker_dns_name:None
              in
              let environment = Service.Environment.create [] in
              (* SNARK WORKER KEYPAIR CONFIG DOCKER BIND VOLUME *)
              ( worker_config.name
              , { Service.image = network_config.docker.mina_image
                ; volumes = [ runtime_config; entrypoint_script ]
                ; command
                ; entrypoint = Some Services.Snark_worker.entrypoint
                ; ports = [ worker_config.rest_port ]
                ; environment
                ; networks = Service.Network.default
                ; dns = Service.Dns.default
                } ) )
          @ [ coordinator ]
          |> StringMap.of_alist_exn
    in

    (* ARCHIVE NODE SERVICE *)
    let archive_node_configs =
      List.mapi network_config.docker.archive_node_configs
        ~f:(fun index config ->
          let postgres_uri =
            Cmd.Cli_args.Postgres_uri.create
              ~host:
                ( network_config.docker.stack_name ^ "_"
                ^ Services.Archive_node.postgres_name ^ "-"
                ^ Int.to_string (index + 1) )
            |> Cmd.Cli_args.Postgres_uri.to_string
          in
          let cmd =
            let server_port = Services.Archive_node.server_port in
            Cmd.(Archive_node { Archive_node.postgres_uri; server_port })
          in

          ( config.name
          , { Service.image = network_config.docker.mina_archive_image
            ; volumes = [ runtime_config; entrypoint_script ]
            ; command =
                Cmd.create_cmd cmd ~config_file:runtime_config.target
                  ?docker_dns_name:None
            ; entrypoint = Some Services.Archive_node.entrypoint
            ; ports = [ config.rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            ; networks = Service.Network.default
            ; dns = Service.Dns.default
            } ) )
      (* Add an equal number of postgres containers as archive nodes *)
      @ List.mapi network_config.docker.archive_node_configs ~f:(fun index _ ->
            let pg_config = Cmd.Cli_args.Postgres_uri.default in
            (* Mount the archive schema on the /docker-entrypoint-initdb.d postgres entrypoint *)
            let archive_config =
              Service.Volume.create mina_create_schema
                (Services.Archive_node.entrypoint_target ^/ mina_create_schema)
            in
            ( Services.Archive_node.postgres_name ^ "-"
              ^ Int.to_string (index + 1)
            , { Service.image = postgres_image
              ; volumes = [ archive_config ]
              ; command = []
              ; ports = []
              ; entrypoint = None
              ; networks = Service.Network.default
              ; dns = Service.Dns.default
              ; environment =
                  Service.Environment.create
                    (Envs.postgres_envs ~username:pg_config.username
                       ~password:pg_config.password ~database:pg_config.db
                       ~port:pg_config.port )
              } ) )
      |> StringMap.of_alist_exn
    in
    (* SEED NODE DOCKER SERVICE *)
    let seed_map =
      List.mapi network_config.docker.seed_configs ~f:(fun index config ->
          let command =
            if List.length network_config.docker.archive_node_configs > 0 then
              (* If an archive node is specified in the test plan, use the seed node to connect to the first mina-archive process *)
              Cmd.create_cmd Seed ~config_file:runtime_config.target
                ?docker_dns_name:(Some config.name)
              @ Cmd.Seed.connect_to_archive
                  ~archive_node:
                    ( "archive-"
                    ^ Int.to_string (index + 1)
                    ^ ":" ^ Services.Archive_node.server_port )
            else
              Cmd.create_cmd Seed ~config_file:runtime_config.target
                ?docker_dns_name:(Some config.name)
          in

          ( Services.Seed.name
          , { Service.image = network_config.docker.mina_image
            ; volumes = [ runtime_config; entrypoint_script ]
            ; command
            ; entrypoint = Some Services.Seed.entrypoint
            ; ports = [ config.rest_port ]
            ; environment = Service.Environment.create Envs.base_node_envs
            ; networks = Service.Network.default
            ; dns = Service.Dns.default
            } ) )
      |> StringMap.of_alist_exn
    in

    let services =
      seed_map |> merge block_producer_map |> merge snark_worker_map
      |> merge archive_node_configs
    in
    { version = docker_swarm_version; services; networks = Network.default }
end

module Network_manager = struct
  type t =
    { logger : Logger.t
    ; stack_name : string
    ; graphql_enabled : bool
    ; docker_dir : string
    ; docker_file_path : string
    ; constants : Test_config.constants
    ; seed_workloads : Docker_network.Service_to_deploy.t Core.String.Map.t
    ; block_producer_workloads :
        Docker_network.Service_to_deploy.t Core.String.Map.t
    ; snark_coordinator_workloads :
        Docker_network.Service_to_deploy.t Core.String.Map.t
    ; snark_worker_workloads :
        Docker_network.Service_to_deploy.t Core.String.Map.t
    ; archive_workloads : Docker_network.Service_to_deploy.t Core.String.Map.t
    ; services_by_id : Docker_network.Service_to_deploy.t Core.String.Map.t
    ; mutable deployed : bool
    ; genesis_keypairs : Network_keypair.t Core.String.Map.t
    }

  let get_current_running_stacks =
    let open Malleable_error.Let_syntax in
    let%bind all_stacks_str =
      Util.run_cmd_or_hard_error "/" "docker"
        [ "stack"; "ls"; "--format"; "{{.Name}}" ]
    in
    return (String.split ~on:'\n' all_stacks_str)

  let remove_stack_if_exists ~logger (network_config : Network_config.t) =
    let open Malleable_error.Let_syntax in
    let%bind all_stacks = get_current_running_stacks in
    if List.mem all_stacks network_config.docker.stack_name ~equal:String.equal
    then
      let%bind () =
        if network_config.debug_arg then
          Deferred.bind ~f:Malleable_error.return
            (Util.prompt_continue
               "Existing stack name of same name detected, pausing startup. \
                Enter [y/Y] to continue on and remove existing stack name, \
                start clean, and run the test; press Ctrl-C to quit out: " )
        else
          Malleable_error.return
            ([%log info]
               "Existing stack of same name detected; removing to start clean" )
      in
      Util.run_cmd_or_hard_error "/" "docker"
        [ "stack"; "rm"; network_config.docker.stack_name; "--force" ]
      >>| Fn.const ()
    else return ()

  let generate_docker_stack_file ~logger ~docker_dir ~network_config =
    let open Deferred.Let_syntax in
    let%bind () =
      if%bind File_system.dir_exists docker_dir then (
        [%log info] "Old docker stack directory found; removing to start clean" ;
        File_system.remove_dir docker_dir )
      else return ()
    in
    let%bind () = Unix.mkdir docker_dir in
    [%log info] "Writing network configuration %s" docker_dir ;
    Out_channel.with_file ~fail_if_exists:true
      (docker_dir ^/ Docker_compose.Compose.compose_file_name) ~f:(fun ch ->
        Network_config.to_docker network_config
        |> Docker_compose.to_string
        |> Out_channel.output_string ch ) ;
    [%log info]
      "Writing out the genesis keys (in case you want to use them manually) to \
       testnet dir %s"
      docker_dir ;
    let kps_base_path = String.concat [ docker_dir; "/genesis_keys" ] in
    let%bind () = Unix.mkdir kps_base_path in
    let%bind () =
      Core.String.Map.iter network_config.genesis_keypairs ~f:(fun kp ->
          Network_keypair.to_yojson kp
          |> Yojson.Safe.to_file
               (String.concat [ kps_base_path; "/"; kp.keypair_name; ".json" ]) )
      |> Deferred.return
    in
    [%log info]
      "Writing custom entrypoint script (libp2p key generation and puppeteer \
       context)" ;
    let entrypoint_filename, entrypoint_script =
      Docker_compose.Compose.entrypoint_script
    in
    Out_channel.with_file ~fail_if_exists:true
      (docker_dir ^/ entrypoint_filename) ~f:(fun ch ->
        entrypoint_script |> Out_channel.output_string ch ) ;
    ignore (Util.run_cmd_exn docker_dir "chmod" [ "+x"; entrypoint_filename ]) ;
    List.iter network_config.docker.docker_volume_configs ~f:(fun config ->
        [%log info] "Writing volume config: %s" (docker_dir ^/ config.name) ;
        Out_channel.with_file ~fail_if_exists:false (docker_dir ^/ config.name)
          ~f:(fun ch -> config.data |> Out_channel.output_string ch) ;
        ignore (Util.run_cmd_exn docker_dir "chmod" [ "600"; config.name ]) ) ;
    return ()

  let initialize_workloads ~logger (network_config : Network_config.t) =
    let seed_workloads =
      List.map network_config.docker.seed_configs ~f:(fun seed_config ->
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name seed_config.name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:None ~has_archive_container:false
                 ~graphql_port:seed_config.rest_port )
          in
          (seed_config.name, node) )
      |> Core.String.Map.of_alist_exn
    in
    let block_producer_workloads =
      List.map network_config.docker.block_producer_configs ~f:(fun bp_config ->
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name bp_config.name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:(Some bp_config.keypair)
                 ~has_archive_container:false ~graphql_port:bp_config.rest_port )
          in
          (bp_config.name, node) )
      |> Core.String.Map.of_alist_exn
    in
    let snark_coordinator_workloads, snark_worker_workloads =
      match network_config.docker.snark_coordinator_config with
      | Some snark_coordinator_config ->
          let snark_coordinator_workloads =
            if List.length snark_coordinator_config.worker_nodes > 0 then
              let coordinator =
                Docker_network.Service_to_deploy.construct_service
                  network_config.docker.stack_name snark_coordinator_config.name
                  (Docker_network.Service_to_deploy
                   .init_service_to_deploy_config ~network_keypair:None
                     ~has_archive_container:false
                     ~graphql_port:snark_coordinator_config.rest_port )
              in
              [ (snark_coordinator_config.name, coordinator) ]
              |> Core.String.Map.of_alist_exn
            else Core.String.Map.empty
          in
          let snark_worker_workloads =
            List.map snark_coordinator_config.worker_nodes
              ~f:(fun snark_worker_config ->
                let worker =
                  Docker_network.Service_to_deploy.construct_service
                    network_config.docker.stack_name snark_worker_config.name
                    (Docker_network.Service_to_deploy
                     .init_service_to_deploy_config ~network_keypair:None
                       ~has_archive_container:false
                       ~graphql_port:snark_worker_config.rest_port )
                in

                (snark_worker_config.name, worker) )
            |> Core.String.Map.of_alist_exn
          in
          (snark_coordinator_workloads, snark_worker_workloads)
      | None ->
          (Core.String.Map.of_alist_exn [], Core.String.Map.of_alist_exn [])
    in
    let archive_workloads =
      List.map network_config.docker.archive_node_configs
        ~f:(fun archive_config ->
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name archive_config.name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:None ~has_archive_container:true
                 ~graphql_port:archive_config.rest_port )
          in
          (archive_config.name, node) )
      |> Core.String.Map.of_alist_exn
    in
    ( seed_workloads
    , block_producer_workloads
    , snark_coordinator_workloads
    , snark_worker_workloads
    , archive_workloads )

  let create ~logger (network_config : Network_config.t) =
    let open Malleable_error.Let_syntax in
    let%bind () = remove_stack_if_exists ~logger network_config in
    let ( seed_workloads
        , block_producer_workloads
        , snark_coordinator_workloads
        , snark_worker_workloads
        , archive_workloads ) =
      initialize_workloads ~logger network_config
    in
    let services_by_id =
      let all_workloads =
        Core.String.Map.data seed_workloads
        @ Core.String.Map.data snark_coordinator_workloads
        @ Core.String.Map.data snark_worker_workloads
        @ Core.String.Map.data block_producer_workloads
        @ Core.String.Map.data archive_workloads
      in
      all_workloads
      |> List.map ~f:(fun w -> (w.service_name, w))
      |> String.Map.of_alist_exn
    in
    let open Deferred.Let_syntax in
    let docker_dir = network_config.docker.stack_name in
    let%bind () =
      generate_docker_stack_file ~logger ~docker_dir ~network_config
    in
    let t =
      { stack_name = network_config.docker.stack_name
      ; logger
      ; docker_dir
      ; docker_file_path = Docker_compose.Compose.compose_file_name
      ; constants = network_config.constants
      ; graphql_enabled = true
      ; seed_workloads
      ; block_producer_workloads
      ; snark_coordinator_workloads
      ; snark_worker_workloads
      ; archive_workloads
      ; services_by_id
      ; deployed = false
      ; genesis_keypairs = network_config.genesis_keypairs
      }
    in
    [%log info] "Initializing docker swarm" ;
    Malleable_error.return t

  let deploy t =
    let logger = t.logger in
    if t.deployed then failwith "network already deployed" ;
    [%log info] "Deploying network from %s" t.docker_dir ;
    [%log info] "Deploying stack: %s" t.stack_name ;
    let open Malleable_error.Let_syntax in
    let%bind (_ : string) =
      Util.run_cmd_or_hard_error t.docker_dir "docker"
        [ "stack"; "deploy"; "-c"; t.docker_file_path; t.stack_name ]
    in
    t.deployed <- true ;

    (* Wait for stack to be deployed *)
    let%bind () = Deferred.bind ~f:return (after (Time.Span.of_ms 6000.)) in
    let config : Docker_network.config =
      { stack_name = t.stack_name; graphql_enabled = t.graphql_enabled }
    in
    let func_for_fold ~(key : string) ~data accum_M =
      let%bind mp = accum_M in
      let%map node =
        Docker_network.Service_to_deploy.get_node_from_service data ~config
      in
      Core.String.Map.add_exn mp ~key ~data:node
    in
    let%map seeds =
      Core.String.Map.fold t.seed_workloads
        ~init:(Malleable_error.return Core.String.Map.empty)
        ~f:func_for_fold
    and block_producers =
      Core.String.Map.fold t.block_producer_workloads
        ~init:(Malleable_error.return Core.String.Map.empty)
        ~f:func_for_fold
    and snark_coordinators =
      Core.String.Map.fold t.snark_coordinator_workloads
        ~init:(Malleable_error.return Core.String.Map.empty)
        ~f:func_for_fold
    and snark_workers =
      Core.String.Map.fold t.snark_worker_workloads
        ~init:(Malleable_error.return Core.String.Map.empty)
        ~f:func_for_fold
    and archive_nodes =
      Core.String.Map.fold t.archive_workloads
        ~init:(Malleable_error.return Core.String.Map.empty)
        ~f:func_for_fold
    in
    let network =
      { Docker_network.namespace = t.stack_name
      ; constants = t.constants
      ; seeds
      ; block_producers
      ; snark_coordinators
      ; snark_workers
      ; archive_nodes
      ; genesis_keypairs = t.genesis_keypairs
      }
    in
    let nodes_to_string =
      Fn.compose (String.concat ~sep:", ") (List.map ~f:Docker_network.Node.id)
    in
    [%log info] "Network deployed" ;
    [%log info] "testnet namespace: %s" t.stack_name ;
    [%log info] "snark coordinators: %s"
      (nodes_to_string (Core.String.Map.data network.snark_coordinators)) ;
    [%log info] "snark workers: %s"
      (nodes_to_string (Core.String.Map.data network.snark_workers)) ;
    [%log info] "block producers: %s"
      (nodes_to_string (Core.String.Map.data network.block_producers)) ;
    [%log info] "archive nodes: %s"
      (nodes_to_string (Core.String.Map.data network.archive_nodes)) ;
    network

  let destroy t =
    [%log' info t.logger] "Destroying network" ;
    if not t.deployed then failwith "network not deployed" ;
    let%bind _ =
      Util.run_cmd_exn "/" "docker" [ "stack"; "rm"; t.stack_name ]
    in
    t.deployed <- false ;
    Deferred.unit

  let cleanup t =
    let%bind () = if t.deployed then destroy t else return () in
    [%log' info t.logger] "Cleaning up network configuration" ;
    let%bind () = File_system.remove_dir t.docker_dir in
    Deferred.unit

  let destroy t =
    Deferred.Or_error.try_with ~here:[%here] (fun () -> destroy t)
    |> Deferred.bind ~f:Malleable_error.or_hard_error
end
