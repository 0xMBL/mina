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

  type docker_config =
    { docker_swarm_version : string
    ; stack_name : string
    ; mina_image : string
    ; mina_agent_image : string
    ; mina_bots_image : string
    ; mina_points_image : string
    ; mina_archive_image : string
    ; runtime_config : Yojson.Safe.t
    ; seed_configs : Docker_node_config.Seed_config.t list
    ; block_producer_configs : Docker_node_config.Block_producer_config.t list
    ; snark_coordinator_config :
        Docker_node_config.Snark_coordinator_config.t option
    ; archive_node_configs : Docker_node_config.Archive_node_config.t list
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
    let user_from_env = Option.value (Unix.getenv "USER") ~default:"auto" in
    let user_sanitized =
      Str.global_replace (Str.regexp "\\W|_-") "" user_from_env
    in
    let user_len = Int.min 5 (String.length user_sanitized) in
    let user = String.sub user_sanitized ~pos:0 ~len:user_len in
    let git_commit = Mina_version.commit_id_short in
    let stack_name = "it-" ^ user ^ "-" ^ git_commit ^ "-" ^ test_name in
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
                (* because we run integration tests with Proof_level = full, the winner account
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
    let genesis_keypairs =
      List.fold genesis_accounts_and_keys ~init:String.Map.empty
        ~f:(fun map ({ account_name; _ }, (pk, sk)) ->
          let keypair = mk_net_keypair account_name (pk, sk) in
          String.Map.add_exn map ~key:account_name ~data:keypair )
    in
    let open Docker_node_config in
    let open Docker_compose.Dockerfile in
    let port_manager = PortManager.create ~min_port:10000 ~max_port:10100 in
    let docker_volumes =
      [ Base_node_config.runtime_config_volume
      ; Base_node_config.entrypoint_volume
      ]
    in
    let seed_config_peer =
      Some (Base_node_config.create_peer ~stack_name ~peer_name:"seed")
    in
    let archive_node_configs =
      List.init num_archive_nodes ~f:(fun index ->
          let postgres_target_port = 5432 in
          let connection_info =
            Postgres_config.create_connection_info
              ~host:("postgres" ^ "_" ^ Int.to_string (index + 1))
              ~username:"postgres" ~password:"password" ~database:"archive"
              ~port:5432
          in
          let postgres_port =
            Service.Port.create
              ~published:(PortManager.allocate_port port_manager)
              ~target:postgres_target_port
          in
          let postgres_config =
            Postgres_config.create ~service_name:connection_info.host
              ~image:postgres_image ~ports:[ postgres_port ]
              ~volumes:
                [ Postgres_config.archive_create_schema_volume
                ; Postgres_config.archive_zkapp_schema_volume
                ; Postgres_config.archive_entrypoint_volume
                ]
              ~connection_info
          in
          let server_port =
            Service.Port.create
              ~published:(PortManager.allocate_port port_manager)
              ~target:3086
          in
          let rest_port =
            Service.Port.create
              ~published:(PortManager.allocate_port port_manager)
              ~target:3085
          in
          let postgres_uri =
            Postgres_config.create_connection_uri connection_info
          in
          Archive_node_config.create
            ~service_name:("archive_" ^ Int.to_string (index + 1))
            ~image:images.archive_node ~ports:[ server_port; rest_port ]
            ~volumes:docker_volumes
            ~config_file:Base_node_config.runtime_config_volume.target
            ~server_port:server_port.target ~schema:mina_archive_schema
            ~schema_aux_files:mina_archive_schema_aux_files ~postgres_config
            ~postgres_uri )
    in
    let archive_address =
      match archive_node_configs with
      | [] ->
          None
      | xs ->
          let archive_address = List.hd_exn xs in
          Some (archive_address.service_name ^ ":3086")
    in
    let seed_configs =
      [ Seed_config.create ~service_name:"seed" ~image:images.mina
          ~ports:(PortManager.allocate_ports_for_node port_manager)
          ~volumes:docker_volumes
          ~config_file:Base_node_config.runtime_config_volume.target ~peer:None
          ~archive_address
      ]
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
                keypair |> mk_net_keypair node.account_name
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
          let volumes =
            [ Service.Volume.create
                ("keys" ^/ node.account_name)
                ("/root/keys" ^/ node.account_name)
            ]
            @ docker_volumes
          in
          Block_producer_config.create ~service_name:node.node_name
            ~image:images.mina
            ~ports:(PortManager.allocate_ports_for_node port_manager)
            ~volumes ~keypair ~libp2p_secret:""
            ~config_file:Base_node_config.runtime_config_volume.target
            ~peer:seed_config_peer )
    in
    let snark_coordinator_config =
      match snark_coordinator with
      | None ->
          None
      | Some snark_coordinator_node ->
          let network_kp =
            match
              String.Map.find genesis_keypairs
                snark_coordinator_node.account_name
            with
            | Some acct ->
                acct
            | None ->
                let failstring =
                  Format.sprintf
                    "Failing because the account key of all initial snark \
                     coordinators must be in the genesis ledger.  name of \
                     Node: %s.  name of Account which does not exist: %s"
                    snark_coordinator_node.node_name
                    snark_coordinator_node.account_name
                in
                failwith failstring
          in
          let public_key =
            Public_key.Compressed.to_base58_check
              (Public_key.compress network_kp.keypair.public_key)
          in
          let coordinator_ports =
            PortManager.allocate_ports_for_node port_manager
          in
          let daemon_port =
            coordinator_ports |> List.find_exn ~f:(fun p -> p.target = 8301)
          in
          let worker_nodes =
            List.init snark_coordinator_node.worker_nodes ~f:(fun index ->
                Docker_node_config.Snark_worker_config.create
                  ~service_name:
                    ("snark-worker" ^ "_" ^ Int.to_string (index + 1))
                  ~image:images.mina
                  ~ports:
                    (Docker_node_config.PortManager.allocate_ports_for_node
                       port_manager )
                  ~volumes:docker_volumes
                  ~daemon_port:(Int.to_string daemon_port.target)
                  ~daemon_address:snark_coordinator_node.node_name )
          in
          Some
            (Snark_coordinator_config.create
               ~service_name:snark_coordinator_node.node_name ~image:images.mina
               ~ports:coordinator_ports ~volumes:docker_volumes
               ~config_file:Base_node_config.runtime_config_volume.target
               ~snark_worker_fee ~worker_nodes ~snark_coordinator_key:public_key
               ~peer:seed_config_peer )
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
    let open Docker_compose.Dockerfile in
    let block_producer_map =
      List.map network_config.docker.block_producer_configs ~f:(fun config ->
          (config.service_name, config.docker_config) )
      |> StringMap.of_alist_exn
    in
    let seed_map =
      List.map network_config.docker.seed_configs ~f:(fun config ->
          (config.service_name, config.docker_config) )
      |> StringMap.of_alist_exn
    in

    let snark_coordinator_map =
      match network_config.docker.snark_coordinator_config with
      | Some config ->
          StringMap.of_alist_exn [ (config.service_name, config.docker_config) ]
      | None ->
          StringMap.empty
    in

    let snark_worker_map =
      match network_config.docker.snark_coordinator_config with
      | Some config ->
          List.map config.specific_config.worker_nodes ~f:(fun config ->
              (config.service_name, config.docker_config) )
          |> StringMap.of_alist_exn
      | None ->
          StringMap.empty
    in
    let archive_node_map =
      List.map network_config.docker.archive_node_configs ~f:(fun config ->
          (config.service_name, config.docker_config) )
      |> StringMap.of_alist_exn
    in
    let postgres_map =
      List.map network_config.docker.archive_node_configs ~f:(fun config ->
          let config = config.specific_config.postgres_config in
          (config.service_name, config.docker_config) )
      |> StringMap.of_alist_exn
    in
    let services =
      seed_map |> merge block_producer_map
      |> merge snark_coordinator_map
      |> merge snark_worker_map |> merge archive_node_map |> merge postgres_map
    in
    { version = docker_swarm_version; services }
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
        [ "stack"; "rm"; network_config.docker.stack_name ]
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
    [%log info] "Writing docker configuration %s" docker_dir ;
    Out_channel.with_file ~fail_if_exists:true
      (docker_dir ^/ Docker_compose.Dockerfile.compose_file_name) ~f:(fun ch ->
        Network_config.to_docker network_config
        |> Docker_compose.to_string
        |> Out_channel.output_string ch ) ;
    [%log info]
      "Writing out the genesis keys (in case you want to use them manually) to \
       testnet dir %s"
      docker_dir ;
    let kps_base_path = String.concat [ docker_dir; "/keys" ] in
    let%bind () = Unix.mkdir kps_base_path in
    let%bind () =
      ignore (Util.run_cmd_exn docker_dir "chmod" [ "700"; "keys" ])
      |> Deferred.return
    in
    [%log info] "Writing genesis keys to %s" kps_base_path ;
    let%bind () =
      Core.String.Map.iter network_config.genesis_keypairs ~f:(fun kp ->
          let keypath = String.concat [ kps_base_path; "/"; kp.keypair_name ] in
          (* write keypair.private_key to filesystem instead *)
          Out_channel.with_file ~fail_if_exists:true keypath ~f:(fun ch ->
              kp.private_key |> Out_channel.output_string ch ) ;
          Out_channel.with_file ~fail_if_exists:true (keypath ^ ".pub")
            ~f:(fun ch -> kp.public_key |> Out_channel.output_string ch) ;
          ignore
            (Util.run_cmd_exn kps_base_path "chmod" [ "600"; kp.keypair_name ]) )
      |> Deferred.return
    in
    [%log info]
      "Writing custom entrypoint script (libp2p key generation and puppeteer \
       context)" ;
    let entrypoint_filename, entrypoint_script =
      Docker_node_config.Base_node_config.entrypoint_script
    in
    Out_channel.with_file ~fail_if_exists:true
      (docker_dir ^/ entrypoint_filename) ~f:(fun ch ->
        entrypoint_script |> Out_channel.output_string ch ) ;
    ignore (Util.run_cmd_exn docker_dir "chmod" [ "+x"; entrypoint_filename ]) ;
    [%log info]
      "Writing custom postgres entrypoint script (libp2p key generation and \
       puppeteer context)" ;
    let entrypoint_filename, entrypoint_script =
      Docker_node_config.Postgres_config.postgres_script
    in
    Out_channel.with_file ~fail_if_exists:true
      (docker_dir ^/ entrypoint_filename) ~f:(fun ch ->
        entrypoint_script |> Out_channel.output_string ch ) ;
    ignore (Util.run_cmd_exn docker_dir "chmod" [ "+x"; entrypoint_filename ]) ;
    [%log info] "Writing postgres schema files" ;
    let%bind () =
      ignore
        (Util.run_cmd_or_hard_error docker_dir "curl"
           [ "-o"
           ; "create_schema.sql"
           ; "https://raw.githubusercontent.com/MinaProtocol/mina/develop/src/app/archive/create_schema.sql"
           ] )
      |> Deferred.return
    in
    let%bind () =
      ignore
        (Util.run_cmd_or_hard_error docker_dir "curl"
           [ "-o"
           ; "zkapp_tables.sql"
           ; "https://raw.githubusercontent.com/MinaProtocol/mina/develop/src/app/archive/zkapp_tables.sql"
           ] )
      |> Deferred.return
    in
    [%log info] "Writing runtime_config %s" docker_dir ;
    let%bind () =
      Yojson.Safe.to_file
        (String.concat [ docker_dir; "/runtime_config.json" ])
        network_config.docker.runtime_config
      |> Deferred.return
    in
    let%bind () = Deferred.bind ~f:return (after (Time.Span.of_ms 8000.)) in
    return ()

  let initialize_workloads ~logger (network_config : Network_config.t) =
    let find_rest_port ports =
      List.find_map_exn ports ~f:(fun port ->
          match port with
          | Docker_compose.Dockerfile.Service.Port.{ published; target } ->
              if target = 3085 then Some published else None )
    in
    [%log info] "Initializing seed workloads" ;
    let seed_workloads =
      List.map network_config.docker.seed_configs ~f:(fun seed_config ->
          let graphql_port = find_rest_port seed_config.docker_config.ports in
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name seed_config.service_name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:None ~has_archive_container:false
                 ~graphql_port )
          in
          (seed_config.service_name, node) )
      |> Core.String.Map.of_alist_exn
    in
    [%log info] "Initializing block producer workloads" ;
    let block_producer_workloads =
      List.map network_config.docker.block_producer_configs ~f:(fun bp_config ->
          let graphql_port = find_rest_port bp_config.docker_config.ports in
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name bp_config.service_name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:(Some bp_config.specific_config.keypair)
                 ~has_archive_container:false ~graphql_port )
          in
          (bp_config.service_name, node) )
      |> Core.String.Map.of_alist_exn
    in
    [%log info] "Initializing snark coordinator and worker workloads" ;
    let snark_coordinator_workloads, snark_worker_workloads =
      match network_config.docker.snark_coordinator_config with
      | Some snark_coordinator_config ->
          let snark_coordinator_workloads =
            if
              List.length snark_coordinator_config.specific_config.worker_nodes
              > 0
            then
              let graphql_port =
                find_rest_port snark_coordinator_config.docker_config.ports
              in
              let coordinator =
                Docker_network.Service_to_deploy.construct_service
                  network_config.docker.stack_name
                  snark_coordinator_config.service_name
                  (Docker_network.Service_to_deploy
                   .init_service_to_deploy_config ~network_keypair:None
                     ~has_archive_container:false ~graphql_port )
              in
              [ (snark_coordinator_config.service_name, coordinator) ]
              |> Core.String.Map.of_alist_exn
            else Core.String.Map.empty
          in
          let snark_worker_workloads =
            List.map snark_coordinator_config.specific_config.worker_nodes
              ~f:(fun snark_worker_config ->
                let graphql_port =
                  find_rest_port snark_worker_config.docker_config.ports
                in
                let worker =
                  Docker_network.Service_to_deploy.construct_service
                    network_config.docker.stack_name
                    snark_worker_config.service_name
                    (Docker_network.Service_to_deploy
                     .init_service_to_deploy_config ~network_keypair:None
                       ~has_archive_container:false ~graphql_port )
                in

                (snark_worker_config.service_name, worker) )
            |> Core.String.Map.of_alist_exn
          in
          (snark_coordinator_workloads, snark_worker_workloads)
      | None ->
          (Core.String.Map.of_alist_exn [], Core.String.Map.of_alist_exn [])
    in
    [%log info] "Initializing archive node workloads" ;
    let archive_workloads =
      List.map network_config.docker.archive_node_configs
        ~f:(fun archive_config ->
          let graphql_port =
            find_rest_port archive_config.docker_config.ports
          in
          let node =
            Docker_network.Service_to_deploy.construct_service
              network_config.docker.stack_name archive_config.service_name
              (Docker_network.Service_to_deploy.init_service_to_deploy_config
                 ~network_keypair:None ~has_archive_container:true ~graphql_port )
          in
          (archive_config.service_name, node) )
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
      ; docker_file_path = Docker_compose.Dockerfile.compose_file_name
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
    (* TODO: Make this better, Wait for stack to be deployed *)
    let%bind () = Deferred.bind ~f:return (after (Time.Span.of_ms 15000.)) in
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
