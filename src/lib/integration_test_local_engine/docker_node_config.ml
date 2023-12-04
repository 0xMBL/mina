open Core_kernel
open Async
open Integration_test_lib
open Docker_compose

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
        port

  let allocate_ports_for_node t =
    let rest_port_source = allocate_port t in
    let client_port_source = allocate_port t in
    let metrics_port_source = allocate_port t in
    let rest_port_target = 3085 in
    let client_port_target = 8301 in
    let metrics_port_target = 10001 in
    [ { Dockerfile.Service.Port.published = rest_port_source
      ; target = rest_port_target
      }
    ; { published = client_port_source; target = client_port_target }
    ; { published = metrics_port_source; target = metrics_port_target }
    ]

  let release_port t port =
    t.used_ports <- List.filter t.used_ports ~f:(fun p -> p <> port) ;
    t.available_ports <- port :: t.available_ports

  let get_latest_used_port t =
    match t.used_ports with [] -> failwith "No used ports" | port :: _ -> port
end

module Base_node_config = struct
  type base_config =
    { peer : string option
    ; log_level : string
    ; log_snark_work_gossip : bool
    ; log_txn_pool_gossip : bool
    ; generate_genesis_proof : bool
    ; client_port : string
    ; rest_port : string
    ; metrics_port : string
    ; config_file : string
    ; libp2p_key_path : string
    }
  [@@deriving to_yojson]

  let create_peer ~dns_name =
    Printf.sprintf
      "/dns4/%s/tcp/10101/p2p/12D3KooWCoGWacXE4FRwAX8VqhnWVKhz5TTEecWEuGmiNrDt2XLf"
      dns_name

  let default ~config_file ?dns_name =
    let peer_option =
      match dns_name with
      | Some name ->
          Some (create_peer ~dns_name:name)
      | None ->
          None
    in
    { log_snark_work_gossip = true
    ; log_txn_pool_gossip = true
    ; generate_genesis_proof = true
    ; log_level = "Debug"
    ; client_port = "8301"
    ; rest_port = "3085"
    ; metrics_port = "10001"
    ; config_file
    ; libp2p_key_path = "/root/keys/libp2p_key"
    ; peer = peer_option
    }

  let to_list t =
    let base_args =
      [ "-config-file"
      ; t.config_file
      ; "-log-level"
      ; t.log_level
      ; "-log-snark-work-gossip"
      ; Bool.to_string t.log_snark_work_gossip
      ; "-log-txn-pool-gossip"
      ; Bool.to_string t.log_txn_pool_gossip
      ; "-generate-genesis-proof"
      ; Bool.to_string t.generate_genesis_proof
      ; "-client-port"
      ; t.client_port
      ; "-rest-port"
      ; t.rest_port
      ; "-metrics-port"
      ; t.metrics_port
      ; "--libp2p-keypair"
      ; t.libp2p_key_path
      ; "-log-json"
      ; "--insecure-rest-server"
      ]
    in
    let peer_args =
      match t.peer with Some peer -> [ "-peer"; peer ] | None -> []
    in
    List.concat [ base_args; peer_args ]
end

module type Node_config_intf = sig
  open Dockerfile.Service

  type t [@@deriving to_yojson]

  type base_config [@@deriving to_yojson]

  type specific_config [@@deriving to_yojson]

  val create_specific_command : specific_config -> string list

  val create_cmd : base_config -> specific_config -> string list

  val create_docker_config :
       image:string
    -> entrypoint:string list option
    -> networks:Network.t list
    -> ports:Port.t list
    -> volumes:Volume.t list
    -> environment:Environment.t
    -> base_config:base_config
    -> specific_config:specific_config
    -> Dockerfile.Service.t
end

module Block_producer_config = struct
  type specific_config =
    { keypair : Network_keypair.t
    ; priv_key_path : string
    ; libp2p_secret : string
    ; enable_flooding : bool
    ; enable_peer_exchange : bool
    }
  [@@deriving to_yojson]

  type base_config = Base_node_config.base_config [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let create_specific_command specific_config =
    [ "daemon"
    ; "-block-producer-key"
    ; specific_config.priv_key_path
    ; "-enable-flooding"
    ; specific_config.enable_flooding |> Bool.to_string
    ; "-enable-peer-exchange"
    ; specific_config.enable_peer_exchange |> Bool.to_string
    ]

  let create_cmd base_config specific_config =
    let base_args = Base_node_config.to_list base_config in
    let block_producer_args = create_specific_command specific_config in
    List.concat [ block_producer_args; base_args ]

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~config_file
      ~keypair ~libp2p_secret =
    let service_name = stack_name ^ "_" ^ name in
    (* TODO: make this better *)
    let priv_key_path = "/root/keys/" ^ name in
    let base_config =
      Base_node_config.default ~config_file ~dns_name:service_name
    in
    let specific_config =
      { keypair
      ; priv_key_path
      ; libp2p_secret
      ; enable_flooding = true
      ; enable_peer_exchange = true
      }
    in
    let entrypoint = Some [ "/root/entrypoint.sh" ] in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes
        ~environment:Dockerfile.Service.Environment.default ~entrypoint
        ~base_config ~specific_config
    in
    { service_name = name; base_config; specific_config; docker_config }
end

module Seed_config = struct
  type specific_config = unit [@@deriving to_yojson]

  type base_config = Base_node_config.base_config [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let create_specific_command () = [ "daemon"; "-seed" ]

  let create_cmd base_config specific_config =
    let base_args = Base_node_config.to_list base_config in
    let seed_args = create_specific_command specific_config in
    List.concat [ seed_args; base_args ]

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~config_file =
    let service_name = stack_name ^ "_" ^ name in
    let base_config =
      Base_node_config.default ~config_file ~dns_name:service_name
    in
    let entrypoint = Some [ "/root/entrypoint.sh" ] in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes
        ~environment:Dockerfile.Service.Environment.default ~entrypoint
        ~base_config ~specific_config:()
    in
    { service_name = name; base_config; specific_config = (); docker_config }
end

module Snark_worker_config = struct
  type specific_config =
    { daemon_address : string; daemon_port : string; proof_level : string }
  [@@deriving to_yojson]

  type base_config = Base_node_config.base_config [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let create_specific_command specific_config =
    [ "internal"
    ; "snark-worker"
    ; "-proof-level"
    ; specific_config.proof_level
    ; "-daemon-address"
    ; specific_config.daemon_address ^ ":" ^ specific_config.daemon_port
    ]

  let create_cmd base_config specific_config =
    let base_args = Base_node_config.to_list base_config in
    let snark_worker_args = create_specific_command specific_config in
    List.concat [ snark_worker_args; base_args ]

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~config_file
      ~daemon_address ~daemon_port =
    let service_name = stack_name ^ "_" ^ name in
    let base_config =
      Base_node_config.default ~config_file ~dns_name:service_name
    in
    let specific_config =
      { daemon_address; daemon_port; proof_level = "Full" }
    in
    let entrypoint = Some [ "/root/entrypoint.sh" ] in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes
        ~environment:Dockerfile.Service.Environment.default ~entrypoint
        ~base_config ~specific_config
    in
    { service_name = name; base_config; specific_config; docker_config }
end

module Snark_coordinator_config = struct
  type specific_config =
    { snark_coordinator_key : string
    ; snark_worker_fee : string
    ; work_selection : string
    ; worker_nodes : Snark_worker_config.t list
    }
  [@@deriving to_yojson]

  type base_config = Base_node_config.base_config [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let create_specific_command specific_config =
    [ "daemon"
    ; "-run-snark-coordinator"
    ; specific_config.snark_coordinator_key
    ; "-snark-worker-fee"
    ; specific_config.snark_worker_fee
    ; "-work-selection"
    ; specific_config.work_selection
    ]

  let snark_coordinator_default_env ~snark_coordinator_key ~snark_worker_fee =
    [ ("MINA_SNARK_KEY", snark_coordinator_key)
    ; ("MINA_SNARK_FEE", snark_worker_fee)
    ; ("WORK_SELECTION", "seq")
    ]

  let create_cmd base_config specific_config =
    let base_args = Base_node_config.to_list base_config in
    let snark_coordinator_args = create_specific_command specific_config in
    List.concat [ snark_coordinator_args; base_args ]

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~config_file
      ~snark_coordinator_key ~snark_worker_fee ~worker_nodes =
    let service_name = stack_name ^ "_" ^ name in
    let base_config =
      Base_node_config.default ~config_file ~dns_name:service_name
    in
    let specific_config =
      { snark_coordinator_key
      ; snark_worker_fee
      ; work_selection = "seq"
      ; worker_nodes
      }
    in
    let entrypoint = Some [ "/root/entrypoint.sh" ] in
    let environment =
      snark_coordinator_default_env ~snark_coordinator_key ~snark_worker_fee
    in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes ~environment
        ~entrypoint ~base_config ~specific_config
    in
    { service_name = name; base_config; specific_config; docker_config }
end

module Postgres_config = struct
  type specific_config =
    { host : string
    ; username : string
    ; password : string
    ; database : string
    ; port : int
    }
  [@@deriving to_yojson]

  type base_config = unit [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let postgres_default_envs ~username ~password ~database ~port =
    [ ("BITNAMI_DEBUG", "false")
    ; ("POSTGRES_USER", username)
    ; ("POSTGRES_PASSWORD", password)
    ; ("POSTGRES_DB", database)
    ; ("POSTGRESQL_PORT_NUMBER", port)
    ; ("POSTGRESQL_ENABLE_LDAP", "no")
    ; ("POSTGRESQL_ENABLE_TLS", "no")
    ; ("POSTGRESQL_LOG_HOSTNAME", "false")
    ; ("POSTGRESQL_LOG_CONNECTIONS", "false")
    ; ("POSTGRESQL_LOG_DISCONNECTIONS", "false")
    ; ("POSTGRESQL_PGAUDIT_LOG_CATALOG", "off")
    ; ("POSTGRESQL_CLIENT_MIN_MESSAGES", "error")
    ; ("POSTGRESQL_SHARED_PRELOAD_LIBRARIES", "pgaudit")
    ; ("POSTGRES_HOST_AUTH_METHOD", "trust")
    ]

  let create_connection_uri { host; username; password; database; port } =
    Printf.sprintf "postgres://%s:%s@%s:%s/%s" username password host
      (Int.to_string port) database

  let create_specific_command _specific_config = []

  let create_cmd base_config specific_config =
    let base_args = match base_config with () -> [] in
    let archive_node_args = create_specific_command specific_config in
    List.concat [ archive_node_args; base_args ]

  let create_connection_info ~host ~username ~password ~database ~port =
    { host; username; password; database; port }

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  (**
    (* Mount the archive schema on the /docker-entrypoint-initdb.d postgres entrypoint *)
            let archive_config =
              Service.Volume.create mina_create_schema
                (Services.Archive_node.entrypoint_target ^/ mina_create_schema)
            in    
    **)
  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~connection_info
      =
    let _service_name = stack_name ^ "_" ^ name in
    let base_config = () in
    let entrypoint = None in
    let environment =
      postgres_default_envs ~username:connection_info.username
        ~password:connection_info.password ~database:connection_info.database
        ~port:(Int.to_string connection_info.port)
    in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes ~environment
        ~entrypoint ~base_config ~specific_config:connection_info
    in
    { service_name = name
    ; base_config
    ; specific_config = connection_info
    ; docker_config
    }
end

module Archive_node_config = struct
  type specific_config =
    { server_port : int
    ; schema : string
    ; schema_aux_files : string list
    ; postgres_uri : string
    ; postgres_config : Postgres_config.t
    }
  [@@deriving to_yojson]

  type base_config = Base_node_config.base_config [@@deriving to_yojson]

  type t =
    { service_name : string
    ; base_config : base_config
    ; specific_config : specific_config
    ; docker_config : Dockerfile.Service.t
    }
  [@@deriving to_yojson]

  let create_specific_command specific_config =
    [ "mina-archive"
    ; "run"
    ; "-postgres-uri"
    ; specific_config.postgres_uri
    ; "-server-port"
    ; Int.to_string specific_config.server_port
    ; "-config-file"
    ; specific_config.postgres_uri
    ]

  let create_cmd base_config specific_config =
    let base_args = Base_node_config.to_list base_config in
    let archive_node_args = create_specific_command specific_config in
    List.concat [ archive_node_args; base_args ]

  let create_docker_config ~image ~entrypoint ~networks ~ports ~volumes
      ~environment ~base_config ~specific_config =
    let command = create_cmd base_config specific_config in
    let docker_config : Dockerfile.Service.t =
      { image
      ; command
      ; entrypoint
      ; ports
      ; environment
      ; volumes
      ; networks
      ; dns = Dockerfile.Service.Dns.default
      }
    in
    docker_config

  let create ~stack_name ~name ~image ~networks ~ports ~volumes ~config_file
      ~postgres_uri ~server_port ~schema ~schema_aux_files ~postgres_config =
    let dns_name = stack_name ^ "_" ^ name in
    let base_config = Base_node_config.default ~config_file ~dns_name in
    let specific_config =
      { postgres_uri; server_port; schema; schema_aux_files; postgres_config }
    in
    let entrypoint = Some [ "/root/entrypoint.sh" ] in
    let docker_config =
      create_docker_config ~image ~ports ~networks ~volumes
        ~environment:Dockerfile.Service.Environment.default ~entrypoint
        ~base_config ~specific_config
    in
    { service_name = dns_name; base_config; specific_config; docker_config }
end
