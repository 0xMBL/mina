open Core_kernel
open Async
open Integration_test_lib

(* exclude from bisect_ppx to avoid type error on GraphQL modules *)
[@@@coverage exclude_file]

[@@@warning "-27"]

let mina_archive_container_id = "archive"

let mina_archive_username = "mina"

let mina_archive_pw = "zo3moong7moog4Iep7eNgo3iecaesahH"

let postgres_url =
  Printf.sprintf "postgres://%s:%s@archive-1-postgresql:5432/archive"
    mina_archive_username mina_archive_pw

let get_container_id service_id =
  let%bind cwd = Unix.getcwd () in
  let open Malleable_error.Let_syntax in
  let%bind raw_container_id =
    Deferred.bind ~f:Malleable_error.or_hard_error
      (Integration_test_lib.Util.run_cmd_or_error cwd "docker"
         [ "ps"; "-f"; sprintf "name=%s" service_id; "--format"; "{{.ID}}" ] )
  in
  let container_id = String.strip raw_container_id in
  return container_id

type config = { stack_name : string; graphql_enabled : bool }

module Node = struct
  type service_info =
    { network_keypair : Network_keypair.t option
    ; service_id : string
    ; has_archive_container : bool
    }

  type t =
    { service_info : service_info
    ; config : config
    ; mutable should_be_running : bool
    }

  let id { service_info; _ } = service_info.service_id

  (** TODO: is this right? *)
  let infra_id { service_info; _ } = service_info.service_id

  let should_be_running { should_be_running; _ } = should_be_running

  let network_keypair { service_info; _ } = service_info.network_keypair

  let get_ingress_uri node =
    let host = Printf.sprintf "http://localhost/" in
    let path = Printf.sprintf "/%s/graphql" node.service_info.service_id in
    Uri.make ~scheme:"http" ~host ~path ~port:80 ()

  let run_in_container ?(exit_code = 10) container_id ~cmd =
    let%bind.Deferred cwd = Unix.getcwd () in
    Integration_test_lib.Util.run_cmd_or_hard_error ~exit_code cwd "docker"
      ([ "exec"; container_id ] @ cmd)

  let dump_archive_data ~logger (t : t) ~data_file =
    failwith "dump_archive_data"

  let dump_mina_logs ~logger (t : t) ~log_file = failwith "dump_mina_logs"

  let dump_precomputed_blocks ~logger (t : t) =
    failwith "dump_precomputed_blocks"

  let run_replayer ?(start_slot_since_genesis = 0) ~logger (t : t) =
    failwith "run_replayer"

  let run_in_postgresql_container _ _ = failwith "run_in_postgresql_container"

  let get_logs_in_container _ = failwith "get_logs_in_container"

  let start ~fresh_state node : unit Malleable_error.t =
    let open Malleable_error.Let_syntax in
    let%bind container_id = get_container_id node.service_info.service_id in
    node.should_be_running <- true ;
    let%bind () =
      if fresh_state then
        run_in_container container_id ~cmd:[ "rm -rf .mina-config/*" ]
        >>| ignore
      else Malleable_error.return ()
    in
    run_in_container ~exit_code:11 container_id ~cmd:[ "/start.sh" ] >>| ignore

  let stop node =
    let open Malleable_error.Let_syntax in
    let%bind container_id = get_container_id node.service_info.service_id in
    node.should_be_running <- false ;
    run_in_container ~exit_code:12 container_id ~cmd:[ "/stop.sh" ] >>| ignore
end

module Service_to_deploy = struct
  type service_to_deploy_config =
    { network_keypair : Network_keypair.t option; has_archive_container : bool }

  type t =
    { stack_name : string
    ; service_name : string
    ; service_info : service_to_deploy_config
    }

  let construct_service stack_name service_name service_info : t =
    { stack_name; service_name; service_info }

  let init_service_to_deploy_config ?(network_keypair = None)
      ?(has_archive_container = false) =
    { network_keypair; has_archive_container }

  let get_node_from_service t ~config =
    let%bind cwd = Unix.getcwd () in
    let open Malleable_error.Let_syntax in
    let service_id = t.stack_name ^ "_" ^ t.service_name in
    let%bind container_id = get_container_id service_id in
    if String.is_empty container_id then
      Malleable_error.hard_error_format "No container id found for service %s"
        t.service_name
    else
      return
        { Node.service_info =
            { service_id
            ; network_keypair = t.service_info.network_keypair
            ; has_archive_container = t.service_info.has_archive_container
            }
        ; config
        ; should_be_running = false
        }
end

type t =
  { namespace : string
  ; constants : Test_config.constants
  ; seeds : Node.t Core.String.Map.t
  ; block_producers : Node.t Core.String.Map.t
  ; snark_coordinators : Node.t Core.String.Map.t
  ; snark_workers : Node.t Core.String.Map.t
  ; archive_nodes : Node.t Core.String.Map.t
  ; genesis_keypairs : Network_keypair.t Core.String.Map.t
  }

let constants { constants; _ } = constants

let constraint_constants { constants; _ } = constants.constraints

let genesis_constants { constants; _ } = constants.genesis

let seeds { seeds; _ } = seeds

let block_producers { block_producers; _ } = block_producers

let snark_coordinators { snark_coordinators; _ } = snark_coordinators

let archive_nodes { archive_nodes; _ } = archive_nodes

let all_mina_nodes
    { seeds; block_producers; snark_coordinators; archive_nodes; _ } =
  List.concat
    [ Core.String.Map.to_alist seeds
    ; Core.String.Map.to_alist block_producers
    ; Core.String.Map.to_alist snark_coordinators
    ; Core.String.Map.to_alist archive_nodes
    ]
  |> Core.String.Map.of_alist_exn

(* all_nodes returns everything in the network.  Remember that snark_workers will never initialize and will never sync, and aren't supposed to. *)
(* TODO snark workers and snark coordinators have the same key name, but different workload ids*)
let all_nodes t =
  List.concat
    [ Core.String.Map.to_alist t.seeds
    ; Core.String.Map.to_alist t.block_producers
    ; Core.String.Map.to_alist t.snark_coordinators
    ; Core.String.Map.to_alist t.snark_workers
    ; Core.String.Map.to_alist t.archive_nodes
    ]
  |> Core.String.Map.of_alist_exn

(* all_non_seed_nodes returns everything in the network except seed nodes. *)
let all_non_seed_nodes t =
  List.concat
    [ Core.String.Map.to_alist t.block_producers
    ; Core.String.Map.to_alist t.snark_coordinators
    ; Core.String.Map.to_alist t.snark_workers
    ; Core.String.Map.to_alist t.archive_nodes
    ]
  |> Core.String.Map.of_alist_exn

let genesis_keypairs { genesis_keypairs; _ } = genesis_keypairs

let all_ids t =
  let deployments = all_nodes t |> Core.Map.to_alist in
  List.fold deployments ~init:[] ~f:(fun acc (_, node) ->
      List.cons node.service_info.service_id acc )

let initialize_infra ~logger network =
  let open Malleable_error.Let_syntax in
  let poll_interval = Time.Span.of_sec 15.0 in
  let max_polls = 60 (* 15 mins *) in
  let all_services =
    all_nodes network |> Core.Map.to_alist
    |> List.map ~f:(fun (_, node) -> node.service_info.service_id)
    |> String.Set.of_list
  in
  let get_service_statuses () =
    let%map output =
      Deferred.bind ~f:Malleable_error.return
        (Util.run_cmd_exn "/" "docker"
           [ "service"; "ls"; "--format"; "{{.Name}}: {{.Replicas}}" ] )
    in
    output |> String.split_lines
    |> List.map ~f:(fun line ->
           let parts = String.split line ~on:':' in
           assert (List.length parts = 2) ;
           (List.nth_exn parts 0, List.nth_exn parts 1) )
    |> List.filter ~f:(fun (service_name, _) ->
           String.Set.mem all_services service_name )
  in
  let rec poll n =
    let%bind pod_statuses = get_service_statuses () in
    (* TODO: detect "bad statuses" (eg CrashLoopBackoff) and terminate early *)
    let bad_service_statuses =
      List.filter pod_statuses ~f:(fun (_, status) ->
          let parts = String.split status ~on:'/' in
          assert (List.length parts = 2) ;
          let num, denom =
            ( String.strip (List.nth_exn parts 0)
            , String.strip (List.nth_exn parts 1) )
          in
          not (String.equal num denom) )
    in
    if List.is_empty bad_service_statuses then return ()
    else if n < max_polls then
      let%bind () =
        after poll_interval |> Deferred.bind ~f:Malleable_error.return
      in
      poll (n + 1)
    else
      let bad_service_statuses_json =
        `List
          (List.map bad_service_statuses ~f:(fun (service_name, status) ->
               `Assoc
                 [ ("service_name", `String service_name)
                 ; ("status", `String status)
                 ] ) )
      in
      [%log fatal]
        "Not all services could be deployed in time: $bad_service_statuses"
        ~metadata:[ ("bad_service_statuses", bad_service_statuses_json) ] ;
      Malleable_error.hard_error_format
        "Some services either were not deployed properly (errors: %s)"
        (Yojson.Safe.to_string bad_service_statuses_json)
  in
  [%log info] "Waiting for pods to be assigned nodes and become ready" ;
  Deferred.bind (poll 0) ~f:(fun res ->
      if Malleable_error.is_ok res then
        let seed_nodes = seeds network in
        let seed_service_ids =
          seed_nodes |> Core.Map.to_alist
          |> List.map ~f:(fun (_, node) -> node.service_info.service_id)
          |> String.Set.of_list
        in
        let archive_nodes = archive_nodes network in
        let archive_service_ids =
          archive_nodes |> Core.Map.to_alist
          |> List.map ~f:(fun (_, node) -> node.service_info.service_id)
          |> String.Set.of_list
        in
        let _non_seed_archive_nodes =
          network |> all_nodes |> Core.Map.to_alist
          |> List.filter ~f:(fun (_, node) ->
                 (not
                    (String.Set.mem seed_service_ids
                       node.service_info.service_id ) )
                 && not
                      (String.Set.mem archive_service_ids
                         node.service_info.service_id ) )
        in
        Deferred.return res
        (* TODO: parallelize (requires accumlative hard errors) *)
      else Deferred.return res )
