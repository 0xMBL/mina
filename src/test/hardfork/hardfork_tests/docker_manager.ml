open Async
open Core
open Integration_test_lib
open Yojson.Basic.Util

module DockerManager = struct
  type docker = { name : string; image : string }

  type t = { network : string option; services : docker list }

  let create network services = { network; services }

  let network_ip t =
    match t.network with
    | Some network ->
        let open Deferred.Let_syntax in
        let%bind output =
          Util.run_cmd_exn "." "docker"
            [ "network"; "inspect"; network; "--format"; "json" ]
        in

        let json : Yojson.Basic.t = Yojson.Basic.from_string output in
        let gateway =
          json |> index 0 |> member "IPAM" |> member "Config" |> index 0
          |> member "Gateway" |> to_string
        in
        Deferred.return gateway
    | None ->
        Deferred.return "localhost"

  let remove_docker_services t =
    let open Deferred.Let_syntax in
    Deferred.List.iter t.services ~f:(fun service ->
        let%bind _ = Util.run_cmd "." "docker" [ "stop"; service.name ] in
        let%bind _ = Util.run_cmd "." "docker" [ "rm"; service.name ] in
        Deferred.unit )

  let recreate_docker_network network =
    let open Deferred.Let_syntax in
    let%bind _ = Util.run_cmd_exn "." "docker" [ "network"; "rm"; network ] in
    Util.run_cmd_exn "." "docker" [ "network"; "create"; network ] >>| ignore

  let setup t =
    let open Deferred.Let_syntax in
    let%bind _ = remove_docker_services t in
    match t.network with
    | Some network ->
        recreate_docker_network network
    | None ->
        Deferred.unit

  let maybe_add_network t =
    match t.network with Some network -> [ "--network"; network ] | None -> []

  let run_in_docker t ~args app docker =
    Util.run_cmd_exn "." "docker"
      ( [ "run" ] @ maybe_add_network t
      @ [ "--volume"
        ; ".:/workdir"
        ; "--volume"
        ; "/tmp:/tmp"
        ; "--workdir"
        ; "/workdir"
        ; docker
        ; app
        ]
      @ args )

  let rec wait_for_docker_init t ~number_of_retries docker ~command ~is_ok =
    if number_of_retries <= 0 then
      failwithf "docker container does not start '%s'" docker () ;
    Unix.sleep 1 ;
    let open Deferred.Let_syntax in
    let%bind output =
      Util.run_cmd "." "docker" ([ "container"; "exec"; docker ] @ command)
    in
    if is_ok output then Deferred.unit
    else
      wait_for_docker_init t ~number_of_retries:(number_of_retries - 1) docker
        ~command ~is_ok

  let start_and_wait_for_docker_init t docker ~args ~poll_command ~is_started =
    let docker =
      List.find_exn t.services ~f:(fun service ->
          String.( = ) service.image docker )
    in
    let%bind _ =
      Util.run_cmd_exn "." "docker"
        ( [ "run"; "--name"; docker.name ]
        @ maybe_add_network t @ args @ [ docker.image ] )
    in
    wait_for_docker_init t docker.name ~number_of_retries:5
      ~command:poll_command ~is_ok:is_started
end
