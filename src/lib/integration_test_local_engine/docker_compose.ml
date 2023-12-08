open Core_kernel
open Async
open Integration_test_lib

module Dockerfile = struct
  module Network = struct
    type t = { name : string; driver : string } [@@deriving to_yojson]

    let create_cmd t =
      [ "docker"; "network"; "create"; "--driver"; t.driver; t.name ]

    let remove_cmd t = [ "docker"; "network"; "rm"; t.name ]

    let create t =
      let%bind cwd = Unix.getcwd () in
      let open Malleable_error.Let_syntax in
      let%bind (_ : string) =
        Deferred.bind ~f:Malleable_error.or_hard_error
          (Integration_test_lib.Util.run_cmd_or_error cwd "docker"
             (create_cmd t) )
      in
      return ()

    let remove t =
      let%bind cwd = Unix.getcwd () in
      let open Malleable_error.Let_syntax in
      let%bind (_ : string) =
        Deferred.bind ~f:Malleable_error.or_hard_error
          (Integration_test_lib.Util.run_cmd_or_error cwd "docker"
             (remove_cmd t) )
      in
      return ()

    let to_yojson t =
      `Assoc [ (t.name, `Assoc [ ("driver", `String t.driver) ]) ]
  end

  module Service = struct
    module Volume = struct
      type t =
        { type_ : string [@key "type"]; source : string; target : string }
      [@@deriving to_yojson]

      let create source target = { type_ = "bind"; source; target }

      let write_config docker_dir ~filename ~data =
        Out_channel.with_file ~fail_if_exists:false
          (docker_dir ^ "/" ^ filename)
          ~f:(fun ch -> data |> Out_channel.output_string ch) ;
        ignore (Util.run_cmd_exn docker_dir "chmod" [ "600"; filename ])
    end

    module Environment = struct
      type t = (string * string) list

      let default =
        [ ("DAEMON_REST_PORT", "3085")
        ; ("DAEMON_CLIENT_PORT", "8301")
        ; ("DAEMON_METRICS_PORT", "10001")
        ; ("MINA_PRIVKEY_PASS", "naughty blue worm")
        ; ("MINA_LIBP2P_PASS", "")
        ; ("RAYON_NUM_THREADS", "6")
        ]

      let to_yojson env = `Assoc (List.map env ~f:(fun (k, v) -> (k, `String v)))
    end

    module Network = struct
      type t = string [@@deriving to_yojson]
    end

    module Dns = struct
      type t = string list [@@deriving to_yojson]

      let default = [ "8.8.8.8" ]
    end

    module Port = struct
      type t = { published : int; target : int } [@@deriving to_yojson]

      let create ~published ~target = { published; target }
    end

    let default_entrypoint = [ "/root/entrypoint.sh" ]

    type t =
      { image : string
      ; command : string list
      ; entrypoint : string list option
            [@to_yojson
              fun j ->
                match j with
                | Some v ->
                    `List (List.map (fun s -> `String s) v)
                | None ->
                    `Null]
      ; ports : Port.t list
      ; environment : Environment.t
      ; volumes : Volume.t list
      ; dns : Dns.t
      }
    [@@deriving to_yojson]

    let create ~image ~command ~entrypoint ~ports ~environment ~volumes ~dns =
      { image; command; entrypoint; ports; environment; volumes; dns }

    let to_yojson
        { image; command; entrypoint; ports; environment; volumes; dns } =
      `Assoc
        ( [ ("image", `String image)
          ; ("command", `List (List.map ~f:(fun s -> `String s) command))
          ; ("ports", `List (List.map ~f:Port.to_yojson ports))
          ; ("environment", Environment.to_yojson environment)
          ; ("volumes", `List (List.map ~f:Volume.to_yojson volumes))
          ; ("dns", `List (List.map ~f:(fun s -> `String s) dns))
          ]
        @
        match entrypoint with
        | Some ep ->
            [ ("entrypoint", `List (List.map ~f:(fun s -> `String s) ep)) ]
        | None ->
            [] )
  end

  module StringMap = Map.Make (String)

  type service_map = Service.t StringMap.t

  let merge (m1 : service_map) (m2 : service_map) =
    Base.Map.merge_skewed m1 m2 ~combine:(fun ~key:_ left _ -> left)

  let service_map_to_yojson m =
    `Assoc (m |> Map.map ~f:Service.to_yojson |> Map.to_alist)

  let network_map_to_yojson m =
    `Assoc (m |> Map.map ~f:Network.to_yojson |> Map.to_alist)

  type t = { version : string; services : service_map } [@@deriving to_yojson]

  let to_string = Fn.compose Yojson.Safe.pretty_to_string to_yojson

  let compose_file_name = "compose.json"
end

type t = Dockerfile.t [@@deriving to_yojson]

let to_string = Fn.compose Yojson.Safe.pretty_to_string to_yojson
