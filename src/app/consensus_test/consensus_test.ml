(**
  Steps that this program needs to accomplish.

  1. Read in all block data in a list ordered by timestamp

  2. For each block, check if the block is valid

  3. If the block is valid, convert it into an OCaml block type

  4. Initialize any PoS data structures

  5. Run the PoS selection algorithm on the blocks

  6. Print out the results
*)

open Core_kernel
open Async

[@@@warning "-32"]

module type CONTEXT = sig
  val logger : Logger.t

  val precomputed_values : Precomputed_values.t

  val constraint_constants : Genesis_constants.Constraint_constants.t

  val consensus_constants : Consensus.Constants.t
end

let context (logger : Logger.t) : (module CONTEXT) =
  ( module struct
    let logger = logger

    let proof_level = Genesis_constants.Proof_level.None

    let precomputed_values =
      { (Lazy.force Precomputed_values.for_unit_tests) with proof_level }

    let consensus_constants = precomputed_values.consensus_constants

    let constraint_constants = precomputed_values.constraint_constants
  end )

let current_chain : Mina_block.Precomputed.t list ref = ref []

let read_directory dir_name =
  let extract_height_from_filename fname =
    (*TODO: replace this with generic network *)
    let prefix = "berkeley-" in
    let prefix_len = String.length prefix in
    match String.index_from fname (String.length prefix) '-' with
    | None ->
        failwith "Could not find block height number in filename"
    | Some suffix_start ->
        let number_str =
          String.sub fname ~pos:prefix_len ~len:(suffix_start - prefix_len)
        in
        int_of_string number_str
  in
  let blocks_in_dir dir =
    let%map blocks_array = Async.Sys.readdir dir in
    Array.sort blocks_array ~compare:(fun a b ->
        Int.compare
          (extract_height_from_filename a)
          (extract_height_from_filename b) ) ;
    let blocks_array =
      Array.map ~f:(fun fname -> Filename.concat dir fname) blocks_array
    in
    Array.to_list blocks_array
  in
  blocks_in_dir dir_name

let read_block_file blocks_filename =
  let read_block_line line =
    match Yojson.Safe.from_string line |> Mina_block.Precomputed.of_yojson with
    | Ok block ->
        block
    | Error err ->
        failwithf "Could not read block: %s" err ()
  in
  let blocks_file = In_channel.create blocks_filename in
  match In_channel.input_line blocks_file with
  | Some line ->
      In_channel.close blocks_file ;
      return (read_block_line line)
  | None ->
      In_channel.close blocks_file ;
      failwithf "File %s is empty" blocks_filename ()

let print_block_info block =
  Mina_block.Precomputed.to_yojson block
  |> Yojson.Safe.to_string |> print_endline

let run_select ~context:(module Context : CONTEXT)
    (existing_block : Mina_block.Precomputed.t)
    (candidate_block : Mina_block.Precomputed.t) =
  let existing_consensus_state_with_hashes =
    { With_hash.hash =
        Mina_state.Protocol_state.hashes existing_block.protocol_state
    ; data =
        Mina_state.Protocol_state.consensus_state existing_block.protocol_state
    }
  in
  let candidate_consensus_state_with_hashes =
    { With_hash.hash =
        Mina_state.Protocol_state.hashes candidate_block.protocol_state
    ; data =
        Mina_state.Protocol_state.consensus_state candidate_block.protocol_state
    }
  in

  match
    Consensus.Hooks.select
      ~context:(module Context)
      ~existing:existing_consensus_state_with_hashes
      ~candidate:candidate_consensus_state_with_hashes
  with
  | `Take ->
      let candidate_length =
        Mina_state.Protocol_state.consensus_state candidate_block.protocol_state
        |> Consensus.Data.Consensus_state.blockchain_length
        |> Unsigned.UInt32.to_int
      in
      let existing_length =
        Mina_state.Protocol_state.consensus_state existing_block.protocol_state
        |> Consensus.Data.Consensus_state.blockchain_length
        |> Unsigned.UInt32.to_int
      in
      if candidate_length = existing_length then
        current_chain := List.rev (candidate_block :: List.tl_exn !current_chain)
      else if candidate_length > existing_length then
        current_chain := candidate_block :: !current_chain
      else
        failwith
          "Candidate block has lower blockchain length than existing block" ;
      return ()
  | `Keep ->
      return ()

let process_block ~context:(module Context : CONTEXT) precomputed_block =
  match List.last current_chain.contents with
  | Some last_block ->
      let%bind () =
        run_select ~context:(module Context) last_block precomputed_block
      in
      return ()
  | None ->
      failwith "Context.current_chain is empty"

let process_precomputed_blocks ~context blocks =
  let%bind () =
    Deferred.List.iter blocks ~f:(fun block ->
        let%bind () = process_block ~context block in
        return () )
  in
  return ()

let main () ~blocks_dir =
  let logger = Logger.create () in

  [%log info] "Starting to read blocks dir"
    ~metadata:[ ("blocks_dir", `String blocks_dir) ] ;
  let%bind block_sorted_filenames = read_directory blocks_dir in
  let%bind precomputed_blocks =
    Deferred.List.map block_sorted_filenames ~f:(fun json ->
        read_block_file json )
  in
  [%log info] "Finished reading blocks dir" ;
  let context = context logger in

  match precomputed_blocks with
  | [] ->
      failwith "No blocks found"
  | first_block :: remaining_blocks ->
      current_chain := [ first_block ] ;
      let precomputed_blocks = remaining_blocks in
      let%bind () = process_precomputed_blocks ~context precomputed_blocks in
      print_block_info (List.hd_exn !current_chain) ;
      return ()

let () =
  Command.(
    run
      (let open Let_syntax in
      async ~summary:"TODO"
        (let%map blocks_dir =
           Param.flag "--blocks-dir" ~doc:"STRING Path of the blocks JSON data"
             Param.(required string)
         in
         main ~blocks_dir )))