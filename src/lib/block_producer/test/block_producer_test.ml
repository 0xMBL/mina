open Core
open Async
open Pipe_lib

(* open Mina_base
   open Mina_state
   open Mina_block *)
open Signature_lib
open Block_producer

let%test_module "Block producer test" =
  ( module struct
    (* module Mock_base_ledger = Network_pool.Mocks.Base_ledger
       module Mock_staged_ledger = Network_pool.Mocks.Staged_ledger

       module Mock_transition_frontier_txn_pool = struct
         module Breadcrumb = struct
           type t = Mock_staged_ledger.t

           let staged_ledger = Fn.id
         end

         type best_tip_diff =
           { new_commands : User_command.Valid.t With_status.t list
           ; removed_commands : User_command.Valid.t With_status.t list
           ; reorg_best_tip : bool
           }

         type t = best_tip_diff Broadcast_pipe.Reader.t * Breadcrumb.t ref

         let create : unit -> t * best_tip_diff Broadcast_pipe.Writer.t =
          fun () ->
           let pipe_r, pipe_w =
             Broadcast_pipe.create
               { new_commands = []; removed_commands = []; reorg_best_tip = false }
           in
           let accounts =
             List.map (Array.to_list test_keys) ~f:(fun kp ->
                 let compressed = Public_key.compress kp.public_key in
                 let account_id = Account_id.create compressed Token_id.default in
                 ( account_id
                 , Account.create account_id
                   @@ Currency.Balance.of_int 1_000_000_000_000 ) )
           in
           let ledger = Account_id.Map.of_alist_exn accounts in
           ((pipe_r, ref ledger), pipe_w)

         let best_tip (_, best_tip_ref) = !best_tip_ref

         let best_tip_diff_pipe (pipe, _) = pipe
       end *)

    let () =
      Core.Backtrace.elide := false ;
      Async.Scheduler.set_record_backtraces true

    module Test_setup = struct
      let precomputed_values = Lazy.force Precomputed_values.for_unit_tests

      let _accounts =
        Lazy.force @@ Precomputed_values.accounts precomputed_values

      let constraint_constants = precomputed_values.constraint_constants

      let logger = Logger.null ()

      let time_controller = Block_time.Controller.basic ~logger

      let consensus_constants = precomputed_values.consensus_constants

      let log_block_creation = false

      let proof_level = precomputed_values.proof_level

      let pids = Child_processes.Termination.create_pid_table ()

      let verifier_deferred () =
        Verifier.create ~logger ~proof_level ~constraint_constants
          ~conf_dir:None ~pids

      let trust_system = Trust_system.null ()

      (* let test_keys = Array.init 10 ~f:(fun _ -> Keypair.create ()) *)

      let block_production_keypair = Keypair.create ()

      let keypairs : Keypair.And_compressed_pk.Set.t =
        let open Signature_lib in
        let kp = Keypair.create () in
        let compressed = Public_key.compress kp.public_key in
        Set.of_list (module Keypair.And_compressed_pk) [ (kp, compressed) ]

      let top_conf_dir =
        let temp_dir = Filename.temp_dir "mina_block_producer_test" "" in
        Core.Unix.mkdir_p temp_dir ; temp_dir

      let mk_conf_dir name =
        let conf_dir = top_conf_dir ^/ "/" ^/ name in
        Core.Unix.mkdir_p conf_dir ; conf_dir

      let vrf_evaluator_deferred () =
        Vrf_evaluator.create ~constraint_constants ~pids ~consensus_constants
          ~conf_dir:(mk_conf_dir "vrf_evaluator")
          ~logger ~keypairs

      let proof_level = Genesis_constants.Proof_level.None

      let prover_deferred () =
        Prover.create ~logger ~pids ~conf_dir:(mk_conf_dir "prover")
          ~proof_level ~constraint_constants

      let frontier_broadcast_pipe_r, _frontier_broadcast_pipe_w =
        Broadcast_pipe.create None

      let _producer_transition_reader, producer_transition_writer =
        Strict_pipe.create Synchronous

      let pool_max_size = 25

      let txn_pool_config verifier =
        Network_pool.Transaction_pool.Resource_pool.make_config ~verifier
          ~trust_system ~pool_max_size

      let transaction_pool txn_pool_config =
        (* make transaction pool return writer for local and incoming diffs *)
        Network_pool.Transaction_pool.create ~config:txn_pool_config
          ~constraint_constants ~consensus_constants ~time_controller ~logger
          ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
          ~on_remote_push:(Fn.const Deferred.unit) ~log_gossip_heard:false

      let transaction_resource_pool transaction_pool =
        Network_pool.Transaction_pool.resource_pool transaction_pool

      let genesis_ledger = Precomputed_values.genesis_ledger precomputed_values

      let genesis_epoch_data =
        Precomputed_values.genesis_epoch_data precomputed_values

      let ledger_depth = constraint_constants.ledger_depth

      let genesis_state_hash =
        precomputed_values.protocol_state_with_hashes.hash.state_hash

      let epoch_ledger_location = mk_conf_dir "epoch_ledger"

      let consensus_local_state =
        Consensus.Data.Local_state.create ~genesis_ledger ~genesis_epoch_data
          ~epoch_ledger_location ~ledger_depth ~genesis_state_hash
          (Public_key.Compressed.Set.of_list
             [ Public_key.compress block_production_keypair.public_key ] )

      let coinbase_receiver = ref `Producer

      let set_next_producer_timing _timing _consensus_state = ()

      let block_produced_bvar = Bvar.create ()

      (* let max_frontier_length =
         Transition_frontier.global_max_length Genesis_constants.compiled *)

      let run () =
        let () = Parallel.init_master () in
        let vrf_evaluator =
          Thread_safe.block_on_async_exn (fun () -> vrf_evaluator_deferred ())
        in
        let prover =
          Thread_safe.block_on_async_exn (fun () -> prover_deferred ())
        in
        let verifier =
          Thread_safe.block_on_async_exn (fun () -> verifier_deferred ())
        in
        let transaction_pool, _, _ =
          transaction_pool (txn_pool_config verifier)
        in
        let transaction_resource_pool =
          transaction_resource_pool transaction_pool
        in
        run ~logger ~vrf_evaluator ~prover ~verifier ~trust_system
          ~get_completed_work:(fun _ -> None)
          ~transaction_resource_pool ~time_controller ~consensus_local_state
          ~coinbase_receiver ~frontier_reader:frontier_broadcast_pipe_r
          ~transition_writer:producer_transition_writer
          ~set_next_producer_timing ~log_block_creation ~precomputed_values
          ~block_reward_threshold:None ~block_produced_bvar

      (* let gen_crumb =
           Transition_frontier.Breadcrumb.For_tests.gen ~logger ~precomputed_values
             ~verifier ~trust_system

         let gen_genesis_breadcrumb =
           Transition_frontier.For_tests.gen_genesis_breadcrumb ~logger
             ~precomputed_values ~verifier () *)

      (* let generate_next_state ~previous_protocol_state ~staged_ledger
           ~transactions ~get_completed_work ~block_data ~winner_pk
           ~scheduled_time ~block_reward_threshold  =
         Block_producer.generate_next_state ~constraint_constants
           ~previous_protocol_state ~time_controller ~staged_ledger ~transactions
           ~get_completed_work ~logger
           ~(block_data : Consensus.Data.Block_data.t)
           ~winner_pk ~scheduled_time ~log_block_creation ~block_reward_threshold
      *)

      (* let gen_transition_frontier =
         Transition_frontier.For_tests.gen ~logger ~verifier ~trust_system
           ~precomputed_values ~max_length:max_frontier_length
           ~size:max_frontier_length () *)
    end

    let%test_unit "run bp" = Test_setup.run ()

    (* let%test_unit "generate next state" =
       Async.Thread_safe.block_on_async_exn (fun () ->
           Quickcheck.async_test
             (Quickcheck.Generator.both Test_setup.gen_genesis_breadcrumb
                Test_setup.gen_transition_frontier )
             ~f:(fun (crumb, transition_frontier) ->
               let open Deferred.Let_syntax in
               (* let%bind vrf_evaluator = Test_setup.vrf_evaluator_deferred in *)
               let staged_ledger =
                 Transition_frontier.Breadcrumb.staged_ledger crumb
               in
               let previous_transition =
                 With_hash.data
                 @@ Transition_frontier.Breadcrumb.block_with_hash crumb
               in
               let previous_protocol_state =
                 Mina_block.Header.protocol_state
                 @@ Mina_block.header previous_transition
               in
               let get_completed_work _ = None in
               let scheduled_time = Block_time.now Test_setup.time_controller in
               let block_reward_threshold = None in
               let now = Block_time.now Test_setup.time_controller in
               let consensus_state =
                 Transition_frontier.best_tip transition_frontier
                 |> Transition_frontier.Breadcrumb.consensus_state
               in
               let x =
                 Test_setup.generate_next_state ~previous_protocol_state
                   ~staged_ledger ~transactions:Sequence.empty
                   ~get_completed_work
                   ~block_data:Consensus.Data.Block_data.empty
               in
               let vrf_evaluation_state = Vrf_evaluation_state.create () in
               let epoch_data_for_vrf, ledger_snapshot =
                 Consensus.Hooks.get_epoch_data_for_vrf
                   ~constants:Test_setup.consensus_constants (time_to_ms now)
                   consensus_state ~local_state:Test_setup.consensus_local_state
                   ~logger:Test_setup.logger
               in
               let%bind () =
                 Vrf_evaluation_state.update_epoch_data ~vrf_evaluator
                   ~logger:Test_setup.logger ~epoch_data_for_vrf
                   vrf_evaluation_state
               in
               let%bind () =
                 Vrf_evaluation_state.poll ~vrf_evaluator
                   ~logger:Test_setup.logger vrf_evaluation_state
               in
               match Core.Queue.dequeue vrf_evaluation_state.queue with
               | Some slot_won ->
                   let winning_global_slot = slot_won.global_slot in
                   let slot, epoch =
                     let t =
                       Consensus.Data.Consensus_time.of_global_slot
                         winning_global_slot ~constants:consensus_constants
                     in
                     Consensus.Data.Consensus_time.(slot t, epoch t)
                   in
                   let winner_pk = fst slot_won.delegator in
                   let data =
                     { Consensus.Data.Block_data.staged_ledger_hash =
                         Staged_ledger.hash staged_ledger
                     ; pending_coinbase_collection =
                         Staged_ledger.pending_coinbase_collection staged_ledger
                     ; snarked_ledger_hash =
                         Staged_ledger.snarked_ledger_hash staged_ledger
                     ; timestamp = Block_time.now time_controller
                     ; vrf_result = slot_won.vrf_result
                     }
                   in
                   Deferred.unit ) ) *)

    (* module Test_setup = struct
               type t = {
               logger:Logger.t;
         vrf_evaluator:Vrf_evaluator.t;
         prover:Prover.t;
         verifier:Verifier.t;
         trust_system:Trust_system.t;
         get_completed_work:(Transaction_snark_work.Statement.t ->
                             Transaction_snark_work.Checked.t option);
         transaction_resource_pool:Network_pool.Transaction_pool.Resource_pool.t ;
         time_controller:Block_time.Controller.t;
         consensus_local_state:Consensus.Data.Local_state.t;
         coinbase_receiver:Consensus.Coinbase_receiver.t Caml.ref;
         frontier_reader:Transition_frontier.t option
                         Pipe_lib.Broadcast_pipe.Reader.t;
         transition_writer:(Frontier_base.Breadcrumb.t, 'a, unit Async.Deferred.t)
                           Pipe_lib.Strict_pipe.Writer.t ;
         set_next_producer_timing:([> `Check_again of Block_time.t
                                    | `Evaluating_vrf of Mina_numbers.Global_slot.t
                                    | `Produce of
                                        int64 * Consensus.Data.Block_data.t *
                                        Signature_lib.Public_key.Compressed.t
                                    | `Produce_now of
                                        Consensus.Data.Block_data.t *
                                        Signature_lib.Public_key.Compressed.t ] ->
                                   Consensus.Data.Consensus_state.Value.t -> unit) ;
         log_block_creation:bool ;
         precomputed_values:Genesis_proof.t ;
         block_reward_threshold:Currency.Amount.t option ;
         block_produced_bvar:(Frontier_base.Breadcrumb.t, [> Core.write ])
                             Async.Bvar.t
               }




















                  let setup_test ?slot_tx_end () =
                   let txn_pool_config =
               Network_pool.Transaction_pool.Resource_pool.make_config ~verifier
                 ~trust_system ~pool_max_size
             in

             let snark_pool_config =
               Network_pool.Snark_pool.Resource_pool.make_config ~verifier
                 ~trust_system ~disk_location:(mk_conf_dir "snark_pool")
             in
             let snark_pool, _snark_remote_sink, _snark_local_sink =
               Network_pool.Snark_pool.create ~config:snark_pool_config
                 ~constraint_constants ~consensus_constants ~time_controller ~logger
                 ~frontier_broadcast_pipe:frontier_broadcast_pipe_r
                 ~on_remote_push:(Fn.const Deferred.unit) ~log_gossip_heard:false
             in

             end


             (* Block_producer.run ~logger ~vrf_evaluator ~prover ~verifier ~trust_system
               ~get_completed_work:
                 (Network_pool.Snark_pool.get_completed_work snark_pool)
               ~transaction_resource_pool:
                 (Network_pool.Transaction_pool.resource_pool transaction_pool)
               ~time_controller ~consensus_local_state ~coinbase_receiver
               ~frontier_reader:frontier_broadcast_pipe_r
               ~transition_writer:producer_transition_writer ~set_next_producer_timing
               ~log_block_creation:false ~precomputed_values
               ~block_reward_threshold:None ~block_produced_bvar  *)

           let%test_unit "generate_next_state with slot_tx_end not set" =
           match Broadcast_pipe.Reader.peek frontier_reader with
               | None ->
                   log_bootstrap_mode () ; Interruptible.return ()
               | Some frontier ->
             let crumb = Transition_frontier.best_tip frontier in
             let previous_transition = Breadcrumb.block_with_hash crumb in
             let previous_protocol_state =
               Header.protocol_state
               @@ Mina_block.header (With_hash.data previous_transition)
             in
             let _bisk =
               Block_producer.generate_next_state ~constraint_constants
                 ~previous_protocol_state ~time_controller ~staged_ledger ~transactions
                 ~get_completed_work ~logger
                 ~(block_data : Consensus.Data.Block_data.t)
                 ~winner_pk ~scheduled_time ~log_block_creation ~block_reward_threshold
                 ~consensus_constants
             in
             () *)

    (* let precomputed_blocks =
       let keys = Array.init 10 ~f:(fun _ -> Keypair.create ()) in
       let fake_transactions =
         let open Quickcheck.Generator.Let_syntax in
         let%bind sender = Quickcheck_lib.gen_signing_key in
         let%bind receiver = Quickcheck_lib.gen_signing_key in
         let%map fee = Currency.Fee.gen and amount = Currency.Amount.gen in
         User_command.forget_check
           (User_command_payload.create ~fee ~nonce:User_command.nonce_zero
              ~memo:User_command_memo.dummy
              ~valid_until:Coda_numbers.Global_slot.max_value
              ~body:
                (Payment
                   { source_pk = Public_key.compress sender.public_key
                   ; receiver_pk = Public_key.compress receiver.public_key
                   ; token_id = Token_id.default
                   ; amount
                   })
              ~signer:sender.private_key)
       in
       let fake_transactions =
         List.map fake_transactions ~f:(fun x -> (x, []))
       in
       let fake_snark_work =
         { Snark_work_lib.Work.Single.Spec.statement= Snark.Statement.dummy
         ; fee= Currency.Fee.zero
         ; prover= Public_key.compress keys.(0).public_key }
       in
       let fake_snark_jobs =
         List.map (Array.to_list keys) ~f:(fun keypair ->
             (fake_snark_work, keypair))
       in
       let fake_protocol_states =
         List.init 10 ~f:(fun _ ->
             let state =
               Protocol_state.create_value
                 ~previous_state_hash:State_hash.(of_hash zero)
                 ~blockchain_state:
                   (Blockchain_state.create_value
                      ~timestamp:Block_time.epoch
                      ~snarked_ledger_hash:Ledger_hash.(of_hash zero)
                      ~staged_ledger_hash:Staged_ledger_hash.(of_hash zero)
                      ~staged_ledger_aux_hash:Staged_ledger_aux_hash.(of_hash zero)
                      ~fee_excess:Currency.Fee.Signed.zero
                      ~supply_increase:Currency.Amount.zero)
                 ~consensus_state:
                   Consensus.Data.Consensus_state.
                     { length= UInt32.of_int 1
                     ; epoch_length= UInt32.of_int 1
                     ; block_window_duration_ms= 1000
                     ; slots_per_epoch= UInt32.of_int 1
                     ; supermajority_fraction= Float.(of_int 2 / of_int 3)
                     ; max_concurrent_dynamic_subwindows= 1
                     ; last_vrf_output= Vrf.Output.dummy
                     ; total_currency= Currency.Amount.of_int 1000
                     ; curr_global_slot= Coda_numbers.Global_slot.of_int 1
                     ; staking_epoch_data=
                         Consensus.Data.Staking_epoch_data.empty }
                 ~blockchain_metadata:None
             in
             let state_hash = Protocol_state.hash state in
             (state_hash, state))
       in
       let open Quickcheck.Generator.Let_syntax in
       let%bind scheduled_time =
         Quickcheck_lib.gen_incl Block_time.epoch
           (Block_time.add (Block_time.now ()) (Block_time.Span.of_ms 1000L))
       in
       let%map () = Quickcheck_lib.gen_unit in
       let precomputed_block =
         { Precomputed_block.transactions= fake_transactions
         ; snark_jobs= fake_snark_jobs
         ; protocol_states= fake_protocol_states
         ; staged_ledger_diff=
             { completed_works= []
             ; user_commands= fake_transactions
             ; coinbase= Coinbase.create (Amount.of_int 10_000_000_000)
             ; fee_transfer= None
             ; coinbase_receiver= Public_key.compress keys.(0).public_key
             ; proofs= [] }
         ; expected_reward= Currency.Amount.of_int 10_000_000_000
         ; producer_private_key= keys.(0).private_key
         ; producer_public_key= Public_key.compress keys.(0).public_key
         ; scheduled_time }
       in
       let%test_unit "emit_next_block" =
         let frontier_broadcast_pipe_r, _ =
           Broadcast_pipe.create (Some (Transition_frontier.create ()))
         in
         let%bind () =
           emit_next_block logger frontier_broadcast_pipe_r precomputed_blocks
         in
         assert true *)
  end )
