open Core
open Caqti_async

module Base = struct
  type t = { address : string; state_hash : string; balance : int; slot : int }
  [@@deriving hlist]

  let typ =
    let open Mina_caqti.Type_spec in
    let spec = Caqti_type.[ string; string; int; int ] in
    let encode t = Ok (hlist_to_tuple spec (to_hlist t)) in
    let decode t = Ok (of_hlist (tuple_to_hlist spec t)) in
    Caqti_type.custom ~encode ~decode (to_rep spec)

  let to_string t =
    Printf.sprintf "{ addres: %s, state_hash: %s, balance: %d, slot: %d }"
      t.address t.state_hash t.balance t.slot

  let equal left right =
    String.equal left.address right.address
    && String.equal left.state_hash right.state_hash
    && Int.equal left.balance right.balance
    && Int.equal left.slot right.slot

  module Mainnet = struct
    let block_at_slot_query =
      Caqti_request.collect Caqti_type.int typ
        {sql| Select value,state_hash,balance,global_slot_since_genesis
                from( 
                  
                  SELECT value,state_hash,balance,global_slot_since_genesis, block_sequence_no, ROW_NUMBER() OVER(PARTITION BY value ORDER BY block_sequence_no desc) as rn
                  from balances 
                    inner join blocks on balances.block_id = blocks.id 
                    inner join public_keys on balances.public_key_id = public_keys.id 
                    where global_slot_since_genesis < ? and chain_status <> 'orphaned'
                    
                ) as result
                where rn = 1
          |sql}

    let user_commands_hashes_query =
      Caqti_request.collect Caqti_type.unit Caqti_type.string
        {sql| Select hash from user_commands |sql}

    let internal_commands_hashes_query =
      Caqti_request.collect Caqti_type.unit Caqti_type.string
        {sql| Select hash from internal_commands |sql}

    let user_commands_hashes (module Conn : CONNECTION) =
      Conn.collect_list user_commands_hashes_query ()

    let internal_commands_hashes (module Conn : CONNECTION) =
      Conn.collect_list internal_commands_hashes_query ()

    let balances_before_slot (module Conn : CONNECTION) ~slot =
      Conn.collect_list block_at_slot_query slot

    let block_id_at_max_slot_query =
      Caqti_request.collect Caqti_type.unit typ
        {sql| Select value as address,state_hash,balance,global_slot_since_genesis
              from( 
                
                SELECT value,state_hash,balance,global_slot_since_genesis, block_sequence_no, ROW_NUMBER() OVER(PARTITION BY value ORDER BY block_sequence_no desc) as rn
                from balances 
                  inner join blocks on balances.block_id = blocks.id 
                  inner join public_keys on balances.public_key_id = public_keys.id 
                  where chain_status <> 'orphaned'
              ) as result
              where rn = 1
        |sql}

    let balances_before_slot_max_slot (module Conn : CONNECTION) =
      Conn.collect_list block_id_at_max_slot_query ()
  end

  module Berkeley = struct
    let balances_before_slot_query =
      Caqti_request.collect Caqti_type.int typ
        {sql| select value as address, state_hash,balance,global_slot_since_genesis from blocks 
              inner join accounts_accessed on blocks.id = accounts_accessed.block_id 
              inner join account_identifiers on accounts_accessed.account_identifier_id  = account_identifiers.id
              inner join public_keys on account_identifiers.public_key_id = public_keys.id
              where global_slot_since_genesis < ? and chain_status <> 'orphaned'
        |sql}

    let balances_before_slot (module Conn : CONNECTION) ~slot =
      Conn.collect_list balances_before_slot_query slot

    let find_internal_command_id_by_hash_query =
      Caqti_request.find_opt Caqti_type.string Caqti_type.int
        {sql| Select id 
        from internal_commands 
        where hash = ?    
      |sql}

    let find_internal_command_id_by_hash (module Conn : CONNECTION) hash =
      Conn.find_opt find_internal_command_id_by_hash_query hash

    let find_user_command_id_by_hash_query =
      Caqti_request.find_opt Caqti_type.string Caqti_type.int
        {sql| Select id 
        from user_commands 
        where hash = ?
      |sql}

    let find_user_command_id_by_hash (module Conn : CONNECTION) hash =
      Conn.find_opt find_user_command_id_by_hash_query hash

    let balances_before_slot_max_slot_query =
      Caqti_request.collect Caqti_type.unit typ
        {sql| select value, state_hash,balance,global_slot_since_genesis from blocks 
            inner join accounts_accessed on blocks.id = accounts_accessed.block_id 
            inner join account_identifiers on accounts_accessed.account_identifier_id  = account_identifiers.id
            inner join public_keys on account_identifiers.public_key_id = public_keys.id
            where chain_status <> 'orphaned'
      |sql}

    let balances_before_slot_max_slot (module Conn : CONNECTION) =
      Conn.collect_list balances_before_slot_max_slot_query ()
  end
end
