open Caqti_async

module Mainnet = struct
  let latest_state_hash_query =
    Caqti_request.find_opt Caqti_type.int Caqti_type.string
      {sql| 
          SELECT state_hash FROM blocks WHERE global_slot_since_genesis < ?
                 and chain_status <> 'orphaned'
           ORDER BY global_slot_since_genesis desc LIMIT 1
          |sql}

  let global_slot_since_genesis_at_state_hash_query =
    Caqti_request.find_opt Caqti_type.string Caqti_type.int
      {sql| 
          SELECT global_slot_since_genesis FROM blocks 
          WHERE state_hash = ? and chain_status <> 'orphaned'
          LIMIT 1
          |sql}

  let global_slot_since_genesis_at_state_hash (module Conn : CONNECTION)
      state_hash =
    Conn.find_opt global_slot_since_genesis_at_state_hash_query state_hash

  let latest_state_hash (module Conn : CONNECTION) slot =
    Conn.find_opt latest_state_hash_query slot

  let user_commands_hashes_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| Select hash from blocks_user_commands 
                  inner join blocks on blocks.id = blocks_user_commands.block_id 
                  inner join user_commands on user_commands.id = blocks_user_commands.user_command_id 
                  where global_slot_since_genesis < ? |sql}

  let internal_commands_hashes_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| Select hash from blocks_internal_commands 
                  inner join blocks on blocks.id = blocks_internal_commands.block_id 
                  inner join internal_commands on internal_commands.id = blocks_internal_commands.internal_command_id 
                  where global_slot_since_genesis < ? |sql}

  let user_commands_hashes (module Conn : CONNECTION) end_global_slot =
    Conn.collect_list user_commands_hashes_query end_global_slot

  let internal_commands_hashes (module Conn : CONNECTION) end_global_slot =
    Conn.collect_list internal_commands_hashes_query end_global_slot

  let block_hashes_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| Select state_hash from blocks where global_slot_since_genesis < ? |sql}

  let block_parent_hashes_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| Select state_hash from blocks where global_slot_since_genesis < ? and state_hash is not null|sql}

  let ledger_hashes_query =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      {sql| Select ledger_hash from block where global_slot_since_genesis < ? |sql}

  let block_hashes (module Conn : CONNECTION) end_global_slot =
    Conn.collect_list block_hashes_query end_global_slot

  let block_parent_hashes (module Conn : CONNECTION) end_global_slot =
    Conn.collect_list block_parent_hashes_query end_global_slot

  let ledger_hashes (module Conn : CONNECTION) end_global_slot =
    Conn.collect_list ledger_hashes_query end_global_slot
end

module Berkeley = struct
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
end
