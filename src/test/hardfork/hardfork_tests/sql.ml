open Caqti_async

module Mainnet = struct
  let latest_state_hash_query =
    Caqti_request.find_opt Caqti_type.int Caqti_type.string
      {sql| 
          SELECT state_hash FROM blocks WHERE global_slot_since_genesis < ?
                 and chain_status <> 'orphaned'
           ORDER BY global_slot_since_genesis desc LIMIT 1
        |sql}

  let latest_state_hash (module Conn : CONNECTION) slot =
    Conn.find_opt latest_state_hash_query slot
end
