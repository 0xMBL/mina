
set -x 

#~/work/hf/generate_random_blocks_compatible.exe --block-count 1 > output.tmp
#cat output.tmp | grep -o '{"accounts.*' output.tmp > genesis.json
#cat output.tmp | grep -o '{"scheduled_time.*' output.tmp > blocks.combined

wget https://raw.githubusercontent.com/MinaProtocol/mina/compatible/src/app/archive/create_schema.sql
wget https://raw.githubusercontent.com/MinaProtocol/mina/rampup/src/app/archive/create_schema.sql -O rampup_create_schema.sql
wget https://raw.githubusercontent.com/MinaProtocol/mina/rampup/src/app/archive/zkapp_tables.sql


docker stop postgres
docker rm postgres

docker run --name postgres -p 5555:5432 -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=archive -v ./create_schema.sql:/docker-entrypoint-initdb.d/init.sql -v .:/src -d postgres

READY=""
while [[ $READY != *"accepting connections"* ]]; do
    READY=$(docker container exec -it  postgres pg_isready -U postgres)

    echo $READY
    sleep 5
done

PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -c 'CREATE DATABASE migrated;'

PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -d migrated -a -f rampup_create_schema.sql

split blocks.combined -a 1 --numeric-suffixes=1 block --additional-suffix ".json"

~/work/hf/archive_blocks_compatible.exe --archive-uri postgres://postgres:postgres@localhost:5555/archive --precomputed $(ls mainnet-2-*.json )

#BLOCKS=""
#while read p; do
#  SCHEDULEDTIME=$(echo "$p" | jq -r .scheduled_time)
#  BLOCKCHAIN_LENGTH=$(echo "$p" | jq -r .protocol_state.body.consensus_state.blockchain_length)
#  GLOBAL_SLOT_SINCE_GENESIS=$(echo "$p" | jq -r .protocol_state.body.consensus_state.global_slot_since_genesis)

#  STATE_HASH=$(PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -d archive -X -A -t -w -c "SELECT state_hash from blocks WHERE timestamp='${SCHEDULEDTIME}' and global_slot_since_genesis='${GLOBAL_SLOT_SINCE_GENESIS}' and height='${BLOCKCHAIN_LENGTH}'")
#  FILE="mainnet-${BLOCKCHAIN_LENGTH}-${STATE_HASH}.json"
#  echo "$p" | jq > $FILE

#  gsutil cp $FILE gs://mina_32423_network_block_data/$FILE

#  BLOCKS="$BLOCKS $FILE"
 
#done <blocks.combined

#MAX_GENESIS_SLOT=$(PGPASSWORD=postgres psql -h localhost -p 5555 -U postgres -d archive -X -A -t -w -c "SELECT MAX(global_slot_since_genesis) from blocks")
  
GENESIS_LEDGER=$(cat "genesis_ledgers/mainnet.json" | jq '(.ledger)' )

echo "{ \"start_slot_since_genesis\":2, \"genesis_ledger\":${GENESIS_LEDGER} }" > genesis_enhanced.json


dune exec src/app/berkeley_migration/berkeley_migration.exe -- --config-file genesis_ledgers/mainnet.json --mainnet-archive-uri postgres://postgres:postgres@localhost:5555/archive --migrated-archive-uri postgres://postgres:postgres@localhost:5555/migrated --mainnet-blocks-bucket "mina_32423_network_block_data"

#dune exec src/app/berkeley_account_tables/berkeley_account_tables.exe -- --archive-uri postgres://postgres:postgres@localhost:5555/migrated --input-file genesis_enhanced.json --output-file output_ledger.json