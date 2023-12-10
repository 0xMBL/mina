#!/bin/bash

#git config --global --add safe.directory $BUILDKITE_BUILD_CHECKOUT_PATH
#source buildkite/scripts/export-git-env-vars.sh

POSTGRES_PORT=5555
REFERENCE_COMMIT=c980ba8
REFERENCE_DOCKER=minaprotocol/mina-archive:1.4.0-$REFERENCE_COMMIT-bullseye

wget https://raw.githubusercontent.com/MinaProtocol/mina/c980ba8/src/app/archive/create_schema.sql

docker stop postgres || true && docker rm postgres || true
docker network create hardfork || true
docker run --name postgres --network hardfork -p $POSTGRES_PORT:5432 -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=postgres -e POSTGRES_DB=archive -v ./create_schema.sql:/docker-entrypoint-initdb.d/init.sql -v .:/src -d postgres

echo "Waiting for db to init..."
READY=""
while [[ $READY != *"accepting connections"* ]]; do
    READY=$(docker container exec postgres pg_isready -U postgres)
    sleep 5
done
    
wget -c https://storage.googleapis.com/mina-archive-dumps/mainnet-archive-dump-2023-11-02_0000.sql.tar.gz -O - | tar -xz 

PGPASSWORD=postgres psql -h localhost -p $POSTGRES_PORT -U postgres archive < mainnet-archive-dump-2023-11-02_0000.sql

#STATE_HASH=$(psql postgres://postgres:postgres@127.0.0.1:5555/archive_balances_migrated -t -c "select state_hash from blocks where global_slot_since_genesis = 200 and chain_status = 'canonical'" | xargs)

#echo "{ \"genesis_ledger\":$(cat "genesis_ledgers/mainnet.json" | jq '(.ledger)' ), \"target_epoch_ledgers_state_hash\":\"${STATE_HASH}\" }" > mainnet_replayer_config.json

#NETWORK_GATEWAY=$(docker network inspect -f "{{(index .IPAM.Config 0).Gateway}}" hardfork)

#docker run --network hardfork --volume .:/workdir minaprotocol/mina-archive:1.4.0-c980ba8-bullseye mina-replayer --archive-uri "postgres://postgres:postgres@172.21.0.1:5555/archive_balances_migrated"  --input /workdir/mainnet_replayer_config.json --output-file /workdir/mainnet_${STATE_HASH}_output.json

PGPASSWORD=postgres psql -h localhost -p $POSTGRES_PORT -U postgres -c "CREATE DATABASE random_mainnet;" 

PGPASSWORD=postgres psql -h localhost -p $POSTGRES_PORT -U postgres -d random_mainnet < src/test/hardfork/test_data/random_migration/dump.sql

#STATE_HASH=$(psql postgres://postgres:postgres@127.0.0.1:5555/random_mainnet -t -c "select state_hash from blocks where chain_status = 'canonical' order by global_slot_since_genesis DESC LIMIT 1" | xargs)
#echo "{ \"genesis_ledger\":$(cat "src/test/hardfork/test_data/random_migration/genesis_ledger.json" | jq '(.ledger)' ), \"target_epoch_ledgers_state_hash\":\"${STATE_HASH}\" }" > random_migrated_replayer_config.json

#docker run --network hardfork --volume .:/workdir minaprotocol/mina-archive:1.4.0-c980ba8-bullseye mina-replayer --archive-uri "postgres://postgres:postgres@172.21.0.1:5555/random_mainnet"  --input /workdir/random_migrated_replayer_config.json --output-file /workdir/random_mainnet_${STATE_HASH}_output.json

docker run minaprotocol/mina-test-suite:1.4.0-$REFERENCE_COMMIT-bullseye min-archive_migration_tests.exe test --env /etc/mina/test/archive_migration_tests/ci.json mainnet_migration