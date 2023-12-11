#!/bin/bash

git config --global --add safe.directory $BUILDKITE_BUILD_CHECKOUT_PATH
source buildkite/scripts/export-git-env-vars.sh

POSTGRES_PORT=5555
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DOCKER=postgres:12-bullseye
REFERENCE_COMMIT=c980ba8
MAINNET_DUMP=mainnet-archive-dump-2023-11-02_0000.sql

wget https://raw.githubusercontent.com/MinaProtocol/mina/c980ba8/src/app/archive/create_schema.sql

docker stop postgres || true && docker rm postgres || true
docker network create hardfork || true
docker run --name postgres --network hardfork -p $POSTGRES_PORT:5432 -e POSTGRES_USER=$POSTGRES_USER -e POSTGRES_PASSWORD=$POSTGRES_PASSWORD -e POSTGRES_DB=archive_balances_migrated -v $BUILDKITE_BUILD_CHECKOUT_PATH/create_schema.sql:/docker-entrypoint-initdb.d/init.sql  -d $POSTGRES_DOCKER

echo "Waiting for db to init..."
while true; do
    if [[ "$(docker container exec postgres pg_isready -U postgres)" == *"accepting connections"* ]]; then
        break;
    fi
    sleep 5
done

wget -c https://storage.googleapis.com/mina-archive-dumps/$MAINNET_DUMP.tar.gz -O - | tar -xz

docker cp  $MAINNET_DUMP postgres:/$MAINNET_DUMP
docker exec postgres psql -U postgres  <  $MAINNET_DUMP
NETWORK_GATEWAY=$(docker network inspect -f "{{(index .IPAM.Config 0).Gateway}}" hardfork)

docker exec postgres  psql  -U postgres -c "CREATE DATABASE random_mainnet;"
docker exec postgres psql -U postgres -d random_mainnet < src/test/hardfork/test_data/random_migration/dump.sql

jq  '.db.host |= "'"$NETWORK_GATEWAY"'"' src/test/hardfork/archive_migration_tests/ci.json > ci.json

docker run --entrypoint=mina-archive-migration-tests -v $BUILDKITE_BUILD_CHECKOUT_PATH/ci.json:/ci.json gcr.io/o1labs-192920/mina-test-suite:$MINA_DOCKER_TAG test --env /ci.json mainnet_migration
