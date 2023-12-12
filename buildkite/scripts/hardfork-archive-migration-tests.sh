#!/bin/bash

#git config --global --add safe.directory $BUILDKITE_BUILD_CHECKOUT_PATH
#source buildkite/scripts/export-git-env-vars.sh

POSTGRES_PORT=5555
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres
POSTGRES_DOCKER=postgres:12-bullseye
MINA_DOCKER_TAG=2.0.0rampup2-dkijania-migration-app-tests-27503a5-focal
TEST_SUITE_DOCKER=gcr.io/o1labs-192920/mina-test-suite:$MINA_DOCKER_TAG
REFERENCE_COMMIT=c980ba8
MAINNET_DUMP=mainnet-archive-dump-2023-11-02_0000.sql

wget https://raw.githubusercontent.com/MinaProtocol/mina/c980ba8/src/app/archive/create_schema.sql

docker stop postgres || true && docker rm postgres || true
docker stop hardfork-tests || true && docker rm hardfork-tests || true

docker network create hardfork || true
docker run --name postgres --network hardfork -p $POSTGRES_PORT:5432 -e POSTGRES_USER=$POSTGRES_USER -e POSTGRES_PASSWORD=$POSTGRES_PASSWORD -e POSTGRES_DB=archive_balances_migrated  -d $POSTGRES_DOCKER

echo "Waiting for db to init..."
while true; do
    if [[ "$(docker container exec postgres pg_isready -U postgres)" == *"accepting connections"* ]]; then
        break;
    fi
    sleep 5
done

wget -c https://storage.googleapis.com/mina-archive-dumps/$MAINNET_DUMP.tar.gz -O - | tar -xz

docker cp  $MAINNET_DUMP postgres:/$MAINNET_DUMP
docker exec postgres psql -U postgres -d archive_balances_migrated  -f /$MAINNET_DUMP
NETWORK_GATEWAY=$(docker network inspect -f "{{(index .IPAM.Config 0).Gateway}}" hardfork)


docker run --name=hardfork-tests -v $BUILDKITE_BUILD_CHECKOUT_PATH:/workdir -d $TEST_SUITE_DOCKER 

docker exec postgres psql  -U postgres -c "CREATE DATABASE random_mainnet;"

#copying between dockers is not supported
docker cp tests:/etc/mina/test/hardfork/test_data/random_migration/dump.sql random_data_dump.sql
docker cp $BUILDKITE_BUILD_CHECKOUT_PATH/random_data_dump.sql postgres:/random_data_dump.sql

docker exec postgres psql -U postgres -d random_mainnet -f /random_data_dump.sql

jq  '.db.host |= "'"$NETWORK_GATEWAY"'"' src/test/hardfork/archive_migration_tests/ci.json > ci.json

docker exec hardfork-tests mina-archive-migration-tests test --env /workdir/ci.json mainnet_migration
