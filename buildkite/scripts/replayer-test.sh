#!/bin/bash

TEST_DIR=/workdir/src/app/replayer/

set -eox pipefail

echo "Updating apt, installing packages"
apt-get update

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

# time zone = US Pacific
apt-get install -y git apt-transport-https ca-certificates curl wget

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

DB=archive
DOCKER_IMAGE=12.4-alpine
CONTAINER_FILE=docker.container

PG_PORT=5433
PG_PASSWORD=somepassword
PG_CONN=postgres://postgres:$PG_PASSWORD@localhost:$PG_PORT/$DB

DOCKER_IMAGE=12.4-alpine
CONTAINER_FILE=docker.container

function cleanup () {
    CONTAINER=`cat $CONTAINER_FILE`

    if [[ ! -z $CONTAINER ]] ; then
	echo "Killing, removing docker container"
	for action in kill rm; do
	    docker container $action $CONTAINER
	done
    fi

    rm -f $CONTAINER_FILE
}

# -v mounts dir with Unix socket on host
echo "Starting docker with Postgresql"
docker run \
       --volume $BUILDKITE_BUILD_CHECKOUT_PATH:/workdir \
       --name replayer-postgres -d -p $PG_PORT:5432 \
       -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=$PG_PASSWORD -e POSTGRES_DB=$DB postgres:$DOCKER_IMAGE > $CONTAINER_FILE

#trap "cleanup; exit 1" SIGINT

# wait for Postgresql to become available
sleep 5

echo "Populating archive database"
docker exec replayer-postgres psql $PG_CONN < $TEST_DIR/test/archive_db.sql

docker run gcr.io/o1labs-192920/mina-archive:$MINA_DOCKER_TAG /workdir/scripts/replayer-test.sh -d $TEST_DIR -a mina-replayer -p $PG_CONN