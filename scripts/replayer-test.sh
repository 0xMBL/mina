#!/bin/bash

# test replayer on known archive db

REPLAYER_DIR=src/app/replayer
REPLAYER_APP=_build/default/src/app/replayer/replayer.exe

while [[ "$#" -gt 0 ]]; do case $1 in
  -d|--dir) REPLAYER_DIR="$2"; shift;;
  -a|--app) REPLAYER_APP="$2"; shift;;
  *) echo "Unknown parameter passed: $1"; exit 1;;
esac; shift; done

DB=archive

DOCKER_IMAGE=12.4-alpine
CONTAINER_FILE=docker.container

PG_PORT=5433
PG_PASSWORD=somepassword
PG_CONN=postgres://postgres:$PG_PASSWORD@localhost:$PG_PORT/$DB

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

function report () {
 if [[ $1 == 0 ]]; then
     echo SUCCEEDED
 else
     echo FAILED
 fi
}

# -v mounts dir with Unix socket on host
echo "Starting docker with Postgresql"
docker run \
       --name replayer-postgres -d -p $PG_PORT:5432 \
       -e POSTGRES_USER=postgres -e POSTGRES_PASSWORD=$PG_PASSWORD -e POSTGRES_DB=$DB postgres:$DOCKER_IMAGE > $CONTAINER_FILE

#trap "cleanup; exit 1" SIGINT

# wait for Postgresql to become available
sleep 5

echo "Populating archive database"
psql $PG_CONN < $REPLAYER_DIR/test/archive_db.sql

echo "Running replayer"
$REPLAYER_APP --archive-uri $PG_CONN --input-file $REPLAYER_DIR/test/input.json

RESULT=$?

report $RESULT

#cleanup

exit $RESULT
