#!/bin/bash

TEST_DIR=/workdir/src/app/replayer/

set -eox pipefail

echo "Updating apt, installing packages"
apt-get update
# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

# time zone = US Pacific
/bin/echo -e "12\n10" | sudo apt-get install -y tzdata
apt-get install -y git apt-transport-https ca-certificates curl wget

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

echo "deb [trusted=yes] http://packages.o1test.net bullseye ${MINA_DEB_RELEASE}" | sudo tee /etc/apt/sources.list.d/mina.list
apt-get update

echo "Installing archive node package: mina-archive=${MINA_DEB_VERSION}"
apt-get install --allow-downgrades -y mina-archive=${MINA_DEB_VERSION}

echo "Starting Postgresql service"
service postgresql start

echo "Postgres is up - executing command"

echo "Running replayer"

./scripts/replayer-test.sh -d $TEST_DIR -a mina-replayer