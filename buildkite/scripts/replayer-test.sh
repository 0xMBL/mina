#!/bin/bash

TEST_DIR=/workdir/src/app/replayer/test
PGPASSWORD=arbitraryduck

set -eox pipefail

echo "Updating apt, installing packages"
sudo apt-get update
# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

#remove any existing postgres installation
sudo apt-get -y remove postgresql postgresql-*

# time zone = US Pacific
/bin/echo -e "12\n10" | sudo apt-get install -y tzdata
sudo apt-get install -y git apt-transport-https ca-certificates curl wget

git config --global --add safe.directory /workdir

source buildkite/scripts/export-git-env-vars.sh

echo "Generating locale for Postgresql"

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

locale-gen en_US.UTF-8


#download postgres 15

# Create the file repository configuration
echo "deb [trusted=yes] http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" | sudo tee /etc/apt/sources.list.d/pgdg.list

# Import the repository signing key
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo tee /etc/apt/trusted.gpg.d/pgdg.asc &>/dev/null

# Update list of available packages
sudo apt update

# Download postgres
sudo apt install postgresql-15 postgresql-client-15 -y

# Check version
psql --version

echo "deb [trusted=yes] http://packages.o1test.net bullseye ${MINA_DEB_RELEASE}" | sudo tee /etc/apt/sources.list.d/mina.list
sudo apt-get update

echo "Installing archive node package: mina-archive=${MINA_DEB_VERSION}"
sudo apt-get install --allow-downgrades -y mina-archive=${MINA_DEB_VERSION}

echo "Starting Postgresql service"
sudo service postgresql start

echo "Postgres is up - executing command"

echo "Populating archive database"
cd ~postgres
sudo -u postgres psql -p 5433 < $TEST_DIR/archive_db.sql
echo "ALTER USER postgres PASSWORD '$PGPASSWORD';" | sudo -u postgres psql -p 5433
cd /workdir

echo "Running replayer"
mina-replayer --archive-uri postgres://postgres:$PGPASSWORD@localhost:5433/archive \
	      --input-file $TEST_DIR/input.json --output-file /dev/null