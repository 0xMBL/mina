#!/bin/bash

set -eo pipefail

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y git apt-transport-https ca-certificates tzdata curl python3 python3-pip wget

git config --global --add safe.directory $BUILDKITE_BUILD_CHECKOUT_PATH

source buildkite/scripts/export-git-env-vars.sh

echo "deb [trusted=yes] http://packages.o1test.net bullseye ${MINA_DEB_RELEASE}" | tee /etc/apt/sources.list.d/mina.list
apt-get update

echo "Installing mina test suite package: mina-test-suite=${MINA_DEB_VERSION}"
apt-get install --allow-downgrades -y mina-test-suite=${MINA_DEB_VERSION} mina-berkeley=${MINA_DEB_VERSION} mina-archive=${MINA_DEB_VERSION}

CONFIG=/etc/mina/test/hardfork_tests/ci.json

mina-hardfork-tests test checkpoints --env $CONFIG -v