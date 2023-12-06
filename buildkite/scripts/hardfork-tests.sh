#!/bin/bash

set -eo pipefail

# Don't prompt for answers during apt-get install
export DEBIAN_FRONTEND=noninteractive

git config --global --add safe.directory $BUILDKITE_BUILD_CHECKOUT_PATH

apt-get update
apt-get install -y git apt-transport-https ca-certificates tzdata curl python3 python3-pip wget

source buildkite/scripts/export-git-env-vars.sh

CONFIG=/etc/mina/test/hardfork_tests/ci.json

sed -i 's/{DEB_VERSION}/'"${MINA_DEB_VERSION}"'/g' $CONFIG

mina-hardfork-tests test random_data --env $CONFIG -v