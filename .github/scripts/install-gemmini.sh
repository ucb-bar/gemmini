#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

mkdir -p $JAVA_TMP_DIR

git clone --progress --verbose https://github.com/ucb-bar/chipyard.git $LOCAL_CHIPYARD_DIR
cd $LOCAL_CHIPYARD_DIR

git fetch
git checkout $CHIPYARD_BRANCH

git submodule update --init generators/gemmini
git -C generators/gemmini fetch origin
git -C generators/gemmini checkout $GEMMINI_SUBMODULE_BRANCH

git submodule update --init --recursive generators/radiance
git -C generators/radiance fetch origin
git -C generators/radiance checkout $RADIANCE_SUBMODULE_BRANCH
git -C generators/radiance submodule update --init --recursive

export RUSTUP_USE_CURL=1
export CURL_IPRESOLVE=4
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse
export CARGO_NET_RETRY=10
export CARGO_HTTP_TIMEOUT=60
(cd generators/radiance/cyclotron && cargo fetch)

export MAKEFLAGS="-j32"
./build-setup.sh riscv-tools -s 6 -s 7 -s 8 -s 9 -v

source env.sh

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'

cd $LOCAL_CHECKOUT_DIR

git submodule update --init --recursive mxgen
git submodule update --init --recursive software/gemmini-rocc-tests
rm -rf $LOCAL_CHIPYARD_DIR/generators/gemmini/* $LOCAL_CHIPYARD_DIR/generators/gemmini/.git*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHECKOUT_DIR/.git* $LOCAL_CHIPYARD_DIR/generators/gemmini/
