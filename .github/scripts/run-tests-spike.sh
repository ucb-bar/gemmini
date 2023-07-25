#!/bin/bash

# turn echo on and error on earliest command
set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'

cd $LOCAL_CHECKOUT_DIR
# Delete the stale libgemmini first installed by chipyard, switch to the one submoduled here
rm -rf $RISCV/lib/libgemmini.so
git submodule update --init software/libgemmini
make -C software/libgemmini install

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
./build.sh

cd build
make -j$LOCAL_MAKE_NPROC test-baremetal
