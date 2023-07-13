#!/bin/bash

# turn echo on and error on earliest command
set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
CFLAGS=-DFAST ./build.sh

cd build
make test-baremetal-bareMetalC RUNNER="'make -C $LOCAL_SIM_DIR CONFIG=$CICONFIG run-binary-hex BINARY='"
#make -j$LOCAL_MAKE_NPROC test-baremetal-bareMetalC RUNNER="'make -C $LOCAL_SIM_DIR CONFIG=$CICONFIG run-binary-hex BINARY='"
