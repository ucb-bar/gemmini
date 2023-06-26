#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
CFLAGS=-DFAST ./build.sh

cd build
make test-baremetal-bareMetalC RUNNER="'make -C $LOCAL_CHIPYARD_DIR/sims/verilator/ CONFIG=$CICONFIG run-binary-hex BINARY='"

