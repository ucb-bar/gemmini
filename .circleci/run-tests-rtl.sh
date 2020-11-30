#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh


TOOLS_DIR=$LOCAL_ESP_DIR
PATH=$PATH:$LOCAL_ESP_DIR/bin

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
CFLAGS=-DFAST ./build.sh

cd build
make test-baremetal-bareMetalC RUNNER="'make -C $LOCAL_CHIPYARD_DIR/sims/verilator/ CONFIG=GemminiRocketConfig run-binary-hex BINARY='"


