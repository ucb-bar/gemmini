#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_CHIPYARD_DIR/toolchains/esp-tools/riscv-isa-sim/build
git checkout $(cat $LOCAL_CHECKOUT_DIR/SPIKE.hash)
make && make install

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
./build.sh

cd build
make test-baremetal

