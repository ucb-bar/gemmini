#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_CHIPYARD_DIR/toolchains/esp-tools/riscv-isa-sim
echo Printing current spike commit
git log -1 --format="%H"

cd $LOCAL_CHIPYARD_DIR/generators/gemmini
echo Printing current gemmini commit
git log -1 --format="%H"

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
echo Printing current gemmini-rocc-tests commit
git log -1 --format="%H"

./build.sh

cd build
make test-baremetal

