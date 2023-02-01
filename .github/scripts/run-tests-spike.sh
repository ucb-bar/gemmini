#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $LOCAL_CHIPYARD_DIR
source env.sh

git clone git@github.com:ucb-bar/esp-isa-sim.git
cd esp-isa-sim
git checkout $(cat $LOCAL_CHECKOUT_DIR/SPIKE.hash)
mkdir build
cd build
../configure --prefix=$RISCV
make && make install

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
./build.sh

cd build
make test-baremetal

