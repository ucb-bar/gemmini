#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh


# clone and build our version of spike
TOOLS_DIR=$LOCAL_ESP_DIR
PATH=$PATH:$LOCAL_ESP_DIR/bin

git clone https://github.com/ucb-bar/esp-isa-sim.git
cd esp-isa-sim
git checkout $(cat $LOCAL_CHECKOUT_DIR/SPIKE.hash)
cp $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests/include/gemmini_params.h gemmini/

mkdir build
cd build
../configure --prefix=$TOOLS_DIR
make -j8 install

cd $LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
./build.sh

cd build
make test-baremetal
