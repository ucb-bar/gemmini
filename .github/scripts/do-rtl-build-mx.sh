#!/bin/bash

# Verilator build of the MX (FP4/FP6/FP8) RTL config used by run-tests-mx.sh.

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_SIM_DIR
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR CONFIG=$MX_CICONFIG
