#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_SIM_DIR
make -C $LOCAL_SIM_DIR clean
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR CONFIG=$CICONFIG
