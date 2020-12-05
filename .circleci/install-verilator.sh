#!/bin/bash

# move verilator to the remote server

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

if [ ! -d "$LOCAL_VERILATOR_DIR" ]; then
    git clone http://git.veripool.org/git/verilator $LOCAL_VERILATOR_DIR
    cd $LOCAL_VERILATOR_DIR
    git checkout $VERILATOR_VERSION
    autoconf
    export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR
    ./configure
    make -j$LOCAL_MAKE_NPROC
fi
