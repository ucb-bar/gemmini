#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

source $SCRIPT_DIR/enable-conda.sh

cd $LOCAL_CHIPYARD_DIR
source env.sh

cd $LOCAL_SIM_DIR
make -C $LOCAL_SIM_DIR clean
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR VERILATOR_OPT_FLAGS="-O0 -OG" JAVA_OPTS="-Xmx2500M -Xss8M" SBT_OPTS="-Dsbt.ivy.home=$LOCAL_CHIPYARD_DIR/.ivy2 -Dsbt.supershell=false -Dsbt.global.base=$LOCAL_CHIPYARD_DIR/.sbt -Dsbt.boot.directory=$LOCAL_CHIPYARD_DIR/.sbt/boot" CONFIG=$CICONFIG

