#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

rm -rf $LOCAL_CHIPYARD_DIR/generators/gemmini/*
cd $LOCAL_CHECKOUT_DIR
git submodule update --init --recursive software/gemmini-rocc-tests
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHIPYARD_DIR/generators/gemmini/


TOOLS_DIR=$LOCAL_ESP_DIR
LD_LIB_DIR=$LOCAL_ESP_DIR/lib

# enter the verilator directory and build the specific config on remote server
make -C $LOCAL_SIM_DIR clean
export RISCV=$TOOLS_DIR
export LD_LIBRARY_PATH=$LD_LIB_DIR
export PATH=$LOCAL_VERILATOR_DIR/bin:$PATH
export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR
export COURSIER_CACHE=$LOCAL_WORK_DIR/.coursier-cache
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR VERILATOR_OPT_FLAGS="-O0 -OG" JAVA_OPTS="-Xmx2500M -Xss8M" SBT_OPTS="-Dsbt.ivy.home=$LOCAL_CHIPYARD_DIR/.ivy2 -Dsbt.supershell=false -Dsbt.global.base=$LOCAL_CHIPYARD_DIR/.sbt -Dsbt.boot.directory=$LOCAL_CHIPYARD_DIR/.sbt/boot" CONFIG=GemminiRocketConfig
