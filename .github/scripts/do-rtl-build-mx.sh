#!/bin/bash

# Verilator build of the MX (FP4/FP6/FP8) RTL config used by run-tests-mx.sh.

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

# Put Synopsys VCS (and its license server) on PATH for the sims/vcs build.
source /ecad/tools/vlsi.bashrc

cd $LOCAL_SIM_DIR
make -j$LOCAL_BUILD_NPROC -C $LOCAL_SIM_DIR CONFIG=$MX_CICONFIG

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'
rm -rf $RISCV/lib/libgemmini.so
git submodule update --init software/libgemmini
make -C software/libgemmini install

TESTS_DIR=$LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
cd $TESTS_DIR
./build_spike.sh
./build.sh
