#!/bin/bash

# VCS build of the int8 (non-MX) GemminiRocketConfig RTL config used by
# run-tests-rocket.sh, plus the rocc-tests baremetal binaries.

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

# Put Synopsys VCS (and its license server) on PATH for the sims/vcs build.
source /ecad/tools/vlsi.bashrc

cd $LOCAL_SIM_DIR
make -j$LOCAL_MAKE_NPROC -C $LOCAL_SIM_DIR CONFIG=$ROCKET_CICONFIG

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'
rm -rf $RISCV/lib/libgemmini.so
git submodule update --init software/libgemmini
make -C software/libgemmini install

TESTS_DIR=$LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests
cd $TESTS_DIR
# These binaries run on both spike and VCS, so build_spike is not needed.
./build.sh
