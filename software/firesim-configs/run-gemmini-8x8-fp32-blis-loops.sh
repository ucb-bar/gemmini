#!/usr/bin/env bash

# run the memcached experiment using the manager. optionally passing "withlaunch" will also
# automatically launch the appropriate runfarm
#
# the runfarm WILL NOT be terminated upon completion

trap "exit" INT
set -e
set -o pipefail

RDIR=$(pwd)

if [ "$1" == "withlaunch" ]; then
    firesim launchrunfarm -c ${RDIR}/config_runtime-gemmini-8x8-fp32-blis-loops.ini
fi


firesim infrasetup -c ${RDIR}/config_runtime-gemmini-8x8-fp32-blis-loops.ini
firesim runworkload -c ${RDIR}/config_runtime-gemmini-8x8-fp32-blis-loops.ini
firesim terminaterunfarm -c ${RDIR}/config_runtime-gemmini-8x8-fp32-blis-loops.ini --forceterminate

