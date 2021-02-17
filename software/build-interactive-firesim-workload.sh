#!/bin/bash

# This script will rebuild the gemmini-rocc-tests, and then
# build the appropriate Linux Image for FireSim execution using
# FireMarshal
echo "Building gemmini-rocc-tests interactive FireSim Workload"

RDIR=$(pwd)
FSIMSW=$(pwd)/../../../sims/firesim/sw/firesim-software/
cd gemmini-rocc-tests
./build.sh
cp -r build/* ../overlay/root/
cd $FSIMSW
rm images/gemmini-tests-interactive*
./marshal -v --workdir $RDIR clean gemmini-tests-interactive.json
./marshal -v --workdir $RDIR build gemmini-tests-interactive.json
./marshal -v --workdir $RDIR install gemmini-tests-interactive.json
cd $RDIR
