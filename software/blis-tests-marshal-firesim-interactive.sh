#!/bin/bash

# This script will rebuild BLIS, and then
# build the appropriate Linux Image for FireSim execution using
# FireMarshal and Spike
echo "Build and Test BLIS Tests Workload"

RDIR=$(pwd)
MARSHALDIR=$(pwd)/../../../software/firemarshal
rm -rf overlay/root/blis/
cd $MARSHALDIR
rm images/blis-tests*
./marshal -v --workdir $RDIR build blis-tests-interactive.json
./marshal -v --workdir $RDIR install blis-tests-interactive.json
cd $RDIR
