#!/bin/bash

# This script will rebuild blasfeo, and then
# build the appropriate Linux Image for FireSim execution using
# FireMarshal and Spike
echo "Build and Test BLASFEO Tests Workload"

RDIR=$(pwd)
MARSHALDIR=$(pwd)/../../../software/firemarshal
rm -rf overlay/root/blasfeo/
cd $MARSHALDIR
rm images/blasfeo-tests*
./marshal -v --no-disk --workdir $RDIR build blasfeo-tests-interactive.json
./marshal -v --no-disk --workdir $RDIR launch --spike blasfeo-tests-interactive.json
cd $RDIR
