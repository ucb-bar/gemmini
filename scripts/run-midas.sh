#!/bin/bash

if [ "$1" == "--help" ]; then
    echo usage: $0 DRAM_CONTROLLER_MODEL binary
    echo " " DRAM_CONTROLLER_MODEL: Either DDR3FCFS or DDR3FRFCFS or DDR3FRFCFSLLC4MB
    echo "   " FCFS is "first come, first serve"
    echo "   " FRFCFS is "first ready, first come, first serve"
    exit
elif [ "$1" == "" ]; then
    echo DRAM model must be provided
    exit 1
fi

path=""
suffix=""

binary="$2"

if [ "$binary" == "" ]; then
    echo You must provide a binary to run
elif [ -f "software/gemmini-rocc-tests/build/bareMetalC/${binary}-baremetal" ]; then
    path="$PWD/software/gemmini-rocc-tests/build/bareMetalC/"
    suffix="-baremetal"
elif [ -f "software/gemmini-rocc-tests/build/imagenet/${binary}-baremetal" ]; then
    path="$PWD/software/gemmini-rocc-tests/build/imagenet/"
    suffix="-baremetal"
elif [ ! -f "$binary" ]; then
    echo Binary not found
    exit 1
fi

gemminidir="$PWD"

cd ../../sims/firesim/
source sourceme-f1-manager.sh &> build.log

cd sim/

cd generated-src/f1/FireSim-${1}_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

./VFireSim ${path}${binary}${suffix} \
    +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=8 +mm_transactionQueueDepth_0=8 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 \
    2>/dev/null
