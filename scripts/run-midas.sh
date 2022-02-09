#!/bin/bash

ROOT="$PWD/"

WAVEFORM_VCD="waveforms/midas-waveform.vcd"
WAVEFORM_VPD="waveforms/midas-waveform.vpd"

help () {
  echo "Run a RISCV Gemmini program on Verilator, a cycle-accurate simulator,"
  echo "using MIDAS to simulate a realistic DRAM model."
  echo
  echo "Usage: $0 [-h|--help] [--pk] [--debug] [--vcs] DRAM_CONTROLLER_MODEL BINARY"
  echo
  echo "Options:"
  echo " DRAM_CONTROLLER_MODEL  Either DDR3FCFS or DDR3FRFCFS or DDR3FRFCFSLLC4MB."
  echo '                        FCFS is "first come, first serve."'
  echo '                        FRFCFS is "first ready, first come, first serve.'
  echo
  echo " pk                     Run binaries on the proxy kernel, which enables"
  echo "                        virtual memory and a few syscalls. If this option is"
  echo "                        not set, binaries will be run in baremetal mode."
  echo
  echo " debug                  Use the debug version of the MIDAS simulator, which"
  echo "                        will output a waveform to \`$WAVEFORM_VCD\` or"
  echo "                        \`$WAVEFORM_VPD\`."
  echo
  echo " vcs                    Runs the MIDAS simulator on VCS. By default, this"
  echo "                        script will instead run on Verilator."
  echo
  echo 'Note:    Run this command after running `scripts/build-midas.sh` or'
  echo '         `scripts/build-midas.sh --debug`.'
  exit
}

if [ $# -le 0 ]; then
    help
fi

show_help=0
pk=0
debug=0
vcs=0
dram_model=""
binary=""
non_flag_arg=""

while [ $# -gt 0 ] ; do
  case $1 in
    --pk) pk=1 ;;
    --debug) debug=1 ;;
    --vcs) vcs=1 ;;
    -h | --help) show_help=1 ;;
    *) non_flag_arg=$1
  esac

  if [ "$non_flag_arg" != "" ] && [ "$dram_model" == "" ]; then
    dram_model="$non_flag_arg"
  elif [ "$non_flag_arg" != "" ]; then
    binary="$non_flag_arg";
  fi

  non_flag_arg=""
  shift
done

if [ $show_help -eq 1 ]; then
   help
fi

if [ $pk -eq 1 ]; then
    default_suffix="-pk"
    PK="pk -p"
else
    default_suffix="-baremetal"
    PK=""
fi

if [ $vcs -eq 1 ]; then
  simulator="FireSim"
  WAVEFORM="${ROOT}${WAVEFORM_VPD}"
else
  simulator="VFireSim"
  WAVEFORM="${ROOT}${WAVEFORM_VCD}"
fi

if [ $debug -eq 1 ]; then
    DEBUG="-debug"
    waveform_flag="+waveform=$WAVEFORM"
else
    DEBUG=""
    waveform_flag=""
fi

path=""
suffix=""

for dir in bareMetalC mlps imagenet ; do
    if [ -f "software/gemmini-rocc-tests/build/${dir}/${binary}$default_suffix" ]; then
        path="${ROOT}/software/gemmini-rocc-tests/build/${dir}/"
        suffix=$default_suffix
    fi
done

full_binary_path="${path}${binary}${suffix}"

if [ ! -f "${full_binary_path}" ]; then
    echo "Binary not found: $full_binary_path"
    exit 1
fi

cd ../../sims/firesim/
source sourceme-f1-manager.sh &> build.log

cd sim/

cd generated-src/f1/FireSim-${dram_model}_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

if [ ! -f ./${simulator}${DEBUG} ]; then
  echo "Simulator not found: ./${simulator}${DEBUG}"
  echo 'Did you run `./scripts/build-midas.sh`?'
fi

./${simulator}${DEBUG} ${PK} ${full_binary_path} ${waveform_flag} \
    +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=8 +mm_transactionQueueDepth_0=8 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 \
    2>/dev/null
