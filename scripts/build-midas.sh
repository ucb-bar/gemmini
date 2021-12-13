#!/bin/bash

help () {
  echo "Build a cycle-accurate MIDAS simulator which uses a realistic DRAM model."
  echo 'The cycle-accurate simulator will match `customConfig` in'
  echo '`configs/GemminiCustomConfigs.scala`.'
  echo
  echo "Usage: $0 [-h|--help] [--debug] [--vcs] DRAM_CONTROLLER_MODEL"
  echo
  echo "Options:"
  echo " DRAM_CONTROLLER_MODEL  Either DDR3FCFS or DDR3FRFCFS or DDR3FRFCFSLLC4MB."
  echo '                        FCFS is "first come, first serve."'
  echo '                        FRFCFS is "first ready, first come, first serve.'
  echo
  echo " debug                  Builds a MIDAS simulator which generates waveforms."
  echo "                        Without this option, the simulator will not generate"
  echo "                        any waveforms."
  echo
  echo " vcs                    Builds a MIDAS simulator which runs on VCS. By"
  echo "                        default, this script will instead build a MIDAS"
  echo "                        simulator which runs on Verilator."
  echo "Examples:"
  echo "         $0 DDR3FRFCFS"
  echo "         $0 --debug DDR3FRFCFSLLC4MB"
  exit
}

if [ $# -le 0 ]; then
    help
fi

show_help=0
debug=""
simulator="verilator"
dram_model=""

while [ $# -gt 0 ] ; do
  case $1 in
    -h | --help) show_help=1 ;;
    --debug) debug="-debug" ;;
    --vcs) simulator="vcs" ;;
    *) dram_model=$1
  esac

  shift
done

if [ $show_help -eq 1 ]; then
 help
fi

if [ dram_model == "" ]; then
  echo DRAM model must be provided.
fi

cd ../../sims/firesim/
source sourceme-f1-manager.sh &> build.log

cd sim/
make ${simulator}${debug} TARGET_CONFIG=${dram_model}_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig

