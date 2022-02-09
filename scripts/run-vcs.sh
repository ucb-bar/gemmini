#!/bin/bash

ROOT="$PWD/"

WAVEFORM="waveforms/waveform.vpd"

help () {
  echo "Run a RISCV Gemmini program on VCS, a cycle-accurate simulator"
  echo
  echo "Usage: $0 [--pk] [--debug] BINARY"
  echo
  echo "Options:"
  echo " pk      Run binaries on the proxy kernel, which enables virtual memory"
  echo "         and a few syscalls. If this option is not set, binaries will be"
  echo "         run in baremetal mode."
  echo
  echo " debug   Use the debug version of the VCS simulator, which will output"
  echo "         a waveform to \`$WAVEFORM\`."
  echo
  echo " BINARY  The RISCV binary that you want to run. This can either be the"
  echo '         name of a program in `software/gemmini-rocc-tests`, or it can'
  echo "         be the full path to a binary you compiled."
  echo
  echo "Examples:"
  echo "         $0 template"
  echo "         $0 --debug template"
  echo "         $0 --pk mvin_mvout"
  echo "         $0 path/to/binary-baremetal"
  echo
  echo 'Note:    Run this command after running `scripts/build-vcs.sh` or'
  echo '         `scripts/build-vcs.sh --debug`.'
  exit
}

if [ $# -le 0 ]; then
    help
fi

pk=0
debug=0
show_help=0
binary=""

while [ $# -gt 0 ] ; do
  case $1 in
    --pk) pk=1 ;;
    --debug) debug=1 ;;
    -h | --help) show_help=1 ;;
    *) binary=$1
  esac

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

if [ $debug -eq 1 ]; then
    DEBUG="-debug +permissive +ntb_random_seed_automatic +vcdplusfile=${ROOT}${WAVEFORM} +permissive-off"
else
    DEBUG=""
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

cd ../../sims/vcs/
./simv-chipyard-CustomGemminiSoCConfig${DEBUG} $PK $full_binary_path

