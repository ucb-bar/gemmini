#!/bin/bash

help () {
  echo "Run a RISCV Gemmini program on Spike, our functional ISA simulator"
  echo
  echo "Usage: $0 [-h|--help] [--pk] BINARY"
  echo
  echo "Options:"
  echo " pk      Run binaries on the proxy kernel, which enables virtual memory"
  echo "         and a few syscalls. If this option is not set, binaries will be"
  echo "         run in baremetal mode."
  echo " BINARY  The RISCV binary that you want to run. This can either be the"
  echo '         name of a program in `software/gemmini-rocc-tests`, or it can'
  echo "         be the full path to a binary you compiled."
  echo
  echo "Examples:"
  echo "         $0 resnet50"
  echo "         $0 --pk mvin_mvout"
  echo "         $0 path/to/binary-baremetal"
  echo
  echo 'Note:    Run this command after running `scripts/build-spike.sh`.'
  echo
  echo "Note:    On Spike, cycle counts, SoC counter values, and performance"
  echo "         statistics are all meaningless. Use Spike only to check if your"
  echo "         programs are functionally correct. For meaningful metrics, you"
  echo "         must run your programs on VCS, Verilator, or Firesim instead."
  exit
}

if [ $# -le 0 ]; then
    help
fi

pk=0
show_help=0
binary=""

while [ $# -gt 0 ] ; do
  case $1 in
    --pk) pk=1 ;;
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

path=""
suffix=""

for dir in bareMetalC mlps imagenet ; do
    if [ -f "software/gemmini-rocc-tests/build/${dir}/${binary}$default_suffix" ]; then
        path="software/gemmini-rocc-tests/build/${dir}/"
        suffix=$default_suffix
    fi
done

full_binary_path="${path}${binary}${suffix}"

if [ ! -f "${full_binary_path}" ]; then
    echo "Binary not found: $full_binary_path"
    exit 1
fi

spike --extension=gemmini $PK "${full_binary_path}"

