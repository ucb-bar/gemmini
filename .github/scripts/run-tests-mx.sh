#!/bin/bash

# Run the ported MX (FP4/FP6/FP8) tiled-matmul tests on both spike and
# verilator. Each test self-checks against the Python golden header and
# prints "PASSED" or "FAILED"; we accept the test iff both simulators see
# PASSED. No byte-level diff is performed (verilator is slow).

set -e
set -o pipefail

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

# Put Synopsys VCS (and its license server) on PATH for the sims/vcs run.
source /ecad/tools/vlsi.bashrc

TESTS_DIR=$LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests

PASS=()
FAIL=()

run_one() {
  local sim=$1   # "spike" or "rtl"
  local name=$2
  local logfile=$3
  if grep -q "PASSED" "$logfile"; then
    PASS+=("$sim:$name")
  else
    FAIL+=("$sim:$name")
    echo "--- $sim:$name FAILED (last 20 lines): ---"
    tail -n 20 "$logfile" || true
  fi
}

echo "================================================================"
echo "  MX tests on spike"
echo "================================================================"
for t in "${MX_TESTS[@]}"; do
  bin=$TESTS_DIR/build_spike/bareMetalC/${t}-baremetal
  log=/tmp/mx-spike-${t}.log
  if [ ! -f "$bin" ]; then
    FAIL+=("spike:$t (binary missing)")
    continue
  fi
  spike --extension=gemmini "$bin" > "$log" 2>&1 || true
  run_one spike "$t" "$log"
done

echo "================================================================"
echo "  MX tests on verilator (CONFIG=$MX_CICONFIG)"
echo "================================================================"
cd $LOCAL_SIM_DIR
for t in "${MX_TESTS[@]}"; do
  bin=$TESTS_DIR/build/bareMetalC/${t}-baremetal
  log=/tmp/mx-rtl-${t}.log
  if [ ! -f "$bin" ]; then
    FAIL+=("rtl:$t (binary missing)")
    continue
  fi
  make -C $LOCAL_SIM_DIR run-binary CONFIG=$MX_CICONFIG LOADMEM=1 BINARY="$bin" \
    > "$log" 2>&1 || true
  run_one rtl "$t" "$log"
done

echo "================================================================"
echo "  Summary"
echo "================================================================"
echo "Passed (${#PASS[@]}/$((2*${#MX_TESTS[@]}))):"
for p in "${PASS[@]}"; do echo "  PASS  $p"; done
echo "Failed (${#FAIL[@]}/$((2*${#MX_TESTS[@]}))):"
for f in "${FAIL[@]}"; do echo "  FAIL  $f"; done

exit ${#FAIL[@]}
