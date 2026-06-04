#!/bin/bash

# Run the int8 (non-MX) sanity tests for GemminiRocketConfig on both spike and
# VCS. Each baremetal test self-checks and exits 0 on success / nonzero on
# failure; spike exits with the test's code and the VCS run-binary-hex runner
# propagates the tohost result, so we accept a test iff its exit code is 0 on
# the given simulator. The same build/ binaries are used for both simulators.

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

record() {
  local sim=$1   # "spike" or "rtl"
  local name=$2
  local rc=$3
  local logfile=$4
  if [ "$rc" -eq 0 ]; then
    PASS+=("$sim:$name")
  else
    FAIL+=("$sim:$name")
    echo "--- $sim:$name FAILED (rc=$rc, last 20 lines): ---"
    tail -n 20 "$logfile" || true
  fi
}

spike_skipped() {
  local t=$1
  for s in "${ROCKET_SPIKE_SKIP[@]}"; do
    [ "$s" = "$t" ] && return 0
  done
  return 1
}

echo "================================================================"
echo "  GemminiRocketConfig (int8) tests on spike"
echo "================================================================"
for t in "${ROCKET_TESTS[@]}"; do
  if spike_skipped "$t"; then
    echo "skipping spike:$t (not modeled by the spike gemmini extension)"
    continue
  fi
  bin=$TESTS_DIR/build/${t}-baremetal
  log=/tmp/rocket-spike-${t//\//-}.log
  if [ ! -f "$bin" ]; then
    FAIL+=("spike:$t (binary missing)")
    continue
  fi
  spike --extension=gemmini "$bin" > "$log" 2>&1 && rc=0 || rc=$?
  record spike "$t" "$rc" "$log"
done

echo "================================================================"
echo "  GemminiRocketConfig (int8) tests on VCS (CONFIG=$ROCKET_CICONFIG)"
echo "================================================================"
cd $LOCAL_SIM_DIR
for t in "${ROCKET_TESTS[@]}"; do
  bin=$TESTS_DIR/build/${t}-baremetal
  log=/tmp/rocket-rtl-${t//\//-}.log
  if [ ! -f "$bin" ]; then
    FAIL+=("rtl:$t (binary missing)")
    continue
  fi
  make -C $LOCAL_SIM_DIR CONFIG=$ROCKET_CICONFIG run-binary-hex BINARY="$bin" \
    > "$log" 2>&1 && rc=0 || rc=$?
  record rtl "$t" "$rc" "$log"
done

echo "================================================================"
echo "  Summary"
echo "================================================================"
echo "Passed (${#PASS[@]}):"
for p in "${PASS[@]}"; do echo "  PASS  $p"; done
echo "Failed (${#FAIL[@]}):"
for f in "${FAIL[@]}"; do echo "  FAIL  $f"; done

exit ${#FAIL[@]}
