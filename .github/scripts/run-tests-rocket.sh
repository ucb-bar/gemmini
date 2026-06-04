#!/bin/bash

# Run the int8 (non-MX) sanity tests for GemminiRocketConfig on both spike and
# VCS. Each baremetal test self-checks and exits 0 on success / nonzero on
# failure; spike exits with the test's code and the VCS run-binary-hex runner
# propagates the tohost result, so we accept a test iff its exit code is 0 on
# the given simulator. The same build/ binaries are used for both simulators.
#
# Tests are launched in parallel (up to LOCAL_MAKE_NPROC at a time) to
# reduce wall-clock time. Results are collected via a per-test temp file.

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

RESULT_DIR=$(mktemp -d)
trap 'rm -rf "$RESULT_DIR"' EXIT

spike_skipped() {
  local t=$1
  for s in "${ROCKET_SPIKE_SKIP[@]}"; do
    [ "$s" = "$t" ] && return 0
  done
  return 1
}

# Throttle: keep at most LOCAL_MAKE_NPROC background jobs running at once.
throttle() {
  while [ "$(jobs -rp | wc -l)" -ge "$LOCAL_MAKE_NPROC" ]; do
    sleep 0.5
  done
}

echo "================================================================"
echo "  GemminiRocketConfig (int8) tests on spike (parallel, up to $LOCAL_MAKE_NPROC)"
echo "================================================================"
for t in "${ROCKET_TESTS[@]}"; do
  result=$RESULT_DIR/spike_${t//\//_}
  if spike_skipped "$t"; then
    echo "SKIP" > "$result"
    continue
  fi
  bin=$TESTS_DIR/build/${t}-baremetal
  log=/tmp/rocket-spike-${t//\//-}.log
  if [ ! -f "$bin" ]; then
    echo "MISSING" > "$result"
    continue
  fi
  throttle
  (
    spike --extension=gemmini "$bin" > "$log" 2>&1 && rc=0 || rc=$?
    echo "$rc" > "$result"
  ) &
done

echo "================================================================"
echo "  GemminiRocketConfig (int8) tests on VCS (CONFIG=$ROCKET_CICONFIG, parallel)"
echo "================================================================"
cd $LOCAL_SIM_DIR
for t in "${ROCKET_TESTS[@]}"; do
  bin=$TESTS_DIR/build/${t}-baremetal
  log=/tmp/rocket-rtl-${t//\//-}.log
  result=$RESULT_DIR/rtl_${t//\//_}
  if [ ! -f "$bin" ]; then
    echo "MISSING" > "$result"
    continue
  fi
  throttle
  (
    make -C $LOCAL_SIM_DIR CONFIG=$ROCKET_CICONFIG run-binary-hex BINARY="$bin" \
      > "$log" 2>&1 && rc=0 || rc=$?
    echo "$rc" > "$result"
  ) &
done

wait

echo "================================================================"
echo "  Summary"
echo "================================================================"
PASS=()
FAIL=()

for t in "${ROCKET_TESTS[@]}"; do
  # spike result
  spike_result_file=$RESULT_DIR/spike_${t//\//_}
  spike_result=$(cat "$spike_result_file" 2>/dev/null || echo "MISSING")
  case "$spike_result" in
    SKIP)
      echo "  SKIP  spike:$t (not modeled by spike gemmini extension)"
      ;;
    MISSING)
      FAIL+=("spike:$t (binary missing)")
      ;;
    0)
      PASS+=("spike:$t")
      ;;
    *)
      log=/tmp/rocket-spike-${t//\//-}.log
      echo "--- spike:$t FAILED (rc=$spike_result, last 20 lines): ---"
      tail -n 20 "$log" || true
      FAIL+=("spike:$t")
      ;;
  esac

  # rtl result
  rtl_result_file=$RESULT_DIR/rtl_${t//\//_}
  rtl_result=$(cat "$rtl_result_file" 2>/dev/null || echo "MISSING")
  case "$rtl_result" in
    MISSING)
      FAIL+=("rtl:$t (binary missing)")
      ;;
    0)
      PASS+=("rtl:$t")
      ;;
    *)
      log=/tmp/rocket-rtl-${t//\//-}.log
      echo "--- rtl:$t FAILED (rc=$rtl_result, last 20 lines): ---"
      tail -n 20 "$log" || true
      FAIL+=("rtl:$t")
      ;;
  esac
done

echo "Passed (${#PASS[@]}):"
for p in "${PASS[@]}"; do echo "  PASS  $p"; done
echo "Failed (${#FAIL[@]}):"
for f in "${FAIL[@]}"; do echo "  FAIL  $f"; done

exit ${#FAIL[@]}
