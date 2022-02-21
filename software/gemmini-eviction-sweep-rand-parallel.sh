#!/usr/bin/env bash

I=401
J=401
K=32
DIM=8
SEED=123
OOO=0
KPORTION=1
INTERLEAVED=1

usage() {
	echo "usage: ${0} [OPTIONS]"
	echo ""
	echo ""
	echo "Options"
	echo "	-i I : dimension of the I and J matrix dimensions"
	echo "  -k K : dimension of the K matrix dimension"
	echo "  -f FILE : explicipt output file path"
	exit "$1"
}


patch_bin() ( # <directory> <NEVICT> <NSEED>
  RDIR=${1}
  shift
  NEVICT=${1}
  shift
  NSEED=${1}

  cd $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/
  git checkout bareMetalC/tiled_matmul_ws_perf_evict_rand.c
  
  echo "--- a/bareMetalC/tiled_matmul_ws_perf_evict_rand.c" >  temp.patch
  echo "+++ b/bareMetalC/tiled_matmul_ws_perf_evict_rand.c" >> temp.patch
  echo "@@ -26,9 +26,9 @@"                       >> temp.patch
  echo " // #define MAT_DIM_K 128"               >> temp.patch
  echo " // #define MAT_DIM_J 128"               >> temp.patch
  echo ""                                        >> temp.patch 
  echo "-#define MAT_DIM_I 32"                  >> temp.patch
  echo "-#define MAT_DIM_K 1000"                  >> temp.patch
  echo "-#define MAT_DIM_J 32"                  >> temp.patch
  echo "+#define MAT_DIM_I $I"                   >> temp.patch
  echo "+#define MAT_DIM_K $K"                   >> temp.patch
  echo "+#define MAT_DIM_J $J"                   >> temp.patch
  echo " "                                       >> temp.patch
  echo " // #define MAT_DIM_I 256"               >> temp.patch
  echo " // #define MAT_DIM_K 512"               >> temp.patch
  echo "@@ -38,8 +38,8 @@"                       >> temp.patch
  echo ""                                        >> temp.patch 
  echo " #define L2SIZE 524288"                  >> temp.patch
  echo " #define L2LINESIZE 64"                  >> temp.patch
  echo "-#define NUMDIRTY 10"                    >> temp.patch
  echo "-#define SEED 111"                       >> temp.patch
  echo "+#define NUMDIRTY $NEVICT"               >> temp.patch
  echo "+#define SEED $NSEED"                    >> temp.patch
  echo ""                                        >> temp.patch 
  echo " #if A_TRANSPOSE==0"                     >> temp.patch 
  echo " #define A_STRIDE MAT_DIM_K"             >> temp.patch 

  echo "patch with NEVICT ${NEVICT} and SEED ${NSEED}"  
  patch bareMetalC/tiled_matmul_ws_perf_evict_rand.c -i temp.patch
  rm temp.patch
 
  cd $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC

  riscv64-unknown-elf-gcc  -DPREALLOCATE=1 -DMULTITHREAD=1 -mcmodel=medany -std=gnu99 -O2 -ffast-math -fno-common -fno-builtin-printf -march=rv64gc -Wa,-march=rv64gcxhwacha -lm -lgcc -I$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests -I$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests/env -I$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/.. -I$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests/benchmarks/common -DID_STRING=  -nostdlib -nostartfiles -static -T $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests/benchmarks/common/test.ld -DBAREMETAL=1  $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../bareMetalC/tiled_matmul_ws_perf_evict_rand.c  -o tiled_matmul_ws_perf_evict_rand_${NEVICT}_${NSEED}-baremetal \
	$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests/benchmarks/common/syscalls.c $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/../riscv-tests/benchmarks/common/crt.S

  cd $RDIR

)


RDIR=$(pwd)
#FILE=${RDIR}/ddr_evict_sweep_ooo_frfcfs_rand_nobias_parallel-shapes.csv
#FATTAILFILE=${RDIR}/ddr_evict_sweep_ooo_frfcfs_rand_nobias_parallel_fattail-shapes.csv
FILE=${RDIR}/ddr_evict_sweep_ooo_frfcfs_rand_nobias_parallel-maxiblocks1.csv
FATTAILFILE=${RDIR}/ddr_evict_sweep_ooo_frfcfs_rand_nobias_parallel_fattail-maxiblocks1.csv
#TESTBIN=$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/tiled_matmul_ws_perf_evict_rand-baremetal

while [ "$1" != "" ];
do
	case $1 in
	  -i )
	    shift
	    I=$1 ;;
	  -j )
	    shift
	    J=$1 ;;
	  -k )
	    shift
	    K=$1 ;;
	  -d )
	    shift
	    DIM=$1 ;;
	  -f )
	    shift
	    FILE=$(realpath $1) ;;
    	  * )
	    error "invalid option $1"
	    usage 1 ;;
	esac
	shift
done


# CSV Header: I, J, K, UTILIZATION, TAIL LATENCY
for NEVICT in {10..90..10}
#for NEVICT in {1..9..1}
do
for SEEDMUL in {1..10..1}
do
  NSEED=$((SEED * SEEDMUL))
  patch_bin $RDIR $NEVICT $NSEED
done
done

for NEVICT in {10..90..10}
#for NEVICT in {1..9..1}
do
for SEEDMUL in {1..10..1}
do
  NSEED=$((SEED * SEEDMUL))
  TESTBIN=$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/tiled_matmul_ws_perf_evict_rand_${NEVICT}_${NSEED}-baremetal
  ./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=16 +mm_transactionQueueDepth_0=16 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 &> DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out &
  pids[${SEEDMUL}]=$!
done

for pid in ${pids[*]}; do
    wait $pid
done

#  SEEDMUL=10
#  NSEED=$((SEED * SEEDMUL))
#  TESTBIN=$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/tiled_matmul_ws_perf_evict_rand_${NEVICT}_${NSEED}-baremetal
#  ./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=16 +mm_transactionQueueDepth_0=16 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 &> DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out

#  TAIL=$(grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n1)
#  UTIL=$(grep Utilization DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | cut -d ' ' -f2 | sed 's/%//')

#  echo "${OOO},${INTERLEAVED},${KPORTION},${I},${J},${K},${NEVICT},${NSEED},${UTIL},${TAIL}" >> ${FILE}
#  echo "${OOO},${INTERLEAVED},${KPORTION},${I},${J},${K},${NEVICT},${NSEED},${UTIL},${TAIL}" >> ${FATTAILFILE}
#  grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n10 >> ${FATTAILFILE}

for SEEDMUL in {1..10..1}
do
  NSEED=$((SEED * SEEDMUL))

  TAIL=$(grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n1)
  UTIL=$(grep Utilization DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | cut -d ' ' -f2 | sed 's/%//')

  echo "${OOO},${INTERLEAVED},${KPORTION},${I},${J},${K},${NEVICT},${NSEED},${UTIL},${TAIL}" >> ${FILE}
  echo "${OOO},${INTERLEAVED},${KPORTION},${I},${J},${K},${NEVICT},${NSEED},${UTIL},${TAIL}" >> ${FATTAILFILE}
  grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}-SEED${NSEED}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n10 >> ${FATTAILFILE}

done

done
