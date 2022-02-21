#!/usr/bin/env bash

IJ=32
K=1000
DIM=8

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


patch_bin() ( # <directory> <NEVICT>
  RDIR=${1}
  shift
  NEVICT=${1}

  cd $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/
  git checkout bareMetalC/tiled_matmul_ws_perf_evict.c
  
  echo "--- a/bareMetalC/tiled_matmul_ws_perf_evict.c" >  temp.patch
  echo "+++ b/bareMetalC/tiled_matmul_ws_perf_evict.c" >> temp.patch
  echo "@@ -26,9 +26,9 @@"                       >> temp.patch
  echo " // #define MAT_DIM_K 128"               >> temp.patch
  echo " // #define MAT_DIM_J 128"               >> temp.patch
  echo ""                                        >> temp.patch 
  echo "-#define MAT_DIM_I 32"                  >> temp.patch
  echo "-#define MAT_DIM_K 1022"                  >> temp.patch
  echo "-#define MAT_DIM_J 32"                  >> temp.patch
  echo "+#define MAT_DIM_I $IJ"                   >> temp.patch
  echo "+#define MAT_DIM_K $K"                   >> temp.patch
  echo "+#define MAT_DIM_J $IJ"                   >> temp.patch
  echo " "                                       >> temp.patch
  echo " // #define MAT_DIM_I 256"               >> temp.patch
  echo " // #define MAT_DIM_K 512"               >> temp.patch
  echo "@@ -38,7 +38,7 @@"                       >> temp.patch
  echo ""                                        >> temp.patch 
  echo " #define L2SIZE 524288"                  >> temp.patch
  echo " #define L2LINESIZE 64"                  >> temp.patch
  echo "-#define NUMDIRTY 1"                     >> temp.patch
  echo "+#define NUMDIRTY $NEVICT"               >>  temp.patch
  echo ""                                        >> temp.patch 
  echo " #if A_TRANSPOSE==0"                     >> temp.patch 
  echo " #define A_STRIDE MAT_DIM_K"             >> temp.patch 
  

  patch bareMetalC/tiled_matmul_ws_perf_evict.c -i temp.patch
  rm temp.patch
  
  ./build.sh
  cd $RDIR

)


RDIR=$(pwd)
FILE=${RDIR}/ddr_evict_sweep_ooo_frfcfs_fattail.csv
TESTBIN=$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/tiled_matmul_ws_perf_evict-baremetal

while [ "$1" != "" ];
do
	case $1 in
	  -i )
	    shift
	    IJ=$1 ;;
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


# CSV Header: IJ, IJ, K, UTILIZATION, TAIL LATENCY

echo "Running sweep experiement with i=$IJ, j=$IJ, w=1"
for NEVICT in {1..10..1}
do

  patch_bin $RDIR $NEVICT

  ./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=16 +mm_transactionQueueDepth_0=16 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 &> DDR_K${K}-EVICT${NEVICT}.out

  TAIL=$(grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n1)
  UTIL=$(grep Utilization DDR_K${K}-EVICT${NEVICT}.out | cut -d ' ' -f2 | sed 's/%//')

  echo "${IJ},${IJ},${K},${NEVICT},${UTIL},${TAIL}" >> ${FILE}
  grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n10 >> ${FILE}

done

for NEVICT in {20..100..10}
do

  patch_bin $RDIR $NEVICT

  ./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=16 +mm_transactionQueueDepth_0=16 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 &> DDR_K${K}-EVICT${NEVICT}.out

  TAIL=$(grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n1)
  UTIL=$(grep Utilization DDR_K${K}-EVICT${NEVICT}.out | cut -d ' ' -f2 | sed 's/%//')

  echo "${IJ},${IJ},${K},${NEVICT},${UTIL},${TAIL}" >> ${FILE}
  grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n10 >> ${FILE}

done

for NEVICT in {200..300..100}
do

  patch_bin $RDIR $NEVICT

  ./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=16 +mm_transactionQueueDepth_0=16 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 &> DDR_K${K}-EVICT${NEVICT}.out

  TAIL=$(grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n1)
  UTIL=$(grep Utilization DDR_K${K}-EVICT${NEVICT}.out | cut -d ' ' -f2 | sed 's/%//')

  echo "${IJ},${IJ},${K},${NEVICT},${UTIL},${TAIL}" >> ${FILE}
  grep TL-XACT-LATENCY DDR_K${K}-EVICT${NEVICT}.out | awk '{printf("%s\n",$2)}' | sort -n | tail -n10 >> ${FILE}

done
