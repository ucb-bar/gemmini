#!/usr/bin/env bash

usage() {
	echo "usage: ${0} [OPTIONS]"
	echo ""
	echo ""
	echo "Options"
	echo "	-i I : I dimension the matrix"
	echo "	-k K : K dimension the matrix"
	echo "	-j J : J dimension the matrix"
	echo "  -w W : AWeight parameter"
	echo "  -f FILE : explicipt output file path"
	exit "$1"
}

RDIR=$(pwd)
MAXAWEIGHT=5
MINI=32
MAXI=512
MINJ=32
MAXJ=512
MINK=32
MAXK=512
DIM=8
FILE=${RDIR}/mat_shape_sweep_adaptive_weighta_fcfs.csv
i=$MINI
j=$MINJ
k=$MINK


while [ "$1" != "" ];
do
	case $1 in
	  -w )
	    shift
	    MAXAWEIGHT=$1 ;;
	  -k )
	    shift
	    MAXK=$1 ;;
	  -i )
	    shift
	    MAXI=$1 ;;
	  -j )
	    shift
	    MAXJ=$1 ;;
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

#echo "DIM,I,J,K,AWEIGHT,UTILIZATION,CYCLES" > ${FILE}

while [ $i -le $MAXI ]
do
  while [ $j -le $MAXJ ]
  do
    while [ $k -le $MAXK ]
    do
      for (( w=0; w <= $MAXAWEIGHT; w++ ))
      do
      
        echo "Running experiement with i=$i, j=$j, k=$k, and w=$w"
        
        cd $RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/
        git checkout bareMetalC/tiled_matmul_ws_perf.c
        
        echo "--- a/bareMetalC/tiled_matmul_ws_perf.c" >  temp.patch
        echo "+++ b/bareMetalC/tiled_matmul_ws_perf.c" >> temp.patch
        echo "@@ -26,9 +26,9 @@"                       >> temp.patch
        echo " // #define MAT_DIM_K 128"               >> temp.patch
        echo " // #define MAT_DIM_J 128"               >> temp.patch
        echo ""                                        >> temp.patch 
        echo "-#define MAT_DIM_I 256"                  >> temp.patch
        echo "-#define MAT_DIM_K 256"                  >> temp.patch
        echo "-#define MAT_DIM_J 256"                  >> temp.patch
        echo "+#define MAT_DIM_I $i"                   >> temp.patch
        echo "+#define MAT_DIM_K $k"                   >> temp.patch
        echo "+#define MAT_DIM_J $j"                   >> temp.patch
        echo " "                                       >> temp.patch
        echo " // #define MAT_DIM_I 256"               >> temp.patch
        echo " // #define MAT_DIM_K 512"               >> temp.patch
        echo "@@ -89,7 +89,7 @@ int main() {"          >> temp.patch
        echo "             NO_ACTIVATION, ACC_SCALE_IDENTITY, 0, REPEATING_BIAS," >> temp.patch
        echo "             A_TRANSPOSE, B_TRANSPOSE,"                             >> temp.patch
        echo "             false, false,"                                         >> temp.patch
        echo "-            3,"                                                    >> temp.patch
        echo "+            $w,"                                                    >> temp.patch
        echo "             WS);"                                                  >> temp.patch
        echo " "                                                                  >> temp.patch
        echo "     unsigned long end = read_cycles();"                            >> temp.patch
        
        
        patch bareMetalC/tiled_matmul_ws_perf.c -i temp.patch
        rm temp.patch
        
        ./build.sh
        cd $RDIR
        
        TESTBIN=$RDIR/../../../../../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/tiled_matmul_ws_perf-baremetal
        
        
        #Latency-Bandwidth Pipeline
        #./FireSim $TESTBIN +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_writeMaxReqs_0=116 +mm_readMaxReqs_0=116 +mm_writeLatency_0=200 +mm_readLatency_0=200 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 > temp.temp
        
        #DDR+LLC model
        #./FireSim $TESTBIN  +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=8 +mm_transactionQueueDepth_0=8 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +mm_llc_wayBits_0=3 +mm_llc_setBits_0=12 +mm_llc_blockBits_0=7 +mm_llc_activeMSHRs_0=8 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 > temp.temp
        
        #DDR (only) model
        ./FireSim $TESTBIN  +vcs+initreg+0 +vcs+initmem+0 +fesvr-step-size=128 +mm_relaxFunctionalModel_0=0 +mm_openPagePolicy_0=1 +mm_backendLatency_0=2 +mm_schedulerWindowSize_0=32 +mm_transactionQueueDepth_0=32 +mm_dramTimings_tAL_0=0 +mm_dramTimings_tCAS_0=14 +mm_dramTimings_tCMD_0=1 +mm_dramTimings_tCWD_0=10 +mm_dramTimings_tCCD_0=4 +mm_dramTimings_tFAW_0=25 +mm_dramTimings_tRAS_0=33 +mm_dramTimings_tREFI_0=7800 +mm_dramTimings_tRC_0=47 +mm_dramTimings_tRCD_0=14 +mm_dramTimings_tRFC_0=160 +mm_dramTimings_tRRD_0=8 +mm_dramTimings_tRP_0=14 +mm_dramTimings_tRTP_0=8 +mm_dramTimings_tRTRS_0=2 +mm_dramTimings_tWR_0=15 +mm_dramTimings_tWTR_0=8 +mm_rowAddr_offset_0=18 +mm_rowAddr_mask_0=65535 +mm_rankAddr_offset_0=16 +mm_rankAddr_mask_0=3 +mm_bankAddr_offset_0=13 +mm_bankAddr_mask_0=7 +shmemportname0=0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 +macaddr0=00:00:00:00:00:02 +niclog0=niclog0 +linklatency0=6405 +netbw0=100 +netburst0=8 +nic-loopback0 +tracefile=TRACEFILE +blkdev-in-mem0=128 +blkdev-log0=blkdev-log0 +autocounter-readrate=1000 +autocounter-filename=AUTOCOUNTERFILE +dramsim +max-cycles=100000000 > temp.temp
        
        
        UTIL=$(tail -n1 temp.temp | cut -d ' ' -f2 | sed 's/%//')
        CYCLES=$(tail -n2 temp.temp | head -n1 | cut -d ' ' -f3)
        rm temp.temp
        
        echo "${DIM},${i},${j},${k},${w},${UTIL},${CYCLES}" >> ${FILE}
        
      done
      k=$(( $k * 2 ))
    done
    k=$MINK
    j=$(( $j * 2 ))
  done
  j=$MINJ
  i=$(( $i * 2 ))
done
i=$MINI
