#!/bin/bash

# This script will run on the guest every time the workload is built.

JULVERSION="v1.5.0"

#go into /root
cd /root

#install dependancies if needed
dnf install llvm llvm-devel

#install blas
dnf install -y blas
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libcblas.so

#install lapack
dnf install -y lapack
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/liblapack.so.3 /usr/lib64/liblapack.so

#clone Julia
git clone git://github.com/JuliaLang/julia.git
cd julia
#git checkout v1.5.0
git checkout "${JULVERSION}"

#tell Julia to use the system BLAS and LAPACK rather than OpenBLAS
echo "USE_SYSTEM_BLAS=1" > Make.user
#echo "LIBBLAS=-lblas" >> Make.user
#echo "LIBBLASNAME=-lblas" >> Make.user
echo "USE_SYSTEM_LAPACK=1" >> Make.user
#echo "LIBLAPACK=-llapack" >> Make.user
#echo "LIBLAPACKNAME=-liblapack" >> Make.user
echo "DISABLE_LIBUNWIND=1" >> Make.user
echo "USE_SYSTEM_LIBM=1" >> Make.user
echo "USE_SYSTEM_LLVM=1" >> Make.user
echo "USE_SYSTEM_PCRE=1" >> Make.user

#build
make
#this will currently fails, because GNU libunwind is not ported to RISC-V
#there is an llvm libunwind for RISC-V, but Julia currently does not support that

#poweroff guest-init
sync
poweroff
