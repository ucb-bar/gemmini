#!/bin/bash

# This script will run on the guest every time the workload is built.

#go into /root
cd /root

#sometimes the shared library symlinks need to be defined manually
#ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
echo "copying /root/blis/lib/riscv64/libblis.so.3 to /usr/lib64/"
cp /root/blis/lib/riscv64/libblis.so.3 /usr/lib64/ 
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblis.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblas.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libcblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libcblas.so

echo "copying /root/lapack-nb32/liblapack.so.3.9.0 /usr/lib64/liblapack.so.3.9.0 to /usr/lib64/"
cp /root/lapack-nb32/liblapack.so.3.9.0 /usr/lib64/liblapack.so.3.9.0
ln -s /usr/lib64/liblapack.so.3.9.0 /usr/lib64/liblapack.so

poweroff guest-init
sync
poweroff
