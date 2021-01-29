#!/bin/bash

# This script will run on the guest every time the workload is built.

#go into /root
cd /root

#EIGENVERSION="3.3.8"
#wget https://gitlab.com/libeigen/eigen/-/archive/3.3.8/eigen-3.3.8.tar.gz
#wget https://gitlab.com/libeigen/eigen/-/archive/${EIGENVERSION}/eigen-${EIGENVERSION}.tar.gz
#tar -xf eigen-3.3.8.tar.gz
#tar -xf "eigen-${EIGENVERSION}.tar.gz"

#clone ilqgames
git clone git://github.com/alonamid/ilqgames.git
cd ilqgames
git checkout n-player


#install dependancies if needed
#glog, gflags, opengl, glut, eigen3
dnf install -y glog glog-devel gflags-devel glfw-devel freeglut-devel eigen3-devel

#install blas
dnf install -y blas
#sometimes the shared library symlinks need to be defined manually
#ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
cp /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/ 
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblis.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblas.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libcblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libcblas.so

#install lapack
dnf install -y lapack
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/liblapack.so.3 /usr/lib64/liblapack.so

#build
mkdir -p build && cd build
cmake ..
#tell eigen to use BLAS instead of internal implementation
make CXX_FLAGS="-DEIGEN_USE_BLAS=1"

#poweroff guest-init
sync
poweroff
