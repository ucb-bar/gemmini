#!/bin/bash

# This script will run on the guest every time the workload is built.

RVERSION="R-4.0.3"

#go into /root
cd /root

#wget https://ftp.osuosl.org/pub/cran/src/base/R-4/R-4.0.3.tar.gz
wget "https://ftp.osuosl.org/pub/cran/src/base/R-4/${RVERSION}.tar.gz"
#tar -xf R-4.0.3.tar.gz
tar -xf "${RVERSION}.tar.gz"

#install dependancies
dnf install -y bzip2 cairo fontconfig freetype fribidi glib2 harfbuzz libX11 libXext libXt libcurl libicu libjpeg libpng libtiff libtirpc libxcrypt ncurses pango pkgconf-pkg-config pcre2 readline tcl tk xz zlib
dnf install -y readline-devel bzip2-devel libcurl-devel texlive-scheme-basic

#install blas
dnf install -y blas
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblis.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libcblas.so

#install lapack
dnf install -y lapack
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/liblapack.so.3 /usr/lib64/liblapack.so


#cd R-4.0.3/
cd "${RVERSION}/"
./configure --with-x=no --disable-java --with-blas="blis"
make
make install

#get benchmark
wget https://mac.r-project.org/benchmarks/R-benchmark-25.R

#run benchmark
#R CMD BATCH R-benchmark-25.R

#poweroff guest-init
sync
poweroff -f
