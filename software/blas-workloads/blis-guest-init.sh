#!/bin/bash

set -e

# This script will run on the guest from the root directory
echo "Linking Custom BLAS Library"
ln -s /root/lib/libblas.so.3 /root/blis/lib/gemmini/libblis.so
ln -s /root/lib/libblas.a /root/blis/lib/gemmini/libblis.a

poweroff
