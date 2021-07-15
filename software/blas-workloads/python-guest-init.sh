#!/bin/bash

# This script will run on the guest every time the workload is built.

#go into /root
cd /root


#install blas
dnf install -y blas
#sometimes the shared library symlinks need to be defined manually
#ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
#cp /root/blis/lib/riscv64/libblis.so.3 /usr/lib64/ 
#cp /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/ 
cp /root/blis/lib/gemminihwacha/libblis.so.3 /usr/lib64/ 
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblis.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libblas.so
ln -s /usr/lib64/libblis.so.3 /usr/lib64/libcblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libblas.so
#ln -s /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/libcblas.so

#install lapack
dnf install -y lapack
#sometimes the shared library symlinks need to be defined manually
ln -s /usr/lib64/liblapack.so.3 /usr/lib64/liblapack.so

#get refernece lapack sources if we want to build from source
#wget https://github.com/Reference-LAPACK/lapack/archive/v3.9.0.tar.gz

#change block size in lapack by changing the lapack rpm
#mkdir /root/lapack-rpm
#cd /root/lapack-rpm
#wget http://fedora.riscv.rocks/kojifiles/packages/lapack/3.9.0/5.fc33/src/lapack-3.9.0-5.fc33.src.rpm
#rpm -i lapack-3.9.0-5.fc33.src.rpm
#cp /root/lapack-3.9.0-ilaenv-blocksize.patch ~/rpmbuild/SOURCES/
#cp /root/lapack-3.9.0-ilaenv-nb.patch ~/rpmbuild/SOURCES/
#cp /root/lapack.spec ~/rpmbuild/SPECS/lapack.spec
#cd /root/rpmbuild/SPECS
#rpmbuild -ba lapack.spec
#cd /root/rpmbuild/RPMS/riscv64
#sudo dnf install --skip-broken lapack-3.9.0-5.fc32.riscv64.rpm
#cd ~
#cp /root/rpmbuild/BUILD/lapack-3.9.0/liblapack.so.3.9.0 /usr/lib64/liblapack.so.3.9.0
#cp /root/rpmbuild/BUILD/lapack-3.9.0/liblapacke.so.3.9.0 /usr/lib64/liblapacke.so.3.9.0
#ln -s /usr/lib64/liblapack.so.3.9.0 /usr/lib64/liblapack.so

#build
python -m pip install --user numpy scipy 
python -m pip install --user sklearn 

#for benchmarking
pip install cython asv

#git clone https://github.com/numpy/numpy.git
#cd numpy
#https://github.com/numpy/numpy/blob/master/benchmarks/benchmarks/bench_linalg.py
#python runtests.py --bench bench_linalg


echo "import numpy as np" > test.py
echo "import scipy" >> test.py
echo "import scipy.linalg" >> test.py
echo "import sklearn" >> test.py
echo "import timeit" >> test.py

echo "n=1000" >> test.py
echo "nb=32" >> test.py
echo "a = np.random.randn(n, n).astype('float32')" >> test.py
echo "y = np.random.randn(n).astype('float32')" >> test.py
#echo "print(scipy.linalg.lapack.sgeqp3(a, lwork=2*n+(n+1)*nb))" >> test.py

echo "print('compute the inverse of a matrix scipy.linalg.inv(a):')" >> test.py
#echo "a_inv = scipy.linalg.inv(a)" >> test.py
echo "t = timeit.timeit('scipy.linalg.inv(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute the determinante of a matrix scipy.linalg.det(a):')" >> test.py
#echo "a_det = scipy.linalg.det(a)" >> test.py
echo "t = timeit.timeit('scipy.linalg.det(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

#echo "print('compute the eigvenvalues and eigenvectors of a matrix scipy.linalg.eig(a):')" >> test.py
#echo "a_eigvals, a_eigvecs = scipy.linalg.eig(a)" >> test.py
#echo "t = timeit.timeit('scipy.linalg.eig(a)', number=5, globals=globals())" >> test.py
#echo "print(t)" >> test.py

echo "print('solve a linear system of equations scipy.linalg.solve(a, b):')" >> test.py
#echo "x_solve = scipy.linalg.solve(a, b)" >> test.py
echo "t = timeit.timeit('scipy.linalg.solve(a, y)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute the singular value decomposition (SVD) of a matrix scipy.linalg.svd(a):')" >> test.py
#echo "svd_U, svd_s, svd_Vh = scipy.linalg.svd(a)" >> test.py
echo "t = timeit.timeit('scipy.linalg.svd(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute the LU decomposition of a matrix scipy.linalg.lu(a):')" >> test.py
#echo "lu_l, lu_u = scipy.linalg.lu(a)" >> test.py
echo "t = timeit.timeit('scipy.linalg.lu(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute the QR decomposition of a matrix scipy.linalg.qr(a):')" >> test.py
#echo "qr_q, qr_r = scipy.linalg.qr(a)" >> test.py
echo "t = timeit.timeit('scipy.linalg.qr(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute kmean clusters of a matrix scipy.cluster.vq.kmeans(a, 3):')" >> test.py
echo "import scipy.cluster" >> test.py
#echo "centroids,_ = scipy.cluster.vq.kmeans(a, 3)" >> test.py
echo "t = timeit.timeit('scipy.cluster.vq.kmeans(a, 3)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute least square regression scipy.linalg.lstsq(a, y):')" >> test.py
#echo "lstsq_p, lstsq_res, lstsq_rnk, lstsq_s = scipy.linalg.lstsq(a, y)" >> test.py
echo "t = timeit.timeit('scipy.linalg.lstsq(a, y)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute PCA using sklearn: PCA(n_components=2).fit(a)')" >> test.py
echo "import sklearn.decomposition" >> test.py
echo "t = timeit.timeit('sklearn.decomposition.PCA(n_components=2).fit(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

# my original attempt with sklearn kmean, using default fit().predict()
#echo "print('compute K-means clustering using sklearn: sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit(a).predict(a)')" >> test.py
#echo "import sklearn.cluster" >> test.py
#echo "t = timeit.timeit('sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit(a).predict(a)', number=5, globals=globals())" >> test.py
#echo "print(t)" >> test.py

echo "print('compute K-means clustering using sklearn elkan algorithm: sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit_predict(a)')" >> test.py
echo "import sklearn.cluster" >> test.py
echo "t = timeit.timeit('sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit_predict(a)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print(\"compute K-means clustering using sklearn EM-style algorithm : sklearn.cluster.KMeans(n_clusters=5, random_state=0, algorithm='full').fit_predict(a)\")" >> test.py
echo "import sklearn.cluster" >> test.py
echo "t = timeit.timeit(\"sklearn.cluster.KMeans(n_clusters=5, random_state=0, algorithm='full').fit_predict(a)\", number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute Ridge regression sklearn: sklearn.linear_model.Ridge(alpha=0.1).fit(a, y)')" >> test.py
echo "import sklearn.linear_model" >> test.py
echo "t = timeit.timeit('sklearn.linear_model.Ridge(alpha=0.1).fit(a, y)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

echo "print('compute linear regression sklearn: sklearn.linear_model.LinearRegression().fit(a, y)')" >> test.py
echo "t = timeit.timeit('sklearn.linear_model.LinearRegression().fit(a, y)', number=5, globals=globals())" >> test.py
echo "print(t)" >> test.py

#echo "print('compute least angle regression sklearn: sklearn.linear_model.Lars().fit(a, y)')" >> test.py
#echo "t = timeit.timeit('sklearn.linear_model.Lars().fit(a, y)', number=5, globals=globals())" >> test.py
#echo "print(t)" >> test.py

#poweroff guest-init
sync
poweroff
