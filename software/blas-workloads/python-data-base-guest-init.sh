#!/bin/bash

# This script will run on the guest every time the workload is built.

#go into /root
cd /root


#install blas
dnf install -y blas
#sometimes the shared library symlinks need to be defined manually
#ln -s /usr/lib64/libblas.so.3 /usr/lib64/libblas.so
#link the system libblas shared object to libblis shared object 
cp /root/blis/lib/riscv64/libblis.so.3 /usr/lib64/ 
#cp /root/blis/lib/gemmini/libblis.so.3 /usr/lib64/ 
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

SCIPYTESTFILE=scipytest.py
DATATESTFILE=scikittest.py

echo "import numpy as np" > $SCIPYTESTFILE
echo "import scipy" >> $SCIPYTESTFILE
echo "import scipy.linalg" >> $SCIPYTESTFILE
echo "import timeit" >> $SCIPYTESTFILE

echo "n=1600" >> $SCIPYTESTFILE

echo "#get matrix with pre-detrmined condition number" >> $SCIPYTESTFILE
echo "#cond_P = 10     # Condition number" >> $SCIPYTESTFILE
echo "#log_cond_P = np.log(cond_P)" >> $SCIPYTESTFILE
echo "#exp_vec = np.arange(-log_cond_P/4., log_cond_P * (n + 1)/(4 * (n - 1)), log_cond_P/(2.*(n-1)))" >> $SCIPYTESTFILE
echo "#s = np.exp(exp_vec)" >> $SCIPYTESTFILE
echo "#S = np.diag(s)" >> $SCIPYTESTFILE
echo "#U, _ = scipy.linalg.qr((np.random.rand(n, n) - 5.) * 200)" >> $SCIPYTESTFILE
echo "#V, _ = scipy.linalg.qr((np.random.rand(n, n) - 5.) * 200)" >> $SCIPYTESTFILE
echo "#P = U.dot(S).dot(V.T)" >> $SCIPYTESTFILE
echo "#P = P.dot(P.T)" >> $SCIPYTESTFILE
echo "#a = P.astype('float32')" >> $SCIPYTESTFILE

echo "#get well-conditioned PSD matrix" >> $SCIPYTESTFILE
echo "a = 1 + np.random.rand(n, n).astype('float32')" >> $SCIPYTESTFILE
echo "a = np.dot(a, a.transpose()) + 100000*np.identity(n)" >> $SCIPYTESTFILE
echo "a = a.astype('float32')" >> $SCIPYTESTFILE

echo "#a = np.random.rand(n, n).astype('float32')" >> $SCIPYTESTFILE
echo "y = np.random.rand(n).astype('float32')" >> $SCIPYTESTFILE

echo "print('compute the LU decomposition of a matrix scipy.linalg.lu(a):')" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.lu(a)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE

echo "print('compute the QR decomposition of a matrix scipy.linalg.qr(a):')" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.qr(a)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE

echo "print('compute the SVD decomposition of a matrix scipy.linalg.svd(a):')" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.svd(a)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE

echo "pos_sem = np.dot(a, a.transpose())" >> $SCIPYTESTFILE
echo "print('compute the cholesky decomposition of a matrix scipy.linalg.cholesky(a):')" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.cholesky(pos_sem)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE

echo "print('solve a linear system of equations scipy.linalg.solve(a, y):')" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.solve(a, y)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE

echo "print('solve a least square scipy.linalg.lapack.sgels(a, y):')" >> $SCIPYTESTFILE
echo "sgelslwork, info = scipy.linalg.lapack.sgels_lwork(a.shape[0], a.shape[1], 1)" >> $SCIPYTESTFILE
echo "t = timeit.timeit('scipy.linalg.lapack.sgels(a, y, lwork=sgelslwork)', number=3, globals=globals())" >> $SCIPYTESTFILE
echo "print(t)" >> $SCIPYTESTFILE


#echo "print('compute the inverse of a matrix scipy.linalg.inv(a):')" >> $SCIPYTESTFILE
#echo "a_inv = scipy.linalg.inv(a)" >> $SCIPYTESTFILE
#echo "t = timeit.timeit('scipy.linalg.inv(a)', number=3, globals=globals())" >> $SCIPYTESTFILE
#echo "print(t)" >> test.py

#echo "print('compute the determinante of a matrix scipy.linalg.det(a):')" >> $SCIPYTESTFILE
#echo "a_det = scipy.linalg.det(a)" >> $SCIPYTESTFILE
#echo "t = timeit.timeit('scipy.linalg.det(a)', number=3, globals=globals())" >> $SCIPYTESTFILE
#echo "print(t)" >> test.py

#echo "print('compute the eigvenvalues and eigenvectors of a matrix scipy.linalg.eig(a):')" >> $SCIPYTESTFILE
#echo "a_eigvals, a_eigvecs = scipy.linalg.eig(a)" >> $SCIPYTESTFILE
#echo "t = timeit.timeit('scipy.linalg.eig(a)', number=5, globals=globals())" >> $SCIPYTESTFILE
#echo "print(t)" >> $SCIPYTESTFILE

#echo "print('compute least square regression scipy.linalg.lstsq(a, y):')" >> $SCIPYTESTFILE
#echo "lstsq_p, lstsq_res, lstsq_rnk, lstsq_s = scipy.linalg.lstsq(a, y)" >> $SCIPYTESTFILE
#echo "t = timeit.timeit('scipy.linalg.lstsq(a, y)', number=3, globals=globals())" >> $SCIPYTESTFILE
#echo "print(t)" >> $SCIPYTESTFILE

#echo "print('compute kmean clusters of a matrix scipy.cluster.vq.kmeans(a, 3):')" >> $SCIPYTESTFILE
#echo "import scipy.cluster" >> $SCIPYTESTFILE
#echo "centroids,_ = scipy.cluster.vq.kmeans(a, 3)" >> $SCIPYTESTFILE
#echo "t = timeit.timeit('scipy.cluster.vq.kmeans(a, 3)', number=3, globals=globals())" >> $SCIPYTESTFILE
#echo "print(t)" >> $SCIPYTESTFILE

echo "import numpy as np" > $DATATESTFILE
echo "import sklearn" >> $DATATESTFILE
echo "import sklearn.cluster" >> $DATATESTFILE
echo "import sklearn.linear_model" >> $DATATESTFILE
echo "import timeit" >> $DATATESTFILE

echo "from numpy import genfromtxt" >> $DATATESTFILE
echo "a = genfromtxt('HRI-test-X.csv', dtype='float32', delimiter=',', skip_header=1)" >> $DATATESTFILE
echo "n = np.shape(a)[0]" >> $DATATESTFILE
echo "y = np.random.randn(n).astype('float32')" >> $DATATESTFILE

#echo "n=1600" >> $DATATESTFILE
#echo "nb=32" >> $DATATESTFILE
#echo "a = np.random.randn(n, n).astype('float32')" >> $DATATESTFILE
#echo "y = np.random.randn(n).astype('float32')" >> $DATATESTFILE

echo "print('compute PCA using sklearn: PCA(n_components=5).fit(a)')" >> $DATATESTFILE
echo "import sklearn.decomposition" >> $DATATESTFILE
echo "t = timeit.timeit('sklearn.decomposition.PCA(n_components=5).fit(a)', number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

echo "print('compute linear regression sklearn: sklearn.linear_model.LinearRegression().fit(a, y)')" >> $DATATESTFILE
echo "t = timeit.timeit('sklearn.linear_model.LinearRegression().fit(a, y)', number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

echo "print('compute Ridge regression sklearn: sklearn.linear_model.Ridge(alpha=0.1).fit(a, y)')" >> $DATATESTFILE
echo "t = timeit.timeit('sklearn.linear_model.Ridge(alpha=0.1).fit(a, y)', number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

echo "print(\"compute K-means clustering using sklearn EM-style algorithm : sklearn.cluster.KMeans(n_clusters=5, random_state=0, algorithm='full').fit_predict(a)\")" >> $DATATESTFILE
echo "t = timeit.timeit(\"sklearn.cluster.KMeans(n_clusters=5, random_state=0, algorithm='full').fit_predict(a)\", number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

echo "import sklearn.random_projection" >> $DATATESTFILE
echo "print('compute random projection of A sklearn : sklearn.random_projection.GaussianRandomProjection().fit_transform(a)')" >> $DATATESTFILE
echo "t = timeit.timeit('sklearn.random_projection.GaussianRandomProjection(eps=0.5).fit_transform(a)', number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

echo "import sklearn.svm" >> $DATATESTFILE
echo "print('compute SVM using sklearn: sklearn.svm.SVC().fit(a, y)')" >> $DATATESTFILE
echo "t = timeit.timeit('sklearn.svm.SVC().fit(a, y.astype("int32"))', number=3, globals=globals())" >> $DATATESTFILE
echo "print(t)" >> $DATATESTFILE

# my original attempt with sklearn kmean, using default fit().predict()
#echo "print('compute K-means clustering using sklearn: sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit(a).predict(a)')" >> $DATATESTFILE
#echo "import sklearn.cluster" >> $DATATESTFILE
#echo "t = timeit.timeit('sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit(a).predict(a)', number=5, globals=globals())" >> $DATATESTFILE
#echo "print(t)" >> $DATATESTFILE

#echo "print('compute K-means clustering using sklearn elkan algorithm: sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit_predict(a)')" >> $DATATESTFILE
#echo "import sklearn.cluster" >> $DATATESTFILE
##echo "t = timeit.timeit('sklearn.cluster.KMeans(n_clusters=5, random_state=0).fit_predict(a)', number=3, globals=globals())" >> $DATATESTFILE
#echo "print(t)" >> $DATATESTFILE

#echo "print('compute least angle regression sklearn: sklearn.linear_model.Lars().fit(a, y)')" >> $DATATESTFILE
#echo "t = timeit.timeit('sklearn.linear_model.Lars().fit(a, y)', number=5, globals=globals())" >> $DATATESTFILE
#echo "print(t)" >> $DATATESTFILE

poweroff guest-init
sync
poweroff
