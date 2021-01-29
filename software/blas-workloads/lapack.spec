%global shortver	3
%global mediumver	%{shortver}.9

%if %{?__isa_bits:%{__isa_bits}}%{!?__isa_bits:32} == 64
%global arch64 1
%else
%global arch64 0
%endif

Summary: Numerical linear algebra package libraries
Name: lapack
Version: %{mediumver}.0
Release: 5%{?dist}
License: BSD
URL: http://www.netlib.org/lapack/
Source0: https://github.com/Reference-LAPACK/lapack/archive/v%{version}.tar.gz
Source1: http://www.netlib.org/lapack/manpages.tgz
Source2: Makefile.blas
Source3: Makefile.lapack
Source4: http://www.netlib.org/lapack/lapackqref.ps
Source5: http://www.netlib.org/blas/blasqr.ps
Source6: Makefile.cblas
Patch3: lapack-3.9.0-make.inc.patch
Patch4: lapack-3.9.0-lapacke-shared.patch
Patch5: lapack-3.4.1-lapacke-disable-testing-functions.patch
Patch6: lapack-3.5.0-lapacke-matgenobj.patch
Patch7: lapack-3.9.0-lapacke-tmglib.patch
# Bugzilla 1814756
Patch8: https://github.com/Reference-LAPACK/lapack/commit/87536aa3c8bb0af00f66088fb6ac05d87509e011.patch
Patch9: lapack-3.9.0-ilaenv-nb.patch
Patch10: lapack-3.9.0-ilaenv-blocksize.patch
BuildRequires: gcc-gfortran, gawk
Requires: blas%{?_isa} = %{version}-%{release}

%global _description_lapack %{expand:
LAPACK (Linear Algebra PACKage) is a standard library for numerical
linear algebra. LAPACK provides routines for solving systems of
simultaneous linear equations, least-squares solutions of linear
systems of equations, eigenvalue problems, and singular value
problems. Associated matrix factorizations (LU, Cholesky, QR, SVD,
Schur, and generalized Schur) and related computations (i.e.,
reordering of Schur factorizations and estimating condition numbers)
are also included. LAPACK can handle dense and banded matrices, but
not general sparse matrices. Similar functionality is provided for
real and complex matrices in both single and double precision. LAPACK
is coded in Fortran90 and built with gcc.
}

%global _description_blas %{expand:
BLAS (Basic Linear Algebra Subprograms) is a standard library which
provides a number of basic algorithms for numerical algebra.
}

%description %_description_lapack

%package devel
Summary: LAPACK development libraries
Requires: %{name}%{?_isa} = %{version}-%{release}
Requires: blas-devel%{?_isa} = %{version}-%{release}

%description devel
LAPACK development libraries (shared).

%package static
Summary: LAPACK static libraries
Requires: lapack-devel%{?_isa} = %{version}-%{release}

%description static
LAPACK static libraries.

%package -n blas
Summary: The Basic Linear Algebra Subprograms library

%description -n blas %_description_blas

%package -n blas-devel
Summary: BLAS development libraries
Requires: blas%{?_isa} = %{version}-%{release}
Requires: gcc-gfortran

%description -n blas-devel
BLAS development libraries (shared).

%package -n blas-static
Summary: BLAS static libraries
Requires: blas-devel%{?_isa} = %{version}-%{release}

%description -n blas-static
BLAS static libraries.

%if 0%{?arch64}
%package -n lapack64
Summary: Numerical linear algebra package libraries
Requires: blas64%{?_isa} = %{version}-%{release}

%description -n lapack64 %_description_lapack
This build has 64bit INTEGER support.

%package -n blas64
Summary: The Basic Linear Algebra Subprograms library (64bit INTEGER)

%description -n blas64 %_description_blas
This build has 64bit INTEGER support.

%package -n lapack64_
Summary: Numerical linear algebra package libraries
Requires: blas64_%{?_isa} = %{version}-%{release}

%description -n lapack64_ %_description_lapack
This build has 64bit INTEGER support and a symbol name suffix.

%package -n blas64_
Summary: The Basic Linear Algebra Subprograms library (64bit INTEGER)

%description -n blas64_ %_description_blas
This build has 64bit INTEGER support and a symbol name suffix.
%endif

%prep
%setup -q
%setup -q -D -T -a1
%patch3 -p1 -b .fedora
%patch4 -p1 -b .shared
# %patch5 -p1 -b .disable-functions
# %patch6 -p1 -b .matgenobj
%patch7 -p1 -b .tmglib
%patch8 -p1 -b .bz1814756
%patch9 -p0 -b .fedora
%patch10 -p0 -b .fedora

mkdir manpages
mv man/ manpages/

cp -f INSTALL/make.inc.gfortran make.inc
cp -f %{SOURCE2} BLAS/SRC/Makefile
cp -f %{SOURCE3} SRC/Makefile
cp -f %{SOURCE6} CBLAS/src/Makefile

sed -i "s|@SHORTVER@|%{shortver}|g" BLAS/SRC/Makefile
sed -i "s|@SHORTVER@|%{shortver}|g" SRC/Makefile
sed -i "s|@SHORTVER@|%{shortver}|g" LAPACKE/Makefile
sed -i "s|@SHORTVER@|%{shortver}|g" CBLAS/src/Makefile
sed -i "s|@LONGVER@|%{version}|g" BLAS/SRC/Makefile
sed -i "s|@LONGVER@|%{version}|g" SRC/Makefile
sed -i "s|@LONGVER@|%{version}|g" LAPACKE/Makefile
sed -i "s|@LONGVER@|%{version}|g" CBLAS/src/Makefile

%build
RPM_OPT_FLAGS="$RPM_OPT_FLAGS -frecursive --no-optimize-sibling-calls"
RPM_OPT_O_FLAGS=$(echo $RPM_OPT_FLAGS | sed 's|-O2|-O0|')
export FC=gfortran

# Build the static lapack library
pushd SRC
make FFLAGS="$RPM_OPT_FLAGS" CFLAGS="$RPM_OPT_FLAGS" static
cp liblapack.a ${RPM_BUILD_DIR}/%{name}-%{version}/
popd

# Build the shared lapack library
pushd SRC
make clean
make FFLAGS="$RPM_OPT_FLAGS -fPIC" CFLAGS="$RPM_OPT_FLAGS -fPIC" LDFLAGS="%{build_ldflags}" shared
cp liblapack.so.%{version} ${RPM_BUILD_DIR}/%{name}-%{version}/
popd


ln -s liblapack.so.%{version} liblapack.so

# Build the lapacke libraries
make FFLAGS="$RPM_OPT_FLAGS -fPIC" FFLAGS_NOOPT="$RPM_OPT_O_FLAGS -fPIC" tmglib
pushd LAPACKE
make clean
make CFLAGS="$RPM_OPT_FLAGS" BUILD_DEPRECATED="true" lapacke
make clean
make CFLAGS="$RPM_OPT_FLAGS -fPIC" BUILD_DEPRECATED="true" LDFLAGS="%{build_ldflags}" shlib
# cp liblapacke.so.%{version} ${RPM_BUILD_DIR}/%{name}-%{version}/
popd

cp -p %{SOURCE4} lapackqref.ps
cp -p %{SOURCE5} blasqr.ps

%install
mkdir -p %{buildroot}%{_libdir}
mkdir -p %{buildroot}%{_mandir}/man3
chmod 755 %{buildroot}%{_mandir}/man3

for f in liblapack.so.%{version} liblapacke.so.%{version} \
         liblapack.a  liblapacke.a; do
  cp -f $f ${RPM_BUILD_ROOT}%{_libdir}/$f
done

find manpages/man/man3 -type f -printf "%{_mandir}/man3/%f*\n" > lapackmans
	
cp -f manpages/man/man3/* ${RPM_BUILD_ROOT}%{_mandir}/man3

# Lapacke headers
mkdir -p %{buildroot}%{_includedir}/lapacke/
cp -a LAPACKE/include/*.h %{buildroot}%{_includedir}/lapacke/

pushd %{buildroot}%{_libdir}
ln -sf liblapack.so.%{version} liblapack.so
ln -sf liblapack.so.%{version} liblapack.so.%{shortver}
ln -sf liblapack.so.%{version} liblapack.so.%{mediumver}
ln -sf liblapacke.so.%{version} liblapacke.so
ln -sf liblapacke.so.%{version} liblapacke.so.%{shortver}
ln -sf liblapacke.so.%{version} liblapacke.so.%{mediumver}
popd

# pkgconfig
mkdir -p %{buildroot}%{_libdir}/pkgconfig/
cp -a lapack.pc.in %{buildroot}%{_libdir}/pkgconfig/lapack.pc
sed -i 's|@CMAKE_INSTALL_FULL_LIBDIR@|%{_libdir}|g' %{buildroot}%{_libdir}/pkgconfig/lapack.pc
sed -i 's|@CMAKE_INSTALL_FULL_INCLUDEDIR@|%{_includedir}|g' %{buildroot}%{_libdir}/pkgconfig/lapack.pc
sed -i 's|@LAPACK_VERSION@|%{version}|g' %{buildroot}%{_libdir}/pkgconfig/lapack.pc
cp -a LAPACKE/lapacke.pc.in %{buildroot}%{_libdir}/pkgconfig/lapacke.pc
sed -i 's|@CMAKE_INSTALL_FULL_LIBDIR@|%{_libdir}|g' %{buildroot}%{_libdir}/pkgconfig/lapacke.pc
sed -i 's|@CMAKE_INSTALL_FULL_INCLUDEDIR@|%{_includedir}/lapacke|g' %{buildroot}%{_libdir}/pkgconfig/lapacke.pc
sed -i 's|@LAPACK_VERSION@|%{version}|g' %{buildroot}%{_libdir}/pkgconfig/lapacke.pc

%ldconfig_scriptlets

%files -f lapackmans
%doc README.md LICENSE lapackqref.ps
%{_libdir}/liblapack.so.*
%{_libdir}/liblapacke.so.*

%files devel
%{_includedir}/lapacke/
%{_libdir}/liblapack.so
%{_libdir}/liblapacke.so
%{_libdir}/pkgconfig/lapack.pc
%{_libdir}/pkgconfig/lapacke.pc

%files static
%{_libdir}/liblapack.a
%{_libdir}/liblapacke.a


%changelog
* Tue Jul 28 2020 Fedora Release Engineering <releng@fedoraproject.org> - 3.9.0-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_33_Mass_Rebuild

* Sun Jun 21 2020 Iñaki Úcar <iucar@fedoraproject.org> - 3.9.0-4
- make separate packages for 64-bit versions with and without suffix (bz1295965)

* Thu Mar 19 2020 Tom Callaway <spot@fedoraproject.org> - 3.9.0-3
- apply upstream fix for accidental removal of deprecated symbols from header file

* Wed Jan 29 2020 Fedora Release Engineering <releng@fedoraproject.org> - 3.9.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_32_Mass_Rebuild

* Tue Nov 26 2019 Tom Callaway <spot@fedoraproject.org> - 3.9.0-1
- update to 3.9.0

* Thu Jul 25 2019 Fedora Release Engineering <releng@fedoraproject.org> - 3.8.0-13
- Rebuilt for https://fedoraproject.org/wiki/Fedora_31_Mass_Rebuild

* Wed May 29 2019 Tom Callaway <spot@fedoraproject.org> - 3.8.0-12
- use --no-optimize-sibling-calls to work around gfortran issues

* Fri Feb 01 2019 Fedora Release Engineering <releng@fedoraproject.org> - 3.8.0-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_30_Mass_Rebuild

* Fri Jul 13 2018 Fedora Release Engineering <releng@fedoraproject.org> - 3.8.0-10
- Rebuilt for https://fedoraproject.org/wiki/Fedora_29_Mass_Rebuild

* Tue Jun 19 2018 Tom Callaway <spot@fedoraproject.org> - 3.8.0-9
- explicitly link liblapacke.so with liblapack to remove undefined-non-weak-symbols

* Mon Mar  5 2018 Tom Callaway <spot@fedoraproject.org> - 3.8.0-8
- use LDFLAGS for shared libs

* Mon Feb 26 2018 Tom Callaway <spot@fedoraproject.org> - 3.8.0-7
- add missing aawork functions back to lapacke makefile (bz1549262)

* Wed Feb 07 2018 Fedora Release Engineering <releng@fedoraproject.org> - 3.8.0-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_28_Mass_Rebuild

* Tue Jan 30 2018 Björn Esser <besser82@fedoraproject.org> - 3.8.0-5
- Rebuilt for GCC8

* Wed Jan  3 2018 Tom Callaway <spot@fedoraproject.org> - 3.8.0-4
- fix cblas

* Mon Dec  4 2017 Tom Callaway <spot@fedoraproject.org> - 3.8.0-3
- build cblas
- include pkgconfig files.

* Fri Nov 17 2017 Tom Callaway <spot@fedoraproject.org> - 3.8.0-2
- add ilaenv2stage

* Wed Nov 15 2017 Tom Callaway <spot@fedoraproject.org> - 3.8.0-1
- update to 3.8.0

* Mon Aug 14 2017 Tom Callaway <spot@fedoraproject.org> - 3.7.1-5
- rename 64_ libraries to lib*64_*

* Fri Aug 11 2017 Tom Callaway <spot@fedoraproject.org> - 3.7.1-4
- move to 64_ suffix and symbol mangling (bz1295965)

* Thu Aug 10 2017 Tom Callaway <spot@fedoraproject.org> - 3.7.1-3
- include DSLASRC and ZCLASRC

* Wed Aug  9 2017 Tom Callaway <spot@fedoraproject.org> - 3.7.1-2
- fixup Makefile.lapack to include new stuff

* Tue Aug  1 2017 Tom Callaway <spot@fedoraproject.org> - 3.7.1-1
- update to 3.7.1

* Wed Jul 26 2017 Fedora Release Engineering <releng@fedoraproject.org> - 3.6.1-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_27_Mass_Rebuild

* Fri Feb 10 2017 Fedora Release Engineering <releng@fedoraproject.org> - 3.6.1-4
- Rebuilt for https://fedoraproject.org/wiki/Fedora_26_Mass_Rebuild

* Sat Jan 28 2017 Björn Esser <besser82@fedoraproject.org> - 3.6.1-3
- Rebuilt for GCC-7

* Mon Oct 10 2016 Tom Callaway <spot@fedoraproject.org> - 3.6.1-2
- properly set NOOPT flags during lapacke compile (thanks to sorear2@gmail.com)

* Wed Jul  6 2016 Tom Callaway <spot@fedoraproject.org> - 3.6.1-1
- update to 3.6.1

* Thu Feb 04 2016 Fedora Release Engineering <releng@fedoraproject.org> - 3.6.0-7
- Rebuilt for https://fedoraproject.org/wiki/Fedora_24_Mass_Rebuild

* Wed Jan 13 2016 Tom Callaway <spot@fedoraproject.org> - 3.6.0-6
- fix lapack Makefile to rebuild every file on every pass (thanks to adm.fkt.physik <at> tu-dortmund.de)

* Thu Dec  3 2015 Tom Callaway <spot@fedoraproject.org> - 3.6.0-5
- fix lapache static lib to include TMGLIB bits

* Wed Dec  2 2015 Tom Callaway <spot@fedoraproject.org> - 3.6.0-4
- build deprecated functions for lapacke (RHBZ #1287405)

* Sat Nov 28 2015 Igor Gnatenko <i.gnatenko.brain@gmail.com> - 3.6.0-3
- build deprecated functions also (RHBZ #1286349)

* Thu Nov 19 2015 Tom Callaway <spot@fedoraproject.org> - 3.6.0-2
- add missing functions, resolves bz1282958

* Tue Nov 17 2015 Tom Callaway <spot@fedoraproject.org> - 3.6.0-1
- update to 3.6.0

* Fri Sep 11 2015 Tom Callaway <spot@fedoraproject.org> - 3.5.0-12
- fix missing dependencies between subpackages
- delete broken and wrongly installed manpages
- fix isa_bits conditional

* Wed Jun 17 2015 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.5.0-11
- Rebuilt for https://fedoraproject.org/wiki/Fedora_23_Mass_Rebuild

* Thu Dec 18 2014 Susi Lehtola <jussilehtola@fedoraproject.org> 3.5.0-10
- Add the -frecursive flag so that the functions are thread safe.

* Mon Oct 13 2014 Peter Robinson <pbrobinson@fedoraproject.org> 3.5.0-9
- Use generic macro to detect 64 bit platforms

* Sun Aug 17 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.5.0-8
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_22_Mass_Rebuild

* Mon Jul  7 2014 Tom Callaway <spot@fedoraproject.org> - 3.5.0-7
- apply BLAS fixes from R

* Thu Jun 19 2014 Tom Callaway <spot@fedoraproject.org> - 3.5.0-6
- compile in tmglib object files, not static lib

* Wed Jun 18 2014 Tom Callaway <spot@fedoraproject.org> - 3.5.0-5
- link tmglib into lapacke

* Tue Jun 17 2014 Tom Callaway <spot@fedoraproject.org> - 3.5.0-4
- include matgen_obj items in lapacke library

* Sat Jun 07 2014 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.5.0-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_21_Mass_Rebuild

* Thu May 29 2014 Michael Schwendt <mschwendt@fedoraproject.org> - 3.5.0-2
- Don't include manual page directories (#1089412).
- Use standard group System Environment/Libraries in runtime library packages.

* Mon Nov 18 2013 Tom Callaway <spot@fedoraproject.org> - 3.5.0-1
- update to 3.5.0

* Sat Aug 03 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.4.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_20_Mass_Rebuild

* Mon Mar 25 2013 Tom Callaway <spot@fedoraproject.org> - 3.4.2-2
- clean out non-free example files from source tarball

* Thu Feb 21 2013 Tom Callaway <spot@fedoraproject.org> - 3.4.2-1
- update to 3.4.2

* Thu Feb 14 2013 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.4.1-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_19_Mass_Rebuild

* Mon Jan  7 2013 Tom Callaway <spot@fedoraproject.org> - 3.4.1-4
- fix 64bit sonames

* Fri Jan  4 2013 Tom Callaway <spot@fedoraproject.org> - 3.4.1-3
- enable 64bit INTEGER variant subpackages

* Wed Oct 24 2012 Tom Callaway <spot@fedoraproject.org> - 3.4.1-2
- fix issue where lapacke was linking to testing functions (bz860332)

* Thu Sep 06 2012 Orion Poplawski <orion@cora.nwra.com> - 3.4.1-1
- Update to 3.4.1
- Rebase lapacke shared lib patch

* Thu Jul 19 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.4.0-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_18_Mass_Rebuild

* Fri Jan 13 2012 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.4.0-2
- Rebuilt for https://fedoraproject.org/wiki/Fedora_17_Mass_Rebuild

* Mon Nov 28 2011 Tom Callaway <spot@fedoraproject.org> - 3.4.0-1
- update to 3.4.0
- build and include lapacke

* Thu Jun 02 2011 Tom Callaway <spot@fedoraproject.org> - 3.3.1-1
- update to 3.3.1
- create /usr/share/man/manl/ as 0755 and own it in lapack and blas (bz634369)
- spec file cleanup

* Mon Feb 07 2011 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.2.2-3
- Rebuilt for https://fedoraproject.org/wiki/Fedora_15_Mass_Rebuild

* Sat Jul 17 2010 Dan Horák <dan[at]danny.cz> - 3.2.2-2
- fix a typo in Makefile.lapack causing #615618

* Wed Jul  7 2010 Tom "spot" Callaway <tcallawa@redhat.com> - 3.2.2-1
- update to 3.2.2
- properly include license text
- static subpackages depend on -devel (they're not useful without it)
- clean up makefiles
- pass on version into makefiles, rather than manually hacking on each update

* Wed Dec  9 2009 Tom "spot" Callaway <tcallawa@redhat.com> - 3.2.1-4
- Move static libs to static subpackages (resolves bz 545143)

* Fri Sep  4 2009 Tom "spot" Callaway <tcallawa@redhat.com> - 3.2.1-3
- use RPM_OPT_O_FLAGS (-O0) everywhere necessary, drop RPM_OPT_SIZE_FLAGS (-Os) (bz 520518)

* Thu Aug 20 2009 Tom "spot" Callaway <tcallawa@redhat.com> - 3.2.1-2
- don't enable xblas yet

* Fri Aug 14 2009 Tom "spot" Callaway <tcallawa@redhat.com> - 3.2.1-1
- update to 3.2.1, spec file cleanups

* Mon Aug 10 2009 Ville Skyttä <ville.skytta@iki.fi> - 3.1.1-7
- Convert specfile to UTF-8.

* Fri Jul 24 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.1.1-6
- Rebuilt for https://fedoraproject.org/wiki/Fedora_12_Mass_Rebuild

* Wed Feb 25 2009 Fedora Release Engineering <rel-eng@lists.fedoraproject.org> - 3.1.1-5
- Rebuilt for https://fedoraproject.org/wiki/Fedora_11_Mass_Rebuild

* Tue Jul  8 2008 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.1-4
- fix missing dependencies (bz 442915)

* Tue Feb 19 2008 Fedora Release Engineering <rel-eng@fedoraproject.org> - 3.1.1-3
- Autorebuild for GCC 4.3

* Thu Aug 23 2007 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.1-2
- fix license (BSD)
- rebuild for BuildID

* Fri May 25 2007 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.1-1
- bump to 3.1.1

* Fri Jan  5 2007 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.0-4
- fix bugzillas 219740,219741

* Wed Dec 20 2006 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.0-3
- make clean everywhere

* Wed Dec 20 2006 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.0-2
- fix the Makefiles

* Tue Nov 14 2006 Tom "spot" Callaway <tcallawa@redhat.com> 3.1.0-1
- bump to 3.1.0

* Thu Sep 14 2006 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-38
- bump for fc-6

* Tue Feb 28 2006 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-37
- bump for FC5

* Mon Dec 19 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-36
- bump for gcc4.1

* Tue Nov 15 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-35
- try not to patch files that do not exist

* Tue Nov 15 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-34
- finish fixing bz 143340

* Thu Oct  6 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-33
- fix bz 169558

* Wed Sep 28 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-32
- move to latest upstream 3.0 tarballs
- add 8 missing BLAS functions from upstream blas tarball (bz 143340)

* Thu Sep 22 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-31
- actually install liblapack_pic.a

* Wed Sep 14 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-30
- make -devel packages
- make liblapack_pic.a package
- use dist tag

* Thu Apr 14 2005 Tom "spot" Callaway <tcallawa@redhat.com> 3.0-29
- package moves to Fedora Extras, gcc4

* Tue Dec 21 2004 Ivana Varekova <varekova@redhat.com>
- fix bug #143420 problem with compiler optimalizations

* Tue Nov 30 2004 Ivana Varekova <varekova@redhat.com>
- fix bug #138683 problem with compilation

* Thu Nov 11 2004 Ivana Varekova <varekova@redhat.com>
- fix build problem bug #138447

* Tue Jun 15 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Tue Mar 02 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Fri Feb 13 2004 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Wed Dec 31 2003 Jeff Johnson <jbj@jbj.org> 3.0-23
- link -lg2c explicitly into liblapack and libblas (#109079).

* Wed Aug 20 2003 Jeremy Katz <katzj@redhat.com> 3.0-22
- nuke -man subpackages (#97506)

* Wed Jun 04 2003 Elliot Lee <sopwith@redhat.com>
- rebuilt

* Wed Jan 22 2003 Tim Powers <timp@redhat.com>
- rebuilt

* Sun Nov 10 2002 Jeff Johnson <jbj@redhat.com> 3.0-19
- rebuild with x86_64.

* Thu Jul 18 2002 Trond Eivind Glomsrod <teg@redhat.com> 3.0-18
- Remove an empty man page (#63569)

* Fri Jun 21 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Thu May 23 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Wed May  1 2002 Trond Eivind Glomsrod <teg@redhat.com> 3.0-15
- Rebuild

* Thu Feb 21 2002 Trond Eivind Glomsrod <teg@redhat.com> 3.0-14
- Rebuild

* Wed Jan 09 2002 Tim Powers <timp@redhat.com>
- automated rebuild

* Mon Aug 13 2001 Trond Eivind Glomsrod <teg@redhat.com> 3.0-12
- The man-pages for xerbla and lsame were in blas-man and lapack-man (#51605)

* Fri Jun  8 2001 Trond Eivind Glomsrod <teg@redhat.com>
- Reenable optimization for IA64

* Fri May 25 2001 Trond Eivind Glomsrod <teg@redhat.com>
- Add all patches from the LAPACK site as of 2001-05-25
- Use this workaround for IA64 instead
- Remove SPARC workaround
- Don't exclude IA64

* Thu Dec 07 2000 Trond Eivind Glomsrod <teg@redhat.com>
- rebuild for main distribution

* Mon Nov 20 2000 Trond Eivind Glomsrod <teg@redhat.com>
- add the LAPACK Quick Reference Guide to the docs
- add the BLAS Quick Reference Guide to the docs

* Tue Aug 01 2000 Trond Eivind Glomsrod <teg@redhat.com>
- fix lack of ldconfig in postuninstall script

* Mon Jul 24 2000 Prospector <prospector@redhat.com>
- rebuilt

* Mon Jul 10 2000 Trond Eivind Glomsrod <teg@redhat.com>
- updated with the latest updates (new tarfile..) from netlib

* Thu Jun 15 2000 Trond Eivind Glomsrod <teg@redhat.com>
- use %%{_mandir}
- added some flags to work around SPARC compiler bug

* Wed Jan 19 2000 Tim Powers <timp@redhat.com>
- bzipped sources to conserve space

* Tue Jan  4 2000 Jeff Johnson <jbj@redhat.com>
- build for PowerTools 6.2.

* Sat Dec 25 1999 Joachim Frieben <jfrieben@hotmail.com>
- updated to version v3.0 + update as of Tue Nov 30 1999

* Sat Oct 23 1999 Joachim Frieben <jfrieben@hotmail.com>
- updated Red Hat makefiles to v3.0

* Mon Aug 2 1999 Tim Powers <timp@redhat.com>
- updated to v3.0
- built for 6.1

* Mon Apr 12 1999 Michael Maher <mike@redhat.com>
- built package for 6.0

* Sat Oct 24 1998 Jeff Johnson <jbj@redhat.com>
- new description/summary text.

* Fri Jul 17 1998 Jeff Johnson <jbj@redhat.com>
- repackage for powertools.

* Sun Feb 15 1998 Trond Eivind Glomsrod <teg@pvv.ntnu.no>
 [lapack-2.0-9]
 - No code updates, just built with a customized rpm -
   this should make dependencies right.

* Sat Feb 07 1998 Trond Eivind Glomsrod <teg@pvv.ntnu.no>
 [lapack-2.0-8]
 - Total rewrite of the spec file
 - Added my own makefiles - libs should build better,
   static libs should work (and be faster than they
	would be if they had worked earlier ;)
 - No patch necessary anymore.
 - Renamed lapack-blas and lapack-blas-man to
   blas and blas-man. "Obsoletes:" tag added.
   (oh - and as always: Dedicated to the girl I
   love, Eline Skirnisdottir)

* Sat Dec 06 1997 Trond Eivind Glomsrod <teg@pvv.ntnu.no>
 [lapack-2.0-7]
  - added a dependency to glibc, so people don't try with libc5

* Thu Nov 20 1997 Trond Eivind Glomsrod <teg@pvv.ntnu.no>
  [lapack-2.0-6]
  - removed etime.c
  - compiled with egcs, and for glibc 2.0

* Sun Oct 12 1997 Trond Eivind Glomsrod <teg@pvv.ntnu.no>
  [lapack-2.0-5]
  - added a changelog
  - cleaned up building of shared libs
  - now uses a BuildRoot
  - cleaned up the specfile
