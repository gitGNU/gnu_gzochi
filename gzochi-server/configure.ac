dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2016 Julian Graham
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this package.  If not, see <http://www.gnu.org/licenses/>.
dnl

AC_INIT([gzochid], [0.9], [joolean@undecidable.net], [],
        [http://www.nongnu.org/gzochi/])
AC_CONFIG_AUX_DIR([.])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/gzochid.c])

AM_INIT_AUTOMAKE(1.6)

AC_LANG(C)
AC_PROG_CC
AM_PROG_CC_C_O

LT_PREREQ([2.4])
LT_INIT

PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([GLIB], [glib-2.0])
PKG_CHECK_MODULES([GOBJECT], [gobject-2.0])
PKG_CHECK_MODULES([GTHREAD], [gthread-2.0])
PKG_CHECK_MODULES([GMODULE], [gmodule-2.0])
PKG_CHECK_MODULES([MICROHTTPD], [libmicrohttpd])
PKG_CHECK_MODULES([ZLIB], [zlib])

AC_ARG_ENABLE([KERBEROS], 
  [AS_HELP_STRING([--enable-kerberos], [use Kerberos v5])],
  [ if test "$enableval" != "no"; then 
      AC_KERBEROS_V5([yes]) 
    fi ],
  [AC_KERBEROS_V5([no])])
AM_CONDITIONAL([BUILD_KRB5_AUTH], [test "x$KRB5_CFLAGS" != "x" ])

GUILE_PROGS
GUILE_SITE_DIR

# Work around lack of VPATH awareness for GUILE_SITE.

AC_MSG_CHECKING([whether this is a 'make distcheck' build])
AS_CASE([$prefix],
  [*/_inst], [AC_MSG_RESULT([yes, rewriting GUILE_SITE])
              GUILE_SITE=${datarootdir}/guile/site],
  [AC_MSG_RESULT([no])])

AC_ARG_WITH([bdb],
  [AS_HELP_STRING([--with-bdb],
    [support for Berkeley DB @<:@default=check@:>@])],
  [],
  [with_bdb=check])

AS_IF([test "x$with_bdb" != xno],
  [AC_PROBE_LIBS([db_create], [db], 
    [AC_CHECK_HEADER([db.h],
      [AC_SUBST([BDB_LIBS], $ac_cv_probe_db_create)
       AC_SUBST([BDB_CFLAGS])
       have_bdb=true],
      [])],
    [])],
  []) 

AM_CONDITIONAL([HAVE_BDB], [test x$have_bdb = xtrue])

AC_SEARCH_LIBS([__gmpz_init], [gmp], [], [AC_MSG_ERROR([libgmp is required.])])
AC_CHECK_HEADER([gmp.h], [], [AC_MSG_ERROR([gmp.h is required.])])

AC_CHECK_FUNCS([fmemopen])

ac_gzochi_common_cflags='-I$(abs_top_srcdir)/../gzochi-common/src'
AC_SUBST([GZOCHI_COMMON_CFLAGS], [$ac_gzochi_common_cflags])

ac_gzochi_common_libs='-L$(abs_top_builddir)/../gzochi-common/src/ \
	-lgzochi-common'
AC_SUBST([GZOCHI_COMMON_LIBS], [$ac_gzochi_common_libs])

AC_CONFIG_FILES([Makefile
		 doc/Makefile
		 meta/Makefile
                 src/Makefile
		 src/api/Makefile
		 src/auth/Makefile
		 src/scheme/Makefile
		 src/storage/Makefile
		 tests/Makefile
		 tests/api/Makefile
		 tests/auth/Makefile
		 tests/scheme/Makefile
		 tests/storage/Makefile])

AC_OUTPUT
