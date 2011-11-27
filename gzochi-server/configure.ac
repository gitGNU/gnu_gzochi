dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2011 Julian Graham
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

AC_INIT([gzochid],[0.0.1],[julian.graham@aya.yale.edu])
AC_CONFIG_SRCDIR([src/gzochid.c])
AM_INIT_AUTOMAKE(1.6)

AC_LANG(C)
AC_PROG_CC
AC_PROG_RANLIB

PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([GLIB], [glib-2.0])
PKG_CHECK_MODULES([GTHREAD], [gthread-2.0])
PKG_CHECK_MODULES([MICROHTTPD], [libmicrohttpd])
PKG_CHECK_MODULES([ZLIB], [zlib])

AC_SEARCH_LIBS([gdbm_open], [gdbm])
AC_CHECK_HEADER([gdbm.h])

AC_SEARCH_LIBS([__gmpz_init], [gmp])
AC_CHECK_HEADER([gmp.h])

GZOCHI_COMMON="$PWD/../gzochi-common"
AC_SUBST(GZOCHI_COMMON)

AC_SERVEEZ

AC_CONFIG_FILES([Makefile
                 src/Makefile
		 src/api/Makefile])

AC_OUTPUT
