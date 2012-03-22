dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2012 Julian Graham
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

AC_INIT([gzochid], [0.1], [joolean@undecidable.net], [],
        [http://www.nongnu.org/gzochi/])
AC_CONFIG_AUX_DIR([.])
AC_CONFIG_SRCDIR([src/gzochid.c])

AM_INIT_AUTOMAKE(1.6)

AC_LANG(C)
AC_PROG_CC

LT_PREREQ([2.4])
LT_INIT

PKG_CHECK_MODULES([GUILE], [guile-2.0])
PKG_CHECK_MODULES([GLIB], [glib-2.0])
PKG_CHECK_MODULES([GTHREAD], [gthread-2.0])
PKG_CHECK_MODULES([MICROHTTPD], [libmicrohttpd])
PKG_CHECK_MODULES([ZLIB], [zlib])

GUILE_SITE_DIR

AC_SEARCH_LIBS([gdbm_open], [gdbm])
AC_CHECK_HEADER([gdbm.h])

AC_SEARCH_LIBS([__gmpz_init], [gmp])
AC_CHECK_HEADER([gmp.h])

GZOCHI_COMMON="$PWD/../gzochi-common"
AC_SUBST(GZOCHI_COMMON)

dnl The `AC_SERVEEZ' macro from serveez.m4 sets up the include directory as
dnl $prefix/include instead of $prefix/include/libserveez, which is where the
dnl files get installed and where they look for each other. The directives
dnl below replicate some of the logic of that macro but gets the directories
dnl right.

AC_ARG_WITH([serveez],
  [AS_HELP_STRING([--with-serveez=DIR], 
    [serveez installation in DIR [default=/usr]])],
  [SVZDIR="$withval"], 
  [SVZDIR="/usr"])

SERVEEZ_CFLAGS="-I$SVZDIR/include/libserveez"
SERVEEZ_LDFLAGS="-L$SVZDIR/lib -lserveez"

AC_SUBST(SERVEEZ_CFLAGS)
AC_SUBST(SERVEEZ_LDFLAGS)

AC_CONFIG_FILES([Makefile
		 doc/Makefile
                 src/Makefile
		 src/api/Makefile
		 src/scheme/Makefile])

AC_OUTPUT
