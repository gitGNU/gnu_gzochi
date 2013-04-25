dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2013 Julian Graham
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

AC_INIT([gzochid], [0.3], [joolean@undecidable.net], [],
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

AC_ARG_WITH([storage-engine],
  [AS_HELP_STRING([--with-storage-engine=ENGINE],
    [storage engine ENGINE [bdb, gdbm]])],
  [GZOCHI_STORAGE_ENGINE="$withval"],
  [GZOCHI_STORAGE_ENGINE="bdb"])

AS_CASE([$GZOCHI_STORAGE_ENGINE],
  [bdb], [AC_SEARCH_LIBS([db_create], [db], [], 
                         [AC_MSG_ERROR([libdb is required.])]) 
          AC_CHECK_HEADER([db.h])],
  [gdbm], [AC_SEARCH_LIBS([gdbm_open], [gdbm], [], 
                          [AC_MSG_ERROR([libgdbm is required.])]) 
           AC_CHECK_HEADER([gdbm.h])],
  [AC_MSG_ERROR([Invalid storage engine $STORAGE_ENGINE.])])

AC_SUBST(GZOCHI_STORAGE_ENGINE)
AM_CONDITIONAL([STORAGE_ENGINE_BDB], [test x$GZOCHI_STORAGE_ENGINE = xbdb])
AM_CONDITIONAL([STORAGE_ENGINE_GDBM], [test x$GZOCHI_STORAGE_ENGINE = xgdbm])

AC_SEARCH_LIBS([__gmpz_init], [gmp], [], [AC_MSG_ERROR([libgmp is required.])])
AC_CHECK_HEADER([gmp.h])

GZOCHI_COMMON="$PWD/../gzochi-common"
AC_SUBST(GZOCHI_COMMON)

AC_SERVEEZ([yes])
if test "x$SERVEEZ_CFLAGS" = "x" ; then
   AC_MSG_ERROR([libserveez is required.])
fi

AC_CONFIG_FILES([Makefile
		 doc/Makefile
                 src/Makefile
		 src/api/Makefile
		 src/scheme/Makefile
		 src/tests/Makefile])

AC_OUTPUT
