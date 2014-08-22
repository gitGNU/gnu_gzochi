dnl configure.ac --- food for -*-autoconf-*- et al
dnl
dnl Copyright (C) 2014 Julian Graham
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

AC_INIT([(gzochi client)], [0.6], [joolean@undecidable.net], [],
        [http://www.nongnu.org/gzochi/])
AC_CONFIG_AUX_DIR([.])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([src/gzochi/client.scm])

AM_INIT_AUTOMAKE(1.6)

PKG_CHECK_MODULES([GUILE], [guile-2.0])

GUILE_PROGS
GUILE_SITE_DIR

AC_CONFIG_FILES([Makefile src/Makefile src/tests/Makefile])

AC_OUTPUT
