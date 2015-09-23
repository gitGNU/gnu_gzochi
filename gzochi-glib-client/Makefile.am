## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2015 Julian Graham
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this package.  If not, see <http://www.gnu.org/licenses/>.
#

ACLOCAL_AMFLAGS = -I m4

GZOCHI_COMMON_CFLAGS = "-I$(srcdir)/../../gzochi-common/src"
GZOCHI_COMMON_LIBS = "$(builddir)/../../gzochi-common/src/libgzochi-common.la"

export GZOCHI_COMMON_CFLAGS
export GZOCHI_COMMON_LIBS

SUBDIRS = doc meta src tests
