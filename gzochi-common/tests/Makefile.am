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

check_PROGRAMS = $(test_programs)

test_programs = \
	test-io

TESTS = $(test_programs)

AM_CFLAGS = @CFLAGS@ -I$(top_srcdir)/src

test_io_SOURCES = test-io.c 
test_io_LDADD = $(top_builddir)/src/libgzochi-common.la
