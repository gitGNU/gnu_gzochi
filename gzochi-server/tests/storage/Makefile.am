## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2016 Julian Graham
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

test_programs =

if HAVE_BDB
test_programs += test-bdb
endif

check_PROGRAMS = $(test_programs)
TESTS = $(test_programs)

test_bdb_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ \
	-DBDB_MODULE_LOCATION=$(top_builddir)/src/storage/bdb.la \
	-Wall -Werror
test_bdb_SOURCES = test-bdb.c
test_bdb_LDADD = @GLIB_LIBS@ @GMODULE_LIBS@
