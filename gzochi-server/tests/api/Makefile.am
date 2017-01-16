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

test_programs = \
	test-tx

check_PROGRAMS = $(test_programs)
AM_TESTS_ENVIRONMENT = GUILE_LOAD_PATH='$(top_srcdir)/src/scheme'; \
	export GUILE_LOAD_PATH;
TESTS = $(test_programs)

test_tx_CFLAGS = -I$(top_srcdir)/src/api @GLIB_CFLAGS@ @GMODULE_CFLAGS@ \
	@GOBJECT_CFLAGS@ @GUILE_CFLAGS@
test_tx_SOURCES = test-tx.c
test_tx_LDADD = $(top_builddir)/src/api/tx.o \
	$(top_builddir)/src/api/util.o \
	\
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-callback.o \
	$(top_builddir)/src/libgzochid_la-config.o \
	$(top_builddir)/src/libgzochid_la-descriptor.o \
	$(top_builddir)/src/libgzochid_la-guile.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-reloc.o \
	$(top_builddir)/src/libgzochid_la-scheme.o \
	$(top_builddir)/src/libgzochid_la-tx.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GOBJECT_LIBS@ \
	@GUILE_LIBS@
