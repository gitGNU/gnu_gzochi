## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2013 Julian Graham
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
	test-data \
	test-event \
	test-util

TESTS = $(test_programs)

AM_CFLAGS = @CFLAGS@ @GLIB_CFLAGS@

GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

test_data_SOURCES = mock-app.c mock-schedule.c mock-storage.c test-data.c 
test_data_LDADD = ../auth.o ../data.o ../log.o ../task.o \
	../tx.o ../txlog.o ../util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@

test_event_SOURCES = mock-app.c mock-storage.c test-event.c
test_event_LDADD = ../event.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@

test_util_SOURCES = test-util.c
test_util_LDADD = ../util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@
