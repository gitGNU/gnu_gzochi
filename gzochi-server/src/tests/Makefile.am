## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2014 Julian Graham
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

SUBDIRS = auth scheme

test_programs = \
	test-data \
	test-event \
	test-reloc \
	test-task \
	test-util

if STORAGE_ENGINE_BDB
test_programs += test-storage-bdb
endif
if STORAGE_ENGINE_HDB
test_programs += test-storage-hdb
endif
if STORAGE_ENGINE_LMDB
test_programs += test-storage-lmdb
endif

check_PROGRAMS = $(test_programs)
TESTS = $(test_programs)

GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

test_data_CFLAGS = @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_data_SOURCES = mock-app.c mock-schedule.c mock-storage.c test-data.c
test_data_LDADD = ../gzochid-auth.o ../gzochid-data.o ../gzochid-event.o \
	../gzochid-io.o ../gzochid-log.o ../gzochid-task.o ../gzochid-tx.o \
	../gzochid-txlog.o ../gzochid-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@ @GMODULE_LIBS@

test_event_CFLAGS = @GLIB_CFLAGS@
test_event_SOURCES = mock-app.c mock-storage.c test-event.c
test_event_LDADD = ../gzochid-event.o ../gzochid-log.o ../gzochid-tx.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@

test_reloc_CFLAGS = @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_reloc_SOURCES = test-reloc.c mock-app.c mock-storage.c
test_reloc_LDADD = ../gzochid-event.o ../gzochid-io.o ../gzochid-reloc.o \
	../gzochid-log.o ../gzochid-tx.o ../gzochid-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@ @GUILE_LIBS@

test_storage_bdb_CFLAGS = @GLIB_CFLAGS@
test_storage_bdb_SOURCES = test-storage-bdb.c
test_storage_bdb_LDADD =  ../gzochid-log.o ../gzochid-storage-bdb.o @GLIB_LIBS@

test_storage_hdb_CFLAGS = @GLIB_CFLAGS@
test_storage_hdb_SOURCES = test-storage-hdb.c
test_storage_hdb_LDADD =  ../gzochid-log.o ../gzochid-storage-hdb.o @GLIB_LIBS@

test_storage_lmdb_CFLAGS = @GLIB_CFLAGS@ -pthread
test_storage_lmdb_SOURCES = test-storage-lmdb.c
test_storage_lmdb_LDADD =  ../gzochid-log.o ../gzochid-storage-lmdb.o \
	@GLIB_LIBS@

test_task_CFLAGS = @GLIB_CFLAGS@ @GMODULE_CFLAGS@
test_task_SOURCES = test-task.c mock-app.c mock-guile.c mock-storage.c 
test_task_LDADD = ../gzochid-auth.o ../gzochid-data.o ../gzochid-event.o \
	../gzochid-task.o ../gzochid-log.o ../gzochid-schedule.o \
	../gzochid-threads.o ../gzochid-tx.o ../gzochid-txlog.o \
	../gzochid-util.o \
	$(GZOCHI_COMMON_LIBS) @GMODULE_LIBS@ @GLIB_LIBS@

test_util_CFLAGS = @GLIB_CFLAGS@
test_util_SOURCES = test-util.c
test_util_LDADD = ../gzochid-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@
