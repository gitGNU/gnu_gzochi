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
	test-scheme \
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

noinst_HEADERS = mock-data.h

check_PROGRAMS = $(test_programs)
TESTS = $(test_programs)

GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

test_data_CFLAGS = @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_data_SOURCES = mock-app.c mock-schedule.c mock-storage.c test-data.c
test_data_LDADD = ../libgzochid_la-auth.o ../libgzochid_la-data.o \
	../libgzochid_la-event.o ../libgzochid_la-io.o ../libgzochid_la-log.o \
	../libgzochid_la-task.o ../libgzochid_la-tx.o ../libgzochid_la-txlog.o \
	../libgzochid_la-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@ @GMODULE_LIBS@

test_event_CFLAGS = @GLIB_CFLAGS@
test_event_SOURCES = mock-app.c mock-storage.c test-event.c
test_event_LDADD = ../libgzochid_la-event.o ../libgzochid_la-log.o \
	../libgzochid_la-tx.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@

test_reloc_CFLAGS = @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_reloc_SOURCES = test-reloc.c mock-app.c mock-storage.c
test_reloc_LDADD = ../libgzochid_la-event.o ../libgzochid_la-io.o \
	../libgzochid_la-reloc.o ../libgzochid_la-log.o ../libgzochid_la-tx.o \
	../libgzochid_la-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@ @GUILE_LIBS@

test_scheme_CFLAGS = @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_scheme_SOURCES = test-scheme.c mock-app.c mock-data.c mock-reloc.c \
	mock-session.c mock-storage.c mock-task.c mock-tx.c mock-txlog.c
test_scheme_LDADD = ../libgzochid_la-auth.o ../libgzochid_la-event.o \
	../libgzochid_la-guile.o ../libgzochid_la-io.o ../libgzochid_la-log.o \
	../libgzochid_la-scheme.o ../libgzochid_la-threads.o \
	../libgzochid_la-util.o \
	$(GZOCHI_COMMON_LIBS) @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_storage_bdb_CFLAGS = @GLIB_CFLAGS@
test_storage_bdb_SOURCES = test-storage-bdb.c
test_storage_bdb_LDADD =  ../libgzochid_la-log.o \
	../libgzochid_la-storage-bdb.o \
	@GLIB_LIBS@

test_storage_hdb_CFLAGS = @GLIB_CFLAGS@
test_storage_hdb_SOURCES = test-storage-hdb.c
test_storage_hdb_LDADD =  ../libgzochid_la-log.o \
	../libgzochid_la-storage-hdb.o \
	@GLIB_LIBS@

test_storage_lmdb_CFLAGS = @GLIB_CFLAGS@ -pthread
test_storage_lmdb_SOURCES = test-storage-lmdb.c
test_storage_lmdb_LDADD =  ../libgzochid_la-log.o \
	../libgzochid_la-storage-lmdb.o \
	@GLIB_LIBS@

test_task_CFLAGS = @GLIB_CFLAGS@ @GMODULE_CFLAGS@
test_task_SOURCES = test-task.c mock-app.c mock-guile.c mock-storage.c 
test_task_LDADD = ../libgzochid_la-auth.o ../libgzochid_la-data.o \
	../libgzochid_la-event.o ../libgzochid_la-task.o \
	../libgzochid_la-log.o ../libgzochid_la-schedule.o \
	../libgzochid_la-threads.o ../libgzochid_la-tx.o \
	../libgzochid_la-txlog.o ../libgzochid_la-util.o \
	$(GZOCHI_COMMON_LIBS) @GMODULE_LIBS@ @GLIB_LIBS@

test_util_CFLAGS = @GLIB_CFLAGS@
test_util_SOURCES = test-util.c
test_util_LDADD = ../libgzochid_la-util.o $(GZOCHI_COMMON_LIBS) @GLIB_LIBS@
