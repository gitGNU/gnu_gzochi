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

SUBDIRS = api auth scheme storage

test_programs = \
	test-app-task \
	test-auth \
	test-config \
	test-data \
	test-data-protocol \
	test-dataserver \
	test-dataserver-protocol \
	test-descriptor \
	test-durable-task \
	test-event \
	test-fsm \
	test-game-protocol \
	test-httpd \
	test-itree \
	test-lock-mem \
	test-lrucache \
	test-oids \
	test-oids-storage \
	test-reloc \
	test-resolver \
	test-scheme \
	test-scheme-task \
	test-session \
	test-socket \
	test-storage-mem \
	test-task \
	test-util

dist_noinst_DATA = test-gzochi-migrate.conf.in test-gzochi-migrate.xml \
	test-gzochi-migrate/game.xml test-gzochi-migrate/gzochi/test/migrate.scm
dist_noinst_HEADERS = mock-data.h
dist_noinst_SCRIPTS = test-gzochi-dump.sh test-gzochi-load.sh \
	test-gzochi-migrate.sh

check_PROGRAMS = $(test_programs) 
check_LTLIBRARIES = treefile.la
AM_TESTS_ENVIRONMENT = GUILE_LOAD_PATH='$(top_srcdir)/src/scheme'; \
	export GUILE_LOAD_PATH;
TESTS = $(test_programs) test-gzochi-dump.sh test-gzochi-load.sh \
	test-gzochi-migrate.sh

treefile_la_SOURCES = treefile.c
treefile_la_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ -Wall -Werror
treefile_la_LIBADD = @GLIB_LIBS@
treefile_la_LDFLAGS = -module -avoid-version -rpath $(abs_srcdir)

test_app_task_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_app_task_SOURCES = test-app-task.c
test_app_task_LDADD = $(top_builddir)/src/libgzochid_la-app.o \
	$(top_builddir)/src/libgzochid_la-app-task.o \
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-context.o \
	$(top_builddir)/src/libgzochid_la-event.o \
	$(top_builddir)/src/libgzochid_la-fsm.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-task.o \
	$(top_builddir)/src/libgzochid_la-tx.o \
	$(top_builddir)/src/libgzochid_la-txlog.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_auth_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GMODULE_CFLAGS@
test_auth_SOURCES = test-auth.c
test_auth_LDADD = $(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@

test_data_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_data_SOURCES = test-data.c
test_data_LDADD = $(top_builddir)/src/libgzochid.la \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_config_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GOBJECT_CFLAGS@
test_config_SOURCES = test-config.c
test_config_LDADD = $(top_builddir)/src/libgzochid_la-config.o \
	@GLIB_LIBS@ @GOBJECT_LIBS@

test_data_protocol_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ \
	@GZOCHI_COMMON_CFLAGS@
test_data_protocol_SOURCES = test-data-protocol.c
test_data_protocol_LDADD = $(top_builddir)/src/libgzochid_la-data-protocol.o \
	@GLIB_LIBS@ @GZOCHI_COMMON_LIBS@

test_dataserver_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GOBJECT_CFLAGS@ \
	@GZOCHI_COMMON_CFLAGS@
test_dataserver_SOURCES = test-dataserver.c
test_dataserver_LDADD = $(top_builddir)/src/libgzochi_metad_la-dataserver.o \
	$(top_builddir)/src/libgzochid_la-config.o \
	$(top_builddir)/src/libgzochid_la-data-protocol.o \
	$(top_builddir)/src/libgzochid_la-itree.o \
	$(top_builddir)/src/libgzochi_metad_la-lock-mem.o \
	$(top_builddir)/src/libgzochid_la-oids.o \
	$(top_builddir)/src/libgzochid_la-oids-storage.o \
	$(top_builddir)/src/libgzochid_la-resolver.o \
	$(top_builddir)/src/libgzochid_la-socket.o \
	$(top_builddir)/src/libgzochid_la-storage.o \
	$(top_builddir)/src/libgzochid_la-storage-mem.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GOBJECT_LIBS@ @GMODULE_LIBS@

test_dataserver_protocol_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ \
	@GOBJECT_CFLAGS@ @GZOCHI_COMMON_CFLAGS@
test_dataserver_protocol_SOURCES = test-dataserver-protocol.c
test_dataserver_protocol_LDADD = \
	$(top_builddir)/src/libgzochi_metad_la-dataserver-protocol.o \
	$(top_builddir)/src/libgzochid_la-data-protocol.o \
	$(top_builddir)/src/libgzochid_la-oids.o \
	$(top_builddir)/src/libgzochid_la-resolver.o \
	$(top_builddir)/src/libgzochid_la-socket.o \
	@GLIB_LIBS@ @GOBJECT_LIBS@ @GZOCHI_COMMON_LIBS@

test_descriptor_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_descriptor_SOURCES = test-descriptor.c
test_descriptor_LDADD = $(top_builddir)/src/libgzochid_la-callback.o \
	$(top_builddir)/src/libgzochid_la-descriptor.o \
	$(top_builddir)/src/libgzochid_la-fmemopen.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@

test_durable_task_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GMODULE_CFLAGS@
test_durable_task_SOURCES = test-durable-task.c
test_durable_task_LDADD = $(top_builddir)/src/libgzochid.la \
	@GZOCHI_COMMON_LIBS@ @GMODULE_LIBS@ @GLIB_LIBS@ @GUILE_LIBS@

test_event_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_event_SOURCES = test-event.c
test_event_LDADD = $(top_builddir)/src/libgzochid_la-event.o \
	$(top_builddir)/src/libgzochid_la-tx.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@

test_fsm_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_fsm_SOURCES = test-fsm.c
test_fsm_LDADD = $(top_builddir)/src/libgzochid_la-fsm.o \
	@GLIB_LIBS@

test_game_protocol_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_game_protocol_SOURCES = test-game-protocol.c
test_game_protocol_LDADD = $(top_builddir)/src/libgzochid.la \
	@GZOCHI_COMMON_LIBS@ @GMODULE_LIBS@ @GLIB_LIBS@ @GUILE_LIBS@

test_httpd_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GOBJECT_CFLAGS@ \
	@MICROHTTPD_CFLAGS@
test_httpd_SOURCES = test-httpd.c
test_httpd_LDADD = $(top_builddir)/src/libgzochid_la-httpd.o \
	@GLIB_LIBS@ @GOBJECT_LIBS@ @MICROHTTPD_LIBS@

test_itree_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_itree_SOURCES = test-itree.c
test_itree_LDADD = $(top_builddir)/src/libgzochid_la-itree.o @GLIB_LIBS@

test_lock_mem_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_lock_mem_SOURCES = test-lock-mem.c
test_lock_mem_LDADD = $(top_builddir)/src/libgzochi_metad_la-lock-mem.o \
	$(top_builddir)/src/libgzochid_la-itree.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@

test_lrucache_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_lrucache_SOURCES = test-lrucache.c
test_lrucache_LDADD = $(top_builddir)/src/libgzochid_la-lrucache.o @GLIB_LIBS@

test_oids_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_oids_SOURCES = test-oids.c
test_oids_LDADD = $(top_builddir)/src/libgzochid_la-oids.o \
	$(top_builddir)/src/libgzochid_la-storage-mem.o \
	@GLIB_LIBS@

test_oids_storage_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_oids_storage_SOURCES = test-oids-storage.c
test_oids_storage_LDADD = $(top_builddir)/src/libgzochid_la-oids.o \
	$(top_builddir)/src/libgzochid_la-oids-storage.o \
	$(top_builddir)/src/libgzochid_la-storage-mem.o \
	@GLIB_LIBS@

test_reloc_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GMODULE_CFLAGS@ \
	@GUILE_CFLAGS@
test_reloc_SOURCES = test-reloc.c
test_reloc_LDADD = $(top_builddir)/src/libgzochid_la-app.o \
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-context.o \
	$(top_builddir)/src/libgzochid_la-event.o \
	$(top_builddir)/src/libgzochid_la-fsm.o \
	$(top_builddir)/src/libgzochid_la-guile.o \
	$(top_builddir)/src/libgzochid_la-io.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-reloc.o \
	$(top_builddir)/src/libgzochid_la-scheme.o \
	$(top_builddir)/src/libgzochid_la-tx.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_resolver_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GOBJECT_CFLAGS@
test_resolver_SOURCES = test-resolver.c
test_resolver_LDADD = $(top_builddir)/src/libgzochid_la-resolver.o \
	@GLIB_LIBS@ @GOBJECT_LIBS@

test_scheme_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_scheme_SOURCES = test-scheme.c mock-data.c
test_scheme_LDADD = $(top_builddir)/src/libgzochid_la-app.o \
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-context.o \
	$(top_builddir)/src/libgzochid_la-event.o \
	$(top_builddir)/src/libgzochid_la-fsm.o \
	$(top_builddir)/src/libgzochid_la-guile.o \
	$(top_builddir)/src/libgzochid_la-io.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-reloc.o \
	$(top_builddir)/src/libgzochid_la-scheme.o \
	$(top_builddir)/src/libgzochid_la-tx.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_scheme_task_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_scheme_task_SOURCES = test-scheme-task.c 
test_scheme_task_LDADD = $(top_builddir)/src/libgzochid.la \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_session_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GUILE_CFLAGS@
test_session_SOURCES = test-session.c 
test_session_LDADD = $(top_builddir)/src/libgzochid.la \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

test_socket_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@ @GOBJECT_CFLAGS@
test_socket_SOURCES = test-socket.c
test_socket_LDADD = $(top_builddir)/src/libgzochid_la-socket.o \
	@GLIB_LIBS@ @GOBJECT_LIBS@

test_storage_mem_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_storage_mem_SOURCES = test-storage-mem.c
test_storage_mem_LDADD = $(top_builddir)/src/libgzochid_la-storage-mem.o \
	@GLIB_LIBS@

test_task_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_task_SOURCES = test-task.c 
test_task_LDADD = $(top_builddir)/src/libgzochid_la-task.o \
	@GLIB_LIBS@

test_util_CFLAGS = -I$(top_srcdir)/src @GLIB_CFLAGS@
test_util_SOURCES = test-util.c
test_util_LDADD = $(top_builddir)/src/libgzochid_la-util.o \
	@GZOCHI_COMMON_LIBS@ @GLIB_LIBS@
