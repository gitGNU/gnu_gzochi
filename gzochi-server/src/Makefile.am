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

plugindir = $(libdir)/gzochid

SUBDIRS = api auth scheme storage

bin_PROGRAMS = gzochid gzochi-metad gzochi-dump gzochi-load gzochi-migrate
dist_noinst_DATA = dtd/game.dtd dtd/migration.dtd
dist_sysconf_DATA = conf/gzochi-metad.conf conf/gzochid.conf
EXTRA_DIST = conf/gzochi-metad.conf.in conf/gzochid.conf.in

substitute = sed -e 's,[@]datarootdir[@],$(datarootdir),g' \
	-e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]localstatedir[@],$(localstatedir),g'

conf/gzochi-metad.conf: conf/gzochi-metad.conf.in $(top_builddir)/config.status
	mkdir -p $(builddir)/conf
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

conf/gzochid.conf: conf/gzochid.conf.in $(top_builddir)/config.status
	mkdir -p $(builddir)/conf
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

dist_include_HEADERS = gzochid-auth.h gzochid-storage.h

noinst_HEADERS = admin.h app.h app-task.h auth_int.h callback.h channel.h \
	config.h context.h data.h data-protocol.h dataclient.h \
	dataclient-protocol.h dataserver.h dataserver-protocol.h debug.h \
	descriptor.h durable-task.h event.h event-app.h event-meta.h \
	fmemopen.h fsm.h game.h game-protocol.h guile.h gzochid.h httpd.h \
	httpd-app.h httpd-meta.h io.h itree.h lifecycle.h lock.h log.h \
	lrucache.h meta-protocol.h metaserver-protocol.h oids.h \
	oids-dataclient.h oids-storage.h protocol.h queue.h reloc.h \
	resolver.h schedule.h scheme.h scheme-task.h session.h socket.h \
	stats.h storage.h storage-dataclient.h storage-mem.h task.h threads.h \
	toollib.h tx.h txlog.h util.h

noinst_LTLIBRARIES = libgzochid.la libgzochi-metad.la

libgzochid_la_CFLAGS = @CFLAGS@ \
	-DGZOCHID_AUTH_PLUGIN_DIR=\"$(plugindir)/auth\" \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" @GUILE_CFLAGS@ \
	@GLIB_CFLAGS@ @GOBJECT_CFLAGS@ @GTHREAD_CFLAGS@ @GMODULE_CFLAGS@ \
	@MICROHTTPD_CFLAGS@ @ZLIB_CFLAGS@ @GZOCHI_COMMON_CFLAGS@ \
	-Wall -Werror

libgzochid_la_SOURCES = admin.c app.c app-task.c auth.c callback.c channel.c \
	config.c context.c data.c data-protocol.c dataclient.c \
	dataclient-protocol.c debug.c descriptor.c durable-task.c event.c \
	event-app.c fmemopen.c fsm.c game.c game-protocol.c guile.c httpd.c \
	httpd-app.c io.c itree.c lifecycle.c log.c lrucache.c oids.c \
	oids-dataclient.c oids-storage.c queue.c reloc.c resolver.c schedule.c \
	scheme.c scheme-task.c session.c socket.c stats.c storage.c \
	storage-dataclient.c storage-mem.c task.c threads.c tx.c txlog.c util.c

libgzochid_la_LIBADD = api/libgzochid-api.la @GZOCHI_COMMON_LIBS@ \
	@GUILE_LIBS@ @GLIB_LIBS@ @GOBJECT_LIBS@ @GTHREAD_LIBS@ @GMODULE_LIBS@ \
	@MICROHTTPD_LIBS@ @ZLIB_LIBS@

gzochid_CFLAGS = @CFLAGS@ -DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	@GUILE_CFLAGS@ @GLIB_CFLAGS@ @GTHREAD_CFLAGS@ @GMODULE_CFLAGS@ \
	@MICROHTTPD_CFLAGS@ @ZLIB_CFLAGS@ @GZOCHI_COMMON_CFLAGS@ \
	-Wall -Werror

gzochid_SOURCES = gzochid.c
gzochid_LDADD = libgzochid.la

gzochi_dump_CFLAGS = @CFLAGS@ \
	-DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" \
	@GLIB_CFLAGS@ @GMODULE_CFLAGS@ \
	-Wall -Werror
gzochi_dump_SOURCES = gzochi-dump.c toollib.c
gzochi_dump_LDADD = libgzochid.la @GLIB_LIBS@ @GMODULE_LIBS@

gzochi_load_CFLAGS = @CFLAGS@ \
	-DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" \
	@GLIB_CFLAGS@ @GMODULE_CFLAGS@ \
	-Wall -Werror
gzochi_load_SOURCES = gzochi-load.c toollib.c
gzochi_load_LDADD = libgzochid.la @GLIB_LIBS@ @GMODULE_LIBS@

gzochi_migrate_CFLAGS = @CFLAGS@ \
	-DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" \
	@GLIB_CFLAGS@ @GMODULE_CFLAGS@ @GUILE_CFLAGS@ @GZOCHI_COMMON_CFLAGS@ \
	-Wall -Werror
gzochi_migrate_SOURCES = gzochi-migrate.c toollib.c
gzochi_migrate_LDADD = libgzochid.la @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@

libgzochi_metad_la_CFLAGS = @CFLAGS@ \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" @GLIB_CFLAGS@ \
	@GMODULE_CFLAGS@ @GTHREAD_CFLAGS@ @GOBJECT_CFLAGS@ @MICROHTTPD_CFLAGS@ \
	@GZOCHI_COMMON_CFLAGS@ \
	-Wall -Werror
libgzochi_metad_la_SOURCES = config.c data-protocol.c dataserver.c \
	dataserver-protocol.c event.c event-meta.c httpd.c httpd-meta.c \
	itree.c lock-mem.c log.c metaserver-protocol.c oids.c oids-storage.c \
	resolver.c socket.c storage.c storage-mem.c util.c
libgzochi_metad_la_LIBADD = @GLIB_LIBS@ @GMODULE_LIBS@ @GTHREAD_LIBS@ \
	@GOBJECT_LIBS@ @MICROHTTPD_LIBS@ @GZOCHI_COMMON_LIBS@

gzochi_metad_CFLAGS = @CFLAGS@ \
	-DGZOCHI_METAD_CONF_LOCATION=$(sysconfdir)/gzochi-metad.conf \
	@GLIB_CFLAGS@ @GOBJECT_CFLAGS@ @GTHREAD_CFLAGS@ -Wall -Werror

gzochi_metad_SOURCES = gzochi-metad.c
gzochi_metad_LDADD = libgzochi-metad.la

CLEANFILES = conf/gzochi-metad.conf conf/gzochid.conf
