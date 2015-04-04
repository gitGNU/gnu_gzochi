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

plugindir = $(libdir)/gzochid

SUBDIRS = api auth scheme storage

GZOCHI_COMMON_CFLAGS = -I@GZOCHI_COMMON@/src
GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

bin_PROGRAMS = gzochid gzochi-dump gzochi-load gzochi-migrate
dist_noinst_DATA = dtd/game.dtd dtd/migration.dtd
dist_sysconf_DATA = conf/gzochid.conf
EXTRA_DIST = conf/gzochid.conf.in

substitute = sed -e 's,[@]datarootdir[@],$(datarootdir),g' \
	-e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]localstatedir[@],$(localstatedir),g'

conf/gzochid.conf: conf/gzochid.conf.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

dist_include_HEADERS = gzochid-auth.h gzochid-storage.h

noinst_HEADERS = admin.h app.h app-task.h auth_int.h callback.h channel.h \
	config.h context.h data.h debug.h descriptor.h event.h fmemopen.h \
	fsm.h game.h guile.h gzochid.h httpd.h io.h lifecycle.h log.h \
	protocol.h reloc.h schedule.h scheme.h scheme-task.h session.h \
	socket.h stats.h storage.h storage-mem.h task.h threads.h toollib.h \
	tx.h txlog.h util.h

noinst_LTLIBRARIES = libgzochid.la

libgzochid_la_CFLAGS = @CFLAGS@ \
	-DGZOCHID_AUTH_PLUGIN_DIR=\"$(plugindir)/auth\" \
	-DGZOCHID_STORAGE_ENGINE_DIR=\"$(plugindir)/storage\" @GUILE_CFLAGS@ \
	@GLIB_CFLAGS@ @GTHREAD_CFLAGS@ @GMODULE_CFLAGS@ @MICROHTTPD_CFLAGS@ \
	@ZLIB_CFLAGS@ $(GZOCHI_COMMON_CFLAGS) \
	-Wall -Werror

libgzochid_la_SOURCES = admin.c app.c app-task.c auth.c callback.c channel.c \
	config.c context.c data.c debug.c descriptor.c event.c fmemopen.c \
	fsm.c game.c guile.c httpd.c io.c lifecycle.c log.c protocol.c \
	reloc.c schedule.c scheme.c scheme-task.c session.c socket.c stats.c \
	storage.c storage-mem.c task.c threads.c tx.c txlog.c util.c

libgzochid_la_LIBADD = api/libgzochid-api.la $(GZOCHI_COMMON_LIBS) \
	@GUILE_LIBS@ @GLIB_LIBS@ @GTHREAD_LIBS@ @GMODULE_LIBS@ \
	@MICROHTTPD_LIBS@ @ZLIB_LIBS@

gzochid_CFLAGS = @CFLAGS@ -DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	@GUILE_CFLAGS@ @GLIB_CFLAGS@ @GTHREAD_CFLAGS@ @GMODULE_CFLAGS@ \
	@MICROHTTPD_CFLAGS@ @ZLIB_CFLAGS@ \
	$(GZOCHI_COMMON_CFLAGS) \
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
	@GLIB_CFLAGS@ @GMODULE_CFLAGS@ @GUILE_CFLAGS@ $(GZOCHI_COMMON_CFLAGS) \
	-Wall -Werror
gzochi_migrate_SOURCES = gzochi-migrate.c toollib.c
gzochi_migrate_LDADD = libgzochid.la @GLIB_LIBS@ @GMODULE_LIBS@ @GUILE_LIBS@
