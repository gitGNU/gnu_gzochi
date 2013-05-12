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

SUBDIRS = api scheme tests

GZOCHI_COMMON_CFLAGS = -I@GZOCHI_COMMON@/src
GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

bin_PROGRAMS = gzochid
dist_noinst_DATA = dtd/game.dtd
dist_sysconf_DATA = conf/gzochid.conf

noinst_HEADERS = admin.h app.h auth.h auth_int.h channel.h config.h context.h \
	data.h debug.h event.h fsm.h game.h guile.h gzochid.h httpd.h io.h \
	log.h protocol.h reloc.h schedule.h scheme.h session.h socket.h \
	storage.h task.h threads.h tx.h txlog.h util.h

gzochid_CFLAGS = @CFLAGS@ -DGZOCHID_CONF_LOCATION=$(sysconfdir)/gzochid.conf \
	@GUILE_CFLAGS@ @GLIB_CFLAGS@ @GTHREAD_CFLAGS@ @GMODULE_CFLAGS@ \
	@MICROHTTPD_CFLAGS@ @ZLIB_CFLAGS@ @SERVEEZ_CFLAGS@ \
	$(GZOCHI_COMMON_CFLAGS) \
	-Wall -pedantic

gzochid_SOURCES = admin.c app.c auth.c channel.c config.c context.c data.c \
	debug.c event.c fsm.c game.c guile.c gzochid.c httpd.c log.c \
	protocol.c reloc.c schedule.c scheme.c session.c socket.c task.c \
	threads.c tx.c txlog.c util.c
gzochid_LDADD = api/libgzochid-server-api.a $(GZOCHI_COMMON_LIBS) @GUILE_LIBS@ \
	@GLIB_LIBS@ @GTHREAD_LIBS@ @MICROHTTPD_LIBS@ @ZLIB_LIBS@ \
	@SERVEEZ_LDFLAGS@

if STORAGE_ENGINE_BDB
gzochid_SOURCES += storage-bdb.c
endif

if STORAGE_ENGINE_GDBM
gzochid_SOURCES += storage-gdbm.c
endif

admin.c: admin.h
app.c: app.h
auth.c: auth.h auth_int.h
channel.c: channel.h
config.c: config.h
context.c: context.h
data.c: data.h
debug.c: debug.h
event.c: event.h
fsm.c: fsm.h
game.c: game.h
guile.c: guile.h
gzochid.c: gzochid.h
httpd.c: httpd.h
log.c: log.h
protocol.c: protocol.h
reloc.c: reloc.h
schedule.c: schedule.h
scheme.c: scheme.h
session.c: session.h
socket.c: socket.h
storage-bdb.c: storage.h
storage-gdbm.c: storage.h
task.c: task.h
threads.c: threads.h
tx.c: tx.h
txlog.c: txlog.h
util.c: util.h
