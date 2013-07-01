## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2012 Julian Graham
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

AM_CFLAGS = @CFLAGS@ @GUILE_CFLAGS@ @GLIB_CFLAGS@ @GTHREAD_CFLAGS@ \
	@SERVEEZ_CFLAGS@ -Wall -Werror

noinst_LIBRARIES = libgzochid-server-api.a
noinst_HEADERS = admin.h channel.h data.h log.h session.h task.h util.h

libgzochid_server_api_a_SOURCES = admin.c channel.c data.c log.c session.c \
	task.c util.c

SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< @DEFS@ @CFLAGS@ @GLIB_CFLAGS@ @GUILE_CFLAGS@ \
		@SERVEEZ_CFLAGS@

BUILT_SOURCES = admin.x channel.x data.x log.x session.x task.x

admin.c: admin.h
channel.c: channel.h
data.c: data.h
log.c: log.h
session.c: session.h
task.c: task.h
util.c: util.h

admin.x: admin.c
channel.x: channel.c
data.x: data.c
log.x: log.c
session.x: session.c
task.x: task.c
