## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2011 Julian Graham
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
	-Wall -pedantic

noinst_LIBRARIES = libgzochid-server-api.a

libgzochid_server_api_a_SOURCES = channel.c data.c log.c session.c task.c

SUFFIXES = .x
.c.x:
	guile-snarf -o $@ $< @DEFS@ @CFLAGS@ @GLIB_CFLAGS@ @GUILE_CFLAGS@

BUILT_SOURCES = channel.x data.x log.x session.x task.x

channel.c: channel.h
data.c: data.h
log.c: log.h
session.c: session.h
task.c: task.h

channel.x: channel.c
data.x: data.c
log.x: log.c
session.x: session.c
task.x: task.c
