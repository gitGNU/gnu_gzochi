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

plugindir = $(libdir)/gzochid/storage

plugin_LTLIBRARIES =

if HAVE_BDB
plugin_LTLIBRARIES += bdb.la
endif

bdb_la_SOURCES = bdb.c
bdb_la_CFLAGS = @GLIB_CFLAGS@ @BDB_CFLAGS@ -Wall -Werror
bdb_la_LIBADD = @GLIB_LIBS@ @BDB_LIBS@
bdb_la_LDFLAGS = -module -avoid-version
