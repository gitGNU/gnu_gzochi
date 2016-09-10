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

AM_CFLAGS = @CFLAGS@ @GUILE_CFLAGS@ @GLIB_CFLAGS@ @GTHREAD_CFLAGS@ -Wall -Werror

noinst_LTLIBRARIES = libgzochid-api.la
noinst_HEADERS = admin.h channel.h data.h log.h session.h task.h tx.h util.h

libgzochid_api_la_SOURCES = admin.c channel.c data.c log.c session.c \
	task.c tx.c util.c

AM_V_SNARF = $(AM_V_SNARF_$(V))
AM_V_SNARF_ = $(AM_V_SNARF_$(AM_DEFAULT_VERBOSITY))
AM_V_SNARF_0 = @echo "  GUILE-SNARF  " $@;

SUFFIXES = .x
.c.x:
	$(AM_V_SNARF)guile-snarf -o $@ $< -I$(builddir) @DEFS@ @CFLAGS@ \
		@GLIB_CFLAGS@ @GUILE_CFLAGS@

BUILT_SOURCES = admin.x channel.x data.x log.x session.x task.x tx.x
CLEANFILES = $(BUILT_SOURCES)
