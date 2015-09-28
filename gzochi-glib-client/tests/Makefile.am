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

check_PROGRAMS = $(test_programs)

test_programs = \
	test-client \
	test-session

TESTS = $(test_programs)

test_client_SOURCES = test-client.c 
test_client_CFLAGS = -I$(top_srcdir)/src \
	$(GLIB_CFLAGS) @GZOCHI_COMMON_CFLAGS@ -Wall -Werror
test_client_LDADD = $(top_builddir)/src/client.o \
	$(top_builddir)/src/protocol.o $(top_builddir)/src/session.o \
	$(GLIB_LIBS) @GZOCHI_COMMON_LIBS@

test_session_SOURCES = test-session.c 
test_session_CFLAGS = -I$(top_srcdir)/src @GZOCHI_COMMON_CFLAGS@ -Wall -Werror
test_session_LDADD = $(top_builddir)/src/session.o @GZOCHI_COMMON_LIBS@
