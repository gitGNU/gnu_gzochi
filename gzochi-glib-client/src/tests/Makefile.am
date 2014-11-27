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

AUTOMAKE_OPTIONS = subdir-objects

check_PROGRAMS = $(test_programs)

test_programs = \
	test-client

TESTS = $(test_programs)

GZOCHI_CLIENT_COMMON_CFLAGS = -I@GZOCHI_CLIENT_COMMON@/src
GZOCHI_CLIENT_COMMON_LIBS = \
	@GZOCHI_CLIENT_COMMON@/src/libgzochi-client-common.la

test_client_CFLAGS = $(GLIB_CFLAGS) $(GZOCHI_CLIENT_COMMON_CFLAGS)
test_client_SOURCES = ../client.c test-client.c 
test_client_LDADD = $(GLIB_LIBS) $(GZOCHI_CLIENT_COMMON_LIBS)
