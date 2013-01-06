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

GZOCHI_CLIENT_COMMON_CFLAGS = -I@GZOCHI_CLIENT_COMMON@/src
GZOCHI_CLIENT_COMMON_LIBS = \
	@GZOCHI_CLIENT_COMMON@/src/libgzochi-client-common.la

AM_CFLAGS = @CFLAGS@ @GLIB_CFLAGS@ $(GZOCHI_CLIENT_COMMON_CFLAGS) -Wall \
	-pedantic
AM_LDFLAGS = @LDFLAGS@ @GLIB_LIBS@

lib_LTLIBRARIES = libgzochi-glib.la

libgzochi_glib_la_SOURCES = client.c
libgzochi_glib_la_LIBADD = $(GZOCHI_CLIENT_COMMON_LIBS)

includedir = $(prefix)/include
dist_include_HEADERS = libgzochi-glib.h
