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

GZOCHI_COMMON_CFLAGS = -I@GZOCHI_COMMON@/src
GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.a

AM_CFLAGS = @CFLAGS@ $(GZOCHI_COMMON_CFLAGS) -Wall -pedantic

lib_LTLIBRARIES = libgzochi.la

libgzochi_la_SOURCES = channel.c client.c protocol.c session.c
libgzochi_la_LIBADD = $(GZOCHI_COMMON_LIBS)

includedir = $(prefix)/include
dist_include_HEADERS = libgzochi.h

channel.c: channel.h
client.c: client.h
protocol.c: protocol.h
session.c: session.h
