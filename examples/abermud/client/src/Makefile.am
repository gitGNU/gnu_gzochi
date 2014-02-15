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

AM_CFLAGS = @CFLAGS@ @PTHREAD_CFLAGS@ \
	-I ../../../../gzochi-client/src \
	-Wall -Werror

AM_LDFLAGS = @LDFLAGS@ @PTHREAD_LIBS@ \
	-L../../../../gzochi-client/src/ -lgzochi

bin_PROGRAMS = abermud

noinst_HEADERS = abermud.h console.h

abermud_SOURCES = abermud.c console-curses.c

console-curses.c: console.h
