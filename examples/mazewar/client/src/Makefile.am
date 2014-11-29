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

AM_CFLAGS = @CFLAGS@ @GLIB_CFLAGS@ @SDL_CFLAGS@ \
	-I ../../../../gzochi-glib-client/src \
	-Wall -Werror

AM_LDFLAGS = @LDFLAGS@ @GLIB_LIBS@ @SDL_LIBS@ \
	-L../../../../gzochi-glib-client/src/ -lgzochi-glib

bin_PROGRAMS = mazewar

noinst_HEADERS = display.h input.h mazewar.h message.h \
	data/avatars.h data/font.h data/views.h

mazewar_SOURCES = display.c display-sdl.c input-sdl.c mazewar.c message.c \
	data/avatars.c data/font.c data/views.c

avatars.c: avatars.h
display.c: display.h
display-sdl.c: display.h
input-sdl.c: input.h
mazewar.c: mazewar.h
message.c: message.h

data/out.c: data/avatars.h
data/font.c: data/font.h
data/views.c: data/views.h
