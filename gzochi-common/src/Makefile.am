noinst_LTLIBRARIES = libgzochi-common.la

AM_CFLAGS = @CFLAGS@ -Wall -pedantic

libgzochi_common_la_SOURCES = io.c

includedir = $(prefix)/include
dist_include_HEADERS = gzochi-common.h
noinst_HEADERS = io.h protocol.h util.h

io.c: io.h
