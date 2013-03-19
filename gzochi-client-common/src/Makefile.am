GZOCHI_COMMON_CFLAGS = -I@GZOCHI_COMMON@/src
GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

AM_CFLAGS = @CFLAGS@ $(GZOCHI_COMMON_CFLAGS) -Wall -pedantic

noinst_LTLIBRARIES = libgzochi-client-common.la

libgzochi_client_common_la_SOURCES = protocol.c session.c
libgzochi_client_common_la_LIBADD = $(GZOCHI_COMMON_LIBS)

includedir = $(prefix)/include
dist_include_HEADERS = gzochi-client-common.h
noinst_HEADERS = protocol.h session.h

protocol.c: protocol.h
session.c: session.h
