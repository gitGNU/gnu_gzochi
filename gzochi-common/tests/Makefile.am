check_PROGRAMS = $(test_programs)

test_programs = \
	test-io

TESTS = $(test_programs)

AM_CFLAGS = @CFLAGS@ -I$(top_builddir)/src

test_io_SOURCES = test-io.c 
test_io_LDADD = $(top_builddir)/src/libgzochi-common.la
