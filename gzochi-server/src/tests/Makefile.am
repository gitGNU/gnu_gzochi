check_PROGRAMS = $(test_programs)

test_programs = \
	test-storage

TESTS = $(test_programs)

AM_CFLAGS = @CFLAGS@ @GLIB_CFLAGS@

test_storage_SOURCES = test-storage.c
test_storage_LDADD = @GLIB_LIBS@