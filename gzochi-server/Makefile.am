ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src doc meta tests

AM_LDFLAGS = @GUILE_LIBS@ @GLIB_LIBS@

dist_noinst_DATA = benchmarks/echo-chamber/README \
	benchmarks/echo-chamber/client.scm \
	benchmarks/echo-chamber/echo-chamber.sh \
	benchmarks/echo-chamber/game.xml benchmarks/echo-chamber/gzochid.conf \
	benchmarks/echo-chamber/server.scm
