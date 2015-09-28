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

test_programs = \
	test-password_file

if BUILD_KRB5_AUTH
test_programs += test-krb5
endif

check_PROGRAMS = $(test_programs)

dist_noinst_DATA = passwords.txt

TESTS = $(test_programs)

test_krb5_CFLAGS = -I$(top_srcdir)/src \
	@CFLAGS@ @GLIB_CFLAGS@ @GMODULE_CFLAGS@ @GZOCHI_COMMON_CFLAGS@
test_krb5_SOURCES = test-krb5.c mock-krb5.c
test_krb5_LDADD = $(top_builddir)/src/auth/krb5.la \
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-log.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GLIB_LIBS@ @GMODULE_LIBS@ @GZOCHI_COMMON_LIBS@

test_password_file_CFLAGS = -DPASSWORDS_TXT_PATH="$(srcdir)/passwords.txt" \
	-I$(top_srcdir)/src \
	@CFLAGS@ @GLIB_CFLAGS@ @GMODULE_CFLAGS@ @GZOCHI_COMMON_CFLAGS@
test_password_file_SOURCES = test-password_file.c
test_password_file_LDADD = $(top_builddir)/src/auth/password_file.la \
	$(top_builddir)/src/libgzochid_la-auth.o \
	$(top_builddir)/src/libgzochid_la-log.o \
	$(top_builddir)/src/libgzochid_la-lrucache.o \
	$(top_builddir)/src/libgzochid_la-util.o \
	@GLIB_LIBS@ @GMODULE_LIBS@ @GZOCHI_COMMON_LIBS@

