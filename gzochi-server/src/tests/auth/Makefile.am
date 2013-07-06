## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2013 Julian Graham
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

check_PROGRAMS = $(test_programs)

test_programs = \
	test-krb5 \
	test-password_file

dist_noinst_DATA = passwords.txt

TESTS = $(test_programs)

GZOCHI_COMMON_CFLAGS = -I@GZOCHI_COMMON@/src
GZOCHI_COMMON_LIBS = @GZOCHI_COMMON@/src/libgzochi-common.la

AM_CFLAGS = @CFLAGS@ @GLIB_CFLAGS@ @GMODULE_CFLAGS@ $(GZOCHI_COMMON_CFLAGS)

test_krb5_SOURCES = test-krb5.c mock-krb5.c
test_krb5_LDADD = ../../auth/krb5.o ../../auth.o \
	../../log.o ../../util.o \
	@GLIB_LIBS@ @GMODULE_LIBS@ $(GZOCHI_COMMON_LIBS)

test_password_file_SOURCES = test-password_file.c
test_password_file_LDADD = ../../auth/password_file.o ../../auth.o \
	../../log.o ../../util.o \
	@GLIB_LIBS@ @GMODULE_LIBS@ $(GZOCHI_COMMON_LIBS)