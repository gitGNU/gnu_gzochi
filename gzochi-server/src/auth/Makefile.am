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

plugindir = $(libdir)/gzochid

plugin_LTLIBRARIES = \
	password_file.la \
	krb5.la

password_file_la_SOURCES = password_file.c
password_file_la_CFLAGS = @GLIB_CFLAGS@ -Wall -Werror
password_file_la_LIBADD = @GLIB_LIBS@
password_file_la_LDFLAGS = -module -avoid-version

krb5_la_SOURCES = krb5.c
krb5_la_CFLAGS = @GLIB_CFLAGS@ @KRB5_CFLAGS@ -Wall -Werror
krb5_la_LIBADD = @GLIB_LIBS@ @KRB5_LIBS@
krb5_la_LDFLAGS = -module -avoid-version
