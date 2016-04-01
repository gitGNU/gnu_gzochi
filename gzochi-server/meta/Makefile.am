## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2016 Julian Graham
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

noinst_DATA = conf/gzochi-metad.conf conf/gzochid.conf
noinst_SCRIPTS = gzochid gzochi-metad gzochi-dump gzochi-load gzochi-migrate \
	uninstalled-env

EXTRA_DIST = conf/gzochi-metad.conf.in conf/gzochid.conf.in gzochid.in \
	gzochi-metad.in gzochi-dump.in gzochi-load.in gzochi-migrate.in \
	uninstalled-env.in

substitute = sed -e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'

conf/gzochi-metad.conf: conf/gzochi-metad.conf.in $(top_builddir)/config.status
	mkdir -p $(builddir)/conf
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

conf/gzochid.conf: conf/gzochid.conf.in $(top_builddir)/config.status
	mkdir -p $(builddir)/conf
	$(substitute) < "$<" > "$@.out"
	mv "$@.out" "$@"

gzochid: gzochid.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

gzochi-metad: gzochi-metad.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

gzochi-dump: gzochi-dump.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

gzochi-load: gzochi-load.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

gzochi-migrate: gzochi-migrate.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

uninstalled-env: uninstalled-env.in $(top_builddir)/config.status
	$(substitute) < "$<" > "$@.out"
	chmod +x "$@.out"
	mv "$@.out" "$@"

CLEANFILES = conf/gzochi-metad.conf conf/gzochid.conf gzochid gzochi-metad \
	gzochi-dump gzochi-load gzochi-migrate uninstalled-env
