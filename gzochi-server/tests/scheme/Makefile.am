## Process this file with automake to produce Makefile.in
#
# Makefile.am: Automake input file.
#
# Copyright (C) 2015 Julian Graham
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

test_programs = gzochi/test-data.scm \
	gzochi/test-tx.scm \
	gzochi/private/test-app.scm \
	gzochi/private/test-data.scm

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(GUILE)
AM_SCM_LOG_FLAGS = -L $(srcdir) -L $(top_srcdir)/src/scheme

check_SCRIPTS = $(test_programs)
TESTS = $(test_programs)

EXTRA_DIST = $(TESTS) gzochi/mock-data.scm gzochi/srfi-64-support.scm

CLEANFILES = gzochi:managed-sequence.log gzochi:transaction-participant.log \
	gzochi:deserialize-managed-record.log gzochi:abort-transaction.log \
	gzochi:make-managed-record-type-descriptor.log \
	gzochi:execute-ready.log gzochi:join-transaction.log \
	gzochi:serialize-managed-record.log gzochi:execute-logged-in.log \
	gzochi:define-managed-record-type.log
