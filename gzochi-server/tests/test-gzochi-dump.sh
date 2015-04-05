#!/bin/sh

# test-gzochi-dump.sh: End-to-end test for the gzochi-dump tool
# Copyright (C) 2015 Julian Graham
#
# gzochi is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

export GZOCHID_STORAGE_ENGINE_DIR=`pwd`
export TMPDIR=`mktemp -d tmp.XXXXXX`

# The initial contents of the database.

cat <<EOF >$TMPDIR/names
foo
bar
baz
quux
EOF

# The expected output.

REFERENCE_OUTPUT=`cat <<EOF
VERSION=3
format=bytevalue
type=btree
HEADER=END
 62617a
 71757578
 666f6f
 626172
DATA=END
EOF`

# Dump the contents of the database.

if ../meta/gzochi-dump -e treefile $TMPDIR:names >$TMPDIR/names.dump; then

    # Compare the dump file with the expected output.

    if echo "$REFERENCE_OUTPUT" | diff - $TMPDIR/names.dump; then
	echo "SUCCESS" >&2
	rm -rf $TMPDIR
	exit 0
    else
	echo "FAILED: Incorrect output." >&2
	rm -rf $TMPDIR
	exit 1
    fi
else
    echo "FAILED: Dump failed." >&2
    rm -rf $TMPDIR
    exit 1
fi
