#!/bin/sh

# test-gzochi-load.sh: End-to-end test for the gzochi-load tool
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

# The expected contents of the database.

REFERENCE_OUTPUT=`cat <<EOF
baz
quux
foo
bar
EOF`

# The dump file to be loaded.

INPUT=`cat <<EOF
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

# Load the dump data into the database.

if echo "$INPUT" | ../meta/gzochi-load -e treefile $TMPDIR:names; then

    # Compare the database with the expected contents.

    if echo "$REFERENCE_OUTPUT" | diff - $TMPDIR/names; then
	echo "SUCCESS" >&2
	rm -rf $TMPDIR
	exit 0
    else
	echo "FAILED: Incorrect output." >&2
	rm -rf $TMPDIR
	exit 1
    fi
else
    echo "FAILED: Load failed." >&2
    rm -rf $TMPDIR
    exit 1
fi
