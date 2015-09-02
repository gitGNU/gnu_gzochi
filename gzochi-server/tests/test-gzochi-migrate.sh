#!/bin/sh

# test-gzochi-migrate.sh: End-to-end test for the gzochi-migrate tool
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
export TMPCONF=`mktemp tmp.conf.XXXXXX`

fail () {
    case $1 in
	"load") echo "FAILED: Failed to load data." >&2 ;;		
	"migrate") echo "FAILED: gzochi-migrate failed." >&2 ;;
	"dump") echo "FAILED: Failed to dump data." >&2 ;;
	"meta") echo "FAILED: Incorrect meta contents." >&2 ;;
	"names") echo "FAILED: Incorrect names contents." >&2 ;;
	"oids") echo "FAILED: Incorrect oids contents." >&2 ;; 
    esac
    rm -rf $TMPDIR
    rm -f $TMPCONF
    exit 1
}

META=`cat <<EOF
VERSION=3
format=bytevalue
type=btree
HEADER=END
DATA=END
EOF`

# Includes bindings:
#   o.foo\000 -> 0\000

NAMES=`cat <<EOF
VERSION=3
format=bytevalue
type=btree
HEADER=END
 6f2e666f6f00
 3000
DATA=END
EOF`

# Includes serialized records:
#   0\x00 -> \x00\x00\x00\x10\x0einteger-holder\x00

OIDS=`cat <<EOF
VERSION=3
format=bytevalue
type=btree
HEADER=END
 3000
 000000100e696e74656765722d686f6c64657200
DATA=END
EOF`

mkdir $TMPDIR/test-gzochi-migrate

echo "$META" | ../meta/gzochi-load \
		   -e treefile $TMPDIR/test-gzochi-migrate:meta || fail "load"
echo "$NAMES" | ../meta/gzochi-load \
		    -e treefile $TMPDIR/test-gzochi-migrate:names || fail "load"
echo "$OIDS" | ../meta/gzochi-load \
		   -e treefile $TMPDIR/test-gzochi-migrate:oids || fail "load"

sed -s "s/__TMPDIR__/$TMPDIR/" <test-gzochi-migrate.conf.in >$TMPCONF

if ! ../meta/gzochi-migrate -c $TMPCONF test-gzochi-migrate.xml; then
    fail "migrate"
fi

REFERENCE_META="$META"
REFERENCE_NAMES="$NAMES"

# Includes serialized records:
#   0\x00 -> \x00\x00\x00\x10\x0einteger-holder\x00\x03foo

REFERENCE_OIDS=`cat <<EOF
VERSION=3
format=bytevalue
type=btree
HEADER=END
 3000
 000000140e696e74656765722d686f6c6465720003666f6f
DATA=END
EOF`

../meta/gzochi-dump -e treefile $TMPDIR/test-gzochi-migrate:meta \
		    >$TMPDIR/meta.dump || fail "dump"
../meta/gzochi-dump -e treefile $TMPDIR/test-gzochi-migrate:names \
		    >$TMPDIR/names.dump || fail "dump"
../meta/gzochi-dump -e treefile $TMPDIR/test-gzochi-migrate:oids \
		    >$TMPDIR/oids.dump || fail "dump"

if ! echo "$REFERENCE_META" | diff - $TMPDIR/meta.dump; then fail "meta"; fi
if ! echo "$REFERENCE_NAMES" | diff - $TMPDIR/names.dump; then fail "names"; fi
if ! echo "$REFERENCE_OIDS" | diff - $TMPDIR/oids.dump; then fail "oids"; fi

echo "SUCCESS" >&2
rm -rf $TMPDIR
rm -f $TMPCONF
exit 0
