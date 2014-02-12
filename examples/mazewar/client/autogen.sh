#!/bin/sh

echo "Generating build files for mazewar example game client."

BASEDIR=`dirname $0`
OLDDIR=$PWD

cd $BASEDIR

# Generate build files.

aclocal
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

automake -a
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

autoconf
if [ $? != 0 ]; then
    cd $OLDDIR
    exit 1
fi

cd $OLDDIR
