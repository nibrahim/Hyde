#!/bin/sh
set -eu

set -x

if [ ! -f hyde.el ]; then
    echo "Please execute this script from root of repository"
    exit 2
fi

VERSION=$(sed -ne 's|^(defconst hyde/hyde-version "\(.*\)"|\1|p' hyde.el)
BUILDDIR=dist/Hyde-$VERSION

if [ -d "$BUILDDIR" ]; then
    rm -rf $BUILDDIR
fi

mkdir -p "$BUILDDIR"

install -m 644 *.el LICENSE $BUILDDIR
install -m 644 README.md $BUILDDIR/README

tar -C dist -cvf dist/Hyde-$VERSION.tar Hyde-$VERSION
echo "dist/Hyde-$VERSION.tar ready for upload."

