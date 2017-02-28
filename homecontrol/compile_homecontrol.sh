#!/bin/bash
set -e

echo "-> Compiling serial for local platform"
pushd $(pwd)
cd HomeControl/_build/default/lib/serial
make
# sudo DESTDIR=/usr/lib make install
popd

echo "-> Compiling jiffy for local platform"
pushd $(pwd)
cd HomeControl/_build/default/lib/jiffy
# rm ./priv/jiffy.so
# make clean
make
# sudo DESTDIR=/usr/lib make install
popd

cd HomeControl
erl -name "$1@$2" -setcookie HOMECONTROL -pa ./_build/default/lib/*/ebin
