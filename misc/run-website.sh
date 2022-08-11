#!/bin/sh

# Run this script from the toplevel source directory
make website
pushd _build/website
pkill "littleton" || true
nohup ./littleton server 8000 &
popd
