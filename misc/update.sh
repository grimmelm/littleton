#!/bin/sh
# This script moves into the Littleton source directory (provided by the first
# argument to the script), pull the latest version of the current branch, builds
# the website, and if that is successful, kills the current running Littleton
# process. This script DOES NOT restart the process. It's recommended you use
# something else (like a systemd service) to restart it.

# Abort on any error
set -e

# Error out if there's no argument
if [ -z "$1" ]
then
    echo "No argument supplied."
    echo  "Please provide the path to the source repository as an argument"
    exit 1
fi

# Move to  the repo directory
pushd $1

# Pull the latest version of the current branch
git pull

# Kill the website process. This needs to happen first so that the output binary
# file is not busy while rebuilding.
pkill "littleton"

# Build the website
eval $(opam env)
make website
