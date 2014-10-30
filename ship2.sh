#!/bin/bash
# Shell script to bundle vmx into a tarball

if [ `uname` == "Darwin" ]; then
    echo 'ship2.sh works only on Linux'
    exit 1
fi
PLATFORM=Linux

#setup the build directory
BUILD_DIR=scratch
mkdir -p $BUILD_DIR/config

#copy over main binary and strip it
cp dist/build/middle/middle $BUILD_DIR/vmx
strip $BUILD_DIR/vmx

#copy over settings/icon/robots files
cp config/settings.yml $BUILD_DIR/config
cp config/favicon.ico $BUILD_DIR/config
cp config/robots.txt $BUILD_DIR/config

tar cfzv middle.linux.tar.gz  -C $BUILD_DIR .
#scp middle.linux.tar.gz root@files.vision.ai:/usr/share/nginx/html/middle.linux.tar.gz
