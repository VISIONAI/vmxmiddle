#!/bin/bash
# Shell script to bundle vmx into a tarball

if [ `uname` == "Darwin" ]; then
    echo 'ship2.sh works only on Linux'
    exit 1
fi
PLATFORM=Linux
HASH=$PLATFORM_`./mac_builder/getVMXversion.sh`

#setup the build directory
BUILD_DIR=dist/vmx
mkdir -p $BUILD_DIR/config

#copy over main binary and strip it
cp dist/build/middle/middle $BUILD_DIR/vmx
strip $BUILD_DIR/vmx

#copy over settings/icon/robots files
cp config/settings.yml $BUILD_DIR/config
cp config/favicon.ico $BUILD_DIR/config
cp config/robots.txt $BUILD_DIR/config

BUILD_NAME="VMXmiddle_"$HASH
echo $BUILD_NAME > $BUILD_DIR/version
mkdir builds 2>/dev/null
OWD=`pwd`
cd $BUILD_DIR
tar cfzv $OWD/builds/$BUILD_NAME.tar.gz .
scp $OWD/builds/$BUILD_NAME.tar.gz root@files.vision.ai:/www/vmx/$PLATFORM/
