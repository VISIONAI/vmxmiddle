#!/bin/bash

#Make sure the branch is set, this is something done in node-git-fish first
if [ -z "$VMXMIDDLE_BRANCH" ]
then
  echo "VMXMIDDLE_BRANCH environment variable must be set"
  exit 1;
fi


#Build the Haskell application using Docker
./cabal_build.sh
EC=$?

if [ "$EC" != "0" ];
then
    echo "Cabal build failed"
    exit 1
fi

#Delete old files
rm -rf scratch/

#Make the scratch directory which will contain the tarball
mkdir -p scratch/config

#copy over upload_script
#cp scripts/upload_model.sh scratch/upload_model.sh

cp dist/build/middle/middle scratch/vmx
cp config/settings.yml config/favicon.ico config/robots.txt scratch/config

PLATFORM="Linux"
HASH=${PLATFORM}_`git describe --tags`
BUILD_NAME="VMXmiddle_"$HASH
echo "build name is" $BUILD_NAME

echo -n $BUILD_NAME > scratch/version

TARBALL=${BUILD_NAME}.tar.gz
#TARBALL=middle.linux-${VMXMIDDLE_BRANCH}.tar.gz

tar cfzv $TARBALL  -C scratch .
scp $TARBALL root@files.vision.ai:/www/vmx/VMXmiddle/Linux/

if [ ! -d "builds/" ]; then
    mkdir builds/
fi
mv $TARBALL builds/
