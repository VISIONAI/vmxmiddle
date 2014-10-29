#!/bin/sh
#
# This shell script will create a VMX bundle for VMX middle, assigning
# the proper icons, initialization scripts, etc.
#
# The resulting build gets sent over to files.vision.ai

echo 'Welcome to Mac Builder of VMX Middle'

if [ `uname` == "Darwin" ]; then
    PLATFORM="Mac"
else
    PLATFORM="Linux"
fi

#Jump into main repo directory, so we can run this script from anywhere
cd `dirname $0`/../

#Get the version
HASH=$PLATFORM_`./mac_builder/getVMXversion.sh`

#Perform the Haskell compilation of vmxmiddle
cabal clean && cabal configure && cabal build

# set up the Mac OS X bundle directory
BUILD_DIR='dist/VMX.app'
rm -rf $BUILD_DIR
mkdir $BUILD_DIR

mkdir $BUILD_DIR/Contents
mkdir $BUILD_DIR/Contents/MacOS/
mkdir $BUILD_DIR/Contents/MacOS/config/
mkdir $BUILD_DIR/Contents/Resources
mkdir $BUILD_DIR/Contents/Frameworks

# copy over default files
cp ./mac_builder/Info.plist $BUILD_DIR/Contents/Info.plist
cp ./mac_builder/run.sh $BUILD_DIR/Contents/MacOS/
cp ./mac_builder/settings.yml $BUILD_DIR/Contents/MacOS/config/
cp ./resources/vmxicon2.icns $BUILD_DIR/Contents/Resources/VMX.icns

#copy over utility files
#cp ~/projects/vmxmiddle/mac_builder/upload.sh $BUILD_DIR/Contents/MacOS/
#cp ~/projects/vmxmiddle/mac_builder/download.sh $BUILD_DIR/Contents/MacOS/

# copy over main binary
BINARY_NAME=$BUILD_DIR/Contents/MacOS/VMX
cp dist/build/middle/middle $BINARY_NAME

#strip binary
strip $BINARY_NAME

#mkdir $BUILD_DIR/Contents/MacOS/assets/
#mkdir $BUILD_DIR/Contents/MacOS/assets/sessions/
#mkdir $BUILD_DIR/Contents/MacOS/assets/models/
#mkdir $BUILD_DIR/Contents/MacOS/static
#mkdir $BUILD_DIR/Contents/MacOS/static/dist/
#mkdir $BUILD_DIR/Contents/MacOS/static/fonts/
#mkdir $BUILD_DIR/Contents/MacOS/static/enter_license/
#mkdir $BUILD_DIR/Contents/MacOS/static/img
#cp static/index.html $BUILD_DIR/Contents/MacOS/static/
#cp static/models.html $BUILD_DIR/Contents/MacOS/static/
#cp static/sessions.html $BUILD_DIR/Contents/MacOS/static/
#cp static/dist/* $BUILD_DIR/Contents/MacOS/static/dist/
#cp static/fonts/* $BUILD_DIR/Contents/MacOS/static/fonts/
#cp static/img/missing.jpg $BUILD_DIR/Contents/MacOS/static/img/
#cp -R static/enter_license/dist $BUILD_DIR/Contents/MacOS/static/enter_license/

#copy over VMXserver from the build directory
#cp -R /Users/tomasz/projects/VMXserver/build/VMXserver.app $BUILD_DIR/Contents/MacOS/

#copy over initial network
#cp /VMXdata/99* $BUILD_DIR/Contents/MacOS/build/VMXdata/

# Clean and move libraries so they are located inside the bundle
./mac_builder/clean_libs.sh $BINARY_NAME

BUILD_NAME="VMXmiddle_"$HASH
echo $BUILD_NAME > $BUILD_DIR/version

TARBALL=$BUILD_NAME".tar"
cd dist/
tar cf $TARBALL VMX.app
gzip -f $TARBALL
if [ ! -d "../builds/" ]; then
    mkdir ../builds/
fi
mv $TARBALL.gz ../builds/
echo "Finished building builds/"$TARBALL.gz
echo "Copying to files.vision.ai/vmx/"

#scp ~/projects/vmxmiddle/builds/$TARBALL.gz tomasz@vm-x.com:/VMXbuilds/
scp ~/projects/vmxmiddle/builds/$TARBALL.gz root@files.vision.ai:/www/vmx/${PLATFORM}/
