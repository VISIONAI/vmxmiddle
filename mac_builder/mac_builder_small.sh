#!/bin/bash
#
# This shell script will create a Mac OS X VMX bundle for VMXmiddle,
# assigning the proper icons, config files, etc.  It does not include
# VMXAppBuilder, which is maintained in another build pipeline.
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
#HASH=$PLATFORM_`./mac_builder/getVMXversion.sh`
HASH=${PLATFORM}_`git describe --tags --dirty`


if [ `uname` == "Darwin" ]; then
    #Perform the Haskell compilation of vmxmiddle
    cabal clean && cabal configure && cabal build
else
    echo 'pwd is' `pwd`
    ./cabal_build.sh
fi
# set up the Mac OS X bundle directory

if [ `uname` == "Darwin" ]; then
    APP_NAME=VMX.app
    BUILD_DIR='dist/VMX.app/'
    BUILD_SUBDIR='dist/VMX.app/Contents/'
    BUILD_SUBDIR2='dist/VMX.app/Contents/MacOS/'
else
    APP_NAME=VMX
    BUILD_DIR='dist/VMX/'
    BUILD_SUBDIR='dist/VMX/'
    BUILD_SUBDIR2='dist/VMX/'
fi

rm -rf $BUILD_DIR
mkdir $BUILD_DIR
mkdir $BUILD_SUBDIR
mkdir $BUILD_SUBDIR2

CONFIG_DIR=$BUILD_SUBDIR2/config/
mkdir $CONFIG_DIR


if [ `uname` == "Darwin" ]; then
    # copy over Mac bundle files
    cp ./mac_builder/Info.plist $BUILD_SUBDIR/Info.plist
    cp ./mac_builder/run.sh $BUILD_SUBDIR2/run.sh
fi

#copy over necessary config files
cp ./mac_builder/settings.yml $CONFIG_DIR/settings.yml


cp ./config/favicon.ico $CONFIG_DIR/favicon.ico
cp ./config/robots.txt $CONFIG_DIR/robots.txt

if [ `uname` == "Darwin" ]; then
    #copy over Mac Bundle icon
    mkdir $BUILD_DIR/Contents/Resources
    cp ./resources/vmxicon2.icns $BUILD_DIR/Contents/Resources/VMX.icns
fi

# copy over main binary
BINARY_NAME=$BUILD_SUBDIR2/VMX
cp dist/build/middle/middle $BINARY_NAME

#strip binary
strip $BINARY_NAME

#copy over utility files
#cp ~/projects/vmxmiddle/mac_builder/upload.sh $BUILD_DIR/Contents/MacOS/
#cp ~/projects/vmxmiddle/mac_builder/download.sh $BUILD_DIR/Contents/MacOS/

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

if [ `uname` == "Darwin" ]; then
    # Clean and move libraries so they are located inside the bundle's Frameworks directory
    ./mac_builder/clean_libs.sh $BINARY_NAME
fi

# Create a tarball and send it to the server
BUILD_NAME="VMXmiddle_"$HASH
echo build name is $BUILD_NAME
echo $BUILD_NAME > $BUILD_DIR/version

TARBALL=$BUILD_NAME".tar"
echo 'tarball is now ' $TARBALL
cd dist/
tar cf $TARBALL $APP_NAME
gzip -f $TARBALL
if [ ! -d "../builds/" ]; then
    mkdir ../builds/
fi
mv $TARBALL.gz ../builds/
echo "Finished building builds/"$TARBALL.gz
echo "Copying to files.vision.ai/vmx/"

scp ../builds/$TARBALL.gz root@files.vision.ai:/www/vmx/VMXmiddle/${PLATFORM}/
