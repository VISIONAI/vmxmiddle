#!/bin/sh
#
# Script to import all models (represented as tarballs) from the
# /incoming folder into the vmx model container
#
# Copyright vision.ai 2015

#The directory where we make VMX model tarballs after downloading
TARGET_DIRECTORY=`pwd`/incoming/
#TARGET_DIRECTORY='/incoming'

NFILES=`ls $TARGET_DIRECTORY/*.gz 2>/dev/null | wc -l`
echo "Number of" $TARGET_DIRECTORY "Models:" $NFILES
if [ "$NFILES" = "0" ]; then
    echo "No models to import, exiting"
    exit
else
    echo "Importing $NFILES models"
fi

if [ "`uname`" = "Linux" ]; then
    docker stop vmx-export 2> /dev/null
    docker rm vmx-export 2> /dev/null
    
    docker run --rm --name vmx-export --volumes-from vmx-userdata:rw \
        -v $TARGET_DIRECTORY:/incoming ubuntu /bin/bash \
        -c "cp -R /incoming/*.gz /vmx/models && cd /vmx/models/ && cat *.gz | tar -xvzf - -i && rm *.gz"
    
else
    cd /Applications/VMX.app/Contents/MacOS/assets/models/
    cp $TARGET_DIRECTORY/*.gz .
    tar xzf *.gz
    rm *.gz
    cd -
fi

#Remove models from /incoming after they have been imported
rm -f $TARGET_DIRECTORY/*.gz
