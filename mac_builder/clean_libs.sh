#!/bin/sh
#
# Some code to clean up libraries on Mac OS X so they are located
# inside a bundle, even though the binary was built using tools from
# Macports
#
# Usage: ./clean_libs.sh dist/VMX.app/Contents/MacOS/VMX
#
# It will place the dependecies inside ${BINARY_NAME}/../../Frameworks/
#
# Tomasz Malisiewicz
# Copyright 2014 vision.ai, LLC

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 BINARY_NAME" >&2
  exit 1
fi

BINARY_NAME=$1
BUILD_DIR=`dirname $1`/../../

#clean up libs
LIBS=`otool -L $BINARY_NAME | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`

for i in $LIBS; do
    echo "i is" $i
    LOCAL_LIB='@executable_path/../Frameworks/'`basename $i`
    #echo "LL is" $LOCAL_LIB
    echo install_name_tool -change $i $LOCAL_LIB $BINARY_NAME
    install_name_tool -change $i $LOCAL_LIB $BINARY_NAME
    cp ${i} $BUILD_DIR/Contents/Frameworks/
    LIBS2=`otool -L ${i} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
    for j in $LIBS2; do
        #echo "i j is " $i $j
        cp ${j} $BUILD_DIR/Contents/Frameworks/
        LIBS3=`otool -L ${j} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
        for k in $LIBS3; do
            #echo "j k is " $j $k
            cp ${k} $BUILD_DIR/Contents/Frameworks/
            LIBS4=`otool -L ${k} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
            for l in $LIBS4; do
                #echo "k l is " $k $l
                cp ${l} $BUILD_DIR/Contents/Frameworks/
                LIBS5=`otool -L ${l} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
                for m in $LIBS5; do
                    #echo "l m is " $l $m
                    cp ${m} $BUILD_DIR/Contents/Frameworks/
                done

                
            done
            
        done
    done
done

### go over all libs and replace /opt/local/bin with @executable_path\/..\/Frameworks

LIBS=`find $BUILD_DIR/Contents/Frameworks/ -type f`
for i in $LIBS; do
    LIBS2=`otool -L ${i} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
    for j in $LIBS2; do
        #LOCAL_LIB='./'`basename $j`
        LOCAL_LIB='@executable_path/../Frameworks/'`basename $j`
        install_name_tool -change $j $LOCAL_LIB $i
    done    
done

#update local id of library
cd $BUILD_DIR/Contents/Frameworks/
LIBS=`ls`
for i in $LIBS; do
    install_name_tool -id $i $i
done
cd -

