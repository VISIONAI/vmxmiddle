#!/bin/sh
#
# A script to get new versions of VMX components for the Mac version
# without having to re-download the entire installer.
#
# Copyright vision.ai, LLC 2015

cd "`dirname "$0"`"
M=`basename "$0"`
echo "VMX Mac Updater (version `md5 -r $M | awk '{print($1)}'`)"
usage() {
    echo "Usage: $0 latest"
    echo "Usage: $0 stable"
    exit 1
}

[ $# -lt 1 ] && {
  usage
}


FLAG=$1

if [ "$FLAG" != "stable" ] && [ "$FLAG" != "latest" ]; then
    echo Flag must be latest or stable
    usage
fi

#Get our version
CHECK=`curl localhost:3000/check 2>/dev/null`
echo $CHECK | jq .

mkdir -p software_updates/

CUR=`echo $CHECK | jq -r ".version[0]"`.tar.gz
SER=`curl -s https://files.vision.ai/vmx/VMXserver/Mac/MD5SUMS.json`
REM=`echo $SER | jq -r "."$FLAG".file"`
echo "Installed VMXserver:" $CUR
echo "$FLAG    VMXserver:" $REM

if [ "$CUR" != "$REM" ]; then
    echo "Downloading..."
    cd software_updates
    curl -# -o $REM https://files.vision.ai/vmx/VMXserver/Mac/$REM
    if [ "`md5 -r $REM | awk '{print($1)}'`" != "`echo $SER | jq -r "."$FLAG".md5"`" ]; then
        echo "md5 sums do not match"
        exit
    else
        echo "md5 sums matched"
    fi
    tar xf $REM
    rm build/VMXserver.app/Contents/MacOS/config.json
    cp -R build/VMXserver.app ../
    rm -rf build
    cd -
else
    echo "Not downloading, versions match"
fi

CUR=`echo $CHECK | jq -r ".version[1]"`.tar.gz
SER=`curl -s https://files.vision.ai/vmx/VMXmiddle/Mac/MD5SUMS.json`
REM=`echo $SER| jq -r "."$FLAG".file"`
echo "Installed VMXmiddle:" $CUR
echo "$FLAG    VMXmiddle:" $REM

if [ "$CUR" != "$REM" ]; then
    echo "Downloading..."
    cd software_updates
    curl -# -o $REM https://files.vision.ai/vmx/VMXmiddle/Mac/$REM

    if [ "`md5 -r $REM | awk '{print($1)}'`" != "`echo $SER | jq -r "."$FLAG".md5"`" ]; then
        echo "md5 sums do not match"
        exit
    else
        echo "md5 sums matched"
    fi

    tar xf $REM
    rm VMX.app/Contents/MacOS/config/settings.yml 
    cp -R VMX.app/Contents/MacOS/ ../
    rm -rf VMX.app
    cd -
else
    echo "Not downloading, versions match"
fi

CUR=`echo $CHECK | jq -r ".version[2]"`.tar.gz
SER=`curl -s https://files.vision.ai/vmx/vmxAppBuilder/MD5SUMS.json`
REM=`echo $SER | jq -r "."$FLAG".file"`
echo "Installed AppBuilder:" $CUR
echo "$FLAG    AppBuilder:" $REM

if [ "$CUR" != "$REM" ]; then
    echo "Downloading..."
    cd software_updates
    curl -# -o $REM https://files.vision.ai/vmx/vmxAppBuilder/$REM
    if [ "`md5 -r $REM | awk '{print($1)}'`" != "`echo $SER | jq -r "."$FLAG".md5"`" ]; then
        echo "md5 sums do not match"
        exit
    else
        echo "md5 sums matched"
    fi

    tar xf $REM
    cp -R static/ ../
    rm -rf static
    cd -
else
    echo "Not downloading, versions match"
fi


#Get our version
CHECK=`curl localhost:3000/check 2>/dev/null`
echo $CHECK | jq .
