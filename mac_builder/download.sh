#!/bin/bash 
#
# Here is a small script for downloading models into your local VMX session.
#
# Copyright 2013-2014 vision.ai, LLC 

server=http://vm-x.com:3085

#Define the location of remote copy when using SSH
#user=tomasz
#server=vmx
#location=/www/incoming

echo 'This script will download shared models'

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 vmx_dir" >&2
  exit 1
fi

while true; do
    read -p "Do you wish to download public models? (y)es or (n)o " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

vmx_dir=$1


status=`curl -s -I --connect-timeout 2 -I $server`

if [ "$?" = "0" ];
then
    echo 'Download server up'
else
    echo 'Download server down'
    exit
fi
./VMXserver.app/Contents/MacOS/VMXserver `pwd`/$vmx_dir uploader none :8090 &
sleep 3
listing=`curl $server -H "Accept: application/json" | jq -r '.[]'`
#echo 'listing is' $listing
for f in $listing
do
    echo 'file is' $server/$f
    curl -X POST -d '{"command":"load_model","uuids":["'$server/$f'"],"compiled":false}' localhost:8090
    curl -X POST -d '{"command":"save_model","name":""}' localhost:8090
    
done

curl -X POST -d '{"command":"exit"}' localhost:8090

