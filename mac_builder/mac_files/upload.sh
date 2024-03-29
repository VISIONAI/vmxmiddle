#!/bin/bash 
#
# Here is a short script for sharing VMX models. Model data files are
# securely copied to a final location.  This script works over SSH and
# requires an SSH key for passwordless copies.  This is an internal
# tool used to aggregate the results of many different training
# sessions.
#
# Copyright 2013-2014 vision.ai, LLC 

server=http://vm-x.com:3084/upload

#Define the location of remote copy when using SSH
#user=tomasz
#server=vmx
#location=/www/incoming


echo 'This script will share your models with the rest of the world'

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 models_dir" >&2
  exit 1
fi

while true; do
    read -p "Do you wish to share your models and make them public? (y)es or (n)o " yn
    case $yn in
        [Yy]* ) break;;
        [Nn]* ) exit;;
        * ) echo "Please answer yes or no.";;
    esac
done

models=$1
uuids=`ls $models`

status=`curl -s -I --connect-timeout 3 -I $server`

if [ "$?" = "0" ];
then
    echo 'Upload server up'
else
    echo 'Upload server down'
    exit
fi

for uuid in $uuids; do
    FILE=$models/$uuid/model.data
    if [ -f $FILE ];
    then
        echo 'Uploading' $FILE
    else
        echo 'File' $FILE 'does not exist'
        break;
    fi
    echo 'Copying to' $server
    #ssh ${user}@${server} "cat > /tmp/${uuid} && cp /tmp/${uuid} ${location}/${uuid}.data && rm /tmp/${uuid}" < $models/$uuid/model.data
    curl --connect-timeout 1 -F name=VMXupload -F filedata="@$FILE" ${server} 2>&1 > /dev/null 
    if [ "$?" = "0" ];
    then
        echo 'Successfully copied'
    else
        echo 'Problem copying'
    fi
done
