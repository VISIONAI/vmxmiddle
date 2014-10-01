#!/bin/sh
#Get all session ids
SIDS=`curl -s http://localhost:3000/session | jq -r '.data[].session'`

#IMAGE='/VMXdata/simple.jpg'
IMAGE='http://people.csail.mit.edu/tomasz/img/tomasz_blue_crop.jpg'
for i in `seq 1 1000000`; do
    for j in $SIDS; do
        SID=$j
        echo SID is $SID
        #Issue a process image request for the current session
        curl -s -X GET -A "pid:"$$  http://localhost:3000/session/$SID/params
    done
done
