#!/bin/sh
NUM_SESSIONS=1

#number of iterations to run for
NITER=10

TEST_DIR=TEST_load_loop
mkdir $TEST_DIR
rm -rf $TEST_DIR
mkdir $TEST_DIR

IMAGE='/VMXdata/simple.jpg'
#IMAGE='http://people.csail.mit.edu/tomasz/img/tomasz_blue_crop.jpg'

#Determine if VMX is running or not
sd=`curl -s http://localhost:3000/session`

if [ "$sd" == "" ]; then
    echo 'VMX is not running, so starting'
    ~/projects/vmxmiddle/dist/VMX.app/Contents/MacOS/VMX > $TEST_DIR/vmx.1.log 2> $TEST_DIR/vmx.2.log &
    VMXPID=$!
    sleep 1
    echo 'PID is now' $VMXPID
else
    echo 'VMX is already running, kill it manually to start the test'
    exit
fi

#Get the UUID of the first model
UUID1=`curl -s -X GET http://localhost:3000/model | jq -r '.data[0].uuid'`
#UUID1=none
echo 'Creating session with model_id=' $UUID1
for i in `seq 1 $1`; do
    #Create a new session with the target UUID
    curl -s -X POST -d '{"uuids":["'$UUID1'BAD"],"compiled":"false"}'  http://localhost:3000/session  | jq .
    echo 'Sessions are now:' `curl -s http://localhost:3000/session | jq -r '.data[].session'`
done

#Get all session ids
SIDS=`curl -s http://localhost:3000/session | jq -r '.data[].session'`

#UUID1=`curl -s -X GET http://localhost:3000/model | jq -r '.data[0].uuid'`
for j in $SIDS; do
    SID=$j
    for i in `seq 1 $NITER`; do
        #issue a new load
        curl -s -X POST -d '{"uuids":["'$UUID1'"],"compiled":true}' localhost:3000/session/$SID/load | jq .
        #Issue a process image request for the current session
        curl -s -X POST -A "pid:"$$ -H "Content-Type: application/json" -d '{"images":[{"image":"'$IMAGE'","time":"now"}],"params":{"crop_radius":80,"crop_threshold":-1,"display_threshold":-1,"jpeg_quality":1,"remove_smooth_below_threshold":true,"display_top_detection":false,"max_windows":10,"learn_mode":false,"learn_iterations":10,"learn_threshold":0,"detect_max_overlap":0.3,"learn_max_positives":1,"detect_add_flip":false,"levels_per_octave":10,"max_image_size":320,"initialize_max_cells":12,"cell_size":8,"initialize_add_flip":true}}' http://localhost:3000/session/$SID | jq .

    done
    #curl -s -X POST -d '{"uuids":["'$UUID1'"],"compiled":false}' localhost:3000/session/$SID/load | jq .
    cp /www/vvv2/sessions/$SID/log.txt $TEST_DIR/VMXserver.log.txt
    curl -s -X DELETE localhost:3000/session/$SID | jq .
done

echo 'Stopping VMX'
kill -9 $VMXPID
