#!/bin/bash
# Simple command line script to create N new sessions
NUM_SESSIONS=1
if [ "x$1" != "x" ]; then
    NUM_SESSIONS=$1
else
    echo 'Usage:' $0 'NUM_SESSIONS'
    exit;
fi

#Get the UUID of the first model
UUID1=`curl -s -X GET http://localhost:3000/model | jq -r '.data[0].uuid'`
echo 'Creating session with UUID=' $UUID1
for i in `seq 1 $1`; do
    #Create a new session with the target UUID
    curl -s -X POST -H "Content-Type: application/json" -d '{"uuids":["'$UUID1'"],"compiled":"false"}' http://localhost:3000/session | jq .
    echo 'Sessions are now:' `curl -s http://localhost:3000/session | jq -r '.data[].session'`
done
