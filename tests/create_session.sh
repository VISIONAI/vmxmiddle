#!/bin/sh

#Get the UUID of the first model
UUID1=`curl -s -X GET http://localhost:3000/model | jq -r '.data[0].uuid'`
echo 'Creating session with UUID=' $UUID1

#Create a new session with the target UUID
curl -s -X POST -H "Content-Type: application/json" -d '{"uuids":["'$UUID1'"],"compiled":"false"}' http://localhost:3000/session
echo 'Sessions are now:' `curl -s http://localhost:3000/session | jq -r '.data[].session'`

