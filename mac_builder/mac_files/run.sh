#!/bin/bash
# VMX Launching script
`dirname $0`/VMX  >> `dirname $0`/vmx.log 2>&1 &
sleep 1
/usr/bin/open http://localhost:3000
