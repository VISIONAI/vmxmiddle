#!/bin/bash
# VMX Launching script
`dirname $0`/VMX  2>&1 >> `dirname $0`/vmx.log &
sleep 1
/usr/bin/open http://localhost:3000
