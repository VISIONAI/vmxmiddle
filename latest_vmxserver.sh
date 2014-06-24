#!/bin/bash
cd /vmx
rm vmxserver.current.tar.gz
wget http://107.170.162.117/supersecret/vmxserver.current.tar.gz
tar xfzv vmxserver.current.tar.gz
cd -

