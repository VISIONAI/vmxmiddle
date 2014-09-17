#! /bin/bash


test -f VMX.dmg && rm VMX.dmg
/Users/tomasz/projects/yoursway-create-dmg/create-dmg --window-size 500 300 --background ~/projects/VMXassets/vision.ai.png --icon-size 80 --volname "VMXServer" --volicon ~/projects/VMXassets/vmxicon.icns --app-drop-link 380 205 --eula build/licenses/vmx_license.txt --icon "VMX Server" 110 205 VMX.dmg dist/build/middle.app
