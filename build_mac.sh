#This is the script to compile VMX on a Mac

#Because of what otool is doing (cabal-macosx package), we need to be root
sudo runghc Setup configure --user
sudo runghc Setup build
sudo chown -R tomasz *

APP=vmx

mkdir dist/build/${APP}.app/Contents/MacOS/static
mkdir dist/build/${APP}.app/Contents/MacOS/static/dist/
mkdir dist/build/${APP}.app/Contents/MacOS/static/fonts/
mkdir dist/build/${APP}.app/Contents/MacOS/static/enter_license/
cp static/index.html dist/build/${APP}.app/Contents/MacOS/static/
cp static/dist/* dist/build/${APP}.app/Contents/MacOS/static/dist/
cp static/fonts/* dist/build/${APP}.app/Contents/MacOS/static/fonts/

cp -R static/enter_license/dist dist/build/${APP}.app/Contents/MacOS/static/enter_license/

mkdir dist/build/${APP}.app/Contents/MacOS/config/
cp config/settings.yml dist/build/${APP}.app/Contents/MacOS/config/

#copy over VMXserver
cp -R /Users/tomasz/projects/VMXserver/build dist/build/${APP}.app/Contents/MacOS/

#strip binary
strip dist/build/${APP}.app/Contents/MacOS/${APP}

