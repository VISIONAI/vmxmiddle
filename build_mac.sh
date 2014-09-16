sudo runghc Setup configure --user
sudo runghc Setup build
sudo chown -R tomasz *
mkdir dist/build/middle.app/Contents/MacOS/static
mkdir dist/build/middle.app/Contents/MacOS/static/dist/
mkdir dist/build/middle.app/Contents/MacOS/static/fonts/
mkdir dist/build/middle.app/Contents/MacOS/static/enter_license/
cp static/index.html dist/build/middle.app/Contents/MacOS/static/
cp static/dist/* dist/build/middle.app/Contents/MacOS/static/dist/
cp static/fonts/* dist/build/middle.app/Contents/MacOS/static/fonts/

cp -R static/enter_license/dist dist/build/middle.app/Contents/MacOS/static/enter_license/

mkdir dist/build/middle.app/Contents/MacOS/config/
cp config/settings.yml dist/build/middle.app/Contents/MacOS/config/

#copy over VMXserver
cp -R /Users/tomasz/projects/VMXserver/build dist/build/middle.app/Contents/MacOS/

#strip binary
strip dist/build/middle.app/Contents/MacOS/middle
