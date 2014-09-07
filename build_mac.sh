sudo runghc Setup configure --user
sudo runghc Setup build
sudo chown -R tomasz *
mkdir dist/build/middle.app/Contents/MacOS/static
mkdir dist/build/middle.app/Contents/MacOS/static/dist/
mkdir dist/build/middle.app/Contents/MacOS/static/fonts/
cp static/index.html dist/build/middle.app/Contents/MacOS/static/
cp static/dist/* dist/build/middle.app/Contents/MacOS/static/dist/
cp static/fonts/* dist/build/middle.app/Contents/MacOS/static/fonts/

mkdir dist/build/middle.app/Contents/MacOS/config/
cp config/*.yml dist/build/middle.app/Contents/MacOS/config/

#strip binary
strip dist/build/middle.app/Contents/MacOS/middle
