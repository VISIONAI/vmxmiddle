mkdir -p scratch/config
cp dist/build/middle/middle scratch/vmx
cp -r config/settings.yml scratch/config
## do grunt stuff
cd static && grunt prod && cd -

mkdir -p scratch/static
cp static/index.html scratch/static/index.html
cp -r static/dist scratch/static
cp -r static/bower_components/font-awesome/fonts scratch/static
mkdir -p scratch/static/js/vmx-api/src/
cp static/js/vmx-api/src/vmxApi.js scratch/static/js/vmx-api/src

cd static/enter_license && grunt build && cd -
mkdir -p scratch/static/enter_license
cp -r static/enter_license/dist/ scratch/static/enter_license

tar cfzv middle.linux.tar.gz  -C scratch .
scp middle.linux.tar.gz root@files.vision.ai:/usr/share/nginx/html/middle.linux.tar.gz
