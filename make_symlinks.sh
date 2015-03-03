#NOTE: this should not be ran twice...
#TODO(TJM) check for the presence of these directories, and do not do double linking
mkdir static3
cd static3
ln -s ../static/bower_components bower_components
ln -s ../static/css css
ln -s ../static/dist dist
ln -s ../static/enter_license enter_license
ln -s ../static/docs docs
ln -s ../static/fonts fonts
ln -s ../static/img img
ln -s ../static/js js
ln -s ../static/demo demo
ln -s ../static/partial partial
ln -s ../static/tmpl tmpl
ln -s ../static/vmxjs vmxjs
ln -s ../static/index.html index.html
ln -s ../static/models.html models.html
ln -s ../static/sessions.html sessions.html
