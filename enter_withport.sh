docker rm vmxmiddle-manual
docker run -t -i  --name vmxmiddle-manual -v `pwd`:/code --link db:db  -v /dockerscratch/sessions:/www/vmx/sessions --volumes-from vmxmodels --volumes-from vmxserver --volumes-from vmxdata -p 3000:3000  gdoteof/d-vmxmiddle  /bin/bash
