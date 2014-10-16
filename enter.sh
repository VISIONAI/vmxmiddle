docker rm vmxmiddle-manual
docker run -t -i  --name vmxmiddle-manual -v `pwd`:/code --link db:db  -v /dockerscratch/sessions:/vmx/sessions --volumes-from vmxmodels --volumes-from vmxserver --volumes-from vmxdata   gdoteof/d-vmxmiddle  /bin/bash
