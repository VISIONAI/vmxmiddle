RUNNING=$(docker ps | awk '{print $NF}' | grep 'vmxmiddle-current$' | wc -l)



docker run -t -i  -p 3000:3000 --name vmxmiddle-current -v `pwd`:/code --link db:db  -v /dockerscratch/sessions:/www/vmx/sessions --volumes-from vmxmodels --volumes-from vmxserver --volumes-from vmxdata   gdoteof/d-vmxmiddle  /bin/bash -c "cd /code && cabal clean && yesod devel"

if [ $? -ne 0 ]; then
  docker rm vmxmiddle-current
  docker run -t -i  -p 3000:3000 --name vmxmiddle-current -v `pwd`:/code --link db:db  -v /dockerscratch/sessions:/www/vmx/sessions --volumes-from vmxmodels --volumes-from vmxserver --volumes-from vmxdata   gdoteof/d-vmxmiddle  /bin/bash -c "cd /code && cabal clean && yesod devel"
  fi
