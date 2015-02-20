RUNNING=$(docker ps | awk '{print $NF}' | grep 'vmxmiddle-current$' | wc -l)
docker stop vmx
docker rm   vmx



docker run -t -i  -p 3000:3000 --link db_middle:db --name vmxmiddle-current -v `pwd`:/code  -v /dockerscratch/sessions:/vmx/sessions -v /dockerscratch/models:/vmx/models --volumes-from vmxserver --volumes-from vmxdata   --rm gdoteof/d-vmxmiddle  /bin/bash -c "cd /code && yesod devel"

if [ $? -ne 0 ]; then
  docker stop vmxmiddle-current
  docker rm vmxmiddle-current
  docker run -t -i  -p 3000:3000 --link db_middle:db --name vmxmiddle-current -v `pwd`:/code   -v /dockerscratch/sessions:/vmx/sessions -v /dockerscratch/models:/vmx/models  --volumes-from vmxserver --volumes-from vmxdata  --rm gdoteof/d-vmxmiddle  /bin/bash -c "cd /code && yesod devel"
  fi

chown -R g:g yesod-devel
chown -R g:g dist
