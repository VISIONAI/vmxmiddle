docker run -t -i  -v `pwd`:/code --link db:db  -v /dockerscratch/sessions:/vmx/sessions --volumes-from vmxmodels --volumes-from vmxserver --volumes-from vmxdata   --rm gdoteof/d-vmxmiddle  /bin/bash -c "cd /code && cabal clean && cabal configure && cabal build"

chown -R g:g yesod-devel
chown -R g:g dist
