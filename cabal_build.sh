docker pull gdoteof/d-vmxmiddle
docker run -v `pwd`:/code --rm gdoteof/d-vmxmiddle /bin/bash -c "cd /code && cabal clean && cabal configure && cabal build"

USER=`whoami`
GROUP=`groups | awk '{print($1)}'`

chown -R ${USER}:${GROUP} dist
