0.0.2

VMXmiddle is the REST API layer between VMXServer and VMXAppBuilder,
because it is sandwiched between the algorithmic back-end and the GUI
front-end, it is called "middle"

## Build instructions using docker:


``` shell

## build the images which will hold the volumes needed for the backend
git clone git@github.com:gdoteof/vmx-dockers.git

# build the image which holds vmxserver
docker build -t vmx-server vmx-dockers/volumes/docker-vmx-server
docker run -d --name vmxserver vmx-server

# build the image which contains the init data for vmxserver
docker build -t vmx-data vmx-dockers/volumes/docker-vmx-data
docker run -d --name vmxdata vmx-data
```


For holding session and model data, there are two different options:
  - use a container to hold the data for you (recommended for models, because we generally want models to be persistent)
  - use a local mount to hold the data  (recommended for sessions, since it allows us to more easily inspect logs, and we don't care about yesterday's sessions during development


To build local container for models
```shell
docker build -t vmx-models vmx-dockers/volumes/docker-vmx-models
docker run -d --name vmxmodels
```


Now, tie it all together using gdoteof/d-vmxmiddle (which contains no sensitive information, just the dependencies)


``` shell

#make sure you have a copy of the vmxmiddle code
git clone git@github.com:gdoteof/vmxmiddle.git

cd vmxmiddle
git submodule update --init --recursive

## this command assumes we are in the approot of vmxmiddle 

docker run -t -i \  # run in interactive mode and keep terminal open so we can view output and get file-watching
-p 3000:3000     \  # expose port 3000 of the docker to the host 3000 so we can interact
--volumes-from vmxdata   \  # attach the init data
--volumes-from vmxserver \  # attach the actual server
--volumes-from vmxmodels \  # add the vmxmodels container volumes to store model information
-v /dockerscratch/vmx/sessions:/www/vmx/sessions   \ # create a mountpoint from the host at /dockerscratch/vmx/sessions which will be linked to /www/vmx/sessions in the container
-v `pwd`:/code \ # load the vmxmiddle source into /code in the docker container
--link db:db \   # make the container, called `db` (left side of colon) which is running the postgres database, available to the vmxmiddle container at host tcp://db (rightside of colon) 
gdoteof/d-vmxmiddle \ # the docker imaqe which contains the dependencies for vmxmiddle
/bin/bash -c "cd /code && yesod devel"  # inside the docker, move to the approot and yesod devel
```

## Build instructions for Mac OS X Build
```
sudo runghc Setup configure --user
sudo runghc Setup build
```

or simply
```
./build_mac.sh
./build_dmg.sh
```
