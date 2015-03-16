cd /root/repos/vmxmiddle_${VMXMIDDLE_BRANCH}
git fetch
git checkout $VMXMIDDLE_BRANCH
git pull

echo "Auto build starting..."
./auto_build.sh
EC=$?


if [ "$EC" != "0" ];
then
    echo "Cabal build failed"
    #NOTE: this still sends a bad one off
    curl --data "build=false" -X POST https://registry.hub.docker.com/u/visionai/vmx-middle/trigger/5997c65a-6b65-11e4-9a92-b6e30c63109a/

else
    echo "Auto build finishes..."
    echo Triggering build on docker hub..
    ##Trigger a rebuild on dockerhub
    curl --data "build=true" -X POST https://registry.hub.docker.com/u/visionai/vmx-middle/trigger/5997c65a-6b65-11e4-9a92-b6e30c63109a/

fi

echo "Completed docker hub ping"
