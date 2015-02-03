./cabal_build.sh
mkdir -p scratch/config

cp dist/build/middle/middle scratch/vmx
cp config/settings.yml config/favicon.ico config/robots.txt scratch/config

BUILD_NAME=`git describe --tags`
echo build name is $BUILD_NAME
echo -n $BUILD_NAME > scratch/version


if [ -z "$VMXMIDDLE_BRANCH" ]
then
  echo "VMXMIDDLE_BRANCH environment variable must be set"
  exit 1;
fi

TARBALL=middle.linux-${VMXMIDDLE_BRANCH}.tar.gz

tar cfzv $TARBALL  -C scratch .



scp $TARBALL root@files.vision.ai:/usr/share/nginx/html/releases/middle
