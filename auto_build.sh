#Make sure the branch is set, this is something done in node-git-fish first
if [ -z "$VMXMIDDLE_BRANCH" ]
then
  echo "VMXMIDDLE_BRANCH environment variable must be set"
  exit 1;
fi

TARBALL=middle.linux-${VMXMIDDLE_BRANCH}.tar.gz

rm $TARBALL

#Build the Haskell application using Docker
./cabal_build.sh
EC=$?

if [ "$EC" != "0" ];
then
    echo "Cabal build failed"
    exit 1
fi

#Make the scratch directory which will contain the tarball
mkdir -p scratch/config

cp dist/build/middle/middle scratch/vmx
cp config/settings.yml config/favicon.ico config/robots.txt scratch/config

PLATFORM="Linux"
HASH=${PLATFORM}_`git describe --tags`
BUILD_NAME="VMXmiddle_"$HASH
echo "build name is" $BUILD_NAME

echo $BUILD_NAME > scratch/version





tar cfzv $TARBALL  -C scratch .



scp $TARBALL root@files.vision.ai:/usr/share/nginx/html/releases/middle
