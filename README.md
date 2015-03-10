VMXmiddle is the REST API layer between VMXServer and VMXAppBuilder.
Because it is sandwiched between the algorithmic back-end and the GUI
front-end, it is called "middle"

## Devel instructions for Linux
```
./yesod_devel.sh
```

## Devel instructions for Mac

First, make sure you have a correct config/settings.yml file

```
yesod devel
```

## Build instructions for Linux

To build using Docker:
```
./cabal_build.sh
```

As part of the automated build, the following script is used
```
auto_build.sh
```

However, `auto_build.sh` requires the branch to be set, so it cannot
be ran manually. auto_build will generate a file called: 
`middle.linux-${VMXMIDDLE_BRANCH}.tar.gz` and scp it to
`files.vision.ai:/usr/share/nginx/html/releases/middle`


## Build instructions for Mac OS X

The Mac OS X build requires a slightly different config file (see
settings.yml.mac)

```
./mac_builder/compile.sh
```

This will generate a file called `VMXmiddle_${PLATFORM}_${GITTAG}` and
scp it to `files.vision.ai:/www/vmx/VMXmiddle/${PLATFORM}/`


NOTES:

cabal sandbox init

.cabal/config must contain this extra line
extra-lib-dirs:/opt/local/lib/


## Related Repositories

Look at VMXserver and VMXAppBuilder

Copyright vision.ai 2013-2015
