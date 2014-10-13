echo 'Welcome to Mac Builder of VMX Middle'

if [ `uname` == "Darwin" ]; then
    echo "Inside Mac"
    PLATFORM="Mac"
else
    #inside virtual box
    echo "Inside Virtual Box Linux Environment"    
    PLATFORM="Linux"
fi

BRANCH_NAME=`git branch | grep "*" | awk '{print($2)}'`
if [ $BRANCH_NAME == "master" ]; then
    BRANCH_NAME=""
else
    BRANCH_NAME=$BRANCH_NAME"-"
fi
DATER=`date "+%Y-%m-%d"`
HASH=`git --no-pager log --format='%h' -n 1`


cabal clean && cabal configure && cabal build

original='/Users/tomasz/projects/vmxmiddle/dist/build/middle/middle'

D='/Users/tomasz/projects/vmxmiddle/dist/VMX.app'
rm -rf $D
mkdir $D

mkdir $D/Contents
mkdir $D/Contents/MacOS/
mkdir $D/Contents/Resources
mkdir $D/Contents/Frameworks

cp ~/projects/vmxmiddle/mac_builder/Info.plist $D/Contents/Info.plist
cp ~/projects/vmxmiddle/mac_builder/run.sh $D/Contents/MacOS

cp ~/projects/VMXassets/vmxicon2.icns $D/Contents/Resources/VMX.icns

cp $original $D/Contents/MacOS/VMX
F=$D/Contents/MacOS/VMX

#strip binary
strip $F

mkdir $D/Contents/MacOS/assets/
mkdir $D/Contents/MacOS/assets/sessions/
mkdir $D/Contents/MacOS/assets/models/

mkdir $D/Contents/MacOS/static
mkdir $D/Contents/MacOS/static/dist/
mkdir $D/Contents/MacOS/static/fonts/
mkdir $D/Contents/MacOS/static/enter_license/
cp static/index.html $D/Contents/MacOS/static/
cp static/dist/* $D/Contents/MacOS/static/dist/
cp static/fonts/* $D/Contents/MacOS/static/fonts/

cp -R static/enter_license/dist $D/Contents/MacOS/static/enter_license/

mkdir $D/Contents/MacOS/config/
cp ~/projects/vmxmiddle/mac_builder/settings.yml $D/Contents/MacOS/config/

#copy over VMXserver from the build directory
cp -R /Users/tomasz/projects/VMXserver/build/VMXserver.app $D/Contents/MacOS/

#copy over initial network
#cp /VMXdata/99* $D/Contents/MacOS/build/VMXdata/

#clean up libs
LIBS=`otool -L ${F} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`

for i in $LIBS; do
    echo "i is" $i
    LOCAL_LIB='@executable_path/../Frameworks/'`basename $i`
    #echo "LL is" $LOCAL_LIB
    echo install_name_tool -change $i $LOCAL_LIB $F
    install_name_tool -change $i $LOCAL_LIB $F
    cp ${i} $D/Contents/Frameworks/
    LIBS2=`otool -L ${i} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
    for j in $LIBS2; do
        #echo "i j is " $i $j
        cp ${j} $D/Contents/Frameworks/
        LIBS3=`otool -L ${j} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
        for k in $LIBS3; do
            #echo "j k is " $j $k
            cp ${k} $D/Contents/Frameworks/
            LIBS4=`otool -L ${k} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
            for l in $LIBS4; do
                #echo "k l is " $k $l
                cp ${l} $D/Contents/Frameworks/
                LIBS5=`otool -L ${l} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
                for m in $LIBS5; do
                    #echo "l m is " $l $m
                    cp ${m} $D/Contents/Frameworks/
                done

                
            done
            
        done
    done
done

### go over all libs and replace /opt/local/bin with @executable_path\/..\/Frameworks

LIBS=`find $D/Contents/Frameworks/ -type f`
for i in $LIBS; do
    LIBS2=`otool -L ${i} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
    for j in $LIBS2; do
        #LOCAL_LIB='./'`basename $j`
        LOCAL_LIB='@executable_path/../Frameworks/'`basename $j`
        install_name_tool -change $j $LOCAL_LIB $i
    done    
done

#update local id of library
cd $D/Contents/Frameworks/
LIBS=`ls`
for i in $LIBS; do
    install_name_tool -id $i $i
done
cd -

echo "VMXmiddle_"$PLATFORM"_"$DATER"_"$BRANCH_NAME$HASH > $D/version

#cp ~/projects/cvmx/VMX $D/Contents/MacOS

