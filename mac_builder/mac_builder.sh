
#here we
echo 'Welcome to Mac Builder'

original='/Users/tomasz/projects/vmxmiddle/dist/build/middle/middle'

D='/Users/tomasz/projects/vmxmiddle/dist/VMX.app'
rm -rf $D
mkdir $D

mkdir $D/Contents
mkdir $D/Contents/MacOS/
mkdir $D/Contents/Resources
mkdir $D/Contents/Frameworks

touch $D/Contents/Info.plist
touch $D/Contents/Resources/something.icns

cp $original $D/Contents/MacOS/vmx

F=$D/Contents/MacOS/vmx

#strip binary
strip $F


mkdir $D/Contents/MacOS/static
mkdir $D/Contents/MacOS/static/dist/
mkdir $D/Contents/MacOS/static/fonts/
mkdir $D/Contents/MacOS/static/enter_license/
cp static/index.html $D/Contents/MacOS/static/
cp static/dist/* $D/Contents/MacOS/static/dist/
cp static/fonts/* $D/Contents/MacOS/static/fonts/

cp -R static/enter_license/dist $D/Contents/MacOS/static/enter_license/

mkdir $D/Contents/MacOS/config/
cp config/settings.yml $D/Contents/MacOS/config/

#copy over VMXserver
cp -R /Users/tomasz/projects/VMXserver/build $D/Contents/MacOS/


#clean up libs
LIBS=`otool -L ${F} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`

for i in $LIBS; do
    echo "i is" $i
    LOCAL_LIB='@executable_path/../Frameworks/'`basename $i`
    echo "LL is" $LOCAL_LIB
    install_name_tool -change $i $LOCAL_LIB $F
    cp ${i} $D/Contents/Frameworks/
    LIBS2=`otool -L ${i} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
    for j in $LIBS2; do
        echo "i j is " $i $j
        cp ${j} $D/Contents/Frameworks/
        LIBS3=`otool -L ${j} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
        for k in $LIBS3; do
            echo "j k is " $j $k
            cp ${k} $D/Contents/Frameworks/
            LIBS4=`otool -L ${k} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
            for l in $LIBS4; do
                echo "k l is " $k $l
                cp ${l} $D/Contents/Frameworks/
                LIBS5=`otool -L ${l} | grep "\t" | grep "/opt/local/lib" | awk '{print($1)}'`
                for m in $LIBS5; do
                    echo "l m is " $l $m
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



