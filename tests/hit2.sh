#!/bin/sh
#Get all session ids
SIDS=`curl -s http://localhost:3000/session | jq -r '.data[].session'`

#IMAGES=`find /www/vvv2/ -name "*jpg" | grep data_set | head -10`
#IMAGES=`find /Users/tomasz/Pictures/ -name "*.jpg"`
IMAGES=`find /Users/tomasz/projects/pascal/VOC2011/ -name "*.jpg"`
for IMAGE in $IMAGES; do
    for j in $SIDS; do
        SID=$j
        echo SID is $SID
        echo "Image is " $IMAGE
        #Issue a process image request for the current session
        json=`curl -s -X POST -A "pid:"$$ -H "Content-Type: application/json" -d '{"images":[{"image":"'$IMAGE'","time":"2014-09-22T06:31:34.447Z"}],"params":{"crop_radius":80,"crop_threshold":-1,"display_threshold":-1,"jpeg_quality":1,"remove_smooth_below_threshold":true,"display_top_detection":false,"max_windows":10,"learn_mode":false,"learn_iterations":10,"learn_threshold":0,"detect_max_overlap":0.3,"learn_max_positives":1,"detect_add_flip":false,"levels_per_octave":10,"max_image_size":320,"initialize_max_cells":12,"cell_size":8,"initialize_add_flip":true}}' http://localhost:3000/session/$SID`
        r0=`echo $json | jq '.objects[0].bb[0]'`
        r1=`echo $json | jq '.objects[0].bb[1]'`
        r2=`echo $json | jq '.objects[0].bb[2]'`
        r3=`echo $json | jq '.objects[0].bb[3]'`
        r0=`echo "console.log(Math.round("$r0"))" | node`
        r1=`echo "console.log(Math.round("$r1"))" | node`
        r2=`echo "console.log(Math.round("$r2"))" | node`
        r3=`echo "console.log(Math.round("$r3"))" | node`
       
        convert $IMAGE -fill none -stroke green -strokewidth 10 -draw "rectangle "$r0","$r1","$r2","$r3"" /tmp/out.jpg
        open /tmp/out.jpg
        #rstring='rectangle 10,10,100,100'
        #rstring='rectangle '+Math.round(bb[0])+','+Math.round(bb[1])+' '+Math.round(bb[2])+','+Math.round(bb[3]);
        
        #im.convert([image_file_name, '-fill', 'none','-stroke','green','-strokewidth','10','-draw',rstring,'out/'+image_file_name],
        #echo 'bb is' `jq '.' $json`
    done
done
