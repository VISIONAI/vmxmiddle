#!/bin/sh
#Get all session ids
SIDS=`curl -s http://localhost:3000/session | jq -r '.data[].session'`

IMAGE='/VMXdata/simple.jpg'
#IMAGE='http://people.csail.mit.edu/tomasz/img/tomasz_blue_crop.jpg'
for i in `seq 1 1000000`; do
    for j in $SIDS; do
        SID=$j
        echo SID is $SID
        #Issue a process image request for the current session
        curl -s -X POST -A "pid:"$$ -H "Content-Type: application/json" -d '{"images":[{"image":"'$IMAGE'","time":"2014-09-22T06:31:34.447Z"}],"params":{"crop_radius":80,"crop_threshold":-1,"display_threshold":-1,"jpeg_quality":1,"remove_smooth_below_threshold":true,"display_top_detection":false,"max_windows":10,"learn_mode":false,"learn_iterations":10,"learn_threshold":0,"detect_max_overlap":0.3,"learn_max_positives":1,"detect_add_flip":false,"levels_per_octave":10,"max_image_size":320,"initialize_max_cells":12,"cell_size":8,"initialize_add_flip":true}}' http://localhost:3000/session/$SID
    done
done
