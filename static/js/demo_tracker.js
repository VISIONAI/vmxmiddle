/*global Detection */
/*global GridTracker */
/*global compatibility */
var num_boxes = 0;

//  the grid initialization function which takes a click from the mouse
function on_canvas_click(e) {    
  console.log('Canvas clicked, adding new grid');
  
  var coords = {};
  coords.x = this.width/2;
  coords.y = this.height/2;
  
  //get the x,y location from the mouse click event...
  if (e) {
    if (e.toElement) {
      coords = e.toElement.relMouseCoords(e);
    }
    else if (e.target) {
      coords = e.target.relMouseCoords(e);
    }
    else {
      console.log('ERROR: cannot get relMouseCoords from click');
    }
  }
  
  if (!this.first_click_coords) {
    this.first_click_coords = coords;
    console.log('first click');
    return;
  }
  
  if (!this.second_click_coords) {
    this.second_click_coords = coords;
    console.log('second click');
  }
  
  num_boxes++;
  
  var x1 = Math.min(this.first_click_coords.x,this.second_click_coords.x);
  var x2 = Math.max(this.first_click_coords.x,this.second_click_coords.x);
  var y1 = Math.min(this.first_click_coords.y,this.second_click_coords.y);
  var y2 = Math.max(this.first_click_coords.y,this.second_click_coords.y);
  
  var d = new Detection(num_boxes,0,
                        x1,y1,x2,y2);
  
  this.add_bb(d);
  
  this.first_click_coords = null;
  this.second_click_coords = null;
}



$(window).load(function() {
  "use strict";
  
  // lets do some fun
  var video = document.getElementById('webcam');
  var video_canvas = document.getElementById('video_canvas');

  // Define the global Grid Tracker
  var global_tracker = new GridTracker(true,true);

  try {
    compatibility.getUserMedia({video: true}, function(stream) {
      try {
        video.src = compatibility.URL.createObjectURL(stream);
      } catch (error) {
        console.log("there was an errorrr????");
        video.src = stream;
      }
      setTimeout(function() {
        video.play();
        
        var imageData = null;
        
        var ctx = video_canvas.getContext('2d');
        var canvas_drawer = function() {
          compatibility.requestAnimationFrame(canvas_drawer);
          var width  = video_canvas.width;
          var height = video_canvas.height;
          
          //ctx.clearRect(0, 0, width, height);
          if (video.readyState === video.HAVE_ENOUGH_DATA) {
            ctx.save();
            ctx.translate(width,0);
            ctx.scale(-1,1);
            var current_time = new Date().getTime();
            ctx.drawImage(video, 0, 0, width, height);
            ctx.restore();
            var current_time2 = new Date().getTime();
            current_time = Math.round((current_time + current_time2)/2);
            imageData = ctx.getImageData(0, 0, width, height);
            global_tracker.tracker_update(imageData, current_time);
            global_tracker.draw_all(); 
          }
        };

        compatibility.requestAnimationFrame(canvas_drawer);
        
        var tracker_canvas = document.getElementById('tracker_canvas');
        global_tracker.initialize_canvas(video_canvas.width,video_canvas.height,tracker_canvas);
        
        var newdet = {};
        newdet.bb = [];
        newdet.bb[0] = 0;
        newdet.bb[1] = 0;
        newdet.bb[2] = video_canvas.width;
        newdet.bb[3] = video_canvas.height;
        newdet.cls = '';
        newdet.score = 1;
        
        var current_time = 0;
        global_tracker.initialize_grid(current_time);
        
        tracker_canvas.addEventListener('click', on_canvas_click.bind(global_tracker), false);
      }, 500);
    }, function () {
      $('#canvas').hide();
      $('#log').hide();
      $('#no_rtc').html('<h4>WebRTC not available.</h4>');
      $('#no_rtc').show();
    });
  } catch (error) {
    $('#canvas').hide();
    $('#log').hide();
    $('#no_rtc').html('<h4>Something goes wrong...</h4>');
    $('#no_rtc').show();
  }
  
  $(window).unload(function() {
    video.pause();
    video.src=null;
  });
});


function relMouseCoords(event) {
  var totalOffsetX=0,totalOffsetY=0,canvasX=0,canvasY=0;
  var currentElement = this;
  do {
    totalOffsetX += currentElement.offsetLeft - currentElement.scrollLeft;
    totalOffsetY += currentElement.offsetTop - currentElement.scrollTop;
  } while(currentElement = currentElement.offsetParent);
  
  canvasX = event.pageX - totalOffsetX;
  canvasY = event.pageY - totalOffsetY;
  
  return {x:canvasX, y:canvasY};
}

HTMLCanvasElement.prototype.relMouseCoords = relMouseCoords;
// convert an imageData object to a dataurl
