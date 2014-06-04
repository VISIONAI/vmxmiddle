// The GridTracker, PointGrid, and utility functions for maintaining and tracking grids over canvas objects

/*global Detection */
/*global jsfeat */
/*global dat */
/*global _ */
/*global profiler */

// a JsTracker object which reads frames from the canvas and then applies the grid tracker.  Uses jsfeat.js for the underlying technology, but makes it easier to use inside other applications.
// Inputs
//     enable_timer:   indicates whether the timer/profiling object is created
//     enable_gui:     indicates whether the datGUI object is create

(function(window){
  
  window.GridTracker = function(enable_timer,enable_gui) {
    "use strict";
    
    // a function for drawing a circle on the canvas
  function draw_circle(ctx, x, y, r) {
    ctx.beginPath();
    ctx.arc(x, y, r | 4, 0, Math.PI*2, true); 
    ctx.closePath();
    ctx.fill();
  }

  function sqr(x) {
    return x*x;
  }

  // Function to compute the mean and standard deviation on an an array in one pass
  function std_dev2(a) {
    var n = a.length;
    var result = {};
    result.std = 1;
    result.mean = 0;
    if (n === 0) {
      return result;
    }
    var sum = 0;
    var sq_sum = 0;
    for (var i = 0; i < n; ++i) {
      sum += a[i];
      sq_sum += a[i] * a[i];
    }
    var mean = sum / n;
    var variance = sq_sum / n - mean * mean;
    
    result.std = Math.sqrt(variance);
    result.mean = mean;
    return result;
  }
    
  // Estimates the rigid xform between these two sets of points (x,y) and (x2,y2)
  function rigid_xform(allx,ally,allx2,ally2) {
    var x_1 = std_dev2(allx);
    var x_2 = std_dev2(allx2);
    
    var y_1 = std_dev2(ally);
    var y_2 = std_dev2(ally2);
    
    var s_1 = x_2.std / x_1.std;
    var s_2 = y_2.std / y_1.std;
    
    // average the scales to keep the aspect ratio fixed
    s_1 = (s_1 + s_2)/2;
    s_2 = s_1;
    
    // if there aren't enough points, keep the scales fixed
    if (allx.length < 4) {
      s_1 = 1;
      s_2 = 1;
    }
    
    // do not allow the scale to get too big or too small too fast
    var max_cap = 1.05;
    var min_cap = 1/max_cap;
    
    if (s_1>max_cap) {
      s_1 = max_cap;
    }
    if (s_2>max_cap) {
      s_2 = max_cap;
    }
    if (s_1<min_cap) {
      s_1 = min_cap;
    }
    if (s_2<min_cap) {
      s_2 = min_cap;
    }
    
    var dx = x_2.mean - x_1.mean;
    var dy = y_2.mean - y_1.mean;
    
    var res = {};
    res.x_1 = x_1;
    res.y_1 = y_1;
    res.x_2 = x_2;
    res.y_2 = y_2;
    res.s_1 = s_1;
    res.s_2 = s_2;
    res.dx = dx;
    res.dy = dy;
    
    return res;
  }


  // Generate a Grid of Points inside detection d with spacing NX/NY and
  // timestamp of time
  function PointGrid(d,NX,NY) {

    // the bounding box we are tracking
    this.bb = d;
    
    this.time_history = [];
    this.point_history = [];
    
    // initial iteration (lifespan of grid)
    this.iteration = 1;
    this.max_size = NX*NY;
    
    // create the initial and last frame points
    this.point_count = NX*NY;
    this.point_status = new Uint8Array(this.max_size);
    this.prev_xy = new Float32Array(this.max_size*2);
    this.curr_xy = new Float32Array(this.max_size*2);
    
    this.grid_points = new Array(NX);

    var i,j;
    for (i = 0; i < NX; ++i) {
      this.grid_points[i] = new Array(NY);                            
    }
        
    var minx = d.x1;
    var miny = d.y1;
    var maxx = d.x2;
    var maxy = d.y2;
    
    var osx = (maxx - minx) / (NX - 1);
    var osy = (maxy - miny) / (NY - 1);
    
    if (NX === 1) {
      osx = 0;
    }
    
    if (NY === 1) {
      osy = 0;
    }
    
    var counter = 0;
    var offset = 0;
    for (i = 0; i < NX; ++i) {
      for (j = 0; j <NY; ++j) {
        var x = minx + osx*i;
        var y = miny + osy*j;        
        this.curr_xy[offset] = x;
        this.curr_xy[offset+1] = y;
        offset = offset + 2;
        
        this.grid_points[i][j] = {};
        this.grid_points[i][j].index = counter;
        this.grid_points[i][j].error = 0;
        this.grid_points[i][j].num_good = 1;
        this.grid_points[i][j].is_valid = 1;
        
        counter = counter + 1;
        
      }
    }
    
    // Moves a box according to the last grid movement.
    // Finds grid points from the last iteration and sees where they moved in the current iteration, then fits a rigid fixed-aspect ratio model that best explains their movement.
    // NOTE: this function has lots of memory allocations and is quite slow for what it is doing
    this.move_bb = function(input_box) {

      var det = input_box;
      if (this.time_history.length === 1) {
        //console.log('no time history, not updating box');
        return det;
      }
      var allx = [];
      var ally = [];
      var allx2 = [];
      var ally2 = [];

      var inside = true;

      //var sum_dx = 0;
      //var num_dx = 0;
      //var sum_dy = 0;
      //var num_dy = 0;
      var a,b,i,x,y,x2,y2;
      for (a = 0; a < this.grid_points.length; ++a) {
        for (b = 0; b < this.grid_points[a].length; ++b) {
          i = this.grid_points[a][b].index;
          x = this.prev_xy[2*i];
          y = this.prev_xy[2*i+1];

          x2 = this.curr_xy[2*i];
          y2 = this.curr_xy[2*i+1];

          if (det) {
            inside = input_box.inside(x,y);
          }
          if (!inside) {
            // || this.point_status[i]==0)// && this.grid_points[a][b].is_valid == 0 || this.point_status[i]==0)
            continue;
          }

          //num_dx++;
          //num_dy++;
          //sum_dx = sum_dx + (x2-x);
          //sum_dy = sum_dy + (y2-y);
          
          allx.push(x);
          ally.push(y);

          allx2.push(x2);
          ally2.push(y2);

        }
      }

      //sum_dx = sum_dx / (num_dx + 0.00000001);
      //sum_dy = sum_dy / (num_dy + 0.00000001);

      var xform = rigid_xform(allx,ally,allx2,ally2);

      // for each coordinate, subtract mean, scale, add mean, add offset
      det.x1 = xform.s_1*(det.x1 - xform.x_1.mean) + xform.x_1.mean + xform.dx;
      det.x2 = xform.s_1*(det.x2 - xform.x_1.mean) + xform.x_1.mean + xform.dx;

      det.y1 = xform.s_2*(det.y1 - xform.y_1.mean) + xform.y_1.mean + xform.dy;
      det.y2 = xform.s_2*(det.y2 - xform.y_1.mean) + xform.y_1.mean + xform.dy;

      if (allx.length < 3) {
        console.log('WARNING: only',allx.length,'grid points inside detection');
      }
        
      
      return new Detection(det.cls,det.score,det.x1,det.y1,det.x2,det.y2,det.fill,det.color,det.time);
    }; // end move_bb

    // takes a detection box and propagates it from an old grid
    // (indexed by timestamp det_time), onto the current time
    this.propagate_det = function(det, det_time) {

      //var newdet = angular.copy(det);
      // Find the matching time stamp from the past
      var time_diffs = this.time_history.map(function(x){return Math.abs(x - det_time);});      
      var min_value = Math.min.apply({},time_diffs);
      var target_index = time_diffs.indexOf(min_value);

      // Issue a warning if the time stamps do not match, because I spent a lot of work making sure that they will always match... The only way they can not match is if the history is not long enough
      if (min_value > 0) {
        //NOTE: time stamps will not match when the frames are going to be dupes of each other
        //return null;
        //console.log('WARNING:time_stamps do not match inside propagate_det');
        // if (target_index === 0) {
        //   console.log('WARNING: time history is not long enough');
        // }
      }
      
      var old_pts = this.point_history[target_index];
      var new_pts = this.point_history[this.point_history.length-1];

      var allx = [];
      var ally = [];
      var allx2 = [];
      var ally2 = [];

      for (var i = 0; i < this.grid_points.length; ++i) {
        for (var j = 0; j < this.grid_points[i].length; ++j) {
          var c = this.grid_points[i][j].index;

          var x = old_pts[c*2];
          var y = old_pts[c*2+1];

          var x2 = new_pts[c*2];
          var y2 = new_pts[c*2+1];

          if (det.inside(x,y)) {
            allx.push(x);
            ally.push(y);
            allx2.push(x2);
            ally2.push(y2);
          }

          
          // if (x > det.x1 && x < det.x2 && y > det.y1 && y < det.y2) {
          //   this.grid_points[i][j].is_valid = 1;
          // } else {
          //   this.grid_points[i][j].is_valid = 0;
          // }
          
        }
      }

      var xform = rigid_xform(allx,ally,allx2,ally2);
      
      // for each coordinate, subtract mean, scale, add mean, add offset
      det.x1 = xform.s_1*(det.x1 - xform.x_1.mean) + xform.x_1.mean + xform.dx;
      det.x2 = xform.s_1*(det.x2 - xform.x_1.mean) + xform.x_1.mean + xform.dx;

      det.y1 = xform.s_2*(det.y1 - xform.y_1.mean) + xform.y_1.mean + xform.dy;
      det.y2 = xform.s_2*(det.y2 - xform.y_1.mean) + xform.y_1.mean + xform.dy;
      
      var nd = new Detection(det.cls,det.score,det.x1,det.y1,det.x2,det.y2,det.fill,det.color,det.time);
      nd.line_width = det.line_width;
      return nd;
    }; // end propagate_det
    
    // move around current/previous points and get ready for tracking
    this.pre_track = function() {
      var _pt_xy = this.prev_xy;
      this.prev_xy = this.curr_xy;
      this.curr_xy = _pt_xy;
    }; // end pre_track
    
    this.add_to_history = function(current_time) {
      //console.log('adding to history ',current_time);
      // save the history of the points
      this.time_history.push(current_time);
      var vvv = new Array(this.curr_xy.length);
      for (var q = 0; q < this.curr_xy.length; ++q) {
        vvv[q] = this.curr_xy[q];
      }
      this.point_history.push(vvv.slice(0));
      
      if (this.time_history.length > max_history_length) {
        this.time_history.pop();
        this.point_history.pop();
        // this.time_history = this.time_history.splice(50);
        // this.point_history = this.point_history.splice(50);
      }
    }; // end add_to_history
    
    // Increment each point's counter of the frequency of being valid
    this.increment_valid_counter = function() {
      var max_good = -1;
      var i,j,id;
      for (i = 0; i < this.grid_points.length; ++i) {
        for (j = 0; j < this.grid_points[i].length; ++j) {
          //var x = (i/this.grid_points.length);
          //var y = (j/this.grid_points.length);
          id = this.grid_points[i][j].index;
          if (this.point_status[id] === 1 && this.grid_points[i][j].is_valid === 1) { // && (i < .2 || i > .8) && (j < .2 || j > .8)) {          
            this.grid_points[i][j].num_good++;
          }
          
          // if (isNaN(this.grid_points[i][j].num_good))
          //   debugger;
          
          if (this.grid_points[i][j].num_good > max_good) {
            max_good = this.grid_points[i][j].num_good;
          }
          
        }
      }
      
      this.iteration = this.iteration + 1;
      //console.log('iteration/max_good is ',this.global_iteration,max_good);
      return max_good;
    }; // end increment_valid_counter
    
    
    // Function to fit an affinte transformation to the deformations of the grid
    this.learn_xform = function() {
      if (this.point_count === 0) {
        return;
      }
      var affine_kernel = new jsfeat.motion_model.affine2d();
      var affine_transform = new jsfeat.matrix_t(3, 3, jsfeat.F32_t | jsfeat.C1_t);
      var from = [];
      var to = [];
      
      var valid_count = 0;
      var i,j,id = 0;
      for (i = 0; i < this.grid_points.length; ++i) {
        for (j = 0; j < this.grid_points[i].length; ++j) {
          id = this.grid_points[i][j].index;
          //var x = (i/this.grid_points.length);
          //var y = (j/this.grid_points.length);
          if (this.point_status[id] === 1 && this.grid_points[i][j].is_valid === 1) { // && (i < .2 || i > .8) && (j < .2 || j > .8)) {
            from[valid_count] = { "x":this.prev_xy[id*2], "y":this.prev_xy[id*2+1] };
            to[valid_count] = { "x":this.curr_xy[id*2], "y":this.curr_xy[id*2+1] };
            valid_count++;
          }
        }
      }
      
      if (valid_count < 3) {
        return;
      }
      
      // for(var i = 0; i < this.point_count; ++i) {
      //   // you can use point2d_t structure
      //   // or just provide object with x and y properties
      //   from[counter] = { "x":this.prev_xy[i*2], "y":this.prev_xy[i*2+1] };
      //   to[counter] = { "x":this.curr_xy[i*2], "y":this.curr_xy[i*2+1] };
      //   counter++;
      // }
      affine_kernel.run(from, to, affine_transform, valid_count);
      
      // you can also calculate transform error for each point
      //var error = new jsfeat.matrix_t(this.point_count, 1, jsfeat.F32_t | jsfeat.C1_t);
      //affine_kernel.error(from, to, affine_transform, error.data, valid_count);
      
      // var counter = 0;
      // for (var i = 0; i < this.grid_points.length; ++i) {
      //   for (var j = 0; j < this.grid_points[i].length; ++j) {
      //     this.grid_points[i][j].error = error.data[counter];
      //     counter++;
      //   }
      // }
      
      //error_object.error = error;
      var m = affine_transform.data;
      if (isNaN(m[0])) {
        console.log('isnan in xform estimation');
        return;
      }
      
      return m;
      
    }; // end learn_xform
    

    // learn a rigid transformation
    this.learn_rigid = function(det) {
      if (this.point_count === 0) {
        return;
      }
      
      var from = [];
      var to = [];
      
      var valid_count = 0;
      var id = 0;
      var is_inside;
      for (var i = 0; i < this.grid_points.length; ++i) {
        for (var j = 0; j < this.grid_points[i].length; ++j) {
          id = this.grid_points[i][j].index;
          //var x = (i/this.grid_points.length);
          //var y = (j/this.grid_points.length);

          is_inside = true;
          if (det) {
            is_inside = det.inside(this.prev_xy[id*2],this.prev_xy[id*2+1]);
            if (is_inside) {
              this.grid_points[i][j].is_valid = 1;
            }
          }

          if (is_inside) { // && (i < .2 || i > .8) && (j < .2 || j > .8)) {
            from[valid_count] = { "x":this.prev_xy[id*2], "y":this.prev_xy[id*2+1] };
            to[valid_count] = { "x":this.curr_xy[id*2], "y":this.curr_xy[id*2+1] };
            valid_count++;
          }
        }
      }
      
      console.log('vc is ',valid_count);
      if (valid_count < 3) {
        return;
      }
      
      var x1 = this.std_dev2(from.map(function(a){return a.x;}));
      var x2 = this.std_dev2(to.map(function(a){return a.x;}));
      
      var y1 = this.std_dev2(from.map(function(a){return a.y;}));
      var y2 = this.std_dev2(to.map(function(a){return a.y;}));
      
      var m = new Array(9);
      m[0] = x2.std / x1.std;
      m[1] = 0;
      m[2] = x2.mean - m[0]*x1.mean;
      
      
      m[3] = 0;
      m[4] = y2.std / y1.std;
      m[5] = y2.mean - m[4]*y1.mean;
      
      m[6] = 0;
      m[7] = 0;
      m[8] = 1;
      
      
      //error_object.error = error;
      
      if (isNaN(m[0])) {
        console.log('isnan in xform estimation');
        return;
      }
      
      return m;
      
    }; // end learn_rigid
    
    
    this.xform_errors = function() {
      
      //var m = this.learn_xform();
      var m = this.learn_rigid();
      if (!m) {
        return;
      }
      var i,j,c,err;
      for (i = 0; i < this.grid_points.length; ++i) {
        for (j = 0; j < this.grid_points[i].length; ++j) {
          c = this.grid_points[i][j].index;    
          
          err = sqr(this.curr_xy[2*c] - m[0]*this.prev_xy[2*c] - m[1]*this.prev_xy[2*c+1] - m[2]) +
            sqr(this.curr_xy[2*c+1] - m[3]*this.prev_xy[2*c] - m[4]*this.prev_xy[2*c+1] - m[5]);
          this.grid_points[i][j].error = Math.sqrt(err);
        }
      }
      return m;
    }; // end xform_errors
    
    // here we apply the learned transformation "xform" onto the previous points
    this.apply_xform_to_current = function(m) {
      
      if (!m) {
        return;
      }

      var i,x,y,oldx,oldy,newx,newy,displacement;
      for (i = 0; i < this.point_count; ++i) {
        x = this.prev_xy[i*2];
        y = this.prev_xy[i*2+1];
        
        oldx = this.curr_xy[i*2];
        oldy = this.curr_xy[i*2+1];
        
        newx = m[0]*x + m[1]*y + m[2];
        newy = m[3]*x + m[4]*y + m[5];
        
        displacement = Math.sqrt((x-newx)*(x-newx)+(y-newy)*(y-newy));
        //if (displacement > max_displacement) { 
        this.curr_xy[i*2] = newx;
        this.curr_xy[i*2+1] = newy;
        //}
        // }
        //this.curr_xy[i*2] = (this.curr_xy[i*2]+x)/2;
        //this.curr_xy[i*2+1] = (this.curr_xy[i*2+1]+y)/2;
        
        //}
        // if (point_status[i] == 1) {
        //   this.curr_xy[i*2] = oldx;
        //   this.curr_xy[i*2+1] = oldy;
        //   continue;
        // }
        
        // continue;
        
        // var diff1 = (this.curr_xy[i*2]-oldx);
        // diff1 = diff1*diff1;
        
        // var diff2 = (this.curr_xy[i*2+1]-oldy);
        // diff2 = diff2*diff2;
        
        // if (diff1 > 9)
        //   this.curr_xy[i*2] = oldx;//(this.curr_xy[i*2] + oldx)/2;
        
        // if (diff2 > 9)
        //   this.curr_xy[i*2+1] = oldy;//(this.curr_xy[i*2+1] + oldy)/2;
        
        // // var z = m[6]*x + m[7]*y + m[8];
        // // this.curr_xy[i*2] = (m[0]*x + m[1]*y + m[2])/z;
        // // this.curr_xy[i*2+1] = (m[3]*x + m[4]*y + m[5]/z);   
        
      }
      
    }; // end apply_xform_to_current
    
        
    this.draw_grid = function(ctx,params) {
      if(!this.grid_points || !this.grid_points[0]) {
        return;
      }
      
      var i,j,c,c2;      
      
      if (params.show_grid) {
        ctx.beginPath();
        for (i = 0; i < this.grid_points.length; ++i) {
          for (j = 0; j < this.grid_points[i].length-1; ++j) {
            c = this.grid_points[i][j].index;
            c2 = this.grid_points[i][j+1].index;
            ctx.moveTo(this.curr_xy[c*2], this.curr_xy[c*2+1]);
            ctx.lineTo(this.curr_xy[c2*2], this.curr_xy[c2*2+1]);
          }
        }
        
        for (i = 0; i < this.grid_points.length-1; ++i) {
          for (j = 0; j < this.grid_points[i].length; ++j) {
            c = this.grid_points[i][j].index;
            c2 = this.grid_points[i+1][j].index;            
            ctx.moveTo(this.curr_xy[c*2], this.curr_xy[c*2+1]);
            ctx.lineTo(this.curr_xy[c2*2], this.curr_xy[c2*2+1]);
          }
        }
        
        ctx.closePath();
        ctx.stroke();
      }// end if (params.show_grid)
      
      var valid_frequency;
      for (i = 0; i < this.grid_points.length; ++i) {
        for (j = 0; j < this.grid_points[i].length; ++j) {
          c = this.grid_points[i][j].index;
          
          if (this.point_status[c] === 0 && this.show_only_valid === 1) {
            continue;
          }
                    
          var rad = 0.2;
          
          if (this.grid_points[i][j].is_valid ) {
            valid_frequency = Math.round(this.grid_points[i][j].num_good / this.iteration * 255);
            ctx.fillStyle = "rgb(0,0,"+valid_frequency+")";
            //ctx.fillStyle = "rgb(0,0,255)";
            draw_circle(ctx, this.curr_xy[c*2], this.curr_xy[c*2+1],rad);
          } else {
            valid_frequency = 0;
            rad = 3;
            ctx.fillStyle = "rgb(255,0,0)";
            draw_circle(ctx, this.curr_xy[c*2], this.curr_xy[c*2+1],rad);
          }
          
          
          if (params.show_affine_error) {
            var e = this.grid_points[i][j].error;
            var error_max = 5;
            if (e > error_max) {
              e = error_max;
            }
            if (e < 0) {
              e = 0;
            }
            e = Math.round(e / error_max * 255);
            
            ctx.fillStyle = "rgb("+(e)+","+(255-e)+","+0+")";
            draw_circle(ctx, this.curr_xy[c*2], this.curr_xy[c*2+1],4);
          }
          
          // draw the velocity vector here
          if (params.show_velocity && this.time_history.length > 1) {            
            var dx = this.curr_xy[c*2] - this.prev_xy[c*2];
            var dy = this.curr_xy[c*2+1] - this.prev_xy[c*2+1];
            var distance = Math.sqrt(dx*dx+dy*dy);
            if (distance > 10) {
              distance = 10;
            }
            distance = distance / 10;
            ctx.strokeStyle = "rgb("+distance+","+(1-distance)+",0)";
            ctx.beginPath();
            ctx.moveTo(this.curr_xy[c*2], this.curr_xy[c*2+1]);
            ctx.lineTo(this.curr_xy[c*2]+dx*3, this.curr_xy[c*2+1]+dy*3);
            ctx.closePath();
            ctx.stroke();
          }
        }
      }
    }; // end draw_grid
    
    // Fix flow such that no point moves too much, if it does cap its movement to max_displacement away from the original position according to the new trajectory
    //NOTE(TJM): max_displacement can be described in terms of fractions.. so that it is not resolution dependent 
    this.fix_large_flow = function(max_displacement) {
      
      if (!this.grid_points || this.grid_points.length === 0) {
        return;
      }
      
      // grid is too small to be fixed
      if (this.grid_points.length === 1 || this.grid_points[0].length === 1) {
        return;
      }
      
      //console.log('fix_large_flow #max_displacement=',max_displacement);
      var num_capped = 0;
      var i,j,c,x,y,xold,yold,displacement;
      for (i = 0; i < this.grid_points.length; ++i) {
        for (j = 0; j < this.grid_points[i].length; ++j) {
          c = this.grid_points[i][j].index;
          x = this.curr_xy[c*2];
          y = this.curr_xy[c*2+1];
          
          xold = this.prev_xy[c*2];
          yold = this.prev_xy[c*2+1];
          
          displacement = Math.sqrt((x-xold)*(x-xold)+(y-yold)*(y-yold));
          if (displacement > max_displacement) {
            var vx = x-xold;
            var vy = y-yold;
            vx = vx / displacement * max_displacement;
            vy = vy / displacement * max_displacement;
            this.curr_xy[c*2] = xold + vx;
            this.curr_xy[c*2+1] = yold + vy;
            num_capped++;
            //console.log('mx is',displacement);
          }
          
          
        } // end for j
      } // end for i
      //console.log('# of capped points: ',num_capped);
    }; //end fix_large_flow
    
    // fix the flow by doing global inference.. not really working as intended
    // this.fix_flow_inference = function(niter) {
      
    //   if (!this.grid_points || this.grid_points.length == 0) {
    //     return;
    //   }
      
    //   // grid is too small to be fixed
    //   if (this.grid_points.length == 1 || this.grid_points[0].length == 1) {
    //     return;
    //   }
      
    //   console.log('fix_flow_inference #iter=',niter);
    //   //console.log('start: first x is ',this.curr_xy[20]);
      
    //   var lambda = .01;
    //   for (var iter = 0; iter < niter; ++iter) {
    //     var new_curr_xy = new Float32Array(this.curr_xy.length);
    //     for (var q = 0; q < this.curr_xy.length; ++q)
    //       new_curr_xy[q] = this.curr_xy[q];
        
    //     var xdiffs = 0;
    //     var ydiffs = 0;
    //     for (var i = 0; i < this.grid_points.length; ++i) {
    //       for (var j = 0; j < this.grid_points[i].length; ++j) {
    //         var c = this.grid_points[i][j].index;
    //         if (this.point_status[c] == 0) {
    //           var allx = 0;
    //           var ally = 0;
    //           var numx = 0;
    //           var numy = 0;
              
    //           // prediction from left point
    //           if (i == 0) { // take right points and predict left point
    //             allx+= 2*this.curr_xy[this.grid_points[i][j].index*2] -
    //               this.curr_xy[this.grid_points[i+1][j].index*2];
    //             numx++;              
    //           } else { // left neighbor is valid
    //             allx += this.curr_xy[this.grid_points[i-1][j].index*2];
    //             numx++;
    //           } 
              
    //           // prediction from right point
    //           if (i == (this.grid_points.length-1)) { // take left points and predict right point
    //             allx += 2*this.curr_xy[this.grid_points[i][j].index*2] -
    //               this.curr_xy[this.grid_points[i-1][j].index*2];
    //             numx++;              
                
    //           } else { // right neighbor is valid
    //             allx += this.curr_xy[this.grid_points[i+1][j].index*2];
    //             numx++;
    //           }
              
    //           /// now do top and bottom updates
              
    //           // prediction from top
    //           if (j == 0) { // take bottom points and predict top
    //             ally += 2*this.curr_xy[this.grid_points[i][j].index*2+1] -
    //               this.curr_xy[this.grid_points[i][j+1].index*2+1];
    //             numy++;              
    //           } else { // top point is valid
    //             ally += this.curr_xy[this.grid_points[i][j-1].index*2+1];
    //             numy++;
    //           } 
              
    //           // prediction from bottom point
    //           if (j == (this.grid_points[i].length-1)) { // take top points and predict top
    //             ally += 2*this.curr_xy[this.grid_points[i][j].index*2+1] -
    //               this.curr_xy[this.grid_points[i][j-1].index*2+1];
    //             numy++;              
                
    //           } else { // bottom point is valid
    //             ally += this.curr_xy[this.grid_points[i][j+1].index*2+1];
    //             numy++;
    //           }
              
              
    //           var newx = allx / numx;
    //           var newy = ally / numy;
              
    //           var xd = (newx - this.curr_xy[this.grid_points[i][j].index*2]);
    //           var yd = (newy - this.curr_xy[this.grid_points[i][j].index*2+1]);
    //           xdiffs += xd*xd;
    //           ydiffs += yd*yd;
    //           // if (this.iteration > 10) {
    //           //   console.log('xd,yd is ',xd,yd);
    //           //   var a=  123123123;
    //           //   }
    //           new_curr_xy[this.grid_points[i][j].index*2] = allx;
    //           new_curr_xy[this.grid_points[i][j].index*2+1] = ally;
    //         }
    //       } // end for j
    //     } // end for i
    //     //console.log('x is ',this.curr_xy[10]);
    //     for (var q = 0; q < new_curr_xy.length; ++q)
    //       this.curr_xy[q] = (lambda*new_curr_xy[q] + this.curr_xy[q]) / (1+4*lambda);
    //   } // end niter
    //   //console.log('end: first x is ',this.curr_xy[20]);
    //   console.log('xd is ',xdiffs,ydiffs);
    // } //end fix_flow_inference
    
    // this.self_update = function() {    
      
    //   var affine_kernel = new jsfeat.motion_model.affine2d();
    //   var affine_transform = new jsfeat.matrix_t(3, 3, jsfeat.F32_t | jsfeat.C1_t);
    //   var from = [];
    //   var to = [];
      
    //   var id = 0;
    //   var valid_count = 0;
    //   for (var i = 0; i < this.grid_points.length; ++i)
    //     for (var j = 0; j < this.grid_points[i].length; ++j) {
    //       index = this.grid_points[i][j].index;
          
    //       if (this.grid_points[i][j].error < 2) {
    //         from[valid_count] = { "x":i, "y":j};
    //         to[valid_count] = { "x":this.curr_xy[index*2], "y":this.curr_xy[index*2+1] };
    //         valid_count++;
    //       }
    //     }
      
    //   affine_kernel.run(from, to, affine_transform, valid_count);
      
      
    //   // // create a brand new grid of points
    //   // if (!this.grid_points.length) {
    //   // }
      
    //   m = affine_transform.data;
      
    //   if (isNaN(m[0])) {
    //     return;
    //   }
      
    //   //this.point_count = 0;
    //   var counter = 0;
    //   for (var i = 0; i < this.grid_points.length; ++i) {
    //     for (var j = 0; j <this.grid_points[i].length; ++j) {
          
    //       if (this.grid_points[i][j].error >=2) {
    //         var x = m[0]*i + m[1]*j + m[2];
    //         var y = m[3]*i + m[4]*j + m[5];
    //         this.curr_xy[2*counter] = x;
    //         this.curr_xy[2*counter+1] = y;
    //         //this.point_count = this.point_count + 2;
    //         //this.grid_points[i][j].index = counter;
    //         //this.grid_points[i][j].error = 0;
    //         //this.grid_points[i][j].num_good = 1;
    //       }
    //       counter++;
    //     }
    //   }
    //   //this.point_count = this.point_count / 2;
      

    //   //this.bb = new Detection('face',1,minx,miny,maxx,maxy,'rgb(128,0,128)','rgb(250,0,0)',0);
      
    // }// end self_update
    
    
    
  }//end PointGrid

  // the box requests, which let the tracker know how much in the past history it has to save
  var requests = [];

  //The maximum life span of a grid
  var max_history_length  = 100;

  var grids               = [];
  var old_grids           = [];
  
  // An array of bounding boxes, which are currently being tracked by the tracker
  var boxes               = [];
    
  var all_prev_xy         = [];
  var all_curr_xy         = [];
  var all_point_count     = 0;
  var all_point_status    = [];
  var all_grid_identity   = [];
  var all_grid_index      = [];
  
  var stat = null;
  
  var params = {};
  // Create the initial parameters
  params.win_size             = 15;
  params.max_iterations       = 3;
  params.epsilon              = 0.01;
  params.min_eigen            = 0.001;
  params.NX                   = 20;
  params.NY                   = 20;
  params.skip                 = 10;
  params.show_velocity        = false;
  params.show_valid_frequency = true;
  params.show_affine_error    = false;
  params.show_grid            = false;
  params.show_only_valid      = false;
  params.apply_xform          = false;
  params.max_displacement     = 100; // maximum displacement of pixels
  params.show_boxes           = true;
  params.heart_beat           = 0;
  params.require_detections   = true; // if enabled, require there to be a box
  params.fps                  = '';
  params.enable_grid          = true;

  // initialize the pyramids
  var curr_img_pyr = new jsfeat.pyramid_t(3);
  var prev_img_pyr = new jsfeat.pyramid_t(3);

  var lastImage = null;
  
  var gui = null;

  var tracker_canvas = null;
  
  var height = null;
  var width = null;

  var ctx = null;

  var global_iteration = 0;  
  
  
  // initialize the tracker given image width/height and the tracker_canvas
  function initialize_canvas(image_width, image_height, t_canvas) {
    console.log('GridTracker initializing');
    // An array of grids
    
    if (enable_timer) {
      // Create the profiler object which will keep track of times
      stat = new profiler();
      stat.add("grayscale_time");
      stat.add("image_pyramid_time");
      stat.add("optical_flow_time");
    } else {
      stat = null;
    }

    if (stat) {
      for (var q = 0; q < stat.timers.length; ++q) {
        var pair = stat.timers[q];
        params[pair[0]] = pair[1].get_runtime() + "ms";
      }
    }
    
  
    if (enable_gui) {
      // create a GUI for modifying the parameters
      gui = new dat.GUI(); 
      // start the GUI in a closed state
      gui.closed = true;
      var f1 = gui.addFolder('Tracker Parameters');
      // f1.add(text, 'speed');
      // f1.add(text, 'noiseStrength');

      f1.add(params, 'win_size'            , 7    , 100).step(1);
      f1.add(params, 'max_iterations'      , 3    , 30).step(1);
      f1.add(params, 'epsilon'             , 0.001, 0.1).step(0.0025);
      f1.add(params, 'min_eigen'           , 0.001, 0.01).step(0.0025);
      f1.add(params, 'NX'                  , 1    , 100).step(1);
      f1.add(params, 'NY'                  , 1    , 100).step(1);
      f1.add(params, 'skip'                , 3    , 100).step(1);

      var f2 = gui.addFolder('Display Parameters');
      f2.add(params, 'show_velocity');
      f2.add(params, 'show_valid_frequency');
      f2.add(params, 'show_affine_error');
      f2.add(params, 'show_only_valid');
      f2.add(params, 'apply_xform');
      f2.add(params, 'max_displacement'    , 0    , 300).step(1);
      f2.add(params, 'show_boxes');
      f2.add(params, 'heart_beat'          , 0    , 100).step(1);
      f2.add(params, 'require_detections');
      //f2.open();
      gui.add(params, 'show_grid');
      gui.add(params, 'require_detections');
      gui.add(params, 'enable_grid');
      var f3 = gui.addFolder('Timing');
      if (stat) {
        f3.add(params, 'fps').listen();
        for (var z = 0; z < stat.timers.length; ++z) {
          var _pair = stat.timers[z];
          f3.add(params,_pair[0]).listen();
        }
      }
      
      
    } else {
      gui = null;
    }

    tracker_canvas      = t_canvas;

    // the width/height of the image we are processing
    height = image_height;
    width  = image_width;

    ctx             = tracker_canvas.getContext('2d');
    ctx.fillStyle   = "rgb(0,255,0)";
    ctx.strokeStyle = "rgb(255,0,0)";

    // initialize the pyramids
    curr_img_pyr.allocate(width, height, jsfeat.U8_t|jsfeat.C1_t);
    prev_img_pyr.allocate(width, height, jsfeat.U8_t|jsfeat.C1_t);

  } // end initialize function

  // update the dimensions, which might happen if the canvas was resized
  this.set_dims = function(width, height) {
    //console.log('setting dims to ',width,height);
    if (width !== this.width || height !== this.height) {
      this.width = width;
      this.height = height;

      delete this.curr_img_pyr;
      delete this.prev_img_pyr;

      this.curr_img_pyr = new jsfeat.pyramid_t(3);
      this.prev_img_pyr = new jsfeat.pyramid_t(3);

      this.curr_img_pyr.allocate(this.width, this.height, jsfeat.U8_t|jsfeat.C1_t);
      this.prev_img_pyr.allocate(this.width, this.height, jsfeat.U8_t|jsfeat.C1_t);
    }

  }; // end set_dims

  // initialize the all* data fields to the new size
  // NOTE: this could be done without resizing/deleting if we are goign to use just one grid
  function resize_all(newsize) {
    if (newsize === all_point_count) {
      return;
    }
    all_prev_xy       = new Float32Array(newsize*2);
    all_curr_xy       = new Float32Array(newsize*2);
    all_point_count   = newsize;
    all_point_status  = new Uint8Array(newsize);
    all_grid_identity = new Array(newsize);
    all_grid_index    = new Array(newsize);
  }
  
  // merge all PointGrids into one point grid, such that the jsfeat
  // tracker can process the points in one shot
  function merge_all() {
    var counter = 0;
    for (var i = 0; i < grids.length; ++i) {
      for (var j = 0; j < grids[i].point_count; ++j) {
        all_prev_xy[2*counter]     = grids[i].prev_xy[2*j];
        all_prev_xy[2*counter+1]   = grids[i].prev_xy[2*j+1];

        all_curr_xy[2*counter]     = grids[i].curr_xy[2*j];
        all_curr_xy[2*counter+1]   = grids[i].curr_xy[2*j+1];

        all_point_status[counter]  = grids[i].point_status[j];
        all_grid_identity[counter] = i;
        all_grid_index[counter]    = j;
        counter++;
      }
    }
  }
  
  // unmerge points from the one global track to the individual little tracks
  function unmerge_all() {
    for (var counter = 0; counter < all_point_count; ++counter) {
      var i = all_grid_identity[counter];
      var j = all_grid_index[counter];
      grids[i].prev_xy[2*j]    = all_prev_xy[2*counter];
      grids[i].prev_xy[2*j+1]  = all_prev_xy[2*counter+1];
      grids[i].curr_xy[2*j]    = all_curr_xy[2*counter];
      grids[i].curr_xy[2*j+1]  = all_curr_xy[2*counter+1];
      grids[i].point_status[j] = all_point_status[counter];
    }
  }

  // consider using _.isEqual instead
  function array_is_same(a,b) {
    for (var i = 0; i < a.length; ++i) {
      if (a[i] !== b[i]) {
        return false;
      }
    }
    return true;
  }

  // The main tracking function, which will take the image contained
  // inside imageData and the current time, and update all of the
  // grids and boxes accordingly
  function tracker_update(imageData, current_time) {
    if(!params.enable_grid) { return; }
    
    // if (!this.counter)
    //   this.counter = 0;
    // console.log(this.counter++);
    //Here we skip images which are identical to the last frame
    //NOTE: consider using _.isEqual
    if (lastImage && array_is_same(lastImage.data, imageData.data)) {
      //console.log('same image, so skipping');
      return;
    } else {
      //console.log('diff image');
    }
  
    lastImage = imageData;

    if (params.require_detections === true && boxes.length === 0) {
      return;
    }

    // Do nothing if no grids are present
    if (grids.length === 0) {
      return;
    }

    // If profiler is enabled, start tracking now
    if (stat) {
      stat.new_frame();
    }
    
    // swap image pointers
    var _pyr = prev_img_pyr;
    prev_img_pyr = curr_img_pyr;
    curr_img_pyr = _pyr;

    var i;

    // Convert to grayscale
    if (stat) {
      stat.start("grayscale_time");
    }
    jsfeat.imgproc.grayscale(imageData.data, curr_img_pyr.data[0].data);
    if (stat) {
      stat.stop("grayscale_time");
    }
    
    // Build the image pyramid
    if (stat) {
      stat.start("image_pyramid_time");
    }
    curr_img_pyr.build(curr_img_pyr.data[0], true);
    if (stat) {
      stat.stop("image_pyramid_time");
    }
    
    // Perform optical flow tracking
    if (stat) {
      stat.start("optical_flow_time");
    }
  
    // Get grids ready for tracking
    for (i = 0; i < grids.length; ++i) {
      grids[i].pre_track();      
    }

    // Merge all grids into a single data structure before processing
    merge_all();
    
    // Run the main jsfeat optical flow tracker
    jsfeat.optical_flow_lk.track(prev_img_pyr, curr_img_pyr, all_prev_xy, all_curr_xy,all_point_count, params.win_size|0, params.max_iterations|0, all_point_status, params.epsilon, params.min_eigen);

    // Unmerge all points back to original grids
    unmerge_all();  

    for (i = 0; i < grids.length; ++i) {
      // if apply_xform is enabled, update the point movement based on
      // a transformation
      if (params.apply_xform) {
        var m = grids[i].xform_errors();
        grids[i].fix_large_flow(params.max_displacement);
        grids[i].apply_xform_to_current(m, params.max_displacement);
      }
      // Update the point history
      grids[i].add_to_history(current_time);
    }

    if (stat) {
      stat.stop("optical_flow_time");
    }
    
    // now we move over each of our bounding boxes according to the
    // grid movement
    for (i = 0; i < boxes.length; ++i) {
      boxes[i] = move_box(boxes[i]);
    }

    // re-initialize grid
    if (grids[0].time_history.length === max_history_length || global_iteration % params.heart_beat === 0) {
     initialize_grid(current_time);
    }

    if (stat) {
      //if (gui) {
      params.fps = stat.log();
      for (var q = 0; q < stat.timers.length; ++q) {
        var pair = stat.timers[q];
        params[pair[0]] = pair[1].get_runtime() + "ms";
      }
      //}
      //$('#log').html(stat.log());
    }

    // update the global iteration
    global_iteration = global_iteration + 1;
  }

  // Move the current detection box based on the grid movement
  // NOTE: only works with grids[0]
  function move_box(box) {
    return grids[0].move_bb(box);
  }

  // Function to draw all of the grids and boxes on the tracker canvas
  function draw_all() {

    // clear the canvas and prepare for showing
    ctx.clearRect(0, 0, tracker_canvas.width, tracker_canvas.height);

    if(! params.enable_grid ) { return; }

    // if nothing to show, do no more work
    if (params.show_grid === 0 && params.show_boxes === 0) {
      return;
    }

    // determine scaling parameters from the active image size
    // (width/height) to the display size
    ctx.save();
    ctx.scale(tracker_canvas.width / width,
              tracker_canvas.height / height);

    var i;
    // if show_grid is enabled, show the grids
    if (params.show_grid) {
      for (i = 0; i < grids.length; ++i) {
        grids[i].draw_grid(ctx, params);
      }
    }

    // if show_boxes is enabled, show the boxes
    if (params.show_boxes) {
      for (i = 0; i < boxes.length; ++i) {
        boxes[i].line_width = 1;
        boxes[i].draw(ctx);
      }
    }
    ctx.restore();
  }
    
  // Notify the grid tracker of a ground-truth location of an object
  // at a given time. This could also be the location used by a strong
  // tracker
  // NOTE: only works with grids[0]
  function add_bb(det, det_time) {
    //console.log('add bb starting cls=',det.cls,'time is ',det_time);

    var newdet = det;

    // if there are no grids, do not add the bb
    if (grids.length === 0) {
      console.log('WARNING: not adding bb because there are no grids');
      return;
    }

    // here we remove old detection with the same cls tag
    boxes = boxes.filter(function(a) {return a.cls !== det.cls;} );

    var nd = {};
    // if there is no detection time, pretend it was given in the last
    // frame
    if (!det_time) {
      det_time = grids[0].time_history[grids[0].time_history.length-1];
    } else {
      // if we have a detection time, then we have to propagate the
      // det from the past until the current time

      // pop current grid to list
      old_grids.push(grids[0]);
      nd = new Detection(newdet.cls,newdet.score,newdet.bb[0],newdet.bb[1],newdet.bb[2],newdet.bb[3]);
      nd.line_width = 1;
      newdet = propagate(nd, det_time);
      
      if (newdet === null) {
        console.log('error, not found in history');
      }
      
      // remove current grid from history list
      //old_grids = old_grids.slice(old_grids.length-1,1);
      old_grids.pop();
      

    }

    // if the detection is in a different format, then lets convert to
    // a "Detection" struct
    if (!newdet.x1) {
      try {
        nd = new Detection(newdet.cls,newdet.score,newdet.bb[0],newdet.bb[1],newdet.bb[2],newdet.bb[3]);
        nd.line_width = 1;
        boxes.push(nd);
      } catch (errski) {
        console.log(errski);
      }
    }
    else {
      boxes.push(nd);
    }

    // now remove the request
    var remove_id = null;
    for (var i = 0; i < requests.length; ++i) {
      if (requests[i].cls === newdet.cls && requests[i].time === det_time) {
        remove_id = i;
        break;
      }
    }
    if (remove_id !== null) {
      //console.log('removing',requests[remove_id].cls,'/',requests[remove_id].time);
      requests.splice(remove_id,1);
    } else {
      console.log('Warning no request to remove');
    }
    
  } // end add_bb

  //Propagate the detection through the history grid
  function propagate(det,det_time) {
    
    var j,curdet,latest_time;

    for (j = old_grids.length-1; j >= 0; j--) {
      //console.log('looking through index ',j);
      curdet = old_grids[j].propagate_det(det,det_time);
      if (curdet !== null) {
        // we found a match
        if (j === old_grids.length-1) {
          return curdet;
        }
        latest_time = old_grids[j].time_history[old_grids[j].time_history.length-1];
        return propagate(curdet,latest_time);
      }
    }
    
    

    return curdet;

  }
    

  // Add a new grid to the grid tracker, given the detection time det_time
  // NOTE: if we want smart grid initialization based on the last grid, then do it here
  function initialize_grid(det_time) {

    // if we have one grid, append it to history
    if (grids.length === 1) {
      old_grids.push(grids[0]);

      // Make sure we have at most 100 old_grids present in memory
      //if (old_grids.length > max_old_grids) {

        //old_grids = old_grids.splice(max_old_grids);

      // get min request time
      var min_time = _.min(requests.map(function(x){return x.time;}));
      
      // only keep grids whose last time stamp is ahead of min_time
      old_grids = old_grids.filter(function(x){return x.time_history[x.time_history.length-1]>min_time;});

      //}
      
    } else if (grids.length === 0) {

    } else {
      throw "Too many grids";
    }
    // remove current grid
    grids.splice(0,1);
    
    // create a new box equal to the size of the video canvas
    var d = new Detection('', 1,
                          params.win_size, params.win_size,
                          width - params.win_size,
                          height - params.win_size);

    // add the new grid
    grids.push(new PointGrid(d, params.NX, params.NY));

    // update the time history of the new grid
    grids[grids.length-1].add_to_history(det_time);

    // update the sizes of some scratch variables
    resize_all(params.NX*params.NY*grids.length);
  }

  // Add a request at a specified time, which lets the tracker know
  // not to delete so far into the past's history as to remove this
  // time stamp
  function add_request(cls,current_time) {
    //console.log('adding request: cls',cls,'time',current_time);
    requests.push({cls:cls,time:current_time});
  }

  // Removes the bounding box from the update queue
  function remove_bb(det) {
    //console.log('remove bb called');
    try {
      boxes = boxes.filter(function(x){return x.cls!==det.cls;});
    }
    catch (er) {
      console.log('WARNING: could not remove box');
    }
    // always re-initialize grid when detections go away
    if (boxes.length === 0) {
      initialize_grid(0);
    }
  }

  function get_current_bb(name){
    var scaleX = tracker_canvas.width  /  width;
    var scaleY = tracker_canvas.height / height;
    for(var i = 0; i < boxes.length; ++i){
      if(boxes[i].cls === name){
        var box = boxes[i];
        var ret = {
          x:  box.x  * scaleX,
          y:  box.y  * scaleY,
          x1: box.x1 * scaleX,
          y1: box.y1 * scaleY,
          x2: box.x2 * scaleX,
          y2: box.y2 * scaleY,
          trackerWidth  :  tracker_canvas.width,
          trackerHeight : tracker_canvas.height,
        };
        return ret;
      }
    }
    return null;
  }

  return {
    initialize_canvas: initialize_canvas,
    initialize_grid: initialize_grid,
    tracker_update: tracker_update,
    draw_all: draw_all,
    add_bb: add_bb,
    remove_bb: remove_bb,
    get_bb: get_current_bb,
    add_request: add_request
  };
    
}; // end GridTracker
})(window);
