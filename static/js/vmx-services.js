/* globals VMX: true, YESOD_SERVER: true,  Detection: true, GridTracker: true*/
var vmxServices;
vmxServices = angular.module('vmx.services',['ng','ui.bootstrap'])
  .factory("vmxapi", function($modal,vmxutils, vmxglobals){
    var magicCanvas, magicContext;
    var intervalHandler;
    var attach_api = function(){
      if (VMX.config.useMagicCanvas) {
        magic_canvas_modal();
        VMX.getMagicCanvas = function(){ 
          if(magicCanvas){ 
            return magicCanvas;
            }
          else { 
            magicCanvas = document.getElementById('magic-canvas');
          }
        };
        VMX.getMagicContext = function(){ if(magicContext) { return magicContext; } else { magicContext = magicCanvas.getContext('2d'); }};
      }

      VMX.getSnapshot = function() { 
        return vmxutils.extract_image(vmxglobals.g.last_image);
      };

      intervalHandler = window.setInterval(cron,500);
    };

    function cron(){
      if(typeof VMX.cron !== 'function'){
        window.clearInterval(intervalHandler);
        return;
      }
      VMX.cron();
    }

    //creates modal for containing magic canvas
    var magic_canvas_modal = function() {
      var  modalInstance = $modal.open({
        templateUrl: 'static/partial/vmxapi-magic-canvas-modal.html',
        windowClass: ['vmx-magic-canvas-modal'],
        controller: function($scope,$modalInstance){
          $scope.cancel = function(){
            $modalInstance.dismiss('cancel');
          };
        },
      });
      modalInstance.result.then(function () {
        //do we need to get any data from the modal?
        //maybe weshoudl reset if they close it...
      });
    };

    var reset_obj = function(){
      VMX = {};
      VMX.config = {};
      VMX.storage = {};
      clear_side_effects();
      magicCanvas = magicContext = null;
    };

    function clear_side_effects(){
      try{
        var magicCanvas = document.getElementById('magic-canvas');
        document.body.removeChild(magicCanvas);
      }catch(e){ 
        // element doesn't exist
      }
    }

    return {
      reset: reset_obj,
      populate: attach_api,
    };
  })
  .factory("vmxgithub", function($q,$http,vmxappcode,vmxapi){
    var errors = [];
    var load_repo = function(repoURL){
      var deferred = $q.defer();  
      var url = "githelper.php?repo=" + encodeURIComponent(repoURL);
      $http.get(url).success(function(response){
        errors = []; //reset errors
        vmxapi.reset();
        for(var filepath in response){
          try{
            /* jshint evil: true */
            eval(response[filepath]);
          }catch(e){
            //console.log("error...",e,e.lineNumber,e.number,e.name,e.message,e.fileName,e.stack);
            errors.push({file:filepath, error: e.stack, line:e.lineNumber});
          }
        }
        if(typeof VMX.callback === 'function'){
          vmxappcode.set_entry_function_direct(VMX.callback);
          vmxappcode.set_execute_flag(true);
        }else{
          console.log(VMX);
        }
        vmxapi.populate(VMX);
        deferred.resolve(true);
      });
      return deferred.promise;
    };
    return {
      load_git_repo: load_repo,
      current_errors: function(){return errors;},
    };
  })
  .factory("vmxipcam", function(){

    var canvas, playing_flag, src;
    var context,img;
    var count = 0;
    function set_canvas(_canvas){
      canvas = _canvas;
      context = canvas.getContext('2d');
      img = document.createElement('img');
      img.src = src;
    }

    function set_source(source){
      src = source;
      if(!img){
        img = document.createElement('img');

      }
      img.src = src;
    }

    function draw_source(){
      if(!playing_flag) {
        return;
      }
      var frame = new Image();
      frame.onload = function() {
                           context.drawImage(frame, 0,0);
                           draw_source();
                       };
      frame.src = src; //+ count.toString();
      ++count;
    }

    function start_drawing(){
      playing_flag = true;
      draw_source();
    }

    function stop_drawing(){
      playing_flag = false;
    }

    return{
        play: start_drawing,
        stop: stop_drawing,
        set_src: set_source,
        set_canvas: set_canvas,
        is_playing: function() { return playing_flag; },
    };
  })

  .factory("vmxui", function(){
    var parameters,detections,editor,response,modeler;
    parameters = detections = editor = response = modeler = false;
    var _response_txt;
    var _detections;
    var param_container = {};
    param_container.response = true;
    return{
      container: param_container,
      toggle_parameters_visible: function(){ return param_container.parameters = parameters = !parameters;  },
      toggle_detections_visible: function(){ return param_container.detections = detections = !detections; },
      toggle_editor_visible: function(){ return param_container.editor = editor = !editor; },
      toggle_response_visible: function(){ return param_container.response = response = !response; },
      toggle_modeler_visible: function() { return param_container.modeler = modeler = !modeler; },
      parameters_visible: function(){ return parameters; },
      detections_visible: function(){ return detections; },
      editor_visible: function(){ return editor; },
      response_visible: function(){ return response; },
      modeler_visible: function(){ return modeler; },
      set_response: function(t){   _response_txt = t;},
      response: function(){ return _response_txt; },
      set_detections: function(detections){ _detections = detections; },
      get_detections: function(){ return _detections; }
    };
  })

  //TODO this should be a provider?  this only allows us a single 'environment' to be running at once
  // maybe we instantiate at a higher level than this anyway?
  // any modules added here will be available within the VMX.callback function
  .factory("vmxappcode", function($http,$q,vmxtracker,vmxglobals){

    var entry_func = null;
    var apps = [];
    var callback_string;  //callback_string is the string version of entry_func
    var current_app;
    var execute_flag = false;

    function set_entry_function(script_string){
        console.log("trying to set entry func");
        callback_string = script_string;
        /*jshint -W054 */
        entry_func= new Function('detections', script_string); 
    }

    function set_entry_as_function(theFunction){
        if(typeof theFunction === 'function'){
          entry_func=theFunction;
        }
    }

    function processRequest(cleanedResponse) {
      /* globals vmxApi */
      var params = {
        detections: cleanedResponse.dets,
      };
      vmxApi.processServerResponse(params);
    }

    vmxApi.fn.getSmooth = function(){
      return vmxtracker.get_bb(this.selector);
    };

    vmxApi.getDimensions = function(){
      return {
        width:  vmxglobals.g.width, 
        height: vmxglobals.g.height, 
      };
    };
    

    return{

      processRequest: processRequest,
      set_entry_function_direct: set_entry_as_function,
      set_execute_flag : function(f) { execute_flag = f },
      get_execute_flag : function() { return execute_flag;},
      get_apps : function(){
        var deferred = $q.defer();  
        $http.get(YESOD_SERVER + 'apps')
          //NOTE this is not error checking
          .success(function(response){
            apps = response;
            deferred.resolve(response);
          })
          .error(function(data,status,headers,config){
            console.log("error");
            console.log([data,status,headers,config]);
            deferred.resolve([]);
          });
        return deferred.promise;
      },

      //Switches to a new app
      set_app : function(index){
        current_app = apps[index];
        set_entry_function(current_app.value.callback);
        return current_app;
      },

      delete_app: function(index){
        var temp = apps[index];
        var deferred = $q.defer();  
        $http.delete(YESOD_SERVER + 'apps/' + temp.key).success(function(response){
          console.log(response);
          deferred.resolve(response);
        });
        return deferred.promise;
      },

      save_app : function(){
        //NOTE we have to manually massage the files to be 'as expected'
        var files = [];
        console.log(current_app);
        for(var i = 0; i < current_app.value.files.length; ++i){
          var tfile = current_app.value.files[i];
          files.push({appfile_name: tfile.name, appfile_body: tfile.body});
        }
        var req = {
          app_name: current_app.value.name,
          app_callback : current_app.value.callback,
          files : files
        };
        $http.post(YESOD_SERVER + 'apps/' + current_app.key,req)
          .success(function(respond){
            console.log("Successful update4??!?");
            console.log(respond);
          })
          .error(function(data,status,headers,config){
            console.log("error trying to save...");
            console.log([data,status,headers,config]);
          });
      },

      new_app : function(name){
        var deferred = $q.defer();  
        var req = {
          app_name: name,
          app_callback: "//This comment should have more info",
          files: [],
          };
        $http.post(YESOD_SERVER + 'apps',req)
          .success(function(respond){
            console.log("Successful made new???");
            console.log(respond);
            apps.push(respond);
            current_app = respond;
            deferred.resolve(current_app);
          })
          .error(function(data,status,headers,config){
            console.log("error trying to makenew......");
            console.log([data,status,headers,config]);
          });
        return deferred.promise;
      },
      /* var files is an array of strings (treated here as 'the contents of a file' 
         all the files in the array are loaded into Scope (not $scope), but are not run 
         for each detection.  

         var entry_func is a function that gets called for each detection with the
         detected objects as an input
      */
      save_file  : function(index,file)   { current_app.value.files[index] = file  },
      get_file   : function(index)        { return current_app.value.files[index]; },
      get_files  : function()            { return current_app.value.files;       },
      delete_file : function(index) { current_app.value.files.splice(index,1); },

      get_callback: function(){
        return current_app.value.callback;
      },

      //script_string is expected to be a string
      set_entry : set_entry_function,
      execute: function(object){
        if(!entry_func || !execute_flag){ return }
        try{
          //Execute the entry function on the detection
          entry_func(object);
        } catch(e) {
          console.log(e);
          console.log(e.stack);
        }
      }
    };
  })

  .factory("vmxglobals", function(){
    var globals = {};
    globals.is_playing     = false;
    globals.width          = 320;
    globals.height         = 240;
    globals.last_image     = [];
    globals.last_time      = [];
    globals.selection_mode = false;

    // add selection div z-index bumping functions, which allow other
    // directives to bump the z-index of the selection div and bring it
    // back to the original z-index after we're done selecting
    var selection_div = null;
    var selection_div_old_zindex = null;


    // create a local canvas so that we don't have to keep creating
    // lots of temporary ones
    var local_canvas = document.createElement('canvas');
    local_canvas.width = globals.width;
    local_canvas.height = globals.height;
    var local_ctx = local_canvas.getContext('2d');

    // Save an image and its timestamp
    function set_image_struct_(vid) {

      local_ctx.save();
      local_ctx.translate(local_canvas.width,0);
      local_ctx.scale(-1,1);

      // Measure time by taking two time stamps, one before the draw
      // from video and one right after the draw from video, and then
      // average them
      var current_time = new Date().getTime();
      local_ctx.drawImage(vid,0,0,local_canvas.width,local_canvas.height);
      var current_time2 = new Date().getTime();

      local_ctx.restore();
      current_time = Math.round((current_time + current_time2)/2);

      //var imageData = local_ctx.getImageData(0, 0, local_canvas.width,local_canvas.height);

      globals.last_time = current_time;
      globals.last_image = local_ctx.getImageData(0, 0, local_canvas.width, local_canvas.height);
      return local_canvas;
    }

    // Save the selection div and its original z-index
    function set_selection_div(cd) {
      selection_div = cd;
      selection_div_old_zindex = selection_div.style.zIndex;
    }

    // turn off selection mode
    function disable_selection_mode() {
      globals.selection_mode = false;
      $("canvas").css('cursor', 'default');
      // bring the canvases to the original location
      selection_div.style.zIndex = selection_div_old_zindex;
    }

    // turn on selection mode
    function enable_selection_mode() {
      globals.selection_mode = true;
      $("canvas").css('cursor', 'crosshair');
      // bring the canvases to front
      selection_div.style.zIndex = 1000; 
    }

    function presser(e) {
      if (e.keyCode === 97) {
        console.log('(a)dd selection');
        enable_selection_mode();
      }
    }
     
    function add_key_listener() {
      window.addEventListener("keypress",presser,false);
    }

    function remove_key_listener() {
      window.removeEventListener("keypress",presser,false);
    }



    return{
      globals                : globals,
      g                      : globals,
      set_image              : set_image_struct_,
      set_selection_div      : set_selection_div,
      disable_selection_mode : disable_selection_mode,
      enable_selection_mode  : enable_selection_mode,
      add_key_listener       : add_key_listener,
      remove_key_listener    : remove_key_listener
    };
  })

  .factory("vmxboxes", function(){
    var boxes = {};
    var add_box = function(connectionId,box){
      if(typeof boxes[connectionId] !== 'object'){
        boxes[connectionId] = [];
      }
      boxes[connectionId].push(box);
    };
    var reset_boxes = function(connectionId){ boxes[connectionId] = []};
    var list_boxes = function(){
      var tmp = [];
      for(var sid in boxes){
        tmp = tmp.concat(boxes[sid]);
      }
      return tmp;
      
    };
    return{
      list: list_boxes,
      reset: reset_boxes,
      add: add_box
    };
  })

.factory("vmxparams",function(VmxParamsProvider){
  var p = new VmxParamsProvider.getInstance();
  return p; 
})
  .factory("VmxParamsProvider", function(){
        
    var VmxParams = function VmxParams(){
      this.makeDefaults();
      this.container = {};
      this.param_cache = {};
    };

    VmxParams.prototype.toggleLearning = function(){
      this.named_params['ilearn'].value         = !this.named_params['ilearn'].value;
      this.named_params['ilearn'].current_value = !this.named_params['ilearn'].current_value;
      this.update_parameters();
      return this.named_params['ilearn'].value;
    };

    VmxParams.prototype.getLearningMode = function(){
      return this.named_params['ilearn'].value;
    };

    VmxParams.prototype.makeDefaults = function(){
      this.ui_params = [{name : "crop_radius",
                        alias : "Detection Crop Percentage",
                        value : 80,
                        current_value : 80,
                        min_value: 0,
                        max_value: 1000,
                        widget: "slider",
                        group: ['det'],
                        learning_mode: false,
                        type: "integer"},
                         
                         {name: "crop_threshold",
                          alias: "Detection Crop Threshold",
                          value: -1,
                          current_value: -1,
                          min_value: -100,
                          max_value: 100,
                          widget: "slider",
                          group: ['hide'],
                          learning_mode: false,
                          type: "float"},
                         
                         {name: "display_threshold",
                          alias: "Detection Display Threshold",
                          value: -1,
                          current_value: -1,
                          min_value: -10,
                          max_value: 10,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "float"},

                         {name: "JPEGQuality",
                          alias: "Image JPEG Quality",
                          value: 1,
                          current_value: 1,
                          min_value: 0.1,
                          max_value: 1,
                          widget: "slider",
                          group: ['hide'],
                         learning_mode: true,
                          type: "float"},

                         {name: "remove_smooth_below_threshold",
                          alias: "Remove smooth box below display threshold",
                          value: true,
                          current_value: true,
                          widget: "checkbox",
                          group: ['det'],
                         learning_mode: true,
                          type: "bool"},

                         {name: "display_top_detection",
                          alias: "Display Top Detection",
                          value: false,
                          current_value: false,
                          widget: "checkbox",
                          group: ['hide'],
                         learning_mode: true,
                          type: "bool"},

                        ];

     this.detect_params = [ 

                         {name : "max_windows",                      
                          alias : "Detector Quality",
                          value : 10,
                          current_value : 10,
                          min_value: 1,
                          max_value: 3000,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "integer"},

                         {name : "ilearn",
                          alias : "Learning Mode",
                          value : false,
                          current_value : false,
                          widget: "checkbox",
                          group: ['hide'],
                         learning_mode: true,
                          type: "bool"},

                         {name : "ilearn_iterations",
                          alias : "# of Learning Updates",
                          value : 10,
                          current_value : 10,
                          min_value: 0,
                          max_value: 1000,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "integer"},

                         {name : "ilearn_threshold",
                          alias : "Learning Update Threshold",
                          value : 0,
                          current_value : 0,
                          min_value: -10,
                          max_value: 10,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "float"},

                         {name : "nms_threshold",
                          alias : "NMS Threshold",
                          value : 0.3,
                          current_value : 0.3,
                          min_value: 0,
                          max_value: 0.7,
                          widget: "slider",
                          group: ['det'],
                          learning_mode: true,
                          type: "float"},


                         {name : "ilearn_max_positives",
                          alias : "# of Learning Positives",
                          value : 1,
                          current_value : 1,
                          min_value: 0,
                          max_value: 100,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "integer"},
                         
                         {name: "detect_add_flip",
                          alias: "Left-Right Image Flip",
                          value: false,
                          current_value: false,
                          widget: "checkbox",
                          group: ['det'],
                         learning_mode: true,
                          type: "bool"},
                         
                         {name: "levels_per_octave",
                          alias: "# of Pyramid Levels",
                          value: 10,
                          current_value: 10,
                          min_value: 1,
                          max_value: 10,
                          widget: "slider",
                          group: ['hide'],
                         learning_mode: true,
                          type: "integer"},

                         {name: "pyramid_padding",
                          alias: "Pyramid Padding",
                          value: 0,
                          current_value: 0,
                          min_value: 0,
                          max_value: 100,
                          widget: "slider",
                          group: ['det'],
                         learning_mode: true,
                          type: "integer"},

                         {name: "max_image_size",
                          alias: "Maximum Image Size",
                          value: 320,
                          current_value: 320,
                          min_value: 100,
                          max_value: 2000,
                          widget: "slider",
                          group: ['hide'],
                         learning_mode: true,
                          type: "integer"},
                        ];
     this.init_params = [
                         {name: "max_template_dim",
                          alias: "Initial Template Max Dimension",
                          value: 12,
                          current_value: 12,
                          min_value: 2,
                          widget: "slider",
                          group: ['init'],
                          max_value: 24,
                         learning_mode: true,
                          type: "integer"},
       
                         {name: "sbin",
                          alias: "sbin",
                          value: 8,
                          current_value: 8,
                          min_value: 2,
                          widget: "slider",
                          group: ['init','det'],
                          max_value: 16,
                         learning_mode: true,
                          type: "integer"},


                         {name: "initialize_add_flip",
                          alias: "Initialize with Left-Right Flips",
                          value: true,
                          current_value: true,
                          widget: "checkbox",
                          group: ['init'],
                         learning_mode: true,
                          type: "bool"}                       
                        ];
      this.named_params        = {};
      this.named_ui_params     = {};
      this.named_detect_params = {};
      this.named_init_params   = {};

      this.generate_named_parameters();
      this.starting_params = angular.copy(this.named_params);
    };

    // return value of param
    VmxParams.prototype.get = function(name){
      return this.named_params[name].value;
    };

    VmxParams.prototype.list_all_params = function(){
      return this.named_params;
    };

    VmxParams.prototype.clean_params = function(p){ 
      var local_params = {};
      for (var i in p) {
        var value = p[i].value;
        if (p[i].type && p[i].type === "bool")  {
          if (value === true) {
            value = 1;
          } else {
            value = 0;
          }
        }
        // local_params[i] = {name  : p[i].name,
        //                    value : value};
        //local_params[i] = {};
        //local_params[i][p[i].name] = value;

        if (this.starting_params[i]!=null) {
          local_params[i] = value;
        }

      }
      return local_params;
    };


  VmxParams.prototype.generate_named_parameters = function() {
    for(var i= 0; i<this.ui_params.length; i++){
      this.named_params[this.ui_params[i].name]     = this.named_ui_params[this.ui_params[i].name]         = this.ui_params[i];
    }
    for(i= 0; i<this.detect_params.length; i++){
      this.named_params[this.detect_params[i].name] = this.named_detect_params[this.detect_params[i].name] = this.detect_params[i];
    }
    for(i= 0; i<this.init_params.length; i++){
      this.named_params[this.init_params[i].name]   = this.named_init_params[this.init_params[i].name]     = this.init_params[i];
    }
  };

  VmxParams.prototype.update_parameters = function() {
    this.update_parameters_one(this.ui_params);
    this.update_parameters_one(this.detect_params);
    this.update_parameters_one(this.init_params);
    this.generate_named_parameters();

    if (this.named_params['ilearn'].value === true) {
      this.named_params['display_top_detection'].value = true;
      this.named_params['display_top_detection'].current_value = true;
      console.log('making dtd true');
    } else {
      this.named_params['display_top_detection'].value = false;
      this.named_params['display_top_detection'].current_value = false;
      console.log('making dtd false');
    }

  };

 // Sanitizing parameters before sending to server
  VmxParams.prototype.update_parameters_one = function(params) {
    for (var i in params) {
      var param = params[i];
      var value = param.current_value;
      
      if (param.type === "float" ||
          param.type === "integer") {
        value = parseFloat(param.current_value);
      }
      
      // if we got an error converting parameter to a number,
      // revert to old value
      if (isNaN(value)) {
        value = param.value;
      }
      
      // for integers, round them
      if (param.type === "integer") {
        value = Math.round(value);
      }
      
      // cap integers and floats to maxes and mins
      if (param.type === "float" ||
          param.type === "integer") {
        
        if (value < param.min_value){
          value = param.min_value;
        }
        
        if (value > param.max_value){
          value = param.max_value;
        }
      }

      if (param.type === "bool") {
        if (value === 1) {
          value = true;
        } else if (value === 0) {
          value = false;
        }
      }

      // set both current_value and true value to value
      param.value = value;
      param.current_value = value;
    }
    return params;
  };
 
 
    return{
      getInstance: function() { 
        var p = new VmxParams(); 
        return p; 
      }
    };
})

  .factory("vmxutils",function(vmxglobals,vmxparams){
    var g = vmxglobals.g;

    //Hacky attempt to check the relative difference between
    //two bounding box in an attempt to determine if this is likely
    //an unmoving detection.
    //We want to smoothe out detections but not make them lag if they are
    //moving.
    //a threshold of ~.35 seems decent form testing
    /*
    var _vmx_shape_moved = function(bb1, bb2, threshold){
      if( threshold === undefined ) { threshold = 0.35; }
      var xd1, xd2, yd1, yd2; 
      var w1,w2,h1,h2;
      var e1,e2;
      xd1 = Math.abs(bb1.x1 - bb2.x1) * 2;
      w1  = bb1.x2 - bb1.x1;
      w2  = bb2.x2 - bb2.x1;
      xd2 = w1 + w2;
      yd1 = Math.abs(bb1.y1 - bb2.y1) * 2;
      h1  = bb1.y2 - bb1.y1;
      h2  = bb2.y2 - bb2.y1;
      yd2 = h1 + h2;
      //yd2 = bb1.h + bb2.h;
      if(xd1 < xd2 && yd1 < yd2){
        //They overlap, so how much have they moved?
        e1 = (xd1) / ( (bb1.w + bb2.w)/2 );
        e2 = (yd1) / ( (bb1.h + bb2.h)/2 );
        if(e1 + e2 > threshold){
          return true;
        } else {
          return false;
        }
      }else{
        //They don't overlap, so it must have moved.
        return true;
      }
    };
    */


    function _vmx_array_difference(a,b,id){ 
    // Make hashtable of ids in B
      var bIds = {};
      b.forEach(function(obj){
          bIds[obj[id]] = obj;
      });

      // Return all elements in A, unless in B
      return a.filter(function(obj){
          return !(obj[id] in bIds);
      });
    }

    var _vmx_clean_dirty_sessionId = function(connectionId){
      if(connectionId.substring(0,9) === 'sessions/') { 
        connectionId = connectionId.substring(9); 
      }
      return connectionId;
    };

    var _unix_to_date = function(UNIX_timestamp){
      var a = new Date(UNIX_timestamp);
      var year = a.getFullYear()-2000;
      var month = a.getMonth()+1;
      var date = a.getDate();
      var hour = a.getHours();
      var amer = 'am';
      if (hour > 12) {
        hour-=12;
        amer = 'pm';
      }
      
      if (hour === 0){
        hour = 12;
      }
      
      var min = a.getMinutes();
      var sec = a.getSeconds();
      
      if (min<10){
        min = '0'+min;
      }
      
      if (sec<10){
        sec = '0'+sec;
      }
      
      var time = month+'/'+date+'/'+year+' '+hour+':'+min+amer;//':'+sec ;
      return time;
    };
    
    
    var _undo_crop = function (obj,cropped_image) {
      var counter = 0;
      for (var i in obj.dets) {
        counter++;
        if (obj.dets[i].bb) {
          if(!cropped_image.crop_bb){
            obj.dets[i].image = _extract_cropped_image(cropped_image.full_image);
          } else{
            obj.dets[i].bb[0] = obj.dets[i].bb[0]+cropped_image.crop_bb[0];
            obj.dets[i].bb[1] = obj.dets[i].bb[1]+cropped_image.crop_bb[1];
            obj.dets[i].bb[2] = obj.dets[i].bb[2]+cropped_image.crop_bb[0];
            obj.dets[i].bb[3] = obj.dets[i].bb[3]+cropped_image.crop_bb[1];
            for (var q = 0; q < 4; ++q) {
              /* jshint -W061 */
              obj.dets[i].bb[q] = eval(obj.dets[i].bb[q].toFixed(2));
            }
            obj.dets[i].image = _extract_image(cropped_image.full_image,obj.dets[i].bb);
          }
        }
      }
      return obj;
    };

    //crop radius is a percentage, rather than pixels (should be both/either!)
    var _apply_crop_radius = function(bb, crop_radius,maxWidth,maxHeight){
        var W = bb[2] - bb[0];
        var H = bb[3] - bb[1];
        var mw = (maxWidth) ? maxWidth : g.width;
        var mh = (maxHeight) ? maxHeight : g.height;

        var out_bb = [];
        out_bb[0] = Math.max(0,   bb[0] - crop_radius * W/100);
        out_bb[1] = Math.max(0,   bb[1] - crop_radius * H/100);
        out_bb[2] = Math.min(mw,  bb[2] + crop_radius * W/100);
        out_bb[3] = Math.min(mh,  bb[3] + crop_radius * H/100);

        return out_bb;
    };



    //Takes a video (or canvas) element,  and a bounding box and return
    // {dataURL: x, crop_bb: y} where x is the dataURL representing the cropped image
    // and crop_bb is is a bounding box that represents its position in the larger 
    // image
    var _extract_cropped_image = function (imageData,bounding_box,max_width,max_height){
      var constrained;
      if(max_width && max_height) {
        constrained = true;
      }
      //var inputWidth = _vmx_get_computed_style(video,"width").slice(0,-2);
      //var inputHeight = _vmx_get_computed_style(video,"height").slice(0,-2);

      var inputWidth = imageData.width;
      var inputHeight = imageData.height;
      
      var backBuffer = document.createElement('canvas');
      var bCtx       = backBuffer.getContext('2d');
      var scalew, scaleh;
      var outputWidth, outputHeight;
      scalew=scaleh=1;
      if(constrained){
        outputWidth  =  max_width;
        outputHeight = max_height;
      } else {
        outputWidth = inputWidth;
        outputHeight = inputHeight;
      }

      backBuffer.width  = outputWidth;
      backBuffer.height = outputHeight;
      bCtx.translate(outputWidth,0);
      bCtx.scale(-1,1);
      //img = document.createElement('img');
      //img.src = image;
      bCtx.putImageData(imageData, 0, 0);
      //bCtx.drawImage(image, 0, 0, backBuffer.width, backBuffer.height);
      bCtx.restore();


      var dataURL;
      if (bounding_box !== null && bounding_box.length === 4) {
        var elem = document.createElement('canvas');
        //elem.setAttribute('id' ,'crop_canvas');
        elem.width  = Math.round(bounding_box[2]-bounding_box[0]);
        elem.height = Math.round(bounding_box[3]-bounding_box[1]);
        
        var myData = bCtx.getImageData(bounding_box[0], bounding_box[1], elem.width, elem.height);
        
        elem.getContext("2d").putImageData(myData,0,0);
        dataURL = elem.toDataURL('image/jpeg',vmxparams.get('JPEGQuality'));
      } else {
        dataURL = backBuffer.toDataURL('image/jpeg',vmxparams.get('JPEGQuality'));      
        bounding_box = [0,0, inputWidth, inputHeight];
      }
      

      var cropped_image = {};
      cropped_image.dataURL = dataURL;
      cropped_image.crop_bb = bounding_box;
      cropped_image.full_image = imageData;
      return cropped_image;
    };

    var _extract_image = function (imageData,bb) {
        var elem = document.createElement('canvas');

        // if the bounding box is not provided, use entire image
        if (!bb) {
          bb = [0,0,imageData.width,imageData.height];
        }
        elem.width = Math.max(1,Math.round(bb[2]-bb[0]));
        elem.height = Math.max(1,Math.round(bb[3]-bb[1]));

        var backBuffer = document.createElement('canvas');
        var bCtx = backBuffer.getContext("2d");
        //NOTE: This is part of the attempt to have fluid video sizes
        var inputWidth = imageData.width;
        var inputHeight = imageData.height;
        //var inputWidth = _vmx_get_computed_style(video,"width").slice(0,-2);
        //var inputHeight = _vmx_get_computed_style(video,"height").slice(0,-2);
        backBuffer.width  = inputWidth; 
        backBuffer.height = inputHeight;
        //bCtx.translate(inputWidth,0);
        //bCtx.scale(-1,1);
        bCtx.putImageData(imageData,0,0);
        //bCtx.drawImage(video, 0, 0, inputWidth, inputHeight);
        //bCtx.restore();
        var myData = bCtx.getImageData(bb[0], bb[1], elem.width, elem.height);
        elem.getContext("2d").putImageData(myData,0,0);
        var dataURL = elem.toDataURL('image/jpeg',vmxparams.get('JPEGQuality'));
        
        //remove the newly added "temp" dom elemement, the temporary canvas
        try {
          elem.parentNode.removeChild(elem);
        } catch(error) {
        }
        //TODO: figure out if an explicit delete of elem is necessary or not
        
        return dataURL;
      };

    return {
      extract_image             : _extract_image,
      apply_crop_radius         : _apply_crop_radius,
      extract_cropped_image     : _extract_cropped_image,
      undo_crop                 : _undo_crop,
      unix_to_date              : _unix_to_date,
      vmx_clean_dirty_sessionId : _vmx_clean_dirty_sessionId,
      array_diff                : _vmx_array_difference,
     };
  })

  .factory("vmxselector", function(){
    var sels = [];
    return {
      add : function(sel){ sels.push(sel);},
      get_selections: function(){ return sels } ,
    };

  })

  .factory("VmxDetectorProviderX", function(vmxboxes,vmxglobals,vmxui,vmxappcode,vmxutils,VmxParamsProvider,vmxtracker, $http,vmxwebsocket){
    var currResponse = null;
    var detectors = [];

    // Function to update the number of positives, number of negatives, and add a new last time flag
    function update_positives(connection,num_pos,num_neg,new_time) {
      var hits = detectors.filter(function(x){
        return x.connection.id === connection;});
      if (hits.length !== 1) {
        console.log('Cannot update positives, no matching connectiosn for id',connection);
        return;
      }
      var hit = hits[0];
      hit.connection.model.num_pos = num_pos;
      hit.connection.model.num_neg = num_neg;
      hit.connection.model.end_time = Math.max(new_time, hit.connection.model.end_time);
    }

    var VmxDetector = function VmxDetector(){
      //Connection object, with id and model information
      this.connection = null;
      //Semaphore to catch edge race conditions
      this.waiter = true;
      //The bounding box of the last detection
      this.prev_box = null;
      //The params for this detector
      this.params = null;//VmxParamsProvider.getInstance();
      //A pause flag/semaphore
      this.pause = false;
      //The last response
      this.response = null;
      // the top detection only
      this.short_response = null;
      // the time of the last request
      this.last_time = 0;
    };

    VmxDetector.prototype.set_connection = function(connection){
      if(this.connection){
        vmxboxes.reset(this.connection.id);
      } 
      this.connection = connection;
      if(connection){
        this.connection.id = vmxutils.vmx_clean_dirty_sessionId(this.connection.id);
      }
    };

    VmxDetector.prototype.start = function(){
      this.waiter = false;
      this.pause = false;
      this.detect();
    };

    VmxDetector.prototype.stop = function(){
      this.pause = true;
      vmxboxes.reset(this.connection.id);
      return;
    };

    VmxDetector.prototype.response = function(){
      return this.response;
    };

      var time_count = 0;
      var time_container ={get:[], socket:[]};

    VmxDetector.prototype.detect = function() {
      if(!this.connection){
        this.stop();
        console.log('detection aborted, no connectionId');
        //vmxboxes.reset();
        return;
      }

      var current_image = vmxglobals.g.last_image;
      var current_time = vmxglobals.g.last_time;


      //TODO this seems to be happenign way more often than it should?
      if (this.last_time === current_time) {
        //console.log('Warning, issuing request at the same time as last');
        // define the waiting time
        var wait_time = 1;

        var that = this;
        setTimeout(function() {that.detect();}, wait_time*1000);
        return;

      }

      if(this.pause){
        //we call stop again, which might seem redundant, to clear the boxes
        this.stop();
        console.log("detection paused");
        if (this.prev_box) {
          vmxtracker.remove_bb(this.prev_box);
        } else {
          console.log('prev_box is not present');
        }
        
        return;
      }

      if (this.waiter || this.pause) {
        this.stop();
        console.log('detection aborted, waiter');
        return;
      }

      var params;
      var newparams;
      var msg = {};
      if (this.params === null) {
        // get params from server
        // TODO put this inside of VmxParamsProvider
        var current_this = this;
        console.log(this.connection);
        var path = VMX_SERVER + "session/" + this.connection.id + "/params";
        console.log(path);
        /*** start of what ***/
        current_this.params = VmxParamsProvider.getInstance();
        var i;
        for (i in params) {
          if (!current_this.params.named_params[i]) {
            current_this.params.named_params[i] = {value:NaN,current_value:NaN};
          }
          current_this.params.named_params[i].current_value = newparams[i];
        }
        current_this.params.update_parameters();
        console.log('do it');
        /*** end of what ***/
        $http({
          method: "GET",
          url: path,
          timeout: 100000,
          async: false,
          beforeSend: function (xhr){ 
            /* globals VMX_DEV_AUTH_HEADER: true */
            xhr.setRequestHeader('Authorization', VMX_DEV_AUTH_HEADER); 
          },
        }).error(function(data,estatus,headers,config){ 
          console.log([data,estatus,headers,config]);
          console.log(VMX_SERVER + "upload.php");
          console.log("there was an error submitting the ajax");
          
        }).success(function( respond ) {
          current_this.params = VmxParamsProvider.getInstance();
          newparams = respond.params;
          if (newparams === undefined){
            console.log("Server didn't respond to GET request on " + path);
            return;
          }
          var i;
          for (i in params) {
            if (!current_this.params.named_params[i]) {
              current_this.params.named_params[i] = {value:NaN,current_value:NaN};
            }
            current_this.params.named_params[i].current_value = newparams[i];
          }
          current_this.params.update_parameters();
          console.log('do it');
        });
      }// end params==null;
      var detection;
      try {
        detection = currResponse.dets[0];
      } catch(error) {
        //TODO: specific error handling for broken response.
        detection = null;
      }


      var videoWidth = current_image.width;
      var videoHeight = current_image.height;

      var cropped_image;
      if (this.params.get('ilearn') || !detection || detection.score < this.params.get('crop_threshold')){ //don't crop, send full image
        cropped_image = vmxutils.extract_cropped_image(current_image,null);
      }
      else{
        //NOTE we are still doing crop radius around a single detection.. what to do with multiple detections?
        var crop_radius   = this.params.get('crop_radius');
        //var videoWidth    = _vmx_get_computed_style(video,'width').slice(0,-2);
        //var videoHeight   = _vmx_get_computed_style(video,'height').slice(0,-2);
        var bounding_box  = vmxutils.apply_crop_radius(detection.bb, crop_radius,videoWidth,videoHeight);
        cropped_image = vmxutils.extract_cropped_image(current_image, bounding_box);
      }

      // add a request to the tracker
      vmxtracker.add_request(this.connection.model.cls,current_time);

      msg = {};
      msg.image = cropped_image.dataURL;
      msg.time = current_time;
      this.last_time = current_time;
      msg.ajax_start_time = new Date().getTime();
      msg.session_id = 'sessions/' + this.connection.id;
      msg.params = [];
      params = this.params.clean_params(this.params.list_all_params());// clean_detect_params();
      msg.params = params;
      this.waiter = true;
      var obj = {};
      if (0 && vmxwebsocket.useSocket) {
        vmxwebsocket.listen("process_image", function(respond){
            var i = null;
           
            try {
              obj = respond;

              obj.ajax_time = (new Date().getTime() - msg.ajax_start_time)/1000;
              if(time_count > 2) {
                time_container.socket.push(obj.ajax_time);
              }
              // here we undo the cropping operation and return raw image bounding box coordinates
              // why?  because the bounding box returned from the server is relative to the image sent to the server
              // but we really want to keep track of the bounding box with respect to the entire image

              obj = vmxutils.undo_crop(obj, cropped_image);
            } catch(error) {
              console.log(["response/message",respond,msg]);
              console.log(error,error.message);
              obj = {};
              obj.error = 1;
            }

            //NOTE: setting object here
            //vmxui.set_response(JSON.stringify(obj,undefined,2));
            vmxui.set_response(obj);
            this.response = obj;

            //console.log(obj);

            // create top detection only
            // so we have a cleaner JSON pane
            // TODO: put this into a function
            var obj2 = angular.copy(obj);
            if (!obj2.dets || obj2.dets.length === 0) {
              console.log('Warning, no dets at all, trying again');
              this.waiter = false;
              this.detect.bind(this)();
              return;
            } else {
              var top_det = obj2.dets.splice(0,1);
              top_det = top_det[0];
              delete obj2.dets;
              for (i in top_det) {
                obj2[i] = top_det[i];
              }
            }
            this.short_response = obj2;

            // set the source image
            this.short_response.jmage = msg.image;

            update_positives(this.connection.id,obj2.num_pos,obj2.num_neg,msg.time);
            
            // verify the point tracks using the detector response
            //console.log('detector done, verify with time=',msg.time);
            if (this.response.dets && this.response.dets.length > 0) {
              if (this.response.dets[0].score > this.params.get('display_threshold')) {
                vmxtracker.add_bb(angular.copy(this.response.dets[0]),msg.time);
              } else if(this.params.get('remove_smooth_below_threshold')){
                vmxtracker.remove_bb(this.prev_box);
              }
            }

            if (obj.error) {
              //console.log('error processing image, disabling process_image');
              
              this.waiter = false;
              console.log("detection aborted due to server error");
              return;
            }
            
            if (obj.dets instanceof Array) { } else { obj.dets = new Array(obj.dets); }
            
            vmxboxes.reset(this.connection.id);

            var threshold = this.params.get('display_threshold');

            // this is to steal colors for bounding box labeling
            // NOTE/TODO put in a link function or the app config or somewhere better
            var d;
            d = document.createElement('a');
            $(d).addClass('btn').addClass('btn-success').css('visibility','hidden');
            document.body.appendChild(d);
            var color_success = $(d).css('background-color');
            document.body.removeChild(d);

            d = document.createElement('a');
            $(d).addClass('btn').addClass('btn-danger').css('visibility','hidden');
            document.body.appendChild(d);
            var color_danger = $(d).css('background-color');
            document.body.removeChild(d);


            var counter = 0;
            
            for (i in obj.dets) {  
              counter++;
              if (!obj.dets[i]) {
                console.log('no object, warning');
              }
              if (obj.dets[i].score > threshold || (this.params.get('display_top_detection') && counter === 1)) {
                var color = [];
                if (obj.dets[i].score > 1){
                  color = color_success;
                }
                else if (obj.dets[i].score < -1){
                  color = color_danger;
                }
                else{
                  color = 'rgba(255,255,0,.5)';
                }

                var now = new Date();
                var box = new Detection(
                  obj.dets[i].cls,
                  obj.dets[i].score,
                  obj.dets[i].bb[0], 
                  obj.dets[i].bb[1],
                  obj.dets[i].bb[2],
                  obj.dets[i].bb[3],
                  '#00A',
                  color,
                  now,
                  obj.dets[i].ilearn_class
                );


                vmxboxes.add(this.connection.id,box);
                this.prev_box = box;
              }
            }

            currResponse = obj;
            //NOTE: manually sending detection as singleton as detections will be an array eventually
            vmxui.set_detections(obj.dets);
            // TODO:  these should be put into a single method
            vmxappcode.processRequest(obj);
            vmxappcode.execute(obj.dets);
           
            //$scope.postOperationFunction($scope);
            this.waiter = false;

          if(!this.waiter){
            this.detect.bind(this)();
          }
          else{
            console.log("the waiter died?????",this);
          }
        }.bind(this));
        vmxwebsocket.send("process_image", {session_id: this.connection.id, image: cropped_image.dataURL, params:params, time:current_time});
      } else {

      $http.post(VMX_SERVER + "session/" + this.connection.id,{image:cropped_image.dataURL, params:params, time:current_time})
      .error(function(jqxhr,estatus,etext){ 
        console.log([jqxhr,estatus,etext,msg]);
        console.log(VMX_SERVER + "upload.php");
        console.log("there was an error submitting the ajax");
        //NOTE(TJM): here we set waiter to 0 so that if we have a timeout error, we just try again next time
        //This lets old requests expire without having to do a hard UI reset
        this.waiter = false;
        this.detect();
      }.bind(this))
      .success(function( respond ) {
        var i = null;
        try {
          obj = respond;
          obj.ajax_time = (new Date().getTime() - msg.ajax_start_time)/1000;
          if(time_count > 2) { time_container.get.push(obj.ajax_time); }
          // here we undo the cropping operation and return raw image bounding box coordinates
          // why?  because the bounding box returned from the server is relative to the image sent to the server
          // but we really want to keep track of the bounding box with respect to the entire image

          obj = vmxutils.undo_crop(obj, cropped_image);
        } catch(error) {
          console.log(["response/message",respond,msg]);
          console.log(error,error.message);
          obj = {};
          obj.error = 1;
        }

        //NOTE: setting object here
        //vmxui.set_response(JSON.stringify(obj,undefined,2));
        vmxui.set_response(obj);
        this.response = obj;

        //console.log(obj);

        // create top detection only
        // so we have a cleaner JSON pane
        // TODO: put this into a function
        var obj2 = angular.copy(obj);
        if (!obj2.dets || obj2.dets.length === 0) {
          console.log('Warning, no dets at all, trying again');
          this.waiter = false;
          this.detect.bind(this)();
          return;
        } else {
          var top_det = obj2.dets.splice(0,1);
          top_det = top_det[0];
          delete obj2.dets;
          for (i in top_det) {
            obj2[i] = top_det[i];
          }
        }
        this.short_response = obj2;

        // set the source image
        this.short_response.jmage = msg.image;

        update_positives(this.connection.id,obj2.num_pos,obj2.num_neg,msg.time);
        
        // verify the point tracks using the detector response
        //console.log('detector done, verify with time=',msg.time);
        if (this.response.dets && this.response.dets.length > 0) {
          if (this.response.dets[0].score > this.params.get('display_threshold')) {
            vmxtracker.add_bb(angular.copy(this.response.dets[0]),msg.time);
          } else if(this.params.get('remove_smooth_below_threshold')){
            vmxtracker.remove_bb(this.prev_box);
          }
        }

        if (obj.error) {
          //console.log('error processing image, disabling process_image');
          
          this.waiter = false;
          console.log("detection aborted due to server error");
          return;
        }
        
        if (obj.dets instanceof Array) { } else { obj.dets = new Array(obj.dets); }
        
        vmxboxes.reset(this.connection.id);

        var threshold = this.params.get('display_threshold');

        // this is to steal colors for bounding box labeling
        // NOTE/TODO put in a link function or the app config or somewhere better
        var d;
        d = document.createElement('a');
        $(d).addClass('btn').addClass('btn-success').css('visibility','hidden');
        document.body.appendChild(d);
        var color_success = $(d).css('background-color');
        document.body.removeChild(d);

        d = document.createElement('a');
        $(d).addClass('btn').addClass('btn-danger').css('visibility','hidden');
        document.body.appendChild(d);
        var color_danger = $(d).css('background-color');
        document.body.removeChild(d);


        var counter = 0;
        
        for (i in obj.dets) {  
          counter++;
          if (!obj.dets[i]) {
            console.log('no object, warning');
          }
          if (obj.dets[i].score > threshold || (this.params.get('display_top_detection') && counter === 1)) {
            var color = [];
            if (obj.dets[i].score > 1){
              color = color_success;
            }
            else if (obj.dets[i].score < -1){
              color = color_danger;
            }
            else{
              color = 'rgba(255,255,0,.5)';
            }

            var now = new Date();
            var box = new Detection(
              obj.dets[i].cls,
              obj.dets[i].score,
              obj.dets[i].bb[0], 
              obj.dets[i].bb[1],
              obj.dets[i].bb[2],
              obj.dets[i].bb[3],
              '#00A',
              color,
              now,
              obj.dets[i].ilearn_class
            );


            vmxboxes.add(this.connection.id,box);
            this.prev_box = box;
          }
        }

        currResponse = obj;
        //NOTE: manually sending detection as singleton as detections will be an array eventually
        vmxui.set_detections(obj.dets);
        // TODO:  these should be put into a single method
        vmxappcode.processRequest(obj);
        vmxappcode.execute(obj.dets);
       
        //$scope.postOperationFunction($scope);
        this.waiter = false;

      if(!this.waiter){
        this.detect.bind(this)();
      }
      else{
        console.log("the waiter died?????",this);
      }

      }.bind(this));

    }

    };

    return {
        getInstance: function() { 
          var d = new VmxDetector(); 
          detectors.push(d); 
          return d; 
        },
        running_detectors: function(){
          return detectors;
        },
        update_positives: update_positives,
    };
  })

  .factory("vmxtracker", function(){
    // create the tracker object
    var global_tracker = new GridTracker(true,true);

    return {
      initialize_canvas: global_tracker.initialize_canvas,
      initialize_grid: global_tracker.initialize_grid,
      tracker_update: global_tracker.tracker_update,
      draw_all: global_tracker.draw_all,
      add_bb: global_tracker.add_bb,
      get_bb: global_tracker.get_bb,
      remove_bb: global_tracker.remove_bb,
      add_request: global_tracker.add_request,
      params: global_tracker.params
    };
    
    
  })

.filter('stripSession',function(){
  return function(input){
    return input.replace('sessions/','');
  };
})

.filter('classIs',function(){
  return function(objects,_class){
    return objects.filter(function(elm){ return elm.class === _class });
  };
})

.filter('includeParams',function(){
  return function(objects,toInclude){
      if (toInclude && toInclude instanceof Array){
        var tmpParams = {};

        for(var paramName in objects){
          if(!paramName){ continue; }
          if(toInclude.indexOf(paramName) !== -1 || _.intersection(objects[paramName].group, toInclude).length){
            tmpParams[paramName] = objects[paramName]; 
          }
        }
        objects = tmpParams;
      }
    return objects;
  };
})

//Make sures that the model name is not empty, and matches the search
.filter('modelNameContains',function(){
  return function(objects,name){
    if(!objects) {
      return [];
    }
    return objects.filter(
      function(elm){ return (elm.model.cls !== "none") && !name || elm.model.cls.indexOf(name) !== -1 }
    );
  };
})

.filter('scoreIs',function(){
  return function(objects,op,_score){
    var op_fun;
    if(typeof _score === 'undefined' || _score === '') {
      return objects;
    }
    switch(op){
      case '<':
      case 'lt': op_fun = function(left,right){ return left < right; };
      break;
      case '>':
      case 'gt': op_fun = function(left,right){ return left > right; };
    }
    return objects.filter(function(elm){ return op_fun(elm.score,_score); });
  };
});
