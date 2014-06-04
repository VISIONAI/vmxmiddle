/* globals vmxApp: true, Ladda: true, IPCAM_PROXY: true, _vmx_object_or_array: true, _vmx_is_encoded_image: true, compatibility: true*/
vmxApp
.directive('vmxJsonPane', function ($compile) {
  return {
    restrict: 'E',
    scope: { 
      data:'=binding'
    },
    link: function(scope, element){
      var template ='<ul>';
          template += "<li ng-repeat='(key,value) in data track by $index'>";
          template += '<div ng-if="object_or_array(value)">';
          template +=   '{{key}}: <vmx-json-pane binding="value"></vmx-json-pane>';
          template += '</div>';
          template += '<div ng-if="!object_or_array(value)">';
          template +=    "<div ng-if='is_encoded_image(value)'>";
          template +=      "<span><em>{{key}}</em> : <br/><img style=\"height:100px\" ng-src=\"{{value}}\"/>";
          template +=    "</div>";
          template +=    "<div ng-if='!is_encoded_image(value)'>";
          template +=      "<span><em>{{key}}</em> : {{value}}</span>";
          template +=    "</div>";
          template += "</div>";
          template += "</li>";
      template += "</ul>";

      var newElement = angular.element(template);

      $compile(newElement)(scope);
      element.replaceWith(newElement);
    },
     controller: function($scope){
       $scope.object_or_array = _vmx_object_or_array;
       $scope.is_encoded_image = _vmx_is_encoded_image;
     }
  };
})
.directive('vmxDetectorParams', function () {
  return {
    restrict: 'E',
    require: '^vmxRunningDetectors',
    scope :{
      paramsObj      : "=params",
      toInclude      : "&",
      showbutton     : "=showbutton",
    },
    // link: function(scope,element,attrs) {
    //   console.log('linking it');
    // },

    templateUrl : 'static/partial/overlay-detector-params.html',
    //"param" is passed in as the param object
    controller: function($scope,vmxui){
      $scope.vmxui = vmxui;

      $scope.$on('savingModeChange',function(event, savingMode){
        /*jshint unused: vars*/
        $scope.saving_mode = savingMode;
      });

      $scope.params = $scope.paramsObj.list_all_params(); 

      if ($scope.showbutton === undefined) {
        $scope.update_button = true;
      } else {
        $scope.update_button = $scope.showbutton;
      }

      // // by default show the updatebutton
      // if ($scope.updatebutton === undefined)
      //   $scope.updatebutton = true;

      // update the current parameters
      $scope.update_current_parameters = function() {
        $scope.paramsObj.update_parameters();
        $scope.params = $scope.paramsObj.list_all_params(); 
        $scope.$emit('updateButtonPressed');
      };

      $scope.collapse_state = true;
      $scope.expand_all_params = function() {
        $scope.collapse_state = !$scope.collapse_state;
        $scope.$broadcast("expandAll",$scope.collapse_state);
      };

      //get the array of which params to be included.
      var toInclude = $scope.toInclude();
      console.log(toInclude);


      console.log($scope.toInclude(), $scope.params);
      console.log("controller $scode", $scope);

      //$scope.learning_mode = paramContainer.learning_mode;
      //$scope.$watch('paramContainer.learning_mode',function(a){console.log('watcher fired');},true)
      
      $scope.update_current_parameters();
      //$scope.params = VmxParamsProvider.update_parameters($scope.params);
      //$scope.update_parameters();

    }
  };
})

// The directive for creating a model from selections
// TODO: when the model name clashes with something already present, then complain explicitly on the screen
// TODO: remove update parameters button
.directive('vmxCreateModel', function (vmxglobals) {
  return {
    restrict: 'E',
    require: '^vmxOverlayContent', 
    templateUrl: 'static/partial/overlay-model-overlay.html',

    link: function(scope, element) {
      //element.parent.draggable();//{ cancel: "span.g-slider" });
      element.parent().draggable({ handle: ".dragbar", stack: ".dbar"});
      //element.disableSelection();

      // when the element is destroyed, make sure the A key is disabled
      scope.$on('$destroy', function() {
        console.log('In destroy of','vmx-CreateModel'); 
        vmxglobals.remove_key_listener();
      });

      // get the create model button, and make it a ladda button
      scope.ladda_button = Ladda.create(document.querySelector('vmx-create-model #create_model_button'));
      
      // make sure that when we are typing inside some text, we do not allow the key listener to mess things up
      $('#create_model_text').focus(function(){vmxglobals.remove_key_listener();});
      $('#create_model_text').focusout(function(){vmxglobals.add_key_listener();});
    },
    controller: function($scope,$stateParams,vmxselector,vmxglobals,vmxmodels,vmxconnections,vmxparams,vmxutils,VmxDetectorProviderX){

      // get the model listing, so that we have names of models, which
      // will make sure when we select a name, it won't clash
      vmxmodels.list();

      $scope.unix_to_date = vmxutils.unix_to_date;
      $scope.paramsObj = vmxparams;

      $scope.model_name= "";
      $scope.container = {};
      $scope.container.show_selections = false;
      
      $scope.error_message = '';

      //$scope.container.selection_mode = false;

      // add shortcut for the character a
      vmxglobals.add_key_listener();

      $scope.get_selection_mode = function () { return vmxglobals.g.selection_mode; };

      $scope.toggle_selection_mode = function() {
        if (vmxglobals.g.selection_mode === true) {
          vmxglobals.disable_selection_mode();
        } else {
          vmxglobals.enable_selection_mode();
        }
      };



      // determines if the given model_name is valid or not by making
      // sure it is not present inside the list of currently available
      // models
      $scope.is_valid_name = function(model_name){
        //console.log('trying out ',model_name);
        if (model_name.length === 0 ) {
          $scope.error_message = 'Model name cannot be empty';
          return false;
        }

        if (model_name.search(' ') >= 0) {
          $scope.error_message = 'Model name cannot contain spaces';
          return false;
        }
        if (model_name === 'none'){
          $scope.error_message = 'Model name cannot be "none"';
          return false;
        }
        var valid_names = vmxmodels.list_cached();
        // if (valid_names.length == 0){
        //   return true;
        // }
        var retval = valid_names.filter(
            function (element) {
              return element.cls === model_name;
            }).length === 0;

        if (retval) {
          $scope.error_message = '';
          //console.log('model_name',model_name,'is valid');
        }
        else {
          $scope.error_message = 'Model name "' + model_name + '" already used';
          //console.log('model_name',model_name,'is invalid');
        }
        return retval;
       
      };

      $scope.has_selections = function() {
        return vmxselector.get_selections().length > 0;
      };

      $scope.remove_selection = function(index) {
        $scope.selections.splice(index,1);
        if (!$scope.has_selections()) {
          $scope.error_message = 'Must have at least one selection';
        }
      };

      if (!$scope.has_selections()) {
        $scope.error_message = 'Must have at least one selection';
      }

      // $scope.exit_create_model = function() {
      //   $state.go('session',{sid: vmxconnections.current()});
      // }

      $scope.create_model = function(model_name){
        $scope.paramsObj.update_parameters();
        $scope.params = $scope.paramsObj.clean_params($scope.paramsObj.list_all_params()); //init_params();

        if(!$scope.is_valid_name(model_name)){ return; }
        
        // start the ladda button
        $scope.ladda_button.start();
        
        vmxconnections.create().then(function(data){
          var session_id = data;
          console.log('sid is ' + session_id);
          vmxmodels.create(vmxselector.get_selections(),model_name,session_id,$scope.params).then(function(){ 
            //$state.go('session',{sid: vmxconnections.current()});
          }).then(function() {
            //NOTE: This is sloppy/bad; we are just waiting for a response and not even looking at the contents of it!
            var detector = VmxDetectorProviderX.getInstance();
            vmxconnections.update().then(function(data){
              var connection = data.filter(function(item){
                return item.id === (session_id);
              }).pop();
              if (connection) {
                detector.set_connection(connection);
                detector.start();
                
                $scope.ladda_button.stop();
                // disable UI element here
                
                $scope.ui.panels.createModel = false;
                
              }
              else {
                console.log('Problem: just created session, but could not find it');
              }
            });


          });
        });
      };
      $scope.selections = vmxselector.get_selections();
      $scope.sid = $stateParams.sid; 

      
    }

  };
})
/*Directive for a SINGLE parameter*/
.directive('vmxDetectorParam', function () {
  return {
    restrict: 'E',
    require: '^vmxDetectorParams', 
    templateUrl: 'static/partial/overlay-detector-params-param.html',
    link: function(scope){
      if(scope.param.widget === 'slider' && scope.param.type === "float"){
        scope.param.slider_step = parseFloat((scope.param.max_value - scope.param.min_value) / 100).toPrecision(2);
      } else {
        scope.param.slider_step = 1;
      }
    },
    controller: function($scope){
      $scope.$on('expandAll', function(event,state){
        /*jshint unused: vars*/
        $scope.paramcontainer.collapse = state;
      });
    }
  };
})

.directive('vmxDebugPanel', function () {
  return {
    restrict: 'E',
    require: '^vmxOverlayContent', 
    templateUrl: 'static/partial/overlay-debug-panel.html',
    link: function(scope, element) { 
      /*jshint unused: vars*/
      element.parent().draggable({ handle: ".dragbar", stack: ".dbar"});
    },
    controller: function() {},
  };
})

.directive('vmxRunningDetectors', function (vmxmodels) {
  return {
    restrict: 'E',
    templateUrl: 'static/partial/overlay-running-detectors.html',

    link: function(scope, element) {
      /*jshint unused: vars*/
      element.parent().draggable({ handle: ".dragbar", stack: ".dbar"});
    },
    controller:['$scope', '$modal', function($scope,$modal){
      $scope.aggro_slider = [];
      $scope.saving_mode = false;
      var aggro_slider_cache = [];
      $scope.$watch('aggro_slider', function(newVal,oldVal){ 
        $scope.aggroSliderInit = true;
        if(newVal.length !== oldVal.length){
          return; // do nothing for new detectors being added.
        }
        for (var i = 0; i < newVal.length; ++i){
          if(newVal[i] !== oldVal[i]){
            applyAggroSlider(i);
            /*
            var ilearn_iterations = 1000 * (newVal[i]) / 100;
            var detector_quality  = 3000 * (newVal[i]) / 100;
            $scope.running_detectors[i].params.named_params.ilearn_iterations.current_value = ilearn_iterations;
            $scope.running_detectors[i].params.named_params.ilearn_iterations.value = ilearn_iterations;
            $scope.running_detectors[i].params.named_params.max_windows.current_value = detector_quality;
            $scope.running_detectors[i].params.named_params.max_windows.value = detector_quality;
            */
          }
        }
      }, true);

      $scope.aggroSliderInit = false;

      function applyAggroSlider(index){
        var numPos = $scope.running_detectors[index].connection.model.num_pos;
        var ilearn_iterations = Math.round(1000 * ($scope.aggro_slider[index]) / 100 / numPos);
        var detector_quality  = 3000 * ($scope.aggro_slider[index]) / 100;
        $scope.running_detectors[index].params.named_params.ilearn_iterations.current_value = ilearn_iterations;
        $scope.running_detectors[index].params.named_params.ilearn_iterations.value = ilearn_iterations;
        $scope.running_detectors[index].params.named_params.max_windows.current_value = detector_quality;
        $scope.running_detectors[index].params.named_params.max_windows.value = detector_quality;
      }

      $scope.toggle_learning_mode = function($index){
        var d = $scope.running_detectors[$index];
        var currLearningMode = !!d.params.get('ilearn');
        if(!currLearningMode){
          // entering learning mode so save the current values to cache.
          aggro_slider_cache[$index]                   = (aggro_slider_cache[$index])                   ? aggro_slider_cache[$index]                   : {};
          aggro_slider_cache[$index].max_windows       = (aggro_slider_cache[$index].max_windows)       ? aggro_slider_cache[$index].max_windows       : {};
          aggro_slider_cache[$index].ilearn_iterations = (aggro_slider_cache[$index].ilearn_iterations) ? aggro_slider_cache[$index].ilearn_iterations : {};

          aggro_slider_cache[$index].ilearn_iterations.current_value = $scope.running_detectors[$index].params.named_params.ilearn_iterations.current_value;
          aggro_slider_cache[$index].ilearn_iterations.value         = $scope.running_detectors[$index].params.named_params.ilearn_iterations.value;
          aggro_slider_cache[$index].max_windows.current_value       = $scope.running_detectors[$index].params.named_params.max_windows.current_value;
          aggro_slider_cache[$index].max_windows.value               = $scope.running_detectors[$index].params.named_params.max_windows.value;
          if ($scope.aggroSliderInit){
            applyAggroSlider($index);
          }
        } else if(currLearningMode && aggro_slider_cache[$index] && aggro_slider_cache[$index].ilearn_iterations){
          // leaving learning mode, so return things to the way they were
          $scope.running_detectors[$index].params.named_params.ilearn_iterations.current_value = aggro_slider_cache[$index].ilearn_iterations.current_value;
          $scope.running_detectors[$index].params.named_params.ilearn_iterations.value         = aggro_slider_cache[$index].ilearn_iterations.value;
          $scope.running_detectors[$index].params.named_params.max_windows.current_value       = aggro_slider_cache[$index].max_windows.current_value;
          $scope.running_detectors[$index].params.named_params.max_windows.value               = aggro_slider_cache[$index].max_windows.value;
        }
        $scope.running_detectors[$index].params.toggleLearning();
      };

      $scope.$on('updateButtonPressed', function(){
        $scope.aggroSliderInit = false;
      });

      // Save the model inside the current connection, and enable the ladda button
      $scope.save_model = function(connection,button_id) {
        $scope.saving_mode = true;
        $scope.$broadcast('savingModeChange', true);
        var l = Ladda.create(document.getElementById(button_id));
        l.start();
        console.log($scope.unning_detectors);
        var promise = vmxmodels.save_current(connection.id);
        promise.then(function() {
          l.stop();
          $scope.saving_mode = false;
          $scope.$broadcast('savingModeChange', false);
        });
      };

      // Function to spawn the advanced model editor 
      $scope.modeler_modal = function(connection){
        $modal.open({
          templateUrl : 'static/partial/modeler_modal.html',
          windowClass : 'vmx-wide-modal',
          controller: function($scope,$modalInstance,vmxui,$http,vmxutils){
            //var ladda_button = Ladda.create($("#update_model_button"));
            //l.start();
            $scope.update_status_number = 0;
            $scope.unix_to_date = vmxutils.unix_to_date;
          $scope.container = {};
          $scope.container.objects = [];
          $scope.objects_cache = [];
          $scope.reverse = true;

          $scope.settings = {};
          $scope.settings.max_positives = 20;
          $scope.settings.max_negatives = 20;
          $scope.settings.positives_order = 1;
          $scope.settings.negatives_order = -1;
          $scope.settings.number_updates = 10;

          $scope.toggle_modeler_visible = vmxui.toggle_modeler_visible;
          $scope.$watch('vmxui.modeler()',function(newVal){$scope.modeler = newVal;});
          console.log('creating modeler pane');

          //NOTE: This should NOT be in a modal instance.
          //NOTE: This should also NOT be in a directive.
          //NOTE: Seems like show_model should take a model, rather than a session?
          // here we get the request from the server
          // command: 'show_model'
          var req = {session_id: connection.id, settings: $scope.settings};
          console.log($scope.settings);

          var server = VMX_SERVER;
          var resource = "session/" + connection.id + "/edit";
          var request = server + resource;
          
          $http.post(request,req).success(function(response){
            if(typeof response ===  'string' || response.error){
              console.log('we got a string instead of an object, so something likely broke...');
              console.log(response);
              return;
            }
            $scope.container.objects = response.data;
            $scope.objects_cache = angular.copy(response.data);
            console.log('just got ' + $scope.container.objects.length + ' objects');
          }); 
          
          console.log('done creating modeler pane');
          
          // Determine if the modeler changed from the cache
          $scope.modeler_changed = function () {
            if (!$scope.objects_cache || $scope.objects_cache.length === 0 ){
              return;
            }

            for (var i = 0; i < $scope.container.objects.length; ++i) {
              if ($scope.container.objects[i].class !== $scope.objects_cache[i].class){
                return true;
              }
            }
            return false;
          };

          $scope.submit_stuff = function () {
            // if ($scope.modeler_changed() == false) {
            //   console.log("not submitting because nothign changed");
            //   return;
            // }
            console.log('Submitting new modeler data');
            
            var changes = [];
            
            for (var i = 0; i < $scope.container.objects.length; ++i) {
              if ($scope.container.objects[i].class !== $scope.objects_cache[i].class) {
                var result = angular.copy($scope.container.objects[i]);
                delete result.image;
                changes.push(result);
              }
            }
            console.log(changes);
            $scope.update_model(changes);
          };

          $scope.update_model = function(/*changes*/) {
           //command : 'update_model" 
            var req = {settings: $scope.settings};
            var resource = VMX_SERVER + "session/" + connection.id + "/edit";

            var l = Ladda.create(document.querySelector('#update_model_button'));
            // $('#update_model_button').click(function(e){
      //   e.preventDefault();
      //   var l = Ladda.create(this);
      //   l.start();
      // });	

            l.start();
            $http.put(resource,req).success(function(response){
              if(typeof response ===  'string' || response.error){
                console.log('we got a string instead of an object, so something likely broke...');
                return;
              }

              console.log('update_model finished with result:');
              console.log(response);
              
              $scope.container.objects = response.data;
              $scope.objects_cache = angular.copy(response.data);
              console.log('just got ' + $scope.container.objects.length + ' objects');

              //console.log('just got ' + $scope.container.objects.length + ' objects');
              l.stop();
            }); 
            
          };

            $scope.input = '';
            $scope.ok = function(input){
              console.log("modal submitted",input);
              $modalInstance.close(input);
            };
            $scope.cancel = function(){
              $modalInstance.dismiss('cancel');
            };
            
          }
        });
      };
      
      $scope.removeDetector = function(connectionId,index){
      //NOTE: Sloppy use of jshint override; this function shoudl be changed
      /*jshint unused:vars */
        var detector = $scope.running_detectors.splice(index,1).pop();
        $scope.aggro_slider.splice(index,1);
        detector.stop();
      };
    }]
  };
})
.directive('vmxAvailableDetectors', function (vmxconnections,VmxDetectorProviderX,vmxipcam,vmxmodels) {
  return {
    restrict: 'E',
    require: '^vmxOverlayContent',
    templateUrl : 'static/partial/overlay-available-detectors.html',
    controller: function($scope,vmxglobals){
      $scope.top_detectors = vmxconnections.list_cached();

      //Choose model for new connection; creates a model.modal that fires a new connection
      //with the chosen the model
      $scope.choose_model = function(){
        return vmxmodels.modal(function(model){
            //Create a session for the selected model
            $scope.spinner = Ladda.create(document.querySelector('.detector-chooser-spinner'));

            //After connection is created, start the detector
            $scope.spinner.start();
            vmxconnections.create(model).then(function(data){
              var connectionId = data;
              vmxglobals.g.is_playing = true;
              var detector = VmxDetectorProviderX.getInstance();
              vmxconnections.update().then(function(data){
                $scope.all_available_connections = data;
                var connection = data.filter(function(item){
                   return item.id.indexOf(connectionId) !== -1;
                }).pop();
                //NOTE DIRTY HACK: it appears images iare getting 'strip slashed' in the "list all connections"
                //connection.model.image = _vmx_fix_stripped_image(connection.model.image);
                if(connection) { $scope.our_connections.push(connection); }
                console.log($scope.our_connections);
                detector.set_connection(connection);
                detector.start();
                $scope.spinner.stop();
              });
              //NOTE we give a temporary dummy "connection" to the detector while we wait
              //for the async call to finish above
              detector.set_connection({id:connectionId});
              //detector.set_video(videoElement);
              if(vmxipcam.is_playing()){
                var ipcanvas = document.getElementById('ipcam');
                detector.set_video(ipcanvas);
                $scope.vid = ipcanvas;
              }
              $scope.our_detectors[connectionId] = detector;
            });
        });
      };

      /* Used for autocomplete, we extract the object returned by the autocomplete
         which in this case is "connection" from vmxconnections.update() */
      $scope.format = function(){
        if(!$scope.search || !$scope.search.model) {
          return "";
        }
        return $scope.search.model.cls;
      };

      $scope.addDetector = function(connection){
        var connectionId = connection.id;
        if(connectionId.substring(0,9) === 'sessions/') { connectionId = connectionId.substring(9); }
        else{
          console.log(connectionId.substring(0,8));
        }
        //This redundant session makes detecting work on iphone??
        //vmxconnections.set(connectionId);
        
        // Make sure that we cannot attach to a currently connected session
        if (VmxDetectorProviderX.running_detectors().filter(function(x){return x.connection.id === connectionId;}).length > 0) {
          console.log('Not attaching session, it is already attached');
          return;
        }

        vmxglobals.g.is_playing = true;
        var detector = VmxDetectorProviderX.getInstance();
        detector.set_connection(connection);
        //detector.set_video(videoElement);
        $scope.our_connections.push(connection);
        if(vmxipcam.is_playing()){
          var ipcanvas = document.getElementById('ipcam');
          detector.set_video(ipcanvas);
          $scope.vid = ipcanvas;
        }
        detector.start();
        $scope.our_detectors[connectionId] = detector;
        $scope.search = '';
        //$scope.existing_connections = _vmx_array_difference($scope.existing_connections,$scope.container.connections,"id");
      };


    }
  };
})

.directive('vmxOverlay', function () {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/overlay.html',
    controller: function($scope,VmxDetectorProviderX,vmxconnections,vmxwebsocket){

      //THIS IS BAD TO START WEBSOCKET HERE!!!!
      //vmxwebsocket.connect();

      $scope.search = undefined;
      //helper function so we can watch for changes to running_detectors
      $scope.running_detectors = VmxDetectorProviderX.running_detectors();
      $scope.ui = {}; 
      $scope.ui.panels = { 
        availableDetectors:true,
      };
      vmxconnections.update().then(function(data){
        $scope.all_available_connections = data;
      });
      $scope.our_connections = [];
      $scope.our_detectors   = [];
      $scope.search = null;
    }
  };
})
.directive('vmxOverlayContent', function () {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/overlay-content.html',
    require: '^vmxOverlay',
    controller: function() {},
  };
})
.directive('vmxOverlayToolbar', function () {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/overlay-toolbar.html',
    require: '^vmxOverlay',
    controller: function($scope){
      // toggle the state of the panel
      $scope.toggle = function(panel) { 
        $scope.ui.panels[panel] = !$scope.ui.panels[panel]; 
      };
      // determine the state of the panel
      $scope.panel_state = function(panel) {
        if ($scope.ui.panels[panel] === undefined ||
            $scope.ui.panels[panel] === false) {
          return false;
        }
        return true;
      };
    },
  };
})
.directive('vmxDetectorThumbnail', function () {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/detector-thumbnail.html',
    scope :{
      model: "=model" ,
    },
    controller: function($scope,vmxutils) {
      $scope.unix_to_date = vmxutils.unix_to_date;
    }
  };
})
.directive('vmxExemplar', function (vmxutils,$compile) {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/modeler-exemplar.html',
    scope :{
      class : "=class" ,
      score : "=score" ,
      other  : "@tyme"  ,
      image : "@image" ,
    },
    link: function(scope,element){
      scope.date = vmxutils.unix_to_date(parseInt(scope.other));
      $compile(element.contents())(scope);
    },
  };
})
.animation(".thumbnail-container", function($timeout){
  var l, e;
  var entered;
  return{
    enter: function(element, done){
      console.log("entering");
      console.log(element[0].style.left);
      e = $(element).offset();
      console.log(e);
      entered = new Date();
      delete element[0].style.transition;
      delete element[0].style.WebkitTransition;
      $timeout(function(){
        var top_diff = l.top - e.top;
        var left_diff = l.left - e.left;
        element[0].style.opacity = 0;
        element[0].style.left = left_diff + "px";
        element[0].style.top = top_diff + "px";
        $timeout(function(){
          element[0].style.opacity          =                1;
          element[0].style.left             =         0 + "px";
          element[0].style.top              =         0 + "px";
          element[0].style.transition       = "all linear .5s";
          element[0].style.WebkitTransition = "all linear .5s";
        },1);
      },50);
      done();
    },
    leave: function(element, done){
      /*jshint unused: vars */
      l = $(element).offset();
      done();
    },
    move: function(element, done){
      /*jshint unused: vars */
      done();
    },
    show: function(element, done){
      /*jshint unused: vars */
      console.log("showing");
      done();
    },
    hide: function(element, done){
      /*jshint unused: vars */
      console.log("hideing");
      done();
    },
    addClass: function(element, className, done){
      /*jshint unused: vars */
      console.log("adding",className);
      done();
    },
    removeClass: function(element, className, done){
      /*jshint unused: vars */
      console.log("removing",className);
      done();
    },
  };
})

.directive('vmxGitPane', function (vmxappcode,vmxgithub,vmxapi) {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/git-pane.html',
    link: function(scope, element) {
      /*jshint unused: vars */
      element.parent().draggable({ handle: ".dragbar", stack: ".dbar"});
    },
    controller: function($scope){
      $scope.container = {};
      $scope.container.errors= null;

      $scope.switches= {};
      $scope.clear = function(){
        vmxapi.reset();
        $scope.switches.run_code_switch=false;
      };

      $scope.repos = [
        { 'name': "PONG", 
          'repo': "https://github.com/gdoteof/vmxapp_pong.git"
        },
        { 'name': "Email on detect", 
          'repo': "https://github.com/gdoteof/vmxapp_email.git"
        },
        { 'name': "Hello World", 
          'repo': "https://github.com/gdoteof/vmxapp-helloworld.git"
        },
        { 'name': "Tweeter", 
          'repo': "https://github.com/gdoteof/vmxapp_tweet.git"
        },
        { 'name': "Counter", 
          'repo': "https://github.com/gdoteof/vmxapp_count.git"
        },
      ];
      $scope.setrepo = function(repo){
        $scope.gitrepo = repo.repo;
      };
      $scope.gitrepo="https://github.com/gdoteof/vmxapp-helloworld.git";
      $scope.$watch('switches.run_code_switch',function(newVal){
        if(newVal){
          vmxappcode.set_execute_flag(true);
        }
        else{
          vmxappcode.set_execute_flag(false);
        }
      });
      $scope.load_git_repo = function(gitrepo){
        vmxgithub.load_git_repo(gitrepo).then(function(val){
            if(val){ //successful load
              $scope.container.errors = vmxgithub.current_errors();
            }
            $scope.switches.run_code_switch = true;
        });
      };
    }
 };
})
.directive('vmxEditorPane', function (vmxui,vmxappcode) {
  return {
    restrict: 'E',
    templateUrl : 'static/partial/editor-pane.html',
    controller: function($scope,$modal){
      var editor = null; //the ace editor
      $scope.files = {};
      $scope.currently_editing='callback';
      $scope.current_app = null;
      $scope.callback_edited = false;
      $scope.current_file_index = null;
      $scope.toggle_editor = vmxui.toggle_editor;

      //data binding is weird.  bidning an ng-model inside of an ng-if loses connection to main scope, so we pass an object
      $scope.container = {};

      $scope.container.app_ediited_ns = false; //app edited but not saved
      $scope.container.buffers_edited_ns = {}; //hash of bools mapped to the array of files (index of callback is 'callback'


      vmxappcode.get_apps().then(function(val){
         $scope.container.apps = val; 
      });


      $scope.set_current_app = function(index){
        vmxappcode.set_execute_flag(false);
        $scope.current_app = vmxappcode.set_app(index);
        $scope.files = vmxappcode.get_files();
        //Helper files should be evaled on app loading (so they are avialable to callback)
        for(var i = 0; i<$scope.files.length; ++i){
          try{
            /*jshint -W061 */
            eval($scope.files[i].body);
          }catch(e){

          }
        }
        $scope.callback = vmxappcode.get_callback();
        $scope.container.script_in_editor = $scope.callback; 
        $scope.current_file_index = 'callback';
        setTimeout(function(){ $scope.container.buffers_edited_ns['callback'] = false;}, 50); //dirty hack to do this
        if($scope.switches.run_code_switch){
          vmxappcode.set_execute_flag(true);
        }
      };

      $scope.save_current_app = function(){
        vmxappcode.save_app();
      };
        

      $scope.switch_buffer = function(index){
        $scope.current_file_index = index;
        console.log("inside set current buffer");
        console.log($scope.files);
        //Save the state
        if($scope.currently_editing === 'callback'){
          $scope.callback = $scope.container.script_in_editor;
        }
        else{
          $scope.files[$scope.currently_editing] = $scope.container.script_in_editor;
        }
        set_current_buffer(index);

      };

      function set_current_buffer(index){
        //filename == callback is a special case!  callback gets called everytime
        if(index === 'callback'){
          $scope.container.script_in_editor = $scope.callback; 
          $scope.currently_editing='callback';
        }
        else{
          $scope.container.script_in_editor = $scope.files[index].body;
          $scope.currently_editing=$scope.files[index].name;
        }
      }

      $scope.save_file = function(){
        try{
          /*jshint -W061 */
          eval($scope.container.script_in_editor);
          $scope.current_app.value.files[$scope.current_file_index].body = $scope.container.script_in_editor;
          vmxappcode.save_file($scope.current_file_index,
                               $scope.current_app.value.files[$scope.current_file_index]);
          vmxappcode.save_app();
        }catch(e){
          console.log(e);
        }
      };

      $scope.callback = $scope.container.script_in_editor = "//the variable `detections` contains the detections; this function is called for every detection";

      $scope.switches= {};
      $scope.$watch('switches.run_code_switch',function(newVal){
        if(newVal){
          vmxappcode.set_execute_flag(true);
          $scope.compile_function($scope.callback);
        }
        else{
          vmxappcode.set_execute_flag(false);
        }
      });

      $scope.get_class = function(forwhat){
        if(forwhat === $scope.currently_editing){ 
          return "active"; 
        }
        if(forwhat === 'helper_file'){
          if($scope.files[$scope.currently_editing]) {
            return "active";
          }
        }
      };

      $scope.aceLoaded = function(_editor){
        //_editor is the ace editor object which can be manipulated per http://ace.c9.io/#nav=howto 
        editor = _editor;
        //set the current file index to callback, which is where we start the editor.
        $scope.current_file_index = 'callback';
        $scope.container.buffers_edited_ns['callback'] = false;
        editor.on("change",function(){ $scope.container.buffers_edited_ns[$scope.current_file_index] = true; });
        /* globals ace:true */
        ace.require('ace/ext/language_tools',function(){
          editor.setOptions({
            enableBasicAutocompletion: true,
            enableSnippets: true,
          });
        });

        //when we load the editor with the call back, that counts as changing it, so we need to set it (for our purproses) to not changed
        setTimeout(function(){ $scope.container.buffers_edited_ns['callback'] = false;}, 50); //dirty hack to do this
      };

      $scope.set_keybinding = function(mode){
        //NOTE this is a ghetto way to load the keybinding handler
        //documentation on ace says we shold be able to do setKeyboardHandler('vim')... 
        //but it doesn't work
        $scope.container.keybinding=mode;
        if(mode){
          editor.setKeyboardHandler(ace.require("ace/keyboard/" + mode).handler);
        }
        else{
          editor.setKeyboardHandler();
        }
      };

      $scope.save_callback = function(script){
        try{
          vmxappcode.set_entry(script);
          $scope.callback = script;
          $scope.current_app.value.callback = script;
          $scope.container.buffers_edited_ns['callback'] = false;
          vmxappcode.save_app();
        }catch(e){
          console.log("Callback couldn't compile:", e);
        }
      };

      $scope.create_file = function(filename){
        var file = {name:filename,body:"//file + " + filename};
        console.log($scope.current_app.value.files);
        $scope.current_app.value.files.push(file);
        $scope.switch_buffer($scope.current_app.value.files.length - 1);
        $scope.files = $scope.current_app.value.files;
      };

      $scope.compile_function = function(script) {
          try {
            vmxappcode.set_entry(script);
          } catch(error) {
            console.log('Cannot compile function');
            console.log(error);
          }
      };

      $scope.new_app = function(){
        $scope.current_app = vmxappcode.new_app("my other namxxxxe").then(function(val){
          $scope.current_app = val;
          flush_app();
        });
      };

      function flush_app(){
        $scope.files = $scope.current_app.value.files;
        $scope.callback = $scope.current_app.value.callback;
        $scope.container.script_in_editor = $scope.callback;
      }

      $scope.delete_app = function(index){
        var deleting_current = $scope.container.apps[index].key === $scope.current_app.key;
        vmxappcode.delete_app(index).then(function(){
          vmxappcode.get_apps().then(function(val){
            console.log("what???");
            console.log(val);
            $scope.container.apps = val; 
            if(deleting_current){ $scope.current_app = null; }
          });
        });
      };

      $scope.new_app_modal = function() {
        var  modalInstance = $modal.open({
          templateUrl: 'static/partial/new_app_model.html',
          controller: function($scope,$modalInstance){
            $scope.input = '';
            $scope.ok = function(input){
              console.log("modal submitted",input);
              $modalInstance.close(input);
            };
            $scope.cancel = function(){
              $modalInstance.dismiss('cancel');
            };
            
          }
        });
        modalInstance.result.then(function (input) {
          if(input){
            $scope.current_app = vmxappcode.new_app(input).then(function(val){
              $scope.current_app = val;
              flush_app();
            });
          }
        });
      };

      $scope.delete_file = function(index){
        if(index === $scope.current_file_index){
          set_current_buffer('callback');
        }
        delete $scope.current_app.value.files[index]; 
        vmxappcode.delete_file(index);
        vmxappcode.save_app();
      };

      $scope.new_file_modal = function() {
        var  modalInstance = $modal.open({
          templateUrl: 'static/partial/file_create_modal.html',
          controller: function($scope,$modalInstance){
            $scope.input = '';
            $scope.ok = function(input){
              console.log("modal submitted",input);
              $modalInstance.close(input);
            };
            $scope.cancel = function(){
              $modalInstance.dismiss('cancel');
            };
            
          }
        });
        modalInstance.result.then(function (input) {
          if(input){
            $scope.modal_input = input;
            console.log($scope.files);
            $scope.files.push({name:input,body:"//file " + input});
            $scope.current_app.value.files = $scope.files;
            $scope.switch_buffer($scope.files.length - 1);
          }
        });
      };


    }//end editor directive
  };
})

//DIRECTIVE FOR SHOWING VIDEO AND HANDLING SELECTIONS
.directive('vmxVideo',['vmxboxes','vmxselector','vmxglobals', function(vmxboxes,vmxselector,vmxglobals){
  return{
    restrict: 'E',
    transclude: true,
    link: function(scope,element,attributes){
      var vname = (attributes['vname']) ? attributes['vname'] : 'vmx';
      scope.videoId = scope.toId('video',vname);
      scope.canvasId = scope.toId('canvas',vname);
      scope.canvasList = [element[0].children[0].children[1].children[0]];
      scope.vname = vname;

      scope.selection_div = element[0].children[0].children[1];

      
      // set the selection div
      vmxglobals.set_selection_div(scope.selection_div);
      
      // Here we create a temporary a/button element and steal its color
      var d = document.createElement('a');
      $(d).addClass('btn').addClass('btn-primary').css('visibility','hidden');
      document.body.appendChild(d);
      scope.color_fg = $(d).css('color');
      scope.color_bg = $(d).css('background-color');
      document.body.removeChild(d);

    },
    templateUrl: 'static/partial/video-canvas.html',
    controller: function($scope,vmxglobals,$attrs,vmxutils,vmxipcam,$modal){
      var vidWidth, vidHeight;
      console.log("vmx video invoked");
      
      $scope.g = vmxglobals.g;
      var vname = (typeof $attrs['vname'] !== undefined) ? $attrs['vname'] : 'vmx';

      $scope.toId = function(t,name){ return t.trim() + '-vmx-' + name.trim(); };

      var topleft_click = null;
      var mouse_x, mouse_y;
      var selection_mode = null;
  

      $scope.player = [];



      var videoId = $scope.toId('video',vname);
      var canvasId = $scope.toId('canvas',vname);

      //$scope.canvasList = [document.getElementById(canvasId)];
      
      // Add a new canvas to the list of canvases
      $scope.addCanvas = function(new_canvas) {

        
        if (typeof(new_canvas) === 'string'){
          $scope.canvasList.push(document.getElementById(new_canvas));
        }
        else{
          $scope.canvasList.push(new_canvas);
        }

        $scope.canvasList[$scope.canvasList.length-1].addEventListener("mousedown", $scope.getPosition, false);
        $scope.canvasList[$scope.canvasList.length-1].addEventListener("mousemove", $scope.savePosition, false);
        
      };

      


      $scope.post_functions = new Array(0);
      $scope.addPostFunction = function(func) {
        $scope.post_functions.push(func);
      };

      

      function adjustDrawingCanvas(){
        //set the height of the drawing canvas AND the tracker canvas
        //we do this here because when the canvas is rendered we don't know height of video
        //vidHeight = $('#'+videoId).height();
        vidWidth  = $('#'+videoId).width();
        //NOTE: the height is not the correct one, but the width is
        //always correct, so we can use the global aspect ratio and
        //figure out the height based on the width
        vidHeight = vmxglobals.g.height / vmxglobals.g.width * vidWidth;
        //console.log('adjustDrawingCanvas() called, setting to ',vidHeight,vidWidth);
        //try{
          for (var q = 0; q < $scope.canvasList.length; ++q) {
            //if ($scope.canvasList[q].height != vidHeight) {
              $scope.canvasList[q].height = vidHeight;
            //}
            //if ($scope.canvasList[q].width  != vidWidth) {
              $scope.canvasList[q].width  = vidWidth;
            //}
          }
          //var finished = true;
        //}catch(e){ }
      }



      //NOTE: EXPERIMENTAL MAYBE DANGEROUS
      //this is to try and make the drawing canvas be the same size as video
      $scope.$watch(function(){adjustDrawingCanvas();});

      //$scope.adjustDrawingCanvas = adjustDrawingCanvas;
      $(window).resize(function() {
        adjustDrawingCanvas();
      });

      // NOTE: to be added for training object detectors
      //   // add data div
      //   $('#vmx').append('<div id="vmxdata"></div>'); 
      // Function gets called when user allows chrome to access camera
      $scope.callbackStreamIsReady = function(stream) {    
        $scope.vid = document.getElementById(videoId);
        $scope.canvas = document.getElementById(canvasId);
        $scope.gCtx = $scope.canvas.getContext('2d');

        /*$scope.vid.setAttribute('width',320*5);
        $scope.vid.setAttribute('height',240*5);*/
        $scope.vid.src = compatibility.URL.createObjectURL(stream);
        setTimeout(function() {
        $scope.vid.play();

        adjustDrawingCanvas();

        $scope.g.is_playing = true;
        // push the is_playing state change explicitly
        $scope.$apply();
        compatibility.requestAnimationFrame($scope.draw);
        },500);
      };

      $scope.video_pause_play = function() {
        // if ($scope.player.node.paused == true) {
        //   console.log("playing it");
        //   $scope.player.node.play();
        // } else {
        //   $scope.player.node.pause();
        //   console.log("pausing it");
        // }

        if ($scope.vid.paused === true) {
          console.log("playing it");
          $scope.vid.play();
        } else {
          $scope.vid.pause();
          console.log("pausing it");
        }

          
      };

      $scope.external_source_modal = function() {
        var  modalInstance = $modal.open({
          templateUrl: 'static/partial/external_src_modal.html',
          controller: function($scope,$modalInstance){
            $scope.input = '';
            $scope.ok = function(input){
              console.log("modal submitted",input);
              $modalInstance.close(input);
            };
            $scope.cancel = function(){
              $modalInstance.dismiss('cancel');
            };
            
          }
        });

        modalInstance.result.then(function (input) {
          if(input){
              set_external_source(input);
          }
        });
      };
      

      var set_external_source = function(source){
        var encoded_src =  encodeURIComponent(source);
        vmxipcam.set_src(IPCAM_PROXY + "?src=" + encoded_src);
        var ipcanvas = document.getElementById('ipcam');
        vmxipcam.set_canvas(ipcanvas);
        vmxipcam.play();
        $scope.vid = ipcanvas;
        if(!$scope.g.is_playing){
          setup();
        }
      };

      var setup = function(){
        $scope.canvas = document.getElementById(canvasId);
        //$scope.player = new MediaElementPlayer('#'+videoId);
        $scope.gCtx = $scope.canvas.getContext('2d');
        //$scope.vid.src = URL.createObjectURL(stream);
        $scope.g.is_playing = true;
        $scope.canvas.addEventListener("mousedown", $scope.getPosition, false);
        $scope.canvas.addEventListener("mousemove", $scope.savePosition, false);

        $scope.vid.addEventListener("mousemove", function(){console.log("overhead");}, false);
        // push the is_playing state change explicitly
        compatibility.requestAnimationFrame($scope.draw);
      };

      //experimental function for trying out local file reading instead of webcam
      $scope.callbackStreamIsFail = function() {    
        
        $scope.external_source_modal();
      };


      // TODO: enable this if we want to load local video
      //setTimeout($scope.callbackStreamIsReady2,2000);
      
      // enable this line to get the webcam up and running in chrome
      if(typeof navigator.webkitGetUserMedia === 'function'){
        compatibility.getUserMedia({video:true}, $scope.callbackStreamIsReady,$scope.callbackStreamIsFail);
      }
      else{
        $scope.callbackStreamIsFail();
      }

      // Draw everything
      $scope.draw = function() {
        compatibility.requestAnimationFrame($scope.draw);

        var e;
        for (var q = 0; q < $scope.canvasList; ++q) {
          //NOTE: should store these canvases, don't need to look them up everytime.
          e = $scope.canvasList[q];
          e.getContext('2d').clearRect(0,0,e.width,e.height);
        }

        //$scope.gCtx.clearRect(0,0,$scope.canvas.width, $scope.canvas.height);

        // This line will get the image from the video and save the
        // snapshot together with its own time stamp, so that further
        // reads can simply access vmxglobals.g.last_image and
        // vmxglobals.g.last_time
        var local_canvas = vmxglobals.set_image($scope.vid);
        $scope.gCtx.drawImage(local_canvas,0,0,$scope.canvas.width,$scope.canvas.height);

        // draw the selection boxes which occur during model creation
        selection_mode = $scope.g.selection_mode;
        if (selection_mode){
          $scope.drawSelection($scope.gCtx);
        }

        // draw all bounding boxes (these are the old ones...)
        $scope.drawBoxes($scope.gCtx);

        // apply each of our "drawing" post_functions to render
        // additional things on the screen
        for (q = 0; q < $scope.post_functions.length; ++q) {
          //console.log('calling function ',q);
          //NOTE(TJM): this might hand the drawing if the post function is expensive
          $scope.post_functions[q]();
        }

        
      };

      $scope.drawBoxes = function(gctx,boxes) {
        if (!boxes){
          boxes = vmxboxes.list();
        }
        boxes.sort(function(a,b) {return a.score>b.score;});

        gctx.save();
        gctx.scale(gctx.canvas.width / vmxglobals.g.width, gctx.canvas.height / vmxglobals.g.height);
        
        var oRec;
        for (var i in boxes) {
          oRec = boxes[i];
          oRec.draw(gctx);
          // gctx.fillStyle = oRec.fill;
          // if (oRec.score < -1) {
          //   gctx.lineWidth = 5;
          // } else {
          //   gctx.lineWidth = 5;
          // }
          // gctx.strokeStyle = oRec.color;
          // gctx.strokeRect(oRec.x, oRec.y, oRec.w, oRec.h);
          // //if (oRec.score > -1) {
          //   //gctx.fillStyle = 'yellow';
          //   gctx.font="20px Courier";
          //   gctx.fillText(oRec.cls,oRec.x,oRec.y-gctx.lineWidth-3);

          //   gctx.font="10px Arial";
          //   gctx.fillText(oRec.score,oRec.x,oRec.y+gctx.lineWidth+3);
          //   //}
        }
        gctx.restore();
      };

      // Process mouse clicks
      $scope.getPosition  = function(event) {
        if (!$scope.g.selection_mode){
          return;
        }
        var x = event.clientX;
        var y = event.clientY;
        var canvas = $scope.canvas;
        console.log("click!",x,y);
        //console.log("canvas offset left: " + canvas.offsetLeft);
        x -= canvas.getBoundingClientRect().left;
        // NOTE: This is sloppy, config should be injected
        /* globals _vmxConfig: true */
        x -= _vmxConfig.columnPadding;
        y -= canvas.getBoundingClientRect().top;
        console.log("oncanvas!",x,y);
        
        if (typeof topleft_click === undefined || topleft_click === null) {
          topleft_click = {};
          topleft_click.x = x;
          topleft_click.y = y;
        } else {
          var current_image = vmxglobals.g.last_image;
          var current_time  = vmxglobals.g.last_time;
          var dataURL = vmxutils.extract_image(current_image);
          var o = {};
          o.image = dataURL;

          var scalex = vmxglobals.g.width / canvas.width;
          var scaley = vmxglobals.g.height / canvas.height;

          o.bb = new Array(topleft_click.x*scalex,topleft_click.y*scaley,x*scalex,y*scaley);

          o.time = current_time;
          //Swap coordinates if chosen in reverse
          var temp;
          if(o.bb[0] > o.bb[2]){ temp = o.bb[0]; o.bb[0] = o.bb[2]; o.bb[2] = temp; }
          if(o.bb[1] > o.bb[3]){ temp = o.bb[1]; o.bb[1] = o.bb[3]; o.bb[3] = temp; }
          console.log(o.bb);
          o.icon = vmxutils.extract_image(current_image,o.bb);
          //console.log('Selection Object is',o);
          vmxselector.add(o);
          //NOTE: WHAT IS THIS DIGEST FOR?
          $scope.$digest();
          topleft_click = null;

          // disable selection mode after each selection
          vmxglobals.disable_selection_mode();
        }
      };

      $scope.savePosition = function(event) {

        if (!$scope.g.selection_mode){
          return;
        }
        mouse_x = event.clientX;
        mouse_y = event.clientY;
        var canvas = $scope.canvas;
        mouse_x -= canvas.getBoundingClientRect().left + _vmxConfig.columnPadding;
        mouse_y -= canvas.getBoundingClientRect().top;
        // console.clear();
        // console.log('mouse_x is ' + mouse_x + ' and mouse_y is ' + mouse_y);
      };

      /* Draws the selection 
        that the user is making 
        when selecting a model 
        (should be more generic) */
      $scope.drawSelection = function(context) {

        // if a click has not beeen made yet, we just draw the
        // crosshair on the entire canvas
        if (topleft_click == null) {
          context.lineWidth = 1;
          context.strokeStyle = $scope.color_bg;

          context.beginPath();
          context.moveTo(0, mouse_y);
          context.lineTo(context.canvas.width, mouse_y);
          context.closePath();
          context.stroke();
          context.beginPath();
          context.moveTo(mouse_x,0);
          context.lineTo(mouse_x,context.canvas.height);
          context.closePath();
          context.stroke();
          return;
        }

        // draw the selection box with one corner anchored on
        // topleft_click, and the other corner dynamically updated
        // based on mouse movement
        context.lineWidth = 5;
        context.strokeStyle = $scope.color_bg;
        context.strokeRect(topleft_click.x,topleft_click.y,mouse_x-topleft_click.x,mouse_y-topleft_click.y);
        context.fillStyle = 'rgba(225,225,225,0.4)';
        context.fillRect(topleft_click.x,topleft_click.y,mouse_x-topleft_click.x,mouse_y-topleft_click.y);
      };
    
    }
  };

}])

.directive('vmxTracker', function () {
  return {
    restrict: 'E',
    require: '^vmxVideo',
    templateUrl : 'static/partial/tracker.html',
    link: function(scope,element){
      scope.trackerId = scope.toId('tracker',scope.vname);
      //scope.addCanvas(scope.trackerId);
      scope.addCanvas(element[0].children[0]);

    },
    controller: function($scope,vmxtracker,vmxglobals){

      $scope.drawTrackerBoxes = function() {
        vmxtracker.draw_all();
      };
      
      $scope.initialized = false;
      $scope.$watch('g.is_playing',function(newVal) {        
        if (newVal === true && $scope.initialized === false) {
          $scope.initialized = true;
          setTimeout(function() {
            $scope.tracker_canvas = document.getElementById($scope.trackerId);
            vmxtracker.initialize_canvas(vmxglobals.g.width,vmxglobals.g.height,$scope.tracker_canvas);
            vmxtracker.initialize_grid(vmxglobals.g.last_time);

            $scope.addPostFunction(function(){vmxtracker.tracker_update(vmxglobals.g.last_image,vmxglobals.g.last_time);});
            $scope.addPostFunction(vmxtracker.draw_all);
            
        

            //$scope.addPostFunction($scope.global_tracker.update_grids.bind($scope.global_tracker));
            //compatibility.requestAnimationFrame($scope.global_tracker.update_grids.bind($scope.global_tracker));
          },400);
          

        }
      });

      

      
    }
    
  };
});
