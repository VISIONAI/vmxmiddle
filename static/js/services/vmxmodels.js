/**
 * `vmxmodels` is a service responsible for creating new models and
 * listing models available for loading from the server 
 * 
 * Copyright VMX 2014
 */
/* globals vmxServices: true*/
vmxServices
  .factory("vmxmodels", function($q,$http,$modal){
    var cached_models = [];

    var resource = "model";
    var path = VMX_SERVER + resource;

    /**
     * REST interface looks like
     *-----
     * GET  /model          ->   get_models()
     * POST /model          -> create_model()
     * PUT  /model/#modelId -> save_current()
     */

    /**
     * Get a list of models promise from the server
     */
    var get_models = function(){
      var deferred = $q.defer();  
      console.log('get_models start');
      $http.get(path).success(function(response){
        for (var i = 0; i < response.data.length; ++i) {
          cached_models[i] = response.data[i];
        }
        cached_models.splice(response.data.length);
        deferred.resolve(response.data);
        console.log('get_models end');
      }).error(function(data,status,headers,config){
        console.log("error in get_models");
        console.log([data,status,headers,config]);
      });
      return deferred.promise;
    };
    
    /**
     * Create a model in a given session
     * @param {array}  curr_selections   Selections and images
     * @param {string} model_name        The name of the model
     * @param {string} session_id        A given session_id
     * @param {object} params            The creation parameters
     */
    var create_model = function(curr_selections,model_name,session_id,params) {
      var deferred = $q.defer();
      var cs = angular.copy(curr_selections);
      for(var i=0; i< cs.length; ++i){
        delete cs[i].$$hashKey;
        //NOTE: is this necessary?, maybe we can limit the payload sent to server
        //delete cs[i].image;
      }
      var req = {command: 'create_model', 
                 selections: cs, 
                 params:params , 
                 session_id:  session_id,
                 cls:model_name};
      
      $http.post(path,req).success(function(response){
        console.log("successful response from create_model is " + response);
        deferred.resolve(response);        
      });
      return deferred.promise;
    };
    

    //Creates a modal with all the models, and fires `callback` with the result
    var model_modal = function(callback) {
      var modalInstance = $modal.open({
        templateUrl: 'static/partial/model_modal.html',
        windowClass: ['vmx-wide-modal'],
        controller: function($scope,$modalInstance,vmxutils){
          get_models().then(function(data){
            $scope.models = data;
          });
          $scope.choose = function(session){
            $modalInstance.close(session);
          };
          $scope.cancel = function(){
            $modalInstance.dismiss('cancel');
          };
          $scope.unix_to_date = vmxutils.unix_to_date;
          
        },
      });
      modalInstance.result.then(function (model) {
        if(model) {
          callback(model); 
        }
      });
    };

    // List the cached models
    function list_cached() {
      return cached_models;
    }

    /**
     * Save the model inside the given session_id to the server
     * @param {string} session_id        A given session_id
     */
    var save_current = function(session_id){
      var deferred = $q.defer();
      var req = {session_id: session_id};
      console.log("before save model call");
      $http.put(path,req).success(function(respond){
        console.log("after save model return");
        if (respond.error && respond.error===1) {
          console.log('Error saving model inside session id',session_id);
        } else {
          // after saving the model, update the models listing
          get_models();

          //NOTE: the save_model command can actually return
          //information about the saved model, that way we don't
          //really have to call get_models and process the bigger and
          //computationally more expensive request
        }
        deferred.resolve();
      }); 
      console.log("after save model call");
      return deferred.promise;
    };

    return{
      list: get_models, 
      list_cached: list_cached,
      create: create_model, 
      modal: model_modal,
      save_current: save_current
    };
  });

