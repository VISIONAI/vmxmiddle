/**
 * `vmxconnections` is a service responsible for
 * opening/closing/listing connections between an angularjs front-end
 * app and the back-end server
 * Copyright VMX 2014
 */
/* globals vmxServices: true*/
vmxServices
  .factory("vmxconnections", function($q,$http,vmxwebsocket){
    var sessions = [];
    var resource = "session";
    var path = VMX_SERVER + resource;
    
    /**
     * Creates a new connection and returns a promise
     * @param {string} model_name    The name of the model
     */
    var create = function(model_name){
      var deferred = $q.defer();
      var req = {model_name: model_name};
      if (vmxwebsocket.useSocket()){
        console.log("Using websocket");
        var command = "new_connection";
        vmxwebsocket.send(command, req);
        vmxwebsocket.listen(command, function(response){
          console.log("we called the listener for command " + command);
          console.log(response);
          deferred.resolve(response.data.replace('sessions/',''));
        });
      } else {
          console.log("not using websocket");
        $http.post(path,req).success(function(response){
          deferred.resolve(response.data.replace('sessions/',''));
        });
      }
      return deferred.promise;
    };

    /**
     * Update and list the available connections from the server,
     * Returns a promise, and updates sessions
     */
    var update = function() { 

      var deferred = $q.defer();
      $http.get(path)
        .success(function(response){
          if(response.error) { 
            console.log(response); 
            return;
          }
          for (var i = 0; i < response.data.length; ++i) {
            sessions[i] = { id     : response.data[i].session,
                            model  : response.data[i].model,
                            status : 0};
          }
          sessions.splice(response.data.length);
          deferred.resolve(sessions);
        });
      return deferred.promise;
    };
    
    /** 
     * List the cached sessions, note that when sessions gets updated
     * after a subsequent vmxconnections.update() call, it will have
     * the new values as long as it is not copied
     */
    var list_cached = function() {
      return sessions;
    };
    
    return{
      create: create, 
      update: update, 
      list_cached: list_cached
    };
  });

