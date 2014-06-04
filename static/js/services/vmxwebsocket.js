/**
 * `vmxwebsockets` is a service responsible for
 * maintaining the websocket connection to the vmx server
 * Copyright VMX 2014
 */
/* globals vmxServices: true*/
vmxServices
  .factory("vmxwebsocket", function($rootScope){

    var socket;  // the websocket
    var listeners = {};  // the callback listeners
    
    /**
     * opens a websocket connection
     * @param {string} model_name    The name of the model
     */
     var connect = function(){
        console.log("Trying to connect inside service");
        var socketURI = (VMX_SERVER + "websocket").replace("http://", "ws://").replace("https://", "wss://");
        try {
          socket = new WebSocket(socketURI);
          socket.onopen = function(){
            console.log("Successful websocket to: " + socketURI);
          };
          socket.onmessage = function(event){
            var data;
            try{
              data = JSON.parse(event.data);
            } catch (e) {
              console.log("Received from websocket: " + event.data);
              console.log(e.stack);
              console.log(e.message);
            }
            
            var command = Object.keys(data)[0];
            if (typeof data[command] === "undefined") {
              console.log("No command found in websocket command");
              console.log(data);
            } else {
              if (typeof listeners[command] === "undefined"){
                console.log("No listener for " + command);
              } else{
                //We have a registered command and valid JSON, invoke the callback!
                listeners[command](data[command]);
                $rootScope.$digest();
              }
            }

          };
        } catch (e) {
          socket = false; 
        }
     };

     /**
      * registers a callback for a given command
      * @param {string} name    The name of the command
      * @param {function} callback   the function that should process the command
      */
     var listen = function(name, callback){
        console.log("Registering " + name + " for " + callback);
        listeners[name] = callback; 
     };

     var send = function(command, msg){
        console.log("About to send to websocket");
        console.log(command);
        console.log(msg);
        var out = {};
        out[command] = msg;
        socket.send(JSON.stringify(out));
     };


     var useSocket = function(){
        return false; //nah
        // return socket !== false;
     };
    
    return{
      connect: connect,
      send:    send,
      listen: listen,
      useSocket: useSocket,
    };
  });

