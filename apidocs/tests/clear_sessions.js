#!/usr/bin/env node

var url = 'http://localhost:'+3000;

console.log('URL: '+url);
console.log('Clearing sessions');

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

myrequest({method: 'DELETE', url:url+'/sessions/bad_evil_id'},function(error, response, body) {
  assert.equal(response.statusCode, 404, 'Problem with status code');

  myrequest({method: 'GET', url:url+'/sessions'},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    var sessions = body.data;
    
    myrequest({method: 'DELETE', url:url+'/sessions/bad_evil_id'},function(error, response, body) {
      assert.equal(response.statusCode, 404, 'Problem with status code');
      
      for (var i = 0; i < sessions.length; ++i) {

        var id = sessions[i].id;
        
        myrequest({method: 'GET', url:url+'/sessions/'+id},function(error, response, body) {
          assert.equal(response.statusCode, 200, 'Problem with status code');

          var newid = body.data.id;
          myrequest({method: 'DELETE', url:url+'/sessions/'+newid},function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');

            var newerid = body.data.id;
            myrequest({method: 'GET', url:url+'/sessions/'+newerid},function(error, response, body) {
              assert.equal(response.statusCode, 404, 'Problem with status code');
              
            });
          });
          
        });
      } // end loop
    });
  });  
});
