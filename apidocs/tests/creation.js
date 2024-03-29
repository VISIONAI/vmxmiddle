#!/usr/bin/env node

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var url = 'http://localhost:3000';

myrequest({method: 'GET', url:url+'/sessions'},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  assert.equal(body.data.length,0,'Not starting with 0 sessions');
  
  myrequest({method: 'POST', url:url+'/sessions'},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');
    
    myrequest({method: 'GET', url:url+'/sessions'},function(error, response, body) {
      assert.equal(response.statusCode, 200, 'Problem with status code');
      assert.equal(body.data.length,0,'Not starting with 0 sessions');
      
      myrequest({method: 'POST', url:url+'/sessions',json:{}},function(error, response, body) {
        assert.equal(response.statusCode, 200, 'Problem with status code');
        
        myrequest({method: 'POST', url:url+'/sessions',json:{random_stuff:"truly_random"}},function(error, response, body) {
          assert.equal(response.statusCode, 200, 'Problem with status code');
          
          var id = body.data.id;        
          myrequest({method: 'GET', url:url+'/sessions/'+id},function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
            
            myrequest({method: 'GET', url:url+'/sessions/XXX'+id},function(error, response, body) {
              assert.equal(response.statusCode, 404, 'Problem with status code');
              
              myrequest({method: 'GET', url:url+'/sessions/'+id+'/'},function(error, response, body) {
                assert.equal(response.statusCode, 200, 'Problem with status code');
                
                myrequest({method: 'POST', url:url+'/sessions',json:{id:234}},function(error, response, body) {
                  assert.equal(response.statusCode, 400, 'Problem with status code');
                  
                  myrequest({method: 'POST', url:url+'/sessions',json:{"id":234}},function(error, response, body) {
                    assert.equal(response.statusCode, 400, 'Problem with status code');
                  });               
                });
              });
            });
          });
        });
      });
    });    
  });
});
