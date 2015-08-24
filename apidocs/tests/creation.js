#!/usr/bin/env node

// NOTE: it requires running from a fresh start (instance with no model loaded)

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var url = 'http://localhost:3000';

myrequest({method: 'GET', url:url+'/session'},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  body = JSON.parse(body);
  assert.equal(body.data.length,0,'Not starting with 0 sessions');

  myrequest({method: 'POST', url:url+'/session'},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');
    
    myrequest({method: 'GET', url:url+'/session'},function(error, response, body) {
      assert.equal(response.statusCode, 200, 'Problem with status code');
      body = JSON.parse(body);
      assert.equal(body.data.length,0,'Not starting with 0 sessions');

      myrequest({method: 'POST', url:url+'/session',json:{}},function(error, response, body) {
        assert.equal(response.statusCode, 200, 'Problem with status code');

      myrequest({method: 'POST', url:url+'/session',json:{random_stuff:"truly_random"}},function(error, response, body) {
        assert.equal(response.statusCode, 200, 'Problem with status code');
        console.log('body is', body);
        var id = body.data.id;
        
        myrequest({method: 'GET', url:url+'/session/'+id},function(error, response, body) {
          assert.equal(response.statusCode, 200, 'Problem with status code');
          
        myrequest({method: 'GET', url:url+'/session/XXX'+id},function(error, response, body) {
          assert.equal(response.statusCode, 404, 'Problem with status code');
          


        myrequest({method: 'GET', url:url+'/session/'+id+'/'},function(error, response, body) {
          assert.equal(response.statusCode, 200, 'Problem with status code');
          



          myrequest({method: 'POST', url:url+'/session',json:{id:234}},function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
            
            myrequest({method: 'POST', url:url+'/session',json:{"id":234}},function(error, response, body) {
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
