#!/usr/bin/env node

// NOTE: it requires running from a fresh start (instance with no model loaded)

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var url = 'http://localhost:3000';


myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  myrequest({method: 'DELETE', url:url+'/sessions/'+body.data.id},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');

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
  });
});
  });
});
});
});
