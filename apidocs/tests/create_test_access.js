#!/usr/bin/env node

// NOTE: it requires running from a fresh start (instance with no model loaded)

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var url = 'http://localhost:3000';


//myrequest({method: 'POST', url:url+'/sessions', json:{}},function(error, response, body) {
//  assert.equal(response.statusCode, 200, 'Problem with status code');  

myrequest({method: 'GET', url:url+'/sessions', json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');  
  console.log('body is', body.data)
  var id = body.data[0].id;

  myrequest({method: 'GET', url:url+'/sessions/'+id, json:{}},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');  

  myrequest({method: 'POST', url:url+'/sessions/'+id, json:{}},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');  
    console.log('body is', body)
    assert.equal(body.error.indexOf("ProcessImage:Images field missing or empty"),0,'wrong output');

  myrequest({method: 'POST', url:url+'/sessions/'+id, json:{images:[]}},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');  
    assert.equal(body.error.indexOf("ProcessImage:Images field missing or empty"),0,'wrong output');

  myrequest({method: 'POST', url:url+'/sessions/'+id, json:{images:[]}},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');  
    assert.equal(body.error.indexOf("ProcessImage:Images field missing or empty"),0,'wrong output');

  myrequest({method: 'POST', url:url+'/sessions/'+id, json:{images:[{image:"someurl"}]}},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');  
    console.log('body is', body);
    assert.equal(body.error.indexOf("Process Image Failure (Empty Model)"),0);

  myrequest({method: 'GET', url:url+'/models', json:{}},function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');  
    //console.log('body is', body);
    var model = body.data[0];
    var uuid = model.uuid;

  myrequest({method: 'POST', url:url+'/sessions/'+id+'/load', json:{uuids:[uuid]}},function(error, response, body) {
    console.log('body is', body);
    assert.equal(response.statusCode, 200, 'Problem with status code');  

  myrequest({method: 'POST', url:url+'/sessions/'+id, json:{images:[{image:"someurl"}]}},function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');  
    console.log('body is', body);
    assert.equal(body.error.indexOf("Process Image Failure (Empty Model)"),0);

});

});
    
});

});

});


    //assert.equal(body.error[0].indexOf("key \"images\" not present"),0,'wrong output');

});

  });
  });
});



