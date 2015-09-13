#!/usr/bin/env node

// A test suite for the load handler
// NOTE: it requires running from a fresh start (instance with no model loaded)

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var vmx_dir = '/tmp/blank/';

//var url = 'http://localhost:8081';
var url = 'http://localhost:3000';

var local_url = url;
var main_url = url;

console.log('URL: '+url);
console.log('Testing simple creation from a blank slate');

var is400ish = require('./vmxtester').is400ish;

myrequest({method: 'GET', url:url+'/models',json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  console.log('models are ',body.data);
  var uuids = [];
  var c = 0;
  for (var i= 0; i < body.data.length; ++i) {
    //console.log('i is', i);
    if (body.data[i].compiled == false) {
      uuids[c] = body.data[i].uuid;
      c++;
    }
  }
  console.log('uuids len is ', uuids.length);

  
  var id = 'tester';

myrequest({method: 'POST', url:url+'/sessions',json:{id:id}},function(error, response, body) {

myrequest({method: 'POST', url:url+'/sessions/'+id+'/load',json:{uuids:uuids}},function(error, response, body) {

});

  });

});

