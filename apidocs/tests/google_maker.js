#!/usr/bin/env node

// A test suite for the load handler
// NOTE: it requires running from a fresh start (instance with no model loaded)

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

var vmx_dir = '/tmp/blank/';
var session_id = 'temp-session2';

var url = 'http://localhost:3000';
var local_url = url;
var main_url = url;

console.log('URL: '+url);
console.log('Testing simple creation from a blank slate');

require('./vmxtester').myexec("rm -rf "+vmx_dir+"/models/*");


myrequest({method:'GET',url:"https://ajax.googleapis.com/ajax/services/search/images?v=1.0&q=cat&rsz=8&start=0"},function(error,response,body){
  //console.log('body is', JSON.stringify(body,4,null));

  var images = [];
  for (var i = 0; i < body.responseData.results.length; ++i) {
    var url2 = body.responseData.results[i].url;
    var width = body.responseData.results[i].width;
    var height = body.responseData.results[i].height;
    console.log('url2 is', url2);
    var objects = [{name:"cat",bb:[1,1,height,width]}];
    images[i] = {image:url2,objects:objects};
  }

myrequest({method: 'POST', url:url+'/sessions',json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  var id = body.data.id;

  myrequest({method: 'POST', url:url+'/'+id+'/create', json:{name:"face2",images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');
    assert.equal(body.data.model.num_pos,1,'num_pos is not one');        

});

});
