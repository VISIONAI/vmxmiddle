#!/usr/bin/env node

// A test suite for the load handler
// NOTE: it requires running from a fresh start (instance with no model loaded)
var sleep = require('sleep');
var http = require('http');
var request = require('request');
var https = require('https');
var fs = require('fs');

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

var target = process.argv[2];
//target="cat";
//target = "batman";

myrequest({method:'GET',url:"https://ajax.googleapis.com/ajax/services/search/images?v=1.0&q="+target+"&rsz=8&start=0"},function(error,response,body){
  //console.log('body is', JSON.stringify(body,4,null));

  var images = [];
  for (var i = 0; i < body.responseData.results.length; ++i) {
    var url2 = body.responseData.results[i].url;
    var width = parseFloat(body.responseData.results[i].width);
    var height = parseFloat(body.responseData.results[i].height);
    console.log('url2 is', url2);
    var objects = [{name:target,bb:[1,1,height,width]}];




    var filer = "out/file"+i+".jpg";
    console.log('writing '+filer);
    request(url2).pipe(fs.createWriteStream(filer));
    //sleep.sleep(1);
    var url3 = __dirname+'/'+filer;
    images[i] = {image:url3,objects:objects};

    //var file = fs.createWriteStream(filer);
    //var request = http.get(url2, function(response) {
    //  response.pipe(file);
    //});

  }

myrequest({method: 'POST', url:url+'/sessions',json:{id:"tester"}},function(error, response, body) {
  //assert.equal(response.statusCode, 200, 'Problem with status code');
  //var id = body.data.id;
  var id = "tester";


  myrequest({method: 'POST', url:url+'/sessions/'+id+'/create', json:{name:target,images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');

  myrequest({method: 'POST', url:url+'/sessions/'+id+'/save', json:{}}, function(error, response, body) {
});
});
});
});


