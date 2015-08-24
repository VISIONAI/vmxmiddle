#!/usr/bin/env node

var url = 'http://localhost:'+3000;

console.log('URL: '+url);
console.log('Clearing sessions');

var assert = require('assert');
var myrequest = require('./vmxtester').myrequest;

myrequest({method: 'GET', url:url+'/session'},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  body = JSON.parse(body);

  for (var i = 0; i < body.data.length; ++i) {
    myrequest({method: 'DELETE', url:url+'/session/'+body.data[i].id},function(error, response, body) {
      assert.equal(response.statusCode, 200, 'Problem with status code');
      //body = JSON.parse(body);
      //console.log('body is', body);
    });
  }
});

