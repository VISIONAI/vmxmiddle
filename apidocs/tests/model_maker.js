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

require('./vmxtester').myexec("rm -rf "+vmx_dir+"/models/*");
var is400ish = require('./vmxtester').is400ish;

var imageurl = '/Users/tomasz/projects/VMXserver/tests/testapi/tomasz_blue_crop.jpg';

imageurl = 'http://people.csail.mit.edu/tomasz/img/tomasz_blue_crop.jpg';
myrequest({method: 'POST', url:url+'/sessions',json:{}},function(error, response, body) {
  assert.equal(response.statusCode, 200, 'Problem with status code');
  var id = body.data.id;
  url = url+'/sessions/'+id;
  local_url = url;


  myrequest({method: 'POST', url:url+'/config', json:{config:{read_only: false} }}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');

myrequest({method: 'POST', url:url+'/load', json:{uuids:["none"]}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');


myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
            assert.equal(body.error.length>0,1,'Error is not 1');
myrequest({method: 'POST', url:url+'/process', json:{images:[{"image":"someurl"}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');

myrequest({method: 'POST', url:url+'/save', json:{name:[]}}, function(error, response, body) {
  assert.equal(response.statusCode, 400, 'Problem with status code');
  //assert.equal(body.message.indexOf('Expected a string')>-1,1);


  myrequest({method: 'GET', url:url+'/params', json:{}}, function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');



myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
  assert.equal(response.statusCode, 400, 'Problem with status code');

  myrequest({method: 'POST', url:local_url+'/config', json:{config:{pretrained: []} }}, function(error, response, body) {
    assert.equal(response.statusCode, 400, 'Problem with status code');
  
  myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: true }}}, function(error, response, body) {

    assert.equal(response.statusCode, 200, 'Problem with status code');

    myrequest({method: 'POST', url:url+'/load', json:{uuids:['tom']}}, function(error, response, body) {
      assert.equal(response.statusCode, 400, 'Problem with status code');
      //assert.equal(body.message.indexOf('read_only==true')>-1,1,'read only error message');
    
    myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
      assert.equal(response.statusCode, 400, 'Problem with status code');
      
      myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: false} }}, function(error, response, body) {
        assert.equal(response.statusCode, 200, 'Problem with status code');

        // check that we are starting out with no model
        myrequest({method: 'GET', url:main_url+'/models', json:{}}, function(error, response, body) {
          assert.equal(response.statusCode, 200, 'Problem with status code');

          assert.equal(body.data.length,0,'initial models are not empty');
          

          var images = [{"image":imageurl,"objects":[{"name":"face","bb":[100,100,300,300]}]}];

  myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: true} }}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');


          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
          
  myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: false }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');


          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'invalid_pretrained'}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
          
          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'https://files.vision.ai/vmx/pretrained/3f61ce5c7642bc2f24f7286f600b3e6b'}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');

          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');


          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'109e8c80074201cda9cfe4d167868337',params:{initialize_add_flip:true}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');

            assert.equal(body.data.model.num_pos,1,'num_pos is not one');                                           

            myrequest({method: 'POST', url:url+'/process', json:{images:[{"image":imageurl}],params:{learn_mode:true,learn_threshold:10}}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 

              assert.equal(body.data.model.num_pos,1,'num_pos is still one after learning mode');


            myrequest({method: 'POST', url:url+'/process', json:{images:[{"image":imageurl}],params:{learn_mode:true,learn_threshold:-10}}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 
              assert.equal(body.data.model.num_pos,2,'num_pos is two after learning mode');

  myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: true }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');



            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}],params:{learn_mode:true,learn_threshold:-10}}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 
              //assert.equal(body.data.model.num_pos,2,'num_pos is three after learning mode');

  myrequest({method: 'POST', url:local_url+'/config', json:{config:{read_only: false }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');



            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}],params:{learn_mode:true,learn_threshold:-10}}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 
              assert.equal(body.data.model.num_pos,3,'num_pos is three after learning mode');

            myrequest({method: 'GET', url:url+'/params', json:{}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              assert.equal(body.data.learn_mode,true,'get params is now in learning mode');

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 
              assert.equal(body.data.model.num_pos,3,'num_pos is not three after learning mode');

            myrequest({method: 'GET', url:url+'/params', json:{}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              assert.equal(body.data.learn_mode,false,'get params is now in learning mode');

          myrequest({method: 'POST', url:url+'/create', json:{name:"missing",
                      images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');



          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');

            assert.equal(body.data.model.num_pos,1,'num_pos is not one');                                           

            myrequest({method: 'POST', url:url+'/process',json:{}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
          

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

            myrequest({method: 'POST', url:url+'/process',json:{params:{detect_add_flip:true},images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');


  myrequest({method: 'POST', url:local_url+'/config',json:{config:{display_images: true ,log_memory:true }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');

  myrequest({method: 'GET', url:local_url+'/config', json:{}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');
    assert.equal(body.data.display_images,true);
    assert.equal(body.data.log_memory,true);


            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');


  myrequest({method: 'POST', url:local_url+'/config',json:{config:{display_images: false, log_memory:false }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');

  myrequest({method: 'GET', url:local_url+'/config', json:{}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');
    assert.equal(body.data.display_images,false);
    assert.equal(body.data.log_memory,false);

            myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              var valid_uuid = body.data.model.uuid;

            myrequest({method: 'POST', url:url+'/save', json:{new_uuid:true}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              var new_uuid = body.data.model.uuid;
              assert.notEqual(valid_uuid,new_uuid,'New uuid should be different');

            myrequest({method: 'POST', url:url+'/load', json:{uuids:[new_uuid,new_uuid]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              assert.equal(body.data.model.meta,'face,face','names not matching');

              var combined_uuid = body.data.model.uuid;

            myrequest({method: 'POST', url:url+'/save', json:{uuids:[new_uuid,new_uuid]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

            myrequest({method: 'POST', url:url+'/save', json:{uuids:[new_uuid,new_uuid]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');



            myrequest({method: 'GET', url:url+'/params', json:{}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');


            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

var dataurl='data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCABOADYDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD3BB8gqve3UdrCzuQAKsKcR1554114QyNbK3PQ0JXGVde8dSwsUtlA96xbTxtdzTKru5yewrFdvtic4BbkE1LHD9nZZdqnHHFbqKSJPUNH15pQm9sqeuetdUGV0DKcg14fF4jMN8MnABwR7V6t4a1EX1jgNnb0rFoo11Ub2yBRQx2npRSAHkCQnPpXkPjOzea/kmjIOTkfLmvTL+6CxOmQCTiuR1WJG5I/LpVR3EzzuMyQoBI6Lj1UipI5hLEFEoOM/dNbVzZ+e7YJI+tRafbxxs6hWOB/drZsk5hrJ5bvI8zGfSvYfBC/ZbXBJPyiuUtrcb/uEggdRXUaddG3nEIwMgdKyk7jR2hGTu9aKis5/Ntw3WisyjmdadhIcdmGazpJlMMjZ5xWp4jj24boOfx4rDkjYWxKgnOOtUhFW2hE8vzbSvOaka0W2w0eADSWEZUsxz1NTXEhbYo/H8qpsRVfKMPnPIHSktbknWAv+yBS3ALFQo5GKq2gY3rNjawYCpGekaEGa0OemaKn0If8S1D3NFSMxPFblLVCo55rl21Q28ILjOQOPwrq/FhK2yBeucVwFzIC4SQ55qgNy2vIJoshcEnNJNLCoBJBINZEMLOuYmIx2NK1tP8Ax8k8gCgRcjuVabPGNtRxbnvGKnOSCf1qm0i20p8wHIUU+zvM3AGActkn0oQHqOgv/wAS5AeOKKq6HdK9ruA4x2opDuZ3iJxMxVWxtB/OuEe0nkuNhVSB/FXo17aiQYOOT1rMawRXK4GMZNIZkaVYbo98pOAdoAreFpA0WEQE4xSiMQW+FHPNS2sR25zx6UyTjtbsBHdZB+Vjz7Uyy05VjEi4IJArotUskYkn73rVK1hb7OwG3g4pFG/oCslpjgAdM0Vb02AQ24Gc5FFMR//Z';

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":dataurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

var dataurl='/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwhMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjL/wAARCABOADYDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD3BB8gqve3UdrCzuQAKsKcR1554114QyNbK3PQ0JXGVde8dSwsUtlA96xbTxtdzTKru5yewrFdvtic4BbkE1LHD9nZZdqnHHFbqKSJPUNH15pQm9sqeuetdUGV0DKcg14fF4jMN8MnABwR7V6t4a1EX1jgNnb0rFoo11Ub2yBRQx2npRSAHkCQnPpXkPjOzea/kmjIOTkfLmvTL+6CxOmQCTiuR1WJG5I/LpVR3EzzuMyQoBI6Lj1UipI5hLEFEoOM/dNbVzZ+e7YJI+tRafbxxs6hWOB/drZsk5hrJ5bvI8zGfSvYfBC/ZbXBJPyiuUtrcb/uEggdRXUaddG3nEIwMgdKyk7jR2hGTu9aKis5/Ntw3WisyjmdadhIcdmGazpJlMMjZ5xWp4jj24boOfx4rDkjYWxKgnOOtUhFW2hE8vzbSvOaka0W2w0eADSWEZUsxz1NTXEhbYo/H8qpsRVfKMPnPIHSktbknWAv+yBS3ALFQo5GKq2gY3rNjawYCpGekaEGa0OemaKn0If8S1D3NFSMxPFblLVCo55rl21Q28ILjOQOPwrq/FhK2yBeucVwFzIC4SQ55qgNy2vIJoshcEnNJNLCoBJBINZEMLOuYmIx2NK1tP8Ax8k8gCgRcjuVabPGNtRxbnvGKnOSCf1qm0i20p8wHIUU+zvM3AGActkn0oQHqOgv/wAS5AeOKKq6HdK9ruA4x2opDuZ3iJxMxVWxtB/OuEe0nkuNhVSB/FXo17aiQYOOT1rMawRXK4GMZNIZkaVYbo98pOAdoAreFpA0WEQE4xSiMQW+FHPNS2sR25zx6UyTjtbsBHdZB+Vjz7Uyy05VjEi4IJArotUskYkn73rVK1hb7OwG3g4pFG/oCslpjgAdM0Vb02AQ24Gc5FFMR//Z';

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":dataurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

var dataurl='sdfaalajsdfla;sjdfa;lsdfkjadslfkjasdflkj';

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":dataurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');

            myrequest({method: 'POST', url:url+'/load', json:{uuids:[new_uuid,combined_uuid]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
              assert.equal(body.data.model.history.length,3,'history length is not 3');
              assert.equal(body.data.model.meta,'face,face,face','names not matching');

            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');

            myrequest({method: 'POST', url:url+'/load', json:{uuids:[new_uuid,""]}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              //var new_uuid = body.data.model.uuid;
              //assert.notEqual(valid_uuid,new_uuid,'New uuid should be different');

            myrequest({method: 'POST', url:url+'/save', json:{name:" "}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              //assert.equal(body.message.indexOf('Expected a string')>-1,1);


            myrequest({method: 'POST', url:url+'/save', json:{name:"tom spaces"}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);


            myrequest({method: 'POST', url:url+'/save', json:{name:"tom/spaces"}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);


            myrequest({method: 'POST', url:url+'/save', json:{name:"tom\\spaces"}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);



            myrequest({method: 'POST', url:url+'/save', json:{name:"."}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);
              

            myrequest({method: 'POST', url:url+'/save', json:{name:".."}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);
              


            myrequest({method: 'POST', url:url+'/save', json:{name:"..."}}, function(error, response, body) {
              assert.equal(response.statusCode, 400, 'Problem with status code');
              assert.equal(body.error.indexOf('Name must not have spaces')>-1,1);
              

              
              myrequest({method: 'POST', url:url+'/load', json:{uuids:[valid_uuid]}}, function(error, response, body) {
                assert.equal(response.statusCode, 200, 'Problem with status code');
                
                var myjson = {uuids:[valid_uuid],compiled:true};
                //console.log('myjson is',myjson);
                myrequest({method: 'POST', url:url+'/load', json:myjson}, function(error, response, body) {
                  assert.equal(response.statusCode, 200, 'Problem with status code');
                  assert.equal(body.data.model.compiled,true,'just loaded a compiled model');

                  myrequest({method:'POST',url:url+'/save',json:{}},function(error,response,body) {

                    assert.equal(response.statusCode, 200, 'problem with status code');

                  myrequest({method:'POST',url:url+'/save',json:{}},function(error,response,body) {

                    assert.equal(response.statusCode, 200, 'problem with status code');

                    

                  //return
      
                    myrequest({method: 'POST', url:local_url+'/config',json:{config:{read_only: false }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');


            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}],params:{learn_mode:true,learn_threshold:-10}}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code'); 


  myrequest({method: 'POST', url:local_url+'/config',json:{config:{read_only: false} }}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');


                  
            myrequest({method: 'POST', url:url+'/process',json:{images:[{"image":imageurl}]}}, function(error, response, body) {
              assert.equal(response.statusCode, 200, 'Problem with status code');
                  
                  
                  myrequest({method: 'GET', url:main_url+'/models', json:{}}, function(error, response, body) {
                    assert.equal(response.statusCode, 200, 'Problem with status code');
                    //assert.equal(body.error,0,'Error is not 0');
                    assert.equal(body.data.length,4,'initial models should be four');
                    
                    
                    

                    var images = [{"image":imageurl,"objects":[{"name":"face2","bb":[100,100,200,200]}]}];
                    
                    myrequest({method: 'POST', url:url+'/create', json:{name:"face2",images:images,pretrained:'109e8c80074201cda9cfe4d167868337'}}, function(error, response, body) {
                      assert.equal(response.statusCode, 200, 'Problem with status code');
                      //assert.equal(body.error,0,'Error is not 0');
                      assert.equal(body.data.model.num_pos,1,'num_pos is not one');                                           
                      
                      
                      myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
                        assert.equal(response.statusCode, 200, 'Problem with status code');
                        
                        
                        myrequest({method: 'GET', url:main_url+'/models', json:{}}, function(error, response, body) {
                          assert.equal(response.statusCode, 200, 'Problem with status code');
                          //assert.equal(body.error,0,'Error is not 0');
                          assert.equal(body.data.length,5,'initial models should be five');
                          // now we get the 'face' and 'face2' models
                          var uuid1 = '';
                          var uuid2 = '';
                          for (var i = 0; i < body.data.length; ++i) {
                            if (body.data[i].name == 'face') {
                              uuid1 = body.data[i].uuid;
                            }
                            if (body.data[i].name == 'face2') {
                              uuid2 = body.data[i].uuid;
                            }
                          }
                          var uuids = [uuid1,uuid2];
                          
                          
                          myrequest({method: 'POST', url:url+'/load', json:{uuids:uuids}}, function(error, response, body) {
                            assert.equal(response.statusCode, 200, 'Problem with status code');
                            
                            
                            myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
                              assert.equal(response.statusCode, 200, 'Problem with status code');
                              var uuid = body.data.model.uuid;
                              //console.log('BUNDLE uuid is',uuid);
                              
                              
                              myrequest({method: 'POST', url:url+'/load', json:{uuids:[uuid],compiled:false}}, function(error, response, body) {
                                assert.equal(response.statusCode, 200, 'Problem with status code');
                                
                                
                                myrequest({method: 'POST', url:url+'/load', json:{uuids:["XXX"],compiled:false}}, function(error, response, body) {  
                                  assert.equal(response.statusCode, 400, 'Problem with status code');
                                  
                                  myrequest({method: 'POST', url:url+'/load', json:{uuidser:["XXX"],compiled:false}}, function(error, response, body) {  
                                    assert.equal(response.statusCode, 400, 'Problem with status code');

                                    // now we test for saving when directory is fuct

                                    myrequest({method: 'POST', url:url+'/load', json:{uuids:[uuid],compiled:false}}, function(error, response, body) {
                                      assert.equal(response.statusCode, 200, 'Problem with status code');
                                
                                      var d1 = vmx_dir+"/models/"+uuid;
                                      var d2 = d1+"XXX";
                                      //var estring = "mv "+d1+" "+d2;
                                      //console.log('estring is',estring);
                                      require('./vmxtester').myexec("mv "+d1+" "+d2);
                                      require('./vmxtester').myexec('touch '+d1);


                                      myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
                                        assert.equal(response.statusCode, 200, 'Problem with status code');
                                        
                                        require('./vmxtester').myexec('rm '+d1);
                                        require('./vmxtester').myexec("mv "+d2+" "+d1);


                                      myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
                                        assert.equal(response.statusCode, 200, 'Problem with status code');
                                        

                                        myrequest({method: 'POST', url:url+'/bad_command', json:{}}, function(error, response, body) {
                                          assert.equal(is400ish(response.statusCode), 1, 'Problem with status code');

                                        myrequest({method: 'POST', url:url+'/bad_command', json:{}}, function(error, response, body) {
                                          assert.equal(is400ish(response.statusCode), 1, 'Problem with status code');


                                        myrequest({method: 'POST', url:url+'/hi', json:'hi'}, function(error, response, body) {
                                          assert.equal(is400ish(response.statusCode), 1, 'Problem with status code');

                                        myrequest({method: 'POST', url:url+'/hi bob', json:'hi , bob, there'}, function(error, response, body) {
                                          assert.equal(is400ish(response.statusCode), 1, 'Problem with status code');

                                        myrequest({method: 'POST', url:url}, function(error, response, body) {
                                          assert.equal(response.statusCode, 400, 'Problem with status code');



          var images = [{"image":imageurl,"objects":[{"name":"face","bb":[100,100,300,300]}]}];
          
          myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'3f61ce5c7642bc2f24f7286f600b3e6b'}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
                        //assert.equal(body.error,0,'Error is not 0');
            assert.equal(body.data.model.num_pos,1,'num_pos is not one');                                           

                      
                      myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
                        assert.equal(response.statusCode, 200, 'Problem with status code');



var images = [{"image":imageurl,"objects":[{"name":"face","bb":[100,100,300,300]}]}];

myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'109e8c80074201cda9cfe4d167868337',params:{initialize_add_flip:true}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
//            assert.equal(body.error,0,'Error is not 0');
            assert.equal(body.data.model.num_pos,1,'num_pos is not one');                             

    //                    console.log('body data is ', body.data.model);


myrequest({method: 'POST', url:url+'/save', json:{}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
//            assert.equal(body.error,0,'Error is not 0');
            var summary = body.data.model;
  //console.log('summary is', summary);


myrequest({method: 'POST', url:url+'/edit', json:{}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
            //assert.equal(body.error,1,'Error is not 1');

myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
            //assert.equal(body.error,0,'Error is not 0');
            var good_images = body.data.images;
myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:1}],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');

myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:1,class_label:10}],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');


myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:1,class_label:1}],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
            assert.equal(body.data.images.length,good_images.length);

myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:1,class_label:-1}],settings:{max_negatives:1}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
            
myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:2,class_label:1}],settings:{max_negatives:0, max_positives:3}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');

            assert.equal(body.data.images.length,2);



  myrequest({method: 'POST', url:local_url+'/config',json:{config:{read_only: true }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
//    assert.equal(body.error,0,'Error is not 0');

myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
  //          assert.equal(body.error,1,'Error is not 1');

  myrequest({method: 'POST', url:local_url+'/config',json:{config:{read_only: false }}}, function(error, response, body) {
    assert.equal(response.statusCode, 200, 'Problem with status code');
    //assert.equal(body.error,0,'Error is not 0');

myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
      //      assert.equal(body.error,0,'Error is not 0');


myrequest({method: 'POST', url:url+'/load', json:{uuids:[summary.uuid],compiled:true}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
        //    assert.equal(body.error,0,'Error is not 0');

myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{}}}, function(error, response, body) {
            assert.equal(response.statusCode, 400, 'Problem with status code');
          //  assert.equal(body.error,1,'Error is not 1');


myrequest({method: 'POST', url:url+'/create', json:{name:"face",
                      images:images,pretrained:'3f61ce5c7642bc2f24f7286f600b3e6b',params:{initialize_add_flip:true}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
         //   assert.equal(body.error,0,'Error is not 0');
            assert.equal(body.data.model.num_pos,1,'num_pos is not one');                             


myrequest({method: 'POST', url:url+'/edit', json:{changes:[],settings:{max_negatives:1,max_positives:1}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
          //  assert.equal(body.error,0,'Error is not 0');
            assert.equal(body.data.images.length,2);

myrequest({method: 'POST', url:url+'/edit', json:{changes:[{id:2,class_label:1}],settings:{max_negatives:0,max_positives:3}}}, function(error, response, body) {
            assert.equal(response.statusCode, 200, 'Problem with status code');
            assert.equal(body.data.images.length,2);





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






/*

                                        myrequest({method: 'POST', url:url,json:{command:'exit'}}, function(error, response, body) {
                                          assert.equal(response.statusCode, 200, 'Problem with status code');
*/


                                
                                   /*myrequest({method: 'POST', url:url, json:{command:'exit'}}, function(error, response, body) {  
                                    assert.equal(response.statusCode, 200, 'Problem with status code');

                                    myrequest({method: 'POST', url:url, json:{command:'list_models'}}, function(error, response, body) {  
                                      //console.log('response is',response);
                                      assert.equal(response, undefined, 'Problem with status code');
                                      
                                    });
                                  });
*/
                                  
                                  //console.log('XXX load model body is', body);
                                  //console.log('BUNDLE uuid is',body.data.model.uuid);
                                  //console.log('input file: /tmp/blank/models/'+body.data.model.uuid+'/compiled.data');
                                  //exec('cp /tmp/blank/models/'+uuid+'/compiled.data /tmp/bundle.data', puts);
                                  //var uuid = '/tmp/bundle.data';
                                  /*
                                    var json = {command:'load',uuids:[uuid],compiled:false};
                                    request({method: 'POST', url:url, json:json}, function(error, response, body) {  
                                    
                                    assert.equal(response.statusCode, 200, 'Problem with status code');
                                    assert.equal(body.error,0,'Error is not 0');
                                    assert.equal(body.message.indexOf('Loaded Model')>-1,1);
                                    var d1 = body.data;
                                    
                                    var uuid = '/Users/tomasz/projects/VMXserver/tests/testapi/testdata/compiled.data';
                                    
                                    var json = {command:'load',uuids:[uuid],compiled:true};
                                    request({method: 'POST', url:url, json:json}, function(error, response, body) {  
                                    
                                    assert.equal(response.statusCode, 200, 'Problem with status code');
                                    assert.equal(body.error,0,'Error is not 0');
                                    assert.equal(body.message.indexOf('Loaded Model')>-1,1);
                                    
                                    var uuid = '/Users/tomasz/projects/VMXserver/tests/testapi/testdata/bad.data';
                                    var json = {command:'load',uuids:[uuid]};
                                    request({method: 'POST', url:url, json:json}, function(error, response, body) {
                                    assert.equal(response.statusCode, 400, 'Problem with status code');
                                    
                                    
                                    });
                                  */
                                  
                                  /*
                                    var uuid = '/Users/tomasz/projects/VMXserver/tests/testapi/testdata/compiled.data';
                                    request({method: 'POST', url:url+'/load', json:{uuids:[uuid],compiled:false}}, function(error, response, body) {  
                                    
                                    assert.equal(response.statusCode, 200, 'Problem with status code');
                                    assert.equal(body.error,0,'Error is not 0');
                                    assert.equal(body.message.indexOf('Loaded Model')>-1,1);
                                    
                                    //var d2 = body.data;
                                    
                                    //assert(JSON.stringify(d1),JSON.stringify(d2),'d1 and d2');
                                    
                                    var uuid = 'https://files.vision.ai/images/compiled.data';
                                    request({method: 'POST', url:url+'/load', json:{uuids:[uuid],compiled:true}}, function(error, response, body) {  
                                    
                                    assert.equal(response.statusCode, 200, 'Problem with status code');
                                    assert.equal(body.error,0,'Error is not 0');
                                    assert.equal(body.message.indexOf('Loaded Model')>-1,1);
                                    
                                    });
                                    });
                                  */
                                });
                              });
                            });
                            
                          });
                          /*  request({method: 'POST', url:url+'/save', json:{'new_uuid':true}}, function(error, response, body) {
                              assert.equal(response.statusCode, 200, 'Problem with status code');
                              
                              });
                          */
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
});

});

});
});

});

});

});

});


//});


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
//});

});

});

});
});

});
