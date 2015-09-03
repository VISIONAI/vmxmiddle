var request = require('request');
var assert = require('assert');
var sys = require('sys')
var exec = require("exec-sync");
var fs = require('fs');

function puts(error, stdout, stderr) { 
  sys.puts(stdout); 
}

function load_session_model(vmx_dir,session_id) {
  return JSON.parse(fs.readFileSync(vmx_dir + 'sessions/' + session_id + '/model.json'));
}


module.exports = {

  myexec:function(str) {
    console.log(str);
    exec(str,puts);
  },

  /**
   * Overwrite the request handler so that we write out a curl-ready line
   **/
  myrequest:function(inp,func) {
    var dstring = ' -d \''+JSON.stringify(inp.json)+'\' ';
    if (inp.json === undefined) {
      dstring = ' ';
    }
    console.log('\ncurl -s -X '+inp.method+dstring+inp.url);
    
    function mybody(error, response, body) {
      console.log(response.statusCode);
      //console.log('body is',body)
      if (body.length > 0) {
        try {
          body = JSON.parse(body);
        } finally {}
      }
       
      
      console.log(JSON.stringify(body,null,4));
      
      if (response.statusCode >= 400 && response.statusCode <500) {
        assert(body.error.length>0,1,'error is non-empty');
      }
      func(error,response,body);
    }
    
    request(inp,mybody);
  }
  
}
