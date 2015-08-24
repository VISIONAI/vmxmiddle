var request = require('request');
var sys = require('sys')
var exec = require("exec-sync");//require('child_process').exec;
var fs = require('fs');

function puts(error, stdout, stderr) { sys.puts(stdout); }

function myexec(str) {
  console.log(str);
  exec(str,puts);
}
//myexec("rm -rf "+vmx_dir+"/models/*");

function load_session_model() {
  return JSON.parse(fs.readFileSync(vmx_dir + 'sessions/' + session_id + '/model.json'));
}


module.exports = {





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
    //console.log('error is',error);
    //console.log('response is',response);
    //  console.log('error is',body);
    if (body.length > 0) {
      console.log(JSON.stringify(JSON.parse(body),null,4));
    }
    func(error,response,body);
  }

  request(inp,mybody);

}

}
