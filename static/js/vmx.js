/* globals Base64 */
//TODO compatibility should be pulled into compatibility.js


//This is for cross browser compatibility
var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
                            window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
window.requestAnimationFrame = requestAnimationFrame;

navigator.getUserMedia = navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia;
window.URL = window.URL || window.webkitURL || window.mozURL || window.msURL;

//TODO: This should be in vmx-api.js
//Empty object for API
var VMX = {};
VMX.config = {};


var VMXWebSocketURI;

VMXWebSocketURI = (VMX_SERVER + 'websocket').replace('http://', 'ws://');


//config
var _vmxConfig = {};
_vmxConfig.columnPadding=15;


var vmxApp = angular.module("vmx",['ng','vmx.services','ui.router','ui.ace','ui.bootstrap','ngAnimate','glider','angular-websocket']);

vmxApp
.config(['$urlRouterProvider','$httpProvider','WebSocketProvider', function($urlRouterProvider,$httpProvider){
  $urlRouterProvider.otherwise("/");
  delete $httpProvider.defaults.headers.common["X-Requested-With"];
  //console.log($httpProvider.defaults.headers);

  $httpProvider.defaults.headers.common['Authorization'] = 'Basic ' + Base64.encode('vmx' + ':' + 'slapperdoodle');
  $httpProvider.defaults.headers.post['Authorization']   = 'Basic ' + Base64.encode('vmx' + ':' + 'slapperdoodle');
  $httpProvider.defaults.useXDomain = true;
  delete $httpProvider.defaults.headers.common['X-Requested-With'];
}]);



// TODO put this somewhere else
/* 
 * Detection is used for drawing boxes,
 * can be used with either 
     x,y and w,h         or,
 *   (x1,y1) (x2,y2)
*/
var Detection;
Detection = function (cls, score, x1, y1, x2, y2, fill,color,time,ilearn_class) {
  this.cls = cls;
  this.score = score;
  this.x = x1;
  this.x1 = x1;
  this.x2 = x2;
  this.w = x2 - x1;
  this.y = y1;
  this.y1 = y1;
  this.y2 = y2;
  this.h = y2 - y1;
  this.fill = fill;
  this.color = color;
  this.ilearn_class = ilearn_class;
  this.line_width = 5;
  if(time){
    this.time = time;
  }
  else{
    this.time = new Date();
  }

  // Returns true if the point (x,y) is inside the detection box, false otherwise
  this.inside = function(x,y,PAD) {
    PAD = PAD | 0;
    return (x>= (this.x1-PAD) && x<= (this.x2+PAD) && y >= (this.y1-PAD) && y <=(this.y2+PAD));
  };

  this.draw = function(ctx) {
    ctx.save();

    if (this.ilearn_class && this.ilearn_class !== 0) {
      ctx.lineWidth = 0;
      //ctx.setLineDash([5]);
      if (this.ilearn_class === 1) {
        ctx.fillStyle = 'rgba(0,255,0,0.1)';
      } else {
        ctx.fillStyle = 'rgba(255,0,0,0.1)';
      }
      ctx.strokeStyle = 'rgb(0,0,0)';
      ctx.fillRect(this.x, this.y, this.w, this.h);
    }

    ctx.fillStyle = this.fill;
    if (this.score < -1) {
      ctx.lineWidth = this.line_width;
    } else {
      ctx.lineWidth = this.line_width;
    }

    ctx.strokeStyle = this.color;
    ctx.strokeRect(this.x, this.y, this.w, this.h);

    if (this.line_width > 1) {

      ctx.font="20px Courier";
      ctx.fillText(this.cls,this.x,this.y-ctx.lineWidth-3);
      ctx.font="10px Arial";
      ctx.fillText(this.score,this.x,this.y+ctx.lineWidth+3);      
     
    }
    ctx.restore();
  };                           
};
