var _vmx_object_or_array;
var _vmx_is_encoded_image;
var _vmx_array_difference;
var _vmx_naive_smoothing;
var _vmx_get_computed_style;
var _vmx_clean_dirty_sessionId;
var image_data_to_dataurl;
var Base64;
var VmxMaxLengthArray;

(function(){


  _vmx_object_or_array = function(val){
  // Determine if an object is an array or an object, or else return
  // false
    if(val instanceof Array || val instanceof Object){
      return true;
    }
  };

  _vmx_is_encoded_image = function(val){
    if(typeof val !== 'string') { 
      return false; 
    } 
    var img_str = 'data:image/jpeg;base64';
    if(val.substring(0,img_str.length) === img_str) {
      return true;
    }
    return false;
  };        
  
  _vmx_array_difference = function(a,b,id){ 
    // Make hashtable of ids in B
    var bIds = {};
    b.forEach(function(obj){
      bIds[obj[id]] = obj;
    });
    
    // Return all elements in A, unless in B
    return a.filter(function(obj){
        return !(obj[id] in bIds);
    });
  };


  _vmx_clean_dirty_sessionId = function(connectionId){
    if(connectionId.substring(0,9) === 'sessions/') { 
      connectionId = connectionId.substring(9); 
    }
    return connectionId;
  };

  //LIFO queue of `maxlength` size
  VmxMaxLengthArray = function(maxlength){
    if(typeof length !== 'number'){
      throw( {
        name: "vmx_maxlength_array type error",
        message: "Exepected `number`, received + \"" + typeof length + "\"",
      });
    }
    this.maxlength = maxlength;
    this.queue =[];
    return this;
  };

  VmxMaxLengthArray.prototype.val = function(){
    return this.queue;
  };

  VmxMaxLengthArray.prototype.add = function(item){
    this.queue.unshift(item);
    if (this.queue.length > this.maxlength){
      this.queue = this.queue.splice(0,this.maxlength);
    }
    return this;
  };

  VmxMaxLengthArray.prototype.clear = function(){
    this.queue = [];
    return this;
  };

  //Hacky attempt to check the relative difference between
  //two bounding box in an attempt to determine if this is likely
  //an unmoving detection.
  //We want to smoothe out detections but not make them lag if they are
  //moving.
  //a threshold of ~.35 seems decent form testing
  function _vmx_shape_moved(bb1, bb2, threshold){
    if( threshold === undefined ) { threshold = 0.35; }
    var xd1, xd2, yd1, yd2; 
    var w1,w2,h1,h2;
    var e1,e2;
    xd1 = Math.abs(bb1.x1 - bb2.x1) * 2;
    w1  = bb1.x2 - bb1.x1;
    w2  = bb2.x2 - bb2.x1;
    xd2 = w1 + w2;
    yd1 = Math.abs(bb1.y1 - bb2.y1) * 2;
    h1  = bb1.y2 - bb1.y1;
    h2  = bb2.y2 - bb2.y1;
    yd2 = h1 + h2;
    //yd2 = bb1.h + bb2.h;
    if(xd1 < xd2 && yd1 < yd2){
      //They overlap, so how much have they moved?
      e1 = (xd1) / ( (bb1.w + bb2.w)/2 );
      e2 = (yd1) / ( (bb1.h + bb2.h)/2 );
      if(e1 + e2 > threshold){
        return true;
      } else {
        return false;
      }
    }else{
      //They don't overlap, so it must have moved.
      return true;
    }
  }


  _vmx_naive_smoothing = function(prev_detection,curr_detection, last_detections){
    if( _vmx_shape_moved(prev_detection,curr_detection,0.5)){
      last_detections.clear();
      return curr_detection;
    }

    var color = 'rgba(199,21,133,1)';
    var num_prev = last_detections.val().length;

    var now = new Date();
    /* globals Detection: true */
    var averaged_box = new Detection(
      curr_detection.cls,
      curr_detection.score,
      last_detections.val().map(function(i){ return i.x1; }).reduce(function(a,b){return a+b; }) / num_prev,
      last_detections.val().map(function(i){ return i.y1; }).reduce(function(a,b){return a+b; }) / num_prev,
      last_detections.val().map(function(i){ return i.x2; }).reduce(function(a,b){return a+b; }) / num_prev,
      last_detections.val().map(function(i){ return i.y2; }).reduce(function(a,b){return a+b; }) / num_prev,
      '#00A',
      color,
      now
    );
    return averaged_box;
  };

  _vmx_get_computed_style = function(element,property){
    return document.defaultView.getComputedStyle(element,null).getPropertyValue(property);
  };


  // function _vmx_fix_stripped_image(dataUrl){
  //   //console.log("NOTE: FIXING A STRIPPED IMAGE");
  //   //return dataUrl.replace(/rn/g,'\r\n');
  //   return dataUrl;
  // }

  //NOTE should probably be moved out of this file
  /**
  *
  *  Base64 encode / decode
  *  http://www.webtoolkit.info/
  *
  **/
   
  Base64 = {
   
    // private property
    _keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",
   
    // public method for encoding
    encode : function (input) {
      var output = "";
      var chr1, chr2, chr3, enc1, enc2, enc3, enc4;
      var i = 0;
   
      input = Base64._utf8_encode(input);
   
      while (i < input.length) {
   
        chr1 = input.charCodeAt(i++);
        chr2 = input.charCodeAt(i++);
        chr3 = input.charCodeAt(i++);
   
        enc1 = chr1 >> 2;
        enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
        enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
        enc4 = chr3 & 63;
   
        if (isNaN(chr2)) {
          enc3 = enc4 = 64;
        } else if (isNaN(chr3)) {
          enc4 = 64;
        }
   
        output = output +
        this._keyStr.charAt(enc1) + this._keyStr.charAt(enc2) +
        this._keyStr.charAt(enc3) + this._keyStr.charAt(enc4);
   
      }
   
      return output;
    },
   
    // public method for decoding
    decode : function (input) {
      var output = "";
      var chr1, chr2, chr3;
      var enc1, enc2, enc3, enc4;
      var i = 0;
   
      input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
   
      while (i < input.length) {
   
        enc1 = this._keyStr.indexOf(input.charAt(i++));
        enc2 = this._keyStr.indexOf(input.charAt(i++));
        enc3 = this._keyStr.indexOf(input.charAt(i++));
        enc4 = this._keyStr.indexOf(input.charAt(i++));
   
        chr1 = (enc1 << 2) | (enc2 >> 4);
        chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
        chr3 = ((enc3 & 3) << 6) | enc4;
   
        output = output + String.fromCharCode(chr1);
   
        if (enc3 !== 64) {
          output = output + String.fromCharCode(chr2);
        }
        if (enc4 !== 64) {
          output = output + String.fromCharCode(chr3);
        }
   
      }
   
      output = Base64._utf8_decode(output);
   
      return output;
   
    },
   
    // private method for UTF-8 encoding
    _utf8_encode : function (string) {
      string = string.replace(/\r\n/g,"\n");
      var utftext = "";
   
      for (var n = 0; n < string.length; n++) {
   
        var c = string.charCodeAt(n);
   
        if (c < 128) {
          utftext += String.fromCharCode(c);
        }
        else if((c > 127) && (c < 2048)) {
          utftext += String.fromCharCode((c >> 6) | 192);
          utftext += String.fromCharCode((c & 63) | 128);
        }
        else {
          utftext += String.fromCharCode((c >> 12) | 224);
          utftext += String.fromCharCode(((c >> 6) & 63) | 128);
          utftext += String.fromCharCode((c & 63) | 128);
        }
   
      }
   
      return utftext;
    },
   
    // private method for UTF-8 decoding
    _utf8_decode : function (utftext) {
      var string = "";
      var i,c,c1,c2,c3;
      i = c = c1 = c2 = 0;
   
      while ( i < utftext.length ) {
   
        c = utftext.charCodeAt(i);
   
        if (c < 128) {
          string += String.fromCharCode(c);
          i++;
        }
        else if((c > 191) && (c < 224)) {
          c2 = utftext.charCodeAt(i+1);
          string += String.fromCharCode(((c & 31) << 6) | (c2 & 63));
          i += 2;
        }
        else {
          c2 = utftext.charCodeAt(i+1);
          c3 = utftext.charCodeAt(i+2);
          string += String.fromCharCode(((c & 15) << 12) | ((c2 & 63) << 6) | (c3 & 63));
          i += 3;
        }
   
      }
   
      return string;
    }
   
  };

  image_data_to_dataurl = function(imageData) {
    var elem = document.createElement('canvas');
    elem.width = imageData.width;
    elem.height = imageData.height;
    elem.getContext('2d').putImageData(imageData,0,0);
    return elem.toDataURL(elem);
  };
})();
