(function() {
  'use strict';

  var getHeight = function(e) {
      if (e.data === "getHeight") {
        e.source.postMessage("getHeight:" + (Number($("body").height()) + (Number($("body").height()) * 0.05)) + 10 + "px", "*");
      }
  }
  window.addEventListener("message", getHeight, false);
}());