(function() {
  'use strict';

  var getHeight = function(e) {
      if (e.data === "getHeight") {
        e.source.postMessage("getHeight:" + (Number($("body").height()) + (Number($("body").height()) * 0.05)) + 10 + "px", "*");
      }
  };
  window.addEventListener("message", getHeight, false);
}());


// Find all iframes
var $iframes = $( "iframe" );
 
// Find &#x26; save the aspect ratio for all iframes
$iframes.each(function () {
  $( this ).data( "ratio", this.height / this.width )
    // Remove the hardcoded width &#x26; height attributes
    .removeAttr( "width" )
    .removeAttr( "height" );
});
 
// Resize the iframes when the window is resized
$( window ).resize( function () {
  $iframes.each( function() {
    // Get the parent container&#x27;s width
    var width = $( this ).parent().width();
    $( this ).width( width )
      .height( width * $( this ).data( "ratio" ) );
  });
// Resize to fix all iframes on page load.
}).resize();