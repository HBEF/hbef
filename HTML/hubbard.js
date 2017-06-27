$(document).ready(function(){"use strict";
  // Add smooth scrolling to all links
  $("a").on('click', function(event) {

    // Make sure this.hash has a value before overriding default behavior
    if (this.hash !== "") {
      // Prevent default anchor click behavior
      event.preventDefault();

      // Store hash
      var hash = this.hash;

      // Using jQuery's animate() method to add smooth page scroll
      // The optional number (800) specifies the number of milliseconds it takes to scroll to the specified area
      $('html, body').animate({
        scrollTop: $(hash).offset().top
      }, 800, function(){
   
        // Add hash (#) to URL when done scrolling (default click behavior)
        window.location.hash = hash;
      });
    } // End if
  });
});


function openTab(evt, tabName) {"use strict";
    var i, x, tablinks;
    x = document.getElementsByClassName("tab");
    for (i = 0; i < x.length; i++) {
        x[i].style.display = "none";
    }
    tablinks = document.getElementsByClassName("tablink");
    for (i = 0; i < x.length; i++) {
        tablinks[i].className = tablinks[i].className.replace(" w3-red", "");
    }
    document.getElementById(tabName).style.display = "block";
    evt.currentTarget.className += " w3-red";
}

 function resizeIframe(obj)
 {
	 obj.style.height = obj.contentWindow.document.body.clientHeight +  'px';
 }


function autoResize(id){"use strict";
  var newheight;
  var newwidth;

  if(document.getElementById){
      newheight=document.getElementById(id).contentWindow.document.body.scrollHeight;
      newwidth=document.getElementById(id).contentWindow.document.body.scrollWidth;
  }

      document.getElementById(id).height= (newheight) + "px";
      document.getElementById(id).width= (newwidth) + "px";
 }

