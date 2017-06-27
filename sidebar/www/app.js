var level = document.location.search.replace(/\?/,'') || 0;
			$('#nested').attr('href','frame.nested.html?'+(++level));

var iFrameResizer = {
					messageCallback: function(message){
						alert(message,parentIFrame.getId());
					}
				}