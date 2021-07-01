/*
 * Handles finding a text string anywhere in the slides and showing the next occurrence to the user
 * by navigatating to that slide and highlighting it.
 *
 * By Jon Snyder <snyder.jon@gmail.com>, February 2013
 */

var RevealSearch = (function() {

	var matchedSlides;
	var currentMatchedIndex;
	var searchboxDirty;
	var myHilitor;

// Original JavaScript code by Chirp Internet: www.chirp.com.au
// Please acknowledge use of this code by including this header.
// 2/2013 jon: modified regex to display any match, not restricted to word boundaries.

function Hilitor(id, tag)
{

	var targetNode = document.getElementById(id) || document.body;
	var hiliteTag = tag || "EM";
	var skipTags = new RegExp("^(?:" + hiliteTag + "|SCRIPT|FORM)$");
	var colors = ["#ff6", "#a0ffff", "#9f9", "#f99", "#f6f"];
	var wordColor = [];
	var colorIdx = 0;
	var matchRegex = "";
	var matchingSlides = [];

	this.setRegex = function(input)
	{
		input = input.replace(/^[^\w]+|[^\w]+$/g, "").replace(/[^\w'-]+/g, "|");
		matchRegex = new RegExp("(" + input + ")","i");
	}

	this.getRegex = function()
	{
		return matchRegex.toString().replace(/^\/\\b\(|\)\\b\/i$/g, "").replace(/\|/g, " ");
	}

	// recursively apply word highlighting
	this.hiliteWords = function(node)
	{
		if(node == undefined || !node) return;
		if(!matchRegex) return;
		if(skipTags.test(node.nodeName)) return;

		if(node.hasChildNodes()) {
			for(var i=0; i < node.childNodes.length; i++)
				this.hiliteWords(node.childNodes[i]);
		}
		if(node.nodeType == 3) { // NODE_TEXT
			if((nv = node.nodeValue) && (regs = matchRegex.exec(nv))) {
				//find the slide's section element and save it in our list of matching slides
				var secnode = node;
				while (secnode != null && secnode.nodeName != 'SECTION') {
					secnode = secnode.parentNode;
				}

				var slideIndex = Reveal.getIndices(secnode);
				var slidelen = matchingSlides.length;
				var alreadyAdded = false;
				for (var i=0; i < slidelen; i++) {
					if ( (matchingSlides[i].h === slideIndex.h) && (matchingSlides[i].v === slideIndex.v) ) {
						alreadyAdded = true;
					}
				}
				if (! alreadyAdded) {
					matchingSlides.push(slideIndex);
				}

				if(!wordColor[regs[0].toLowerCase()]) {
					wordColor[regs[0].toLowerCase()] = colors[colorIdx++ % colors.length];
				}

				var match = document.createElement(hiliteTag);
				match.appendChild(document.createTextNode(regs[0]));
				match.style.backgroundColor = wordColor[regs[0].toLowerCase()];
				match.style.fontStyle = "inherit";
				match.style.color = "#000";

				var after = node.splitText(regs.index);
				after.nodeValue = after.nodeValue.substring(regs[0].length);
				node.parentNode.insertBefore(match, after);
			}
		}
	};

	// remove highlighting
	this.remove = function()
	{
		var arr = document.getElementsByTagName(hiliteTag);
		while(arr.length && (el = arr[0])) {
			el.parentNode.replaceChild(el.firstChild, el);
		}
	};

	// start highlighting at target node
	this.apply = function(input)
	{
		if(input == undefined || !input) return;
		this.remove();
		this.setRegex(input);
		this.hiliteWords(targetNode);
		return matchingSlides;
	};

}

	function openSearch() {
		//ensure the search term input dialog is visible and has focus:
		var inputboxdiv = document.getElementById("searchinputdiv");
		var inputbox = document.getElementById("searchinput");
		inputboxdiv.style.display = "inline";
		inputbox.focus();
		inputbox.select();
	}

	function closeSearch() {
		var inputboxdiv = document.getElementById("searchinputdiv");
		inputboxdiv.style.display = "none";
		if(myHilitor) myHilitor.remove();
	}

	function toggleSearch() {
		var inputboxdiv = document.getElementById("searchinputdiv");
		if (inputboxdiv.style.display !== "inline") {
			openSearch();
		}
		else {
			closeSearch();
		}
	}

	function doSearch() {
		//if there's been a change in the search term, perform a new search:
		if (searchboxDirty) {
			var searchstring = document.getElementById("searchinput").value;

			if (searchstring === '') {
				if(myHilitor) myHilitor.remove();
				matchedSlides = null;
			}
			else {
				//find the keyword amongst the slides
				myHilitor = new Hilitor("slidecontent");
				matchedSlides = myHilitor.apply(searchstring);
				currentMatchedIndex = 0;
			}
		}

		if (matchedSlides) {
			//navigate to the next slide that has the keyword, wrapping to the first if necessary
			if (matchedSlides.length && (matchedSlides.length <= currentMatchedIndex)) {
				currentMatchedIndex = 0;
			}
			if (matchedSlides.length > currentMatchedIndex) {
				Reveal.slide(matchedSlides[currentMatchedIndex].h, matchedSlides[currentMatchedIndex].v);
				currentMatchedIndex++;
			}
		}
	}

	var dom = {};
	dom.wrapper = document.querySelector( '.reveal' );

	if( !dom.wrapper.querySelector( '.searchbox' ) ) {
			var searchElement = document.createElement( 'div' );
			searchElement.id = "searchinputdiv";
			searchElement.classList.add( 'searchdiv' );
			searchElement.style.position = 'absolute';

			// MARIO: adjust position, size, color
			searchElement.style.top  = "calc(var(--whiteboard-icon-size) * 0.25)";
			searchElement.style.left = "calc(var(--whiteboard-icon-size) * 2.25)";
			searchElement.style.padding = "calc(var(--whiteboard-icon-size) * 0.5)";
			searchElement.style.borderRadius = "0.25em";
			searchElement.style.background = "white";
			searchElement.style.fontSize = "var(--whiteboard-icon-size)";
			searchElement.style.color = "var(--whiteboard-active-color)";
			searchElement.style.zIndex = 10;

            // MARIO: adjust border color and search icon (requires font-awesome)
			searchElement.innerHTML = '<span style="display:flex; align-items:center;"><i class="fas fa-search searchicon" id="searchbutton" style="padding-right: 10px;"></i><input type="search" id="searchinput" class="searchinput" style="border: 3px solid var(--whiteboard-active-color); border-radius:4px;"/></span>';

			dom.wrapper.appendChild( searchElement );
	}

	document.getElementById( 'searchbutton' ).addEventListener( 'click', function(event) {
		doSearch();
	}, false );


	document.getElementById( 'searchinput' ).addEventListener( 'keyup', function( event ) {
		switch (event.keyCode) {

			case 13:
				event.preventDefault();
				doSearch();
				searchboxDirty = false;
				break;

            // MARIO: close search field on key Escape
            case 27:
                closeSearch();
                break;

			default:
				searchboxDirty = true;
		}
	}, false );

	document.addEventListener( 'keydown', function( event ) {
		// MARIO: use standard search shortcut
		// if( event.key == "F" && (event.ctrlKey || event.metaKey) ) { //Control+Shift+f
		if( event.key == "f" && (event.ctrlKey || event.metaKey) ) { //Control+Shift+f
			event.preventDefault();
			toggleSearch();
		}
	}, false );

	// MARIO: use standard search shortcut
	if( window.Reveal ) Reveal.registerKeyboardShortcut( 'CTRL/CMD + F', 'Search' );
	// if( window.Reveal ) Reveal.registerKeyboardShortcut( 'CTRL + Shift + F', 'Search' );

	closeSearch();

    // MARIO: also export toggle (will be used in menu plugin)
	return { open: openSearch, toggle: toggleSearch };
})();
