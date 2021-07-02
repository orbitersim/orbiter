function parser(fn) {
var X, Y, sl, a, ra, link;
ra = /:/;
a = location.href.search(ra);
if (a == 2)
X = 14;
else
X = 7;
sl = "\\";
Y = location.href.lastIndexOf(sl) + 1;
link = 'file:///' + location.href.substring(X, Y) + fn;
location.href = link;
}

function doSec (secNum){
//display the section if it's not displayed; hide it if it is displayed
if (secNum.style.display=="none"){secNum.style.display=""}
else{secNum.style.display="none"}
}

function noSec (secNum){
//remove the section when user clicks in the opened DIV
if (secNum.style.display==""){secNum.style.display="none"}
}
