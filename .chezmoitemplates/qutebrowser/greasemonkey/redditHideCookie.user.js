// ==UserScript==
// @name Reddit Hide Cookie Notification
// @description  hides cookie notification on old.reddit
// @author       reddit.com/user/DisillusionedExLib
// ==/UserScript==

(function() {
    'use strict';

    var x = document.getElementsByClassName('infobar-toaster-container');
    for (var y of x) {
        y.remove();
    }
})();
