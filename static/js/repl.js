document.addEventListener("DOMContentLoaded", function () {
    var textarea = document.getElementById("code");
    var messages = Object.values(presets);
    var currMesg = 0;
    var buttonState = 1;


    function openSidebar() {
        document.getElementById("mySidebar").style.width = "250px";
    };

    function closeSidebar() {
        document.getElementById("mySidebar").style.width = "0";
    };

    function advanceMessage() {
        textarea.value = messages[currMesg++ % messages.length];
        resizeTextarea(inputEl);
        highlight();
    }

    function exportCode() {
        let hashedCode = btoa(textarea.value)
            .replace(/=+$/, '')
            .replace(/\+/g, '-')
            .replace(/\//g, '_');
        let urlString = (window.location.href.replace(/\/$/, "") + "#" + hashedCode);
        navigator.clipboard.writeText(urlString);
        console.log(urlString);
    }

    window.toggleButton = function () {
        var buttonToggle = document.getElementById("button_toggle");

        if (buttonState % 2 == 1) {
            buttonToggle.className = "glyphicon glyphicon-stop";  // Change to stop icon
            buttonToggle.innerHTML = "";  // Optional: empty it if you only use the icon class
            doEvaluate();
            buttonState++;
        } else {
            buttonToggle.className = "glyphicon glyphicon-play";  // Change to play icon
            buttonToggle.innerHTML = "";  // Optional: empty it if you only use the icon class
            stop();
            buttonState++;
        }
    };

    function readURL() {
        if (window.location.hash) {
            let hashedCode = window.location.hash.substring(1);
            let decodedCode = atob(hashedCode.replace(/-/g, '+').replace(/_/g, '/'));
            textarea.value = decodedCode;
        } else {
            console.log("No Hash");
        }
    }

    document.addEventListener('keydown', function (event) {
        if (event.shiftKey && event.key === ' ') {
            event.preventDefault();
            doEvaluate();
        }
    });

    window.advanceMessage = advanceMessage;
    window.exportCode = exportCode;
    window.readURL = readURL;
    window.openSidebar = openSidebar;  // Make sure openSidebar is globally accessible
    window.closeSidebar = closeSidebar; // Same for closeSidebar

    readURL();
});
