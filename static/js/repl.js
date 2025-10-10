document.addEventListener("DOMContentLoaded", function () {
    var textarea = document.getElementById("code");
    var messages = Object.values(presets);
    var randmessages = Object.values(randpresets);

    var currMesg = 0;
    var buttonState = 1;


    function openSidebar() {
        document.getElementById("mySidebar").style.width = "250px";
    };

    function closeSidebar() {
        document.getElementById("mySidebar").style.width = "0";
    };

    function advanceMessage() {
        textarea.value = messages[(currMesg++ % messages.length)];
        resizeTextarea(inputEl);
        highlight();
    }

    function exportCode() { // fixed for github??
        let hashedCode = btoa(textarea.value)
            .replace(/=+$/, '')
            .replace(/\+/g, '-')
            .replace(/\//g, '_');

        let url = new URL(window.location.href);
        url.hash = ""; // Remove current hash

        if (!url.pathname.endsWith("/")) {
            url.pathname += "/";
        }

        let urlString = url.toString() + "#" + hashedCode;

        navigator.clipboard.writeText(urlString);
        console.log(urlString);
    };

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
    };


    function loadLocalStorage(){
        bgColor = localStorage.getItem("theme");
        const colorPicker = document.getElementById('head');
        const inputEl = document.querySelector('[data-el="input"]');
        inputEl.style.backgroundColor = bgColor;
        document.body.style.backgroundColor = bgColor;

        if (localProgram = localStorage.getItem("program")){;

        textarea.value = localProgram}
        else{     
            textarea.value = messages[0];

        }

    }

    function randPres(){
        seed = Math.floor(Math.random() * 3)
        textarea.value = randmessages[seed];
        resizeTextarea(inputEl);
        highlight();

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

    //readURL();
    //loadLocalStorage();
    randPres();
});

function hexToHsl(hex) {
  let r = parseInt(hex.slice(1, 3), 16) / 255;
  let g = parseInt(hex.slice(3, 5), 16) / 255;
  let b = parseInt(hex.slice(5, 7), 16) / 255;

  let max = Math.max(r, g, b), min = Math.min(r, g, b);
  let h, s, l = (max + min) / 2;

  if (max === min) {
    h = s = 0;
  } else {
    let d = max - min;
    s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
    switch (max) {
      case r: h = (g - b) / d + (g < b ? 6 : 0); break;
      case g: h = (b - r) / d + 2; break;
      case b: h = (r - g) / d + 4; break;
    }
    h /= 6;
  }
  return [h * 360, s * 100, l * 100];
}

function hslToCss([h, s, l]) {
  return `hsl(${h}, ${s}%, ${l}%)`;
}

function adjustColors(bgHex) {
  const [h, s, l] = hexToHsl(bgHex);

  // Dark background → make highlights lighter
  // Light background → make highlights darker
  const invert = l < 50 ? 70 : 30;

  document.documentElement.style.setProperty("--hl-number", hslToCss([200, 80, invert]));
  document.documentElement.style.setProperty("--hl-string", hslToCss([50, 70, invert]));
  document.documentElement.style.setProperty("--hl-control", hslToCss([340, 70, invert]));
  document.documentElement.style.setProperty("--hl-keyword", hslToCss([100, 70, invert]));
  document.documentElement.style.setProperty("--hl-symbol", hslToCss([190, 80, invert]));
}

function changeBG() {
  const colorPicker = document.getElementById('head');
  const inputEl = document.querySelector('[data-el="input"]');
  const bg = colorPicker.value;

  inputEl.style.backgroundColor = bg;
  document.body.style.backgroundColor = bg;
  localStorage.setItem("theme", bg);

  adjustColors(bg);
}
