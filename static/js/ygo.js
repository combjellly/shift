
let radius;
let img;
let mood;

function preload() {
    var url = 'https://db.ygoprodeck.com/api/v7/randomcard.php';
    radius = loadJSON(url);

  }
   
function setup() {
    mood = ("https://storage.googleapis.com/ygoprodeck.com/pics/"+radius.id+".jpg");
    img = loadImage(mood);

    createCanvas(windowWidth, windowHeight);

    print(mood);

    
}
   
function draw() {
    image(img, 0, 0);
}
