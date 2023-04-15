function setup() {
    noCanvas();

  let cnv = createCanvas(windowWidth, windowHeight);
  cnv.position(0, 0, 'fixed');
  createA('http://p5js.org/', 'this is a link');


}


function draw() {
  circle(mouseX+10,mouseY+10,20);

}

function mousePressed() {
  if ((mouseX > 0 && mouseX < 30) && (mouseY > 0 && mouseY < 30)) {
    location.reload();location.href='http://nyiden.ca';
    }
}