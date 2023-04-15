// get cursor position
// get size of our window - DONE

// draw the circle at cursor position
let x = 1;
let y = 1;
let rn = 1;
let xspeed = 1;
let yspeed = 1;

function setup(){
	let cnv =  createCanvas(windowWidth,windowHeight);
	cnv.position(0,0,"fixed");
	cnv.style("z-index","-1");
}

function draw(){



 	x = x + xspeed;
  y = y + yspeed;

  if (x >= windowWidth) {
    xspeed = -xspeed;

  } else if (x <= 0) {
    xspeed = -xspeed;
  }

  if (y >= windowHeight) {
    yspeed = - yspeed;

  } else if (y <= 0) {
    yspeed = - yspeed;
  }

	circle(x,y,mouseX/3);

}