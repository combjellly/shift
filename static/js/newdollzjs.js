let gtree;
let ptree;
let x = 0;
let y = 0;
let counter = 0;
let size = 0;
let picnum;
let dollz = [];
let img;

function preload(){
  
      for(let i = 0;i < 150;i++) {

         dollz[i] = loadImage("static/js/assets/" + i +".gif");


        }
}



function setup(){
  
    createCanvas(windowWidth, windowHeight);
    background(0);
    imageMode(CENTER);
    setFrameRate(1)

}

function draw(){
  
    for(let i = 0;i < 1/5;i++) {
       x= random(windowWidth);
       y=random(windowHeight);
       size = random(0, 100);
       img = random(dollz);
       image(img,x,y,size,size);
    }


}
