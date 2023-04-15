const URL  = "https://coolors.co/000000-14213d-fca311-e5e5e5-ffffff";
const COLS = createCols(URL);
const NOISE_SCL = Math.random() * 0.0025 + 0.05;
let texArr = [];
const MAXSIZE_RATIO = Math.random() * 0.25 + 0.15;
const NUM = Math.random() * 45 + 5;

function setup() {
	let cnv = createCanvas(windowWidth, windowHeight);
  cnv.position(0, 0, 'fixed');

	noStroke();
	for(let i = 0; i < 10; i++)texArr[i] = createRandTex(width, height);
	texArr.push(createNoiseTex(width, height));
	noLoop();

}

function draw() {
	
	image(random(texArr), 0, 0);
	
	for(let i = 0; i < NUM; i++)
	{
		let cx = random(width);
		let cy = random(width);
		let w = random(100, min(width, height) *MAXSIZE_RATIO);
		let h = random(100, min(width, height) * MAXSIZE_RATIO);
		let num = int((w + h) / random(20,40));
		let offsetAmount = random(100, max(w, h));
		let maskGra = noisedMask(cx, cy, w, h, num, offsetAmount);
		let texImg = random(texArr).get();
		texImg.mask(maskGra);
		image(texImg, 0, 0);
	}

}


function createRandTex(w, h)
{
	let c = randSelFromArr(COLS, 2);
	let g = createGraphics(w, h);
	let rn = random(3);
	
	g.noStroke();
	g.fill(c[1]);
	
	if(rn < 1){
		let span = random(5,50);
		let dia = span * random(0.2, 1);
		for(let y = 0; y  < h + span; y  += span)
		{
			for(let x = 0; x  < w + span; x  += span)g.circle(x, y, dia);
		}
	}
	
	else if(rn < 2.5){
		let span = random(5, 50);
		let xs = random() > 0.5 ? span * random(0.2, 0.9) : span + 1;
		let xy = random() > 0.5 ? span * random(0.2, 0.9) : span + 1;
		for(let y = 0; y  < h + span; y  += span)
		{
			for(let x = 0; x  < w + span; x  += span)g.rect(x, y, xs, xy);
		}
	}
	
	else if(rn < 3){
		let span = random(5, 50);
		let cy = 0;
		for(let y = 0; y  < h + span; y  += span)
		{
			let off = cy % 2 == 0 ? 0 : span /2;
			for(let x = 0; x  < w + span; x  += span)g.triangle(x + off, y, x + off + span, y, x + off + span /2, y + span);
			cy ++;
		}
	}	
	return g;
}


function createNoiseTex(w,h){
	let c = randSelFromArr(COLS, 2);
	let g = createGraphics(width, height);
	g.noStroke();
	g.fill(c[1] + "BB");
	
	for(let i = 0; i < w * h * 0.1; i++){
		let x = random(width);
		let y = random(height);
		let d = noise(x * 0.01, y * 0.01) * 0.5 + 0.5;
		g.ellipse(x, y, d, d);
	}
	
	return g;
}


////////

function noisedMask(cx, cy, w, h, num, offsetAmount)
{
	let g = createGraphics(width, height);
	g.noStroke();
	g.fill(0);
	drawNoisedCircle(cx, cy, w, h, num, offsetAmount, g);
	return g;
}

function drawNoisedCircle(cx, cy, w, h, num, offsetAmount, g = this)
{
	const stepRad = TAU / num;
	g.beginShape();
	for(let rad = 0; rad < TAU + stepRad * 3; rad += TAU / num)
	{
		let x = cx + cos(rad) * w/2;
		let y = cy + sin(rad) * h/2;
		let radiusOff = noise(x * NOISE_SCL, y * NOISE_SCL) * offsetAmount;
		x = cx + cos(rad) * (w/2 + radiusOff);
		y = cy + sin(rad) *  (h/2 + radiusOff);
		g.curveVertex(x, y);
	}
	g.endShape();
}


////
function randSelFromArr(arr, num)
{
	let clone = arr.slice();
	if(arr.length <= num)return clone;
	let out = [];
	for(let i = 0; i < num; i++)
	{
		let ri = int(random(clone.length));
		out.push(clone[ri]);
		clone.splice(ri, 1);
	}
	return out;
}

function createCols(url)
{
	let slaIndex = url.lastIndexOf("/");
	let colStr = url.slice(slaIndex + 1);
	let colArr = colStr.split("-");
	for(let i = 0; i < colArr.length; i++)colArr[i] = "#" + colArr[i];
	return colArr;
}
