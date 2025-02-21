var presets = {
    script1: `y=0
every 1.5
	y = 0
	i = [0 4 8]

every 0.5
	play.piano:i:y[70,0,0]
	y = y +1

every 3 
	i = [0 5 7]

every 0.75
	play.piano:(i:y)-1[70,0,100]
    `,

    script2: `change = 6
time = 1/1.3

every 0.75 / time
	y = 0
	i = [0 4 8]

every 1 / time
	vol = 0
	change = change + 1

every 0.5/ time
	play.snare:4[vol]
	vol = vol + 60

every 0.25/ time
	play.teringherrie:o*change[50,0,pan]
	y = y +1
 	o = i:y
	pan = random. 40 70

every 1.5 / time
	i = [0 5 7]
	
every 0.75/ time
	play.hardbass:(o*change)-1 [70,0,40]	
	play.hardbass:(o*change)-2 [70,0,70]

every 3/ time
	y = 0
	i = [0 4 8]

every 1/ time
	play.k2:2[80,(-4),50]

every 2/ time
	play.hi*change [50,0,100]
	y = y +1
 	o = i:y

every 6 / time
	i = [0 5 7]
	change = 0

every 0.75*2/ time
	play.hi:(o*change)-1[50,0,0]
    `,

    script3: `i=3
every y
	x = random. 1 3
	y = x / 3
	
	play.hi[70,x,p]
	play.hi:1[70,x+4,o]

every 0.1
	play.pingu:x-1[60,x,p]

every 0.2 
	p = random. 0 100
	o = random. 0 100
	d = random. 0 100

every 0.05
	play.organ[50,0+i,p]
	play.organ[50,7+i,o]
	play.organ1[50,12+i,d]

every 16
	i= x
    `,

    script4: `i=0
notes = [1 6 10 13 18]
note = 3
pCounter = 0 
pnote = 0

every 0.5
	pnote = notes:pCounter
	midi[pnote+ 50,60,300]
	pCounter = pCounter + 1
	if pCounter == 5 [ pCounter = 0]
    `
	,
	
	script5: `y=0
organ = [0 (-1) (-5) 0]
o = 0

time = 1

every 1.5 * time
	y = 0
	i = [0 4 8]

every 0.5* time
	pan = random. 0 100
	play.guitar:i:y[70,0,0]
	y = y +1

every 3 * time
	i = [0 5 7]

every 0.75* time
	play.guitar:(i:y)-1[70,0,100]

every 4* time
	play.organ[70, -12,pan]

every 1* time
	play.organ:12[70, organ:o]

{- every 1
	play.cello[70, organ:o] -}

every 12 * (time/3)
	o = o +1
	if o == 3 [ 
		o=0 ]
`
};

