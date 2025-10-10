var presets = {

	script0: `-- slowcore

rideamp =  [ 50 40 40 40 50 40 40 40 ]
snareamp = [  0  0 55  0  0  0  55 30 ]
kickamp =  [  30  0  0 40 50  0  0  0 ]
counter = 0

note = 0
melody = [0 2 9 5 4 0 0]
guitarmelody = [ 0 0 0 0 0 0 2 2 2 7 7 5 ]
guitarlead = [0 0 0 0 2 2 7 7 5 5 0 0]
sustainmelody = [7 5 0 2]
sustaincounter = 0

time = 1
mod = 0

-- some midi in this, channels 1, 2 and 5



every 0.5 * time
	-- sequencer 'clock'

	counter = counter + 1
	if counter >= 8 [ counter = 0]

	--stereo!! 
	play.ride:9[(rideamp:counter), 0, 70 ]
	play.ride:2[(rideamp:counter), 0, 30 ]

	play.snare:9[(snareamp:counter) + 20, 0, 70]
	play.snare:11[(snareamp:counter) + 10, 0,30]

	play.kick:10[(kickamp:counter) + 30,0,70]
	play.kick:9[(kickamp:counter) + 30,0,30]



----- melody

every 3
	note = guitarmelody:counter
	play.guitar:0[60,0,50]
	--midi[60,60,50]

every 2
	leadnote = guitarlead:counter
	play.guitar:leadnote[60,0,50]
	midi:1[60+leadnote,60,200]

every 4
	--counter = counter + 1
	if counter == 9 [ counter = 0 ]
	play.guitar:mod+note[60,0,30]
	midi:1[48+note,60,200]

every 1
	hatvolume = 50
	guitarcounter = guitarcounter + 1
	if guitarcounter == 12 [ guitarcounter = 0]
	guitarlead1 = guitarmelody:guitarcounter
	play.guitar:guitarlead1[60,(-19),50]
	midi[60+guitarlead1,60,200]

every 16
	sustaincounter = sustaincounter + 1
	if sustaincounter >= 4 [ sustaincounter = 0]
	midi:4[72+(sustainmelody:sustaincounter),60,4000]
	`,
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

    script2: `-- danceeee


change = 6
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

    script3: ` -- dronezzzzz
pan = 0 

notes = [ 0 2 4 7 12 ]

p = 0
pc = 0
pnotes = [ 0 0 0 7 12 9 7 0]


every 8
	pc = pc + 1
	if pc == 8 [ pc = 0 ]

every 0.25 
	pan = random. 0 100
	play.organ[50, 12 + (pnotes:pc) , pan]

every 0.25 
	pan = random. 0 100
	play.organ[50, 24 + (pnotes:pc) , pan]

every 0.25 
	pan = random. 0 100
	play.organ[50, 19 + (pnotes:pc), pan]

every 16
	if counter == 5 [ counter = 0] 
	counter = counter + 1
 	play.bird:notes:counter[100, 0 ,50,2]

every 8
	play.piano:4[ 90, -24 + p, 50, 4]

every 0.25 
	play.organ[60,notes:counter,pan]

every 0.125 
	play.organ[60,(notes:counter)+ 12,pan]


    `,

    script4: `-- midi example
-- midi[note,velocity,duration] (notes up to 127, velocity up to 100, duration in ms)

i=0
notes = [1 6 10 13 18]
note = 3
pCounter = 0 
pnote = 0

every 0.5
	pnote = notes:pCounter
	midi[pnote+ 50,60,300] -- here is the midi code :-)
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

