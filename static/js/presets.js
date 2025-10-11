var presets = {
    script0: `-- twinkle

volume = 40

every 1
    note = [ 0 2 4 7 9 11]
    play.junoceleste[gate * volume, note:c]

    c = c + 1
    if c == 6 

    [ 

    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
    
    ]
`,
    script1: `-- fastttt flute

volume = 40

every 0.25
    note = [ 0 2 4 7 9 11]
    play.junoflute[gate * volume, note:c]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script3: `birdsong = 0

volume = 40
pitch = 0 

every 20
    birdsong = random. [0 1 4 5 8 9]
    play.bird:birdsong[volume, pitch]
`,
    script4: `-- fastttt guitar

volume = 40

every 0.25
    note = [ 0 2 4 7 9 11]
    play.junoguitar[gate * volume, note:c]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script5: `-- fasttt twinkle

volume = 40


every 0.25
    note = [ 0 2 4 7 9 11]
    play.junoceleste[gate * volume, note:c]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script6: `-- rando twinkle

volume = 40

every 2
    note = random . [ 0 2 4 7 9 11]
    play.junoceleste[gate * volume, note]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script7: `mod = 0
volmod = 10
rideamp =  [ 50 40 40 40 50 40 40 40 ]
snareamp = [  0  0 55  0  0  0  55 30 ]
kickamp =  [  30  0  0 40 50  0  0  0 ]
counter = 0
off = 0

pitch = -2
pitch2 = 7 + pitch
time = 1
drone = 35

every 4 * time
	mod = mod + 1
	if mod >= 12 [ mod = 0 ]

every ((mod%2) + 1 ) * time
	play.junoceleste[40, 0, 50]

every 0.5 * time
	randomnote = [ 0 2 4 7 9 11]
	play.junoceleste[mod%3 * 20, randomnote:c, 30]

	c = c + 1
	if c >= 6 [ 
		c = 0 ]

every 0.75 * time
	randomnote = [ 0 2 4 7 9 11]
	play.junoceleste[mod%4 * 15, randomnote:c, 70]

	c = c + 1
	if c >= 6 [ 
		c = 0 ]

every 1 * time 
		-- first number 
	play.gba:0[off%2*50, -24]
	off = off + 1


every 4 * time -- chords
	chords = [ 0 2 0 2 5 5 0 2 0 4 5 2] 
	play.juno:(mod%2)+16[70, (chords:mod) - 12]



every 0.5 * time -- drums 
	
	-- sequencer 'clock'

	counter = counter + 1
	if counter >= 8 [ counter = 0]

	--stereo!! 
	play.ride:9[(rideamp:counter) - volmod, 0, 70 ]
	play.ride:2[(rideamp:counter) - volmod, 0, 30 ]

	play.snare:9[(snareamp:counter) , 0, 70]
	play.snare:11[(snareamp:counter), 0,30]

	play.kick:10[(kickamp:counter),0,70]
	play.kick:9[(kickamp:counter),0,30]

{- every 1 * time
	play.organ[drone, 20 + pitch2] -}

{- every 0.25 * time
	randompan = random. 0 100
	play.organ[drone,20 + pitch,randompan] -}

{- every 0.1 * time
	randompan = random. 0 100
	play.organ[drone,30 + pitch,randompan] -}

{- every 0.125 * time
	randompan = random. 0 100
	play.organ[drone, 27 + pitch,randompan] -}

`
};

