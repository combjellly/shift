var randpresets = {
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
`
};

