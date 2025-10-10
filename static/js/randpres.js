var randpresets = {
    script0: `every 1
    note = [ 0 2 4 7 9 11]
    play.junoceleste[gate * 40, note:c]

    c = c + 1
    if c == 6 

    [ 

    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
    
    ]
`,
    script1: `every 0.25
    note = [ 0 2 4 7 9 11]
    play.junoflute[gate * 40, randomnote:c]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script3: `birdsong = 0

every 20
    birdsong = random. [0 1 4 5 8 9]
    play.bird:birdsong
`,
    script4: `every 0.25
    note = [ 0 2 4 7 9 11]
    play.junoguitar[gate * 40, note:c]

    c = c + 1


    if c == 6 [ 
    gate = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script5: `every 0.25
    randomnote = [ 0 2 4 7 9 11]
    play.junoceleste[trigger * 40, randomnote:c]

    c = c + 1


    if c == 6 [ 
    trigger = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`,
    script6: `every 2
    randomnote = random . [ 0 2 4 7 9 11]
    play.junoceleste[trigger * 40, randomnote]

    c = c + 1


    if c == 6 [ 
    trigger = random. [ 0 0 0 0 1 0 ]
    c = 0 
        ]
`
};

