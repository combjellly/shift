# shift

Tutorials and Documentation: https://shift.nickhernandez.ca/tutorial/intro
Try it in browser: https://combjellly.github.io/shift/


## What is it?

Shift is a live coding language designed to make music with code! Existing in a large canon of similar languages, Shift is designed from the ground up to be easy to pick up by anyone. It is also a great intro to coding and was deisnged to use in classrooms! Research and process documentation can be found in the accompanying thesis, published in 2022: http://hdl.handle.net/11375/28305. Shift now features midi output, conditionals and variable states - features not included in the original thesis work. 

## How to use it

Shift is entirely web based, to access it you can go to: https://combjellly.github.io/shift/

The syntax borrows heavily from python & javascript.

In short, there are Every loops that look like this

``` every 20 ```

These every loops can be seen as a timing mechanism that execute any code below them every X number of beats. 

so in this example

```
every 20
  play.piano
```

```piano``` will play every 20 beats. 

It is important to note that shift is set so ```every 1``` is 120 beats per minute. Therefore, ```every 0.5``` would be like eighth notes (occuring every 240 beats). Don't be fooled though, there is no meter, just serendipitous chaos! 


if you have any questions please reach out to nicholas.brown2012@gmail.com.

hope u have a great day 🐸
