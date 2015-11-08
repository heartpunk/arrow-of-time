Arrow of Time
=============

*This project is on (loving) hiatus!*

In [Why Information Grows](http://www.amazon.com/Why-Information-Grows-Evolution-Economies/dp/0465048994) there's a beautiful visual-imagination heavy explanation of why time is irreversible, based on the idea of [chaos](https://en.wikipedia.org/wiki/Chaos_theory).

>Imagine that the ping-pong balls collide with one another without losing energy, so these interactions never cease. Next, assume that you started observing the system when all of the ping-pong balls were located neatly in one quadrant of the box, but also were endowed with enough kinetic energy—or speed—to eventually scatter all around the box. This is similar to the drop-of-ink example we used before.
>
>In this simple statistical system, the question of the reversibility of time is the question of whether it is possible, at any given point in time, to reverse the motion of the ping-pong balls such that time is seen to run backward. That is, is it possible to put the ping-pong balls on a trajectory where the final state is the neat arrangement that we defined as the initial configuration?
>
>Thinking of what happens when we run this “movie” forward is easy. Ping-pong balls fill up the box with their incessant motion, ending up in what we now know as a dynamic steady state. But let’s give the time-reversal experiment a shot. To make things easy I will assume that we have two machines. One of the machines is able to take any number of balls and modify their velocities instantly if we provide the machine with an input file containing the desired velocities for each ball. This machine has infinite precision, but it executes instructions only with the precision of the information that it is fed. That is, if positions and velocities are provided with a precision of two digits (i.e., speed in centimeters per second), then the machine will assign the velocities of the balls with only that precision, making all unspecified decimals (i.e., millimeters per second and beyond) random. The second machine we have available measures the position and velocity of each ball with a finite but arbitrarily large precision. So the question is, can we use these two imaginary machines to reverse the velocities of the system such that the “movie” plays backward?

I wanted to turn this into an interactive visualization, rather than just a bunch of text. I still intend to do that, but it's lower priority than my remaining projects at [Recurse Center](https://recurse.com), and I've learned most of what I would learn from this. So, I'll finish this up once I'm back in the working world again. I hope. I'm pretty confident I actually will, honestly, because there's not much left to this, and the idea is very, very interesting to me.

Interesting Notes
-----------------

There are a lot of ways to approach <strike>collision detection</strike>thermodynamics simulations! It's worth reading up on them if you're interested in this, and definitely worth making sure you research the one that's more your cup of tea (the approaches used in the thermodynamics world and the gaming world are different in important ways!). For some good reading on how to do it, [check this out](http://introcs.cs.princeton.edu/java/assignments/collisions.html). There's some other cool stuff in [this paper](http://www.cse.unsw.edu.au/~pls/thesis/munc-thesis.pdf), but it's more gaming oriented, and less thermodynamics, and is _really_ heavily [FRP](https://en.wikipedia.org/wiki/Functional_reactive_programming), which is not a good or bad thing, but might be helpful to know before you start reading the paper.

Quadtrees are really cool, and pretty simple! I wish I'd started using one early on in the project, but I kept limping along without it. My implementation would be a fair bit faster if I'd used one, and it turns out there already is a library in Elm for this. More importantly, my debugging would've been faster. Oh well!

I'm pretty happy that I derived pretty much both of the approaches described in the [Princeton link above](http://introcs.cs.princeton.edu/java/assignments/collisions.html) on my own, even if I only implemented one of them. The derivation for collision prediction was admittedly beyond me (at least with how much I was rushing – I'm sure I could've gotten it with time), but that's a caveat I'm happy to own up to.
