# Claudius Examples

A library of examples of using [Claudius](https://github.com/claudiusFX/claudius), a simple retro-style graphics library for OCaml. Many originally created for [Tiny Code Christmas](https://tcc.lovebyte.party) and [Genuary](https://genuary.art).

# Examples Directory

## Using primitives

These examples just show a single feature of Claudius, and are a good place to start if you're trying to understand how Claudius works.

* [Polygons](/polygons/) - rotating filled polygons.
* [Red Hexagon](/red_hexagon/) - a different style of rotating polyons.
* [Screen saver](/screen_saver/) - a recreation of a classic screen saver effect from the early Macintosh era.
* [Shapes](/shapes/) - using simple shape outlines to create some animation effects.

## Algorithmic effects

These examples start to use Claudius to build up interesting effects based on some simple algorithms

* [Julia Set](/julia_set/) - an animated [Julia set fractal](https://en.wikipedia.org/wiki/Julia_set).
* [Lorenz](/lorenz/) - an animated [Lorenz Attractor](https://en.wikipedia.org/wiki/Lorenz_system).
* [Murmuration](/murmuration/) - another Lorenz Attractor, but this time used to mimic a flock of swarming birds.
* [Plasma](/plasma/) - an example of a classic [plasma effect](https://en.wikipedia.org/wiki/Plasma_effect).

## Misc effects

Other more complex effects created using Claudius

* [Anni Albers](/anni_albers/) - a homage to leading textile artist [Anni Albers](https://en.wikipedia.org/wiki/Anni_Albers).
* [Particles](/particles/) - creating a pseudo-3D point cloud of a planet.
* [Landscape](/landscape/) - using the algorithm from the [Plasma example](/plasma/) to create a pseudo-3D landscape.
* [Lava lamp](/lava_lamp/) - yet another effect routed in the [Plasma example](/plasma/), this time with dithering added.
* [Vera Molnár](/vera_molnar/) - a homage to pioneering digital artist [Vera Molnár](https://en.wikipedia.org/wiki/Vera_Molnár).

## Interactive programs

* [Paint](/paint/) - a simple painting program in a few lines of OCaml using Claudius.
* [Filedrop](/filedrop/) - a simple GIF image viewer that accepts files dropped on it.
* [keytest](/keytest/) - an example that shows what key was pressed.