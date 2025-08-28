# Claudius Examples

A library of examples of using [Claudius](https://github.com/claudiusFX/claudius), a simple retro-style graphics library for OCaml. Many originally created for [Tiny Code Christmas](https://tcc.lovebyte.party) and [Genuary](https://genuary.art).

To run these you will need to install Claudius via opam, and then you can `dune exec [example_name]`.

# Examples Directory

## Using primitives

These examples just show a single feature of Claudius, and are a good place to start if you're trying to understand how Claudius works.

* [Bounce](/bounce/) - example of a bouncing ball made from ellipses.
* [Flying Camels](/flying_camels/) - example of using image files to recreate a classic screen saver effect.
* [Polygons](/polygons/) - rotating filled polygons.
* [Red Hexagon](/red_hexagon/) - a different style of rotating polyons.
* [Screen saver](/screen_saver/) - a recreation of a classic screen saver effect from the early Macintosh era.
* [Shapes](/shapes/) - using simple shape outlines to create some animation effects.
* [TextGrid](/textgrid/) - drawing characters using the built in font for Claudius.
* [DSOTM](/dsotm/) - using a combination of primitive shapes to build up a familiar image.

## Algorithmic effects

These examples start to use Claudius to build up interesting effects based on some simple algorithms

* [Fern](/fern/) - drawing a [Bansley Fern fractal](https://en.wikipedia.org/wiki/Barnsley_fern).
* [Julia Set](/julia_set/) - an animated [Julia set fractal](https://en.wikipedia.org/wiki/Julia_set).
* [Koch Snowflake](/koch_snowflake/) - an example of using recursion to draw a [Koch snowflake fractal](https://en.wikipedia.org/wiki/Koch_snowflake).
* [Koch Snowflake Filled](/koch_snowflake_filled/) - a filled in version of the [Koch snowflake example](/koch_snowflake/).
* [Lorenz](/lorenz/) - an animated [Lorenz Attractor](https://en.wikipedia.org/wiki/Lorenz_system).
* [Murmuration](/murmuration/) - another Lorenz Attractor, but this time used to mimic a flock of swarming birds.
* [Plasma](/plasma/) - an example of a classic [plasma effect](https://en.wikipedia.org/wiki/Plasma_effect).
* [Tunnel](/tunnel/) - an example of how simple math functions can create complex looking scenes.

## Misc effects

Other more complex effects created using Claudius

* [Anni Albers](/anni_albers/) - a homage to leading textile artist [Anni Albers](https://en.wikipedia.org/wiki/Anni_Albers).
* [Anni Albers Also](/anni_albers_too/) - a second homage to leading textile artist [Anni Albers](https://en.wikipedia.org/wiki/Anni_Albers).
* [Islamic pattern](/islamic_pattern/) - an example playing with [Islamic geometric patterns](https://en.wikipedia.org/wiki/Islamic_geometric_patterns) and positive/negative space.
* [Islamic pattern filled](/islamic_pattern_filled/) - a solid verson of the [Islamic pattern example](/islamic_pattern/).
* [Particles](/particles/) - creating a pseudo-3D point cloud of a planet.
* [Landscape](/landscape/) - using the algorithm from the [Plasma example](/plasma/) to create a pseudo-3D landscape.
* [Lava lamp](/lava_lamp/) - yet another effect routed in the [Plasma example](/plasma/), this time with dithering added.
* [Physics](/physics/) - an example of using a simple [Hooke's law](https://en.wikipedia.org/wiki/Hooke%27s_law) library to add physics to the [Landscape](/landscape/) example.
* [Walking Mesh](/walking_mesh/) - an example of what looks like a stateful effect being implemented functionally.
* [Vera Molnár](/vera_molnar/) - a homage to pioneering digital artist [Vera Molnár](https://en.wikipedia.org/wiki/Vera_Molnár).

## Interactive programs

* [Paint](/paint/) - a simple painting program in a few lines of OCaml using Claudius.
* [Filedrop](/filedrop/) - a simple GIF image viewer that accepts files dropped on it.
* [keytest](/keytest/) - an example that shows what key was pressed.
