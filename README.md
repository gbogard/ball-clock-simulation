# Scala Ball clock simulation

A simple ball clock simulation written using Monocle. [Exercise instructions](./ball_clock.txt).

## Goals

* Purely functional : no mutable state, side effects are handled through algebraic effects. State updates
are handled through lenses.
* Segregation of responsibilities : the application follows *hexagonal architecture* principles. The domain
objects are separated from the core business logic, itself separated from the user interface. This allows to
expose the application through a variety of interfaces, not just command-line, with no changes from the core logic.
* Tested

## TODO 

* Get rid of the `reverse` method and write a recursive function that moves balls from one track to
another one-by-one to comply with the constraints of the exercise.

## How to use

`sbt run <number of balls> [<number of minutes>]`

The number of balls must be between 21 and 127 (inclusive). Using the program
with wrong arguments will yield a help message.


