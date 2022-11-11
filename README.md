# Linalg

Linear algebra module written in Jai, for games.

# Dependencies
Linalg depends on a module named 'Math', that provides your good old trigonometry and square root functions (cos, sin, tan, acos, asin, atan2, sqrt).
It also depends on my [Fmt module](https://github.com/ostef/jai-fmt).

# Coordinate system
There is a `Coordinate_Axes` struct, that holds a right, up and forward vector. A variable named `coordinate_axes` is in the Context as well.
Some functions take a `Coordinate_Axes` as parameter, defaulting to `context.coordinate_axes`, that let you specify what vectors the function should use as coordinate axes.
The function `coordinate_axes :: (right, up, forward : string)` lets you create a `Coordinate_Axes` by specifying strings in the format `[+-][XYZ]`.  
By default, `context.coordinate_axes` is set to right = +X, up = +Y, forward = +Z, which is a left handed coordinate system.

> Note: This is an experimental feature that is not used everywhere it should be.
