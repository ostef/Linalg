# Linalg

Linear algebra module written in Jai, for games.

# Coordinate system
There is a `CoordinateAxes` struct, that holds a right, up and forward vector. A variable named `coordinate_axes` is in the Context as well.
Some functions take a `CoordinateAxes` as parameter, defaulting to `context.coordinate_axes`, that let you specify what vectors the function should use as coordinate axes.
The function `coordinate_axes :: (right, up, forward : string)` lets you create a `CoordinateAxes` by specifying strings in the format `[+-][XYZ]`.
By default, `context.coordinate_axes` is set to right = +X, up = +Y, forward = +Z, which is a left handed coordinate system.

> Note: This is an experimental feature that is not used everywhere it should be.
