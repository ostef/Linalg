// @Note (stefan): We import Math for trig functions.
// I've moved a lot of the math functions from my Math module
// to utils.jai so that users can use any Math module, since
// any sensible Math module should provide trigonometry.
math :: #import "Math";
#import "Fmt";

#load "utils.jai";
#load "vector.jai";
#load "matrix.jai";
#load "quaternion.jai";
#load "euler.jai";

Coordinate_Axes :: struct
{
	right, up, forward : Vec3f;
}

#add_context coordinate_axes := Coordinate_Axes.{ .{ 1, 0, 0 }, .{ 0, 1, 0 }, .{ 0, 0, 1 } };

coordinate_axes :: inline (right : Vec3f, up : Vec3f, forward : Vec3f) -> Coordinate_Axes #must
{
	result : Coordinate_Axes = ---;
	result.right = right;
	result.up = up;
	result.forward = forward;

	return result;
}

coordinate_axes :: inline (right : string, up : string, forward : string) -> Coordinate_Axes #must
{
	is_axis :: inline (str : string) -> bool #must
	{
		if str.count != 2
			return false;
		if str[0] != #char "+" && str[0] != #char "-"
			return false;
		if str[1] != #char "X" && str[1] != #char "Y" && str[1] != #char "Z"
			return false;
		return true;
	}

	to_vector :: inline (axis : string) -> Vec3f #must
	{
		sign := ifx axis[0] == #char "-" then -1.0 else 1.0;
		vector : Vec3f;
		if axis[1] == #char "X"
			vector.x = sign;
		else if axis[1] == #char "Y"
			vector.y = sign;
		else if axis[1] == #char "Z"
			vector.z = sign;
		return vector;
	}

	assert (is_axis (right), "Invalid parameter for right (must be [-+][XYZ]).");
	assert (is_axis (up), "Invalid parameter for up (must be [-+][XYZ]).");
	assert (is_axis (forward), "Invalid parameter for forward (must be [-+][XYZ]).");
	assert (right[1] != up[1] && up[1] != forward[1] && right[1] != forward[1], "Cannot have the same value for right, up and forward.");

	return coordinate_axes (to_vector (right), to_vector (up), to_vector (forward));
}

#scope_module

f32 :: float32;
f64 :: float64;

PI   : f32 : 3.1415927;
PI64 : f64 : 3.141592653589793;
