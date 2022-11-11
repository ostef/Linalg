Vector :: struct (T : Type, N : s64)
{
	#assert #run is_numeric (T) "T needs to be a numeric type.";
	#assert N > 0 "Invalid vector dimensions.";

	// @Note (stefan): F is a floating point type that is 4 bytes
	// long if T is at most 4 bytes long, or 8 bytes if T is more
	// than 4 bytes long. It is the same a T if T is a floating
	// point type. It is the type of the vector's length.
#if T == f32 || T == f64
	F :: T;
else #if size_of (T) > 4
	F :: f64;
else
	F :: f32;

	x : T;
#if N > 1
	y : T;
#if N > 2
	z : T;
#if N > 3
	w : T;

#if N > 2
{
	#place x;
	xy : Vector (T, 2);
}

#if N > 3
{
	#place y;
	yz : Vector (T, 2);
	#place z;
	zw : Vector (T, 2);
	#place x;
	xyz : Vector (T, 3);
	#place y;
	yzw : Vector (T, 3);
}

	#place x;
	n : [N]T;
}

Vec2 :: #bake_arguments Vector (N = 2);
Vec3 :: #bake_arguments Vector (N = 3);
Vec4 :: #bake_arguments Vector (N = 4);
Vec2f :: Vec2 (f32);
Vec3f :: Vec3 (f32);
Vec4f :: Vec4 (f32);
Vec2d :: Vec2 (f64);
Vec3d :: Vec3 (f64);
Vec4d :: Vec4 (f64);
Vec2u :: Vec2 (u32);
Vec3u :: Vec3 (u32);
Vec4u :: Vec4 (u32);
Vec2s :: Vec2 (s16);
Vec3s :: Vec3 (s16);
Vec4s :: Vec4 (s16);
Vec2i :: Vec2 (s32);
Vec3i :: Vec3 (s32);
Vec4i :: Vec4 (s32);
Vec2l :: Vec2 (s64);
Vec3l :: Vec3 (s64);
Vec4l :: Vec4 (s64);

// @Note (stefan): This works for Vec2 and Vec3, I'm assuming it does for
// more dimensions, but I'm not sure.
angle :: inline (a : $V/Vector, b : V) -> V.F #must
{
	denominator := sqrt (dot (a, a) * dot (b, b));
	if approx_zero (denominator)
		return 0;
	d := clamp (dot (a, b) / denominator, -1, 1);

	return math.acos (d);
}

scale :: inline (left : $V/Vector, right : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = left.n[{0}] * right.n[{0}];", i);

	return to_string (builder);
};

	return result;
}

dot :: inline (left : $V/Vector, right : V) -> V.T #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
	{
		print_to_builder (*builder, "left.n[{0}] * right.n[{0}]", i);
		if i != V.N - 1
			print_to_builder (*builder, " + ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

sqrd_length :: inline (vec : $V/Vector) -> V.F #must
{
	return dot (vec, vec);
}

length :: inline (vec : $V/Vector) -> V.F #must
{
	return sqrt (cast (V.F) dot (vec, vec));
}

normalized :: inline (vec : $V/Vector, fallback : V = .{}, epsilon : V.F = 0.00001) -> V #must, len : V.F
{
	if approx_zero (vec, epsilon)
		return fallback, 0;
	len := length (vec);

	return vec * (1.0 / len), len;
}

clamp :: inline (vec : $V/Vector, min : V, max : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = clamp (vec.n[{0}], min.n[{0}], max.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

round :: inline (vec : $V/Vector) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = round (vec.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

floor :: inline (vec : $V/Vector) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = floor (vec.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

ceil :: inline (vec : $V/Vector) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = ceil (vec.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

min :: inline (a : $V/Vector, b : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = min (a.n[{0}], b.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

max :: inline (a : $V/Vector, b : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = max (a.n[{0}], b.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

abs :: inline (vec : $V/Vector) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = abs (vec.n[{0}]);", i);

	return to_string (builder);
};

	return result;
}

lerp :: inline (a : $V/Vector, b : V, t : $T2) -> V #must
#modify { return is_floating_point (T2), "T2 must be a float type."; }
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = lerp (a.n[{0}], b.n[{0}], t);", i);

	return to_string (builder);
};

	return result;
}

sqrd_distance :: inline (a : $V/Vector, b : V) -> V.F #must
{
	return sqrd_length (b - a);
}

distance :: inline (a : $V/Vector, b : V) -> V.F #must
{
	return length (b - a);
}

reflect :: inline (incident : $V/Vector, normal : V) -> V #must
{
	return incident - 2 * dot (normal, incident) * normal;
}

project :: inline (left : $V/Vector, right : V) -> V #must
{
	return right * (dot (left, right) / dot (right, right));
}

reject :: inline (left : $V/Vector, right : V) -> V #must
{
	return left - right * (dot (left, right) / dot (right, right));
}

add :: inline (left : $V/Vector, right : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = left.n[{0}] + right.n[{0}];", i);

	return to_string (builder);
};

	return result;
}

sub :: inline (left : $V/Vector, right : V) -> V #must
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = left.n[{0}] - right.n[{0}];", i);

	return to_string (builder);
};

	return result;
}

neg :: inline (vec : $V/Vector) -> V #must
{
	#assert #run is_signed (V.T) "Cannot negate an unsigned vector.";

	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = -vec.n[{0}];", i);

	return to_string (builder);
};

	return result;
}

mul :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = cast (V.T) (left.n[{0}] * right);", i);

	return to_string (builder);
};

	return result;
}

div :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	result : V = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
		println_to_builder (*builder, "result.n[{0}] = cast (V.T) (left.n[{0}] / right);", i);

	return to_string (builder);
};

	return result;
}

equals :: inline (left : $V/Vector, right : V) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
	{
		print_to_builder (*builder, "left.n[{0}] == right.n[{0}]", i);
		if i != V.N - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

approx_zero :: inline (vec : $V/Vector, epsilon : V.F) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
	{
		print_to_builder (*builder, "approx_zero (vec.n[{0}], epsilon)", i);
		if i != V.N - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

approx_equals :: inline (left : $V/Vector, right : V, epsilon : V.F) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
	{
		print_to_builder (*builder, "approx_equals (left.n[{0}], right.n[{0}], epsilon)", i);
		if i != V.N - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

operator *[] :: inline (vec : $V/Vector, index : s64) -> *V.T #must
{
	return *vec.n[index];
}

operator + :: inline (left : $V/Vector, right : V) -> V #must
{
	return add (left, right);
}

operator - :: inline (left : $V/Vector, right : V) -> V #must
{
	return sub (left, right);
}

operator - :: inline (vec : $V/Vector) -> V #must
{
	#assert #run is_signed (V.T) "Cannot negate an unsigned vector.";

	return neg (vec);
}

operator * :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return is_numeric (T), "T must be numeric."; }
#symmetric
{
	return mul (left, right);
}

operator / :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	return div (left, right);
}

operator == :: inline (left : $V/Vector, right : V) -> bool #must
{
	return equals (left, right);
}

/* Vec2 */

vec2 :: inline (x : $T, y : T) -> Vec2 (T) #must
{
	result : Vec2 (T) = ---;
	result.x = x;
	result.y = y;

	return result;
}

vec2f :: inline (x : f32, y : f32) -> Vec2f #must
{
	return vec2 (x, y);
}

vec2d :: inline (x : f64, y : f64) -> Vec2d #must
{
	return vec2 (x, y);
}

vec2u :: inline (x : u32, y : u32) -> Vec2u #must
{
	return vec2 (x, y);
}

vec2s :: inline (x : s16, y : s16) -> Vec2s #must
{
	return vec2 (x, y);
}

vec2i :: inline (x : s32, y : s32) -> Vec2i #must
{
	return vec2 (x, y);
}

vec2l :: inline (x : s64, y : s64) -> Vec2l #must
{
	return vec2 (x, y);
}

rotate :: inline (v : $V/Vec2, theta : V.F) -> V #must
{
	c := math.cos (theta);
	s := math.sin (theta);

	return vec2 (cast (V.T) (v.x * c - v.y * s), cast (V.T) (v.x * s + v.y * c));
}

signed_angle :: inline (a : $V/Vec2, b : V) -> V.F #must
{
	s := sign (a.x * b.y - a.y * b.y);

	return angle (a, b) * s;
}

perpendicular_cw :: inline (vec : $V/Vec2) -> V #must
{
	return vec2 (vec.y, -vec.x);
}

perpendicular_ccw :: inline (vec : $V/Vec2) -> V #must
{
	return vec2 (-vec.y, vec.x);
}

/* Vec3 */

vec3 :: inline (x : $T, y : T, z : T) -> Vec3 (T) #must
{
	result : Vec3 (T) = ---;
	result.x = x;
	result.y = y;
	result.z = z;

	return result;
}

vec3 :: inline (xy : Vec2 ($T), z : T = 0) -> Vec3 (T) #must
{
	result : Vec3 (T) = ---;
	result.xy = xy;
	result.z = z;

	return result;
}

vec3f :: inline (x : f32, y : f32, z : f32) -> Vec3f #must
{
	return vec3 (x, y, z);
}

vec3f :: inline (xy : Vec2f, z : f32 = 0) -> Vec3f #must
{
	return vec3 (xy, z);
}

vec3d :: inline (x : f64, y : f64, z : f64) -> Vec3d #must
{
	return vec3 (x, y, z);
}

vec3d :: inline (xy : Vec2d, z : f64 = 0) -> Vec3d #must
{
	return vec3 (xy, z);
}

vec3u :: inline (x : u32, y : u32, z : u32) -> Vec3u #must
{
	return vec3 (x, y, z);
}

vec3u :: inline (xy : Vec2u, z : u32 = 0) -> Vec3u #must
{
	return vec3 (xy, z);
}

vec3s :: inline (x : s16, y : s16, z : s16) -> Vec3s #must
{
	return vec3 (x, y, z);
}

vec3s :: inline (xy : Vec2s, z : s16 = 0) -> Vec3s #must
{
	return vec3 (xy, z);
}

vec3i :: inline (x : s32, y : s32, z : s32) -> Vec3i #must
{
	return vec3 (x, y, z);
}

vec3i :: inline (xy : Vec2i, z : s32 = 0) -> Vec3i #must
{
	return vec3 (xy, z);
}

vec3l :: inline (x : s64, y : s64, z : s64) -> Vec3l #must
{
	return vec3 (x, y, z);
}

vec3l :: inline (xy : Vec2l, z : s64 = 0) -> Vec3l #must
{
	return vec3 (xy, z);
}

cross :: inline (left : Vector ($T, 3), right : Vector (T, 3)) -> Vector (T, 3) #must
{
	result : Vector (T, 3) = ---;
	result.x = left.y * right.z - left.z * right.y;
	result.y = left.z * right.x - left.x * right.z;
	result.z = left.x * right.y - left.y * right.x;

	return result;
}

signed_angle :: inline (a : $V/Vec3, b : V, axis : V) -> V.F #must
{
	c := cross (a, b);
	s := sign (dot (axis, c));

	return angle (a, b) * s;
}

right_to_orthonormal_basis :: inline (right : $V/Vec3, base_axes := context.coordinate_axes) -> up : V, forward : V
{
	up := cross (base_axes.forward, right);
	forward : V = ---;
	if approx_zero (dot (up, up), 0.000001)
	{
		forward = cross (right, base_axes.up);
		up = cross (forward, right);
	}
	else
		forward = cross (right, up);
	
	return normalized (up), normalized (forward);
}

up_to_orthonormal_basis :: inline (up : $V/Vec3, base_axes := context.coordinate_axes) -> right : V, forward : V
{
	right := cross (up, base_axes.forward);
	forward : V = ---;
	if approx_zero (dot (right, right), 0.000001)
	{
		forward = cross (base_axes.right, up);
		right = cross (up, forward);
	}
	else
		forward = cross (right, up);
	
	return normalized (right), normalized (forward);
}

forward_to_orthonormal_basis :: inline (forward : $V/Vec3, base_axes := context.coordinate_axes) -> right : V, up : V
{
	right := cross (base_axes.up, forward);
	up : V = ---;
	if approx_zero (dot (right, right), 0.000001)
	{
		up = cross (forward, base_axes.right);
		right = cross (up, forward);
	}
	else
		up = cross (forward, right);
	
	return normalized (right), normalized (up);
}

/* Vec4 */

vec4 :: inline (x : $T, y : T, z : T, w : T) -> Vec4 (T) #must
{
	result : Vec4 (T) = ---;
	result.x = x;
	result.y = y;
	result.z = z;
	result.w = w;

	return result;
}

vec4 :: inline (xyz : Vector ($T, 3), w : T = 0) -> Vec4 (T) #must
{
	result : Vec4 (T) = ---;
	result.xyz = xyz;
	result.w = w;

	return result;
}

vec4f :: inline (x : f32, y : f32, z : f32, w : f32) -> Vec4f #must
{
	return vec4 (x, y, z, w);
}

vec4f :: inline (xyz : Vec3f, w : f32 = 0) -> Vec4f #must
{
	return vec4 (xyz, w);
}

vec4d :: inline (x : f64, y : f64, z : f64, w : f64) -> Vec4d #must
{
	return vec4 (x, y, z, w);
}

vec4d :: inline (xyz : Vec3d, w : f64 = 0) -> Vec4d #must
{
	return vec4 (xyz, w);
}

vec4u :: inline (x : u32, y : u32, z : u32, w : u32) -> Vec4u #must
{
	return vec4 (x, y, z, w);
}

vec4u :: inline (xyz : Vec3u, w : u32 = 0) -> Vec4u #must
{
	return vec4 (xyz, w);
}

vec4s :: inline (x : s16, y : s16, z : s16, w : s16) -> Vec4s #must
{
	return vec4 (x, y, z, w);
}

vec4s :: inline (xyz : Vec3s, w : s16 = 0) -> Vec4s #must
{
	return vec4 (xyz, w);
}

vec4i :: inline (x : s32, y : s32, z : s32, w : s32) -> Vec4i #must
{
	return vec4 (x, y, z, w);
}

vec4i :: inline (xyz : Vec3i, w : s32 = 0) -> Vec4i #must
{
	return vec4 (xyz, w);
}

vec4l :: inline (x : s64, y : s64, z : s64, w : s64) -> Vec4l #must
{
	return vec4 (x, y, z, w);
}

vec4l :: inline (xyz : Vec3l, w : s64 = 0) -> Vec4l #must
{
	return vec4 (xyz, w);
}
