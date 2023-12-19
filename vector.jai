Vector :: struct (T : Type, N : s64)
{
    #assert #run IsNumeric (T) "T needs to be a numeric type.";
    #assert N > 0 "Invalid vector dimensions.";

    // @Note (stefan): F is a floating point type that is 4 bytes
    // long if T is at most 4 bytes long, or 8 bytes if T is more
    // than 4 bytes long. It is the same a T if T is a floating
    // point type. It is the type of the vector's Length.
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

    #place x;
    r, g, b : T;
}

#if N > 3
{
    #place w;
    a : T;

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
Angle :: inline (a : $V/Vector, b : V) -> V.F #must
{
    denominator := math.sqrt (Dot (a, a) * Dot (b, b));
    if ApproxZero (denominator, 0.00001)
        return 0;
    d := Clamp (Dot (a, b) / denominator, -1, 1);

    return math.acos (d);
}

Scale :: inline (left : $V/Vector, right : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = left.n[%1] * right.n[%1];", i);

    return to_string (*builder);
};

    return result;
}

Dot :: inline (left : $V/Vector, right : V) -> V.T #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
    {
        print_to_builder (*builder, "left.n[%1] * right.n[%1]", i);
        if i != V.N - 1
            print_to_builder (*builder, " + ");
    }
    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

SqrdLength :: inline (vec : $V/Vector) -> V.F #must
{
    return Dot (vec, vec);
}

Length :: inline (vec : $V/Vector) -> V.F #must
{
    return math.sqrt (cast (V.F) Dot (vec, vec));
}

Normalized :: inline (vec : $V/Vector, fallback : V = .{}, epsilon : V.F = 0.00001) -> V #must, len : V.F
{
    if ApproxZero (vec, epsilon)
        return fallback, 0;
    len := Length (vec);

    return vec * (1.0 / len), len;
}

Clamp :: inline (vec : $V/Vector, min : V, max : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Clamp (vec.n[%1], min.n[%1], max.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Round :: inline (vec : $V/Vector) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Round (vec.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Floor :: inline (vec : $V/Vector) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Floor (vec.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Ceil :: inline (vec : $V/Vector) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Ceil (vec.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Min :: inline (a : $V/Vector, b : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Min (a.n[%1], b.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Max :: inline (a : $V/Vector, b : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Max (a.n[%1], b.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Abs :: inline (vec : $V/Vector) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Abs (vec.n[%1]);", i);

    return to_string (*builder);
};

    return result;
}

Lerp :: inline (a : $V/Vector, b : V, t : $T2) -> V #must
#modify { return IsFloatingPoint (T2), "T2 must be a float type."; }
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = Lerp (a.n[%1], b.n[%1], t);", i);

    return to_string (*builder);
};

    return result;
}

SqrdDistance :: inline (a : $V/Vector, b : V) -> V.F #must
{
    return SqrdLength (b - a);
}

Distance :: inline (a : $V/Vector, b : V) -> V.F #must
{
    return Length (b - a);
}

Reflect :: inline (incident : $V/Vector, normal : V) -> V #must
{
    return incident - 2 * Dot (normal, incident) * normal;
}

Project :: inline (left : $V/Vector, right : V) -> V #must
{
    return right * (Dot (left, right) / Dot (right, right));
}

Reject :: inline (left : $V/Vector, right : V) -> V #must
{
    return left - right * (Dot (left, right) / Dot (right, right));
}

Add :: inline (left : $V/Vector, right : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = left.n[%1] + right.n[%1];", i);

    return to_string (*builder);
};

    return result;
}

Sub :: inline (left : $V/Vector, right : V) -> V #must
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = left.n[%1] - right.n[%1];", i);

    return to_string (*builder);
};

    return result;
}

Neg :: inline (vec : $V/Vector) -> V #must
{
    #assert #run IsSigned (V.T) "Cannot negate an unsigned vector.";

    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = -vec.n[%1];", i);

    return to_string (*builder);
};

    return result;
}

Mul :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = cast (V.T) (left.n[%1] * right);", i);

    return to_string (*builder);
};

    return result;
}

Div :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    result : V = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
        println_to_builder (*builder, "result.n[%1] = cast (V.T) (left.n[%1] / right);", i);

    return to_string (*builder);
};

    return result;
}

Equals :: inline (left : $V/Vector, right : V) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
    {
        print_to_builder (*builder, "left.n[%1] == right.n[%1]", i);
        if i != V.N - 1
            print_to_builder (*builder, " && ");
    }
    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

ApproxZero :: inline (vec : $V/Vector, epsilon : V.F) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
    {
        print_to_builder (*builder, "ApproxZero (vec.n[%1], epsilon)", i);
        if i != V.N - 1
            print_to_builder (*builder, " && ");
    }
    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

ApproxEquals :: inline (left : $V/Vector, right : V, epsilon : V.F) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..V.N - 1
    {
        print_to_builder (*builder, "ApproxEquals (left.n[%1], right.n[%1], epsilon)", i);
        if i != V.N - 1
            print_to_builder (*builder, " && ");
    }
    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

operator *[] :: inline (vec : $V/Vector, index : s64) -> *V.T #must
{
    return *vec.n[index];
}

operator + :: inline (left : $V/Vector, right : V) -> V #must
{
    return Add (left, right);
}

operator - :: inline (left : $V/Vector, right : V) -> V #must
{
    return Sub (left, right);
}

operator - :: inline (vec : $V/Vector) -> V #must
{
    #assert #run IsSigned (V.T) "Cannot negate an unsigned vector.";

    return Neg (vec);
}

operator * :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return IsNumeric (T), "T must be numeric."; }
#symmetric
{
    return Mul (left, right);
}

operator / :: inline (left : $V/Vector, right : $T) -> V #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return Div (left, right);
}

operator == :: inline (left : $V/Vector, right : V) -> bool #must
{
    return Equals (left, right);
}

/* Vec2 */

Rotate :: inline (v : $V/Vec2, theta : V.F) -> V #must
{
    c := math.cos (theta);
    s := math.sin (theta);

    return .{cast (V.T) (v.x * c - v.y * s), cast (V.T) (v.x * s + v.y * c)};
}

SignedAngle :: inline (a : $V/Vec2, b : V) -> V.F #must
{
    s := Sign (a.x * b.y - a.y * b.y);

    return Angle (a, b) * s;
}

PerpendicularCW :: inline (vec : $V/Vec2) -> V #must
{
    return .{vec.y, -vec.x};
}

PerpendicularCCW :: inline (vec : $V/Vec2) -> V #must
{
    return .{-vec.y, vec.x};
}

/* Vec3 */

MakeVec3 :: inline (xy : Vec2 ($T), z : T = 0) -> Vec3 (T) #must
{
    result : Vec3 (T) = ---;
    result.xy = xy;
    result.z = z;

    return result;
}

MakeVec3f :: inline (xy : Vec2f, z : f32 = 0) -> Vec3f #must
{
    return MakeVec3 (xy, z);
}

MakeVec3d :: inline (xy : Vec2d, z : f64 = 0) -> Vec3d #must
{
    return MakeVec3 (xy, z);
}

MakeVec3u :: inline (xy : Vec2u, z : u32 = 0) -> Vec3u #must
{
    return MakeVec3 (xy, z);
}

MakeVec3s :: inline (xy : Vec2s, z : s16 = 0) -> Vec3s #must
{
    return MakeVec3 (xy, z);
}

MakeVec3i :: inline (xy : Vec2i, z : s32 = 0) -> Vec3i #must
{
    return MakeVec3 (xy, z);
}

MakeVec3l :: inline (xy : Vec2l, z : s64 = 0) -> Vec3l #must
{
    return MakeVec3 (xy, z);
}

Cross :: inline (left : Vector ($T, 3), right : Vector (T, 3)) -> Vector (T, 3) #must
{
    result : Vector (T, 3) = ---;
    result.x = left.y * right.z - left.z * right.y;
    result.y = left.z * right.x - left.x * right.z;
    result.z = left.x * right.y - left.y * right.x;

    return result;
}

SignedAngle :: inline (a : $V/Vec3, b : V, axis : V) -> V.F #must
{
    c := Cross (a, b);
    s := Sign (Dot (axis, c));

    return Angle (a, b) * s;
}

RightToOrthonormalBasis :: inline (right : $V/Vec3, base_axes := context.coordinate_axes) -> up : V, forward : V
{
    up := Cross (base_axes.forward, right);
    forward : V = ---;
    if ApproxZero (Dot (up, up), 0.000001)
    {
        forward = Cross (right, base_axes.up);
        up = Cross (forward, right);
    }
    else
        forward = Cross (right, up);

    return Normalized (up), Normalized (forward);
}

UpToOrthonormalBasis :: inline (up : $V/Vec3, base_axes := context.coordinate_axes) -> right : V, forward : V
{
    right := Cross (up, base_axes.forward);
    forward : V = ---;
    if ApproxZero (Dot (right, right), 0.000001)
    {
        forward = Cross (base_axes.right, up);
        right = Cross (up, forward);
    }
    else
        forward = Cross (right, up);

    return Normalized (right), Normalized (forward);
}

ForwardToOrthonormalBasis :: inline (forward : $V/Vec3, base_axes := context.coordinate_axes) -> right : V, up : V
{
    right := Cross (base_axes.up, forward);
    up : V = ---;
    if ApproxZero (Dot (right, right), 0.000001)
    {
        up = Cross (forward, base_axes.right);
        right = Cross (up, forward);
    }
    else
        up = Cross (forward, right);

    return Normalized (right), Normalized (up);
}

/* Vec4 */

MakeVec4 :: inline (xyz : Vector ($T, 3), w : T = 0) -> Vec4 (T) #must
{
    result : Vec4 (T) = ---;
    result.xyz = xyz;
    result.w = w;

    return result;
}

MakeVec4f :: inline (xyz : Vec3f, w : f32 = 0) -> Vec4f #must
{
    return MakeVec4 (xyz, w);
}

MakeVec4d :: inline (xyz : Vec3d, w : f64 = 0) -> Vec4d #must
{
    return MakeVec4 (xyz, w);
}

MakeVec4u :: inline (xyz : Vec3u, w : u32 = 0) -> Vec4u #must
{
    return MakeVec4 (xyz, w);
}

MakeVec4s :: inline (xyz : Vec3s, w : s16 = 0) -> Vec4s #must
{
    return MakeVec4 (xyz, w);
}

MakeVec4i :: inline (xyz : Vec3i, w : s32 = 0) -> Vec4i #must
{
    return MakeVec4 (xyz, w);
}

MakeVec4l :: inline (xyz : Vec3l, w : s64 = 0) -> Vec4l #must
{
    return MakeVec4 (xyz, w);
}
