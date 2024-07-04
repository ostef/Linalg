Quat :: struct (T : Type)
{
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";

    V3 :: Vec3 (T);

    x, y, z : T;
    w : T = 1;

    #place x;
    vec : Vec3 (T) = ---;
    #place w;
    scalar : T = ---;

    #place x;
    as_vec4 : Vec4 (T) = ---;

    #place x;
    n : [4]T = ---;
}

Quatf :: Quat (f32);
Quatd :: Quat (f64);

MakeQuat :: inline (vec : Vec3 ($T), scalar : T) -> Quat (T) #must
{
    result : Quat (T) = ---;
    result.vec = vec;
    result.scalar = scalar;

    return result;
}

MakeQuatf :: inline (vec : Vec3f, scalar : f32) -> Quatf #must
{
    return MakeQuat (vec, scalar);
}

MakeQuatd :: inline (vec : Vec3d, scalar : f64) -> Quatd #must
{
    return MakeQuat (vec, scalar);
}

QuatfLookAt :: QuatfFromForwardUp;
//QuatdLookAt :: QuatdFromForwardUp;
QuatLookAt :: QuatFromForwardUp;
QuatfFromForwardUp :: #bake_constants QuatFromForwardUp (T = f32);
//QuatdFromForwardUp :: #bake_constants QuatFromForwardUp (T = f64);
QuatFromForwardUp :: inline (forward : Vec3 ($T), up : Vec3 (T)) -> Quat (T) #must
{
    f := Normalized (forward, context.coordinate_axes.forward);
    u := Normalized (up, context.coordinate_axes.up);
    d := Dot (context.coordinate_axes.forward, f);
    if ApproxEquals (d, -1, 0.00001)
        return QuatFromAxisAngle (u, PI);
    else if ApproxEquals (d, 1, 0.00001)
        return Quat (T).{};
    angle := math.acos (d);
    axis := Cross (context.coordinate_axes.forward, f);
    axis = Normalized (axis);

    return QuatFromAxisAngle (axis, angle);
}

QuatfFromAxisAngle :: #bake_constants QuatFromAxisAngle (T = f32);
QuatdFromAxisAngle :: #bake_constants QuatFromAxisAngle (T = f64);
QuatFromAxisAngle :: inline (axis : Vector ($T, 3), angle : T) -> Quat (T) #must
{
    a := Normalized (axis);
    c := math.cos (angle / 2.0);
    s := math.sin (angle / 2.0);

    return .{a.x * s, a.y * s, a.z * s, c};
}

QuatfFromEulerAngles :: #bake_constants QuatFromEulerAngles (T = f32);
QuatdFromEulerAngles :: #bake_constants QuatFromEulerAngles (T = f64);
QuatFromEulerAngles :: inline (euler : EulerAngles ($T)) -> Quat (T) #must
{
    yaw   := QuatFromAxisAngle (Vec3 (T).{ 0, 1, 0 }, euler.yaw);
    pitch := QuatFromAxisAngle (Vec3 (T).{ 1, 0, 0 }, euler.pitch);
    roll  := QuatFromAxisAngle (Vec3 (T).{ 0, 0, 1 }, euler.roll);

    return roll * yaw * pitch;
}

QuatfFromMatrix :: #bake_constants QuatFromMatrix (T = f32);
QuatdFromMatrix :: #bake_constants QuatFromMatrix (T = f64);
QuatFromMatrix :: inline (mat : Matrix ($T, $M, $N)) -> Quat (T) #must
{
    #assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

    result : Quat (T) = ---;
    t : T = ---;

    if mat.r2c2 < 0
    {
        if mat.r0c0 > mat.r1c1
        {
            t = 1 + mat.r0c0 - mat.r1c1 - mat.r2c2;
            result = .{t, mat.r1c0 + mat.r0c1, mat.r0c2 + mat.r2c0, mat.r2c1 - mat.r1c2};
        }
        else
        {
            t = 1 - mat.r0c0 + mat.r1c1 - mat.r2c2;
            result = .{mat.r1c0 + mat.r0c1, t, mat.r1c2 + mat.r2c1, mat.r2c0 - mat.r0c2};
        }
    }
    else
    {
        if mat.r0c0 < -mat.r1c1
        {
            t = 1 - mat.r0c0 - mat.r1c1 + mat.r2c2;
            result = .{mat.r0c2 + mat.r2c0, mat.r2c1 + mat.r1c2, t, mat.r1c0 - mat.r0c1};
        }
        else
        {
            t = 1 + mat.r0c0 + mat.r1c1 + mat.r2c2;
            result = .{mat.r2c1 - mat.r1c2, mat.r0c2 - mat.r2c0, mat.r1c0 - mat.r0c1, t};
        }
    }

    return Normalized (result * 0.5 / math.sqrt (t));
}

ToAxisAngle :: inline (q : $Q/Quat, fallback_axis : Q.V3 = .{0,0,1}, epsilon : Q.T = 0.00001) -> axis : Q.V3, angle : Q.T
{
    w     := Clamp (q.w, -1, 1);
    theta := math.acos (w);
    sint  := math.sin (theta);
    theta *= 2;
    if ApproxZero (sint, epsilon)
        return fallback_axis, theta;

    return .{q.x / sint, q.y / sint, q.z / sint}, theta;
}

Dot :: inline (left : $Q/Quat, right : Q) -> Q.T #must
{
    return left.x * right.x + left.y * right.y + left.z * right.z + left.w * right.w;
}

Length :: inline (q : $Q/Quat) -> Q.T #must
{
    return math.sqrt (Dot (q, q));
}

Normalized :: inline (q : $Q/Quat, fallback : Q = .{}, epsilon : Q.T = 0.00001) -> Q #must, len : Q.T
{
    if ApproxZero (q, epsilon)
        return fallback, 0;
    len := Length (q);

    return q * (1.0 / len), len;
}

Inverse :: inline (q : $Q/Quat, epsilon : Q.T = 0.00001) -> Q #must
{
    sqrd_length := Dot (q, q);
    if ApproxZero (sqrd_length, epsilon)
        return q;

    return Q.{-q.x, -q.y, -q.z, q.w} / sqrd_length;
}

Conjugate :: inline (q : $Q/Quat) -> Q #must
{
    return .{-q.x, -q.y, -q.z, q.w};
}

Angle :: inline (q : $Q/Quat) -> Q.T #must
{
    return math.acos (q.w) * 2;
}

Angle :: inline (a : $Q/Quat, b : Q) -> Q.T #must
{
    return Angle (b * Inverse (a));
}

Rotate :: inline (vec : $V/Vec3, q : Quat (V.T)) -> V #must
{
    return (q * MakeQuat (vec, 0) * Conjugate (q)).vec;
}

RightVector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
    return Rotate (Vec3 (Q.T).{ 1, 0, 0 }, q);
}

UpVector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
    return Rotate (Vec3 (Q.T).{ 0, 1, 0 }, q);
}

ForwardVector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
    return Rotate (Vec3 (Q.T).{ 0, 0, 1 }, q);
}

NLerp :: inline (a : $Q/Quat, b : Q, t : $T2, $$shortest_path := true) -> Q #must
#modify { return IsFloatingPoint (T2), "T2 must be a float type."; }
{
    first  := a;
    second := b;
    if shortest_path
    {
        if Dot (first, second) < 0
            second = -second;
    }

    return Normalized ((1 - t) * first + t * second);
}

Add :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    result : Q = ---;
    result.x = left.x + right.x;
    result.y = left.y + right.y;
    result.z = left.z + right.z;
    result.w = left.w + right.w;

    return result;
}

Sub :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    result : Q = ---;
    result.x = left.x - right.x;
    result.y = left.y - right.y;
    result.z = left.z - right.z;
    result.w = left.w - right.w;

    return result;
}

Neg :: inline (q : $Q/Quat) -> Q #must
{
    result : Q = ---;
    result.x = -q.x;
    result.y = -q.y;
    result.z = -q.z;
    result.w = -q.w;

    return result;
}

Mul :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    result : Q = ---;
    result.x = left.x * right;
    result.y = left.y * right;
    result.z = left.z * right;
    result.w = left.w * right;

    return result;
}

Mul :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    w1 := left.w;
    x1 := left.x;
    y1 := left.y;
    z1 := left.z;

    w2 := right.w;
    x2 := right.x;
    y2 := right.y;
    z2 := right.z;

    result : Q = ---;
    result.x = w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2;
    result.y = w1 * y2 - x1 * z2 + w2 * y1 + z1 * x2;
    result.z = w1 * z2 + x1 * y2 - y1 * x2 + w2 * z1;
    result.w = w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2;

    return result;
}

Div :: inline (left : $Q/Quat, right : $T) -> Q #must
{
    result : Q = ---;
    result.x = left.x / right;
    result.y = left.y / right;
    result.z = left.z / right;
    result.w = left.w / right;

    return result;
}

Equals :: inline (left : $Q/Quat, right : Q) -> bool #must
{
    return left.x == right.x && left.y == right.y && left.z == right.z && left.w == right.w;
}

ApproxZero :: inline (val : $Q/Quat, epsilon : Q.T) -> bool #must
{
    return ApproxZero (val.x, epsilon) && ApproxZero (val.y, epsilon) && ApproxZero (val.z, epsilon) && ApproxZero (val.w, epsilon);
}

ApproxEquals :: inline (left : $Q/Quat, right : Q, epsilon : Q.T) -> bool #must
{
    return ApproxEquals (left.x, right.x, epsilon) && ApproxEquals (left.y, right.y, epsilon) && ApproxEquals (left.z, right.z, epsilon) && ApproxEquals (left.w, right.w, epsilon);
}

operator *[] :: inline (q : $Q/Quat, index : s64) -> *Q.T #must
{
    return *q.n[index];
}

operator + :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    return Add (left, right);
}

operator - :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    return Sub (left, right);
}

operator - :: inline (q : $Q/Quat) -> Q #must
{
    return Neg (q);
}

operator * :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return IsNumeric (T), "T must be numeric."; }
#symmetric
{
    return Mul (left, right);
}

operator * :: inline (left : $Q/Quat, right : Q) -> Q #must
{
    return Mul (left, right);
}

operator / :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return Div (left, right);
}

operator == :: inline (left : $Q/Quat, right : Q) -> bool #must
{
    return Equals (left, right);
}
