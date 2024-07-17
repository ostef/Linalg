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

    #if T == float
    {
        #place x;
        #as _ : math.Quaternion = ---;
    }
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
#deprecated
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

    correct_mat, _, det := PolarDecompose (mat);
    nan, inf := math.is_nan_is_inf (det);
    if nan || inf || det == 0
        return .{};

    correct_mat *= det;

    return QuatFromOrthoNormalizedPositiveMatrix (correct_mat);
}

// From Blender source code:
// https://github.com/blender/blender/blob/756538b4a117cb51a15e848fa6170143b6aafcd8/source/blender/blenlib/intern/math_rotation.c#L272
// This function will fail for matrices that are not ortho normalized and positive
QuatfFromOrthoNormalizedPositiveMatrix :: #bake_constants QuatFromOrthoNormalizedPositiveMatrix (T = f32);
QuatdFromOrthoNormalizedPositiveMatrix :: #bake_constants QuatFromOrthoNormalizedPositiveMatrix (T = f64);
QuatFromOrthoNormalizedPositiveMatrix :: inline (mat : Matrix ($T, $M, $N)) -> Quat (T) #must
{
    #assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

    /* Caller must ensure matrices aren't negative for valid results, see: T24291, T94231. */
    // Assert (Determinant (mat) > 0);

    /* Method outlined by Mike Day, ref: https://math.stackexchange.com/a/3183435/220949
        * with an additional `sqrtf(..)` for higher precision result.
        * Removing the `sqrt` causes tests to fail unless the precision is set to 1e-6 or larger. */

    q : Quat (T) = ---;

    if mat.r2c2 < 0
    {
        if mat.r0c0 > mat.r1c1
        {
            trace := 1 + mat.r0c0 - mat.r1c1 - mat.r2c2;
            s := 2 * math.sqrt (trace);
            if mat.r1c2 < mat.r2c1
            {
                /* Ensure W is non-negative for a canonical result. */
                s = -s;
            }
            q.x = 0.25 * s;
            s = 1 / s;
            q.w = (mat.r1c2 - mat.r2c1) * s;
            q.y = (mat.r0c1 + mat.r1c0) * s;
            q.z = (mat.r2c0 + mat.r0c2) * s;
            if trace == 1 && q.w == 0 && q.y == 0 && q.z == 0
            {
                /* Avoids the need to normalize the degenerate case. */
                q.x = 1;
            }
        }
        else
        {
            trace := 1 - mat.r0c0 + mat.r1c1 - mat.r2c2;
            s := 2 * math.sqrt (trace);
            if mat.r2c0 < mat.r0c2
            {
                /* Ensure W is non-negative for a canonical result. */
                s = -s;
            }
            q.y = 0.25 * s;
            s = 1 / s;
            q.w = (mat.r2c0 - mat.r0c2) * s;
            q.x = (mat.r0c1 + mat.r1c0) * s;
            q.z = (mat.r1c2 + mat.r2c1) * s;
            if trace == 1 && q.w == 0 && q.x == 0 && q.z == 0
            {
                /* Avoids the need to normalize the degenerate case. */
                q.y = 1;
            }
        }
    }
    else
    {
        if mat.r0c0 < -mat.r1c1
        {
            trace := 1 - mat.r0c0 - mat.r1c1 + mat.r2c2;
            s := 2 * math.sqrt (trace);
            if mat.r0c1 < mat.r1c0
            {
                /* Ensure W is non-negative for a canonical result. */
                s = -s;
            }
            q.z = 0.25 * s;
            s = 1 / s;
            q.w = (mat.r0c1 - mat.r1c0) * s;
            q.x = (mat.r2c0 + mat.r0c2) * s;
            q.y = (mat.r1c2 + mat.r2c1) * s;
            if trace == 1 && q.w == 0 && q.x == 0 && q.y == 0
            {
                /* Avoids the need to normalize the degenerate case. */
                q.z = 1;
            }
        }
        else
        {
            /* NOTE(@campbellbarton): A zero matrix will fall through to this block,
                * needed so a zero scaled matrices to return a quaternion without rotation, see: T101848. */
            trace := 1 + mat.r0c0 + mat.r1c1 + mat.r2c2;
            s := 2 * math.sqrt (trace);
            q.w = 0.25 * s;
            s = 1 / s;
            q.x = (mat.r1c2 - mat.r2c1) * s;
            q.y = (mat.r2c0 - mat.r0c2) * s;
            q.z = (mat.r0c1 - mat.r1c0) * s;
            if trace == 1 && q.x == 0 && q.y == 0 && q.z == 0
            {
                /* Avoids the need to normalize the degenerate case. */
                q.w = 1;
            }
        }
    }

    // Not sure why I need to return the conjugate, the original code returns
    // quaternions that go in the opposite direction
    return Conjugate (q);
}

QuatfFromVectorToVector :: #bake_constants QuatFromVectorToVector (T = f32);
QuatdFromVectorToVector :: #bake_constants QuatFromVectorToVector (T = f64);
QuatFromVectorToVector :: (a : Vec3 ($T), b : Vec3 (T)) -> Quat (T) #must
{
    a_norm := Normalized (a);
    b_norm := Normalized (b);

    d := Dot (a_norm, b_norm);

    // If a and b are parallel then there is no rotation
    if ApproxEquals (d, 1, 0.0001)
        return .{};

    // If a and b are opposite
    if ApproxEquals (d, -1, 0.0001)
    {
        if Abs (a_norm.x) > Abs (a_norm.z)
            return Normalized (Quat (T).{-a_norm.y, a_norm.x, 0, 0});
        else
            return Normalized (Quat (T).{0, -a_norm.z, a_norm.y, 0});
    }

    w := Cross (a_norm, b_norm);

    return Normalized (Quat (T).{vec=w, w=d + 1});
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
    s := Dot (q, q);
    if !IsFinite (s)
        return s;

    return math.sqrt (s);
}

Normalized :: inline (q : $Q/Quat, fallback : Q = .{}, epsilon : Q.T = 0.00001) -> Q #must, len : Q.T
{
    if ApproxZero (q, epsilon)
        return fallback, 0;
    len := Length (q);

    return q * (1.0 / len), len;
}

// This is here for completeness but for valid rotation
// quaternions (unit quaternions) the conjugate should be used
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
    return Angle (b * Conjugate (a));
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

IsFinite :: inline (val : $Q/Quat) -> bool #must
{
    return IsFinite (val.x) && IsFinite (val.y) && IsFinite (val.z) && IsFinite (val.w);
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
