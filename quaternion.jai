Quat :: struct (T : Type)
{
	#assert #run is_floating_point (T) "T needs to be a floating point type.";

	V3 :: Vec3 (T);

	x, y, z : T;
	w : T = 1;

	#place x;
	vec : Vec3 (T) = ---;
	#place w;
	scalar : T = ---;

	#place y;
	as_vec4 : Vec4 (T) = ---;

	#place x;
	n : [4]T = ---;
}

Quatf :: Quat (f32);
Quatd :: Quat (f64);

quat :: inline (x : $T, y : T, z : T, w : T) -> Quat (T) #must
{
	result : Quat (T) = ---;
	result.x = x;
	result.y = y;
	result.z = z;
	result.w = w;

	return result;
}

quat :: inline (vec : Vec3 ($T), scalar : T) -> Quat (T) #must
{
	result : Quat (T) = ---;
	result.vec = vec;
	result.scalar = scalar;

	return result;
}

quatf :: inline (x : f32, y : f32, z : f32, w : f32) -> Quatf #must
{
	return quat (x, y, z, w);
}

quatf :: inline (vec : Vec3f, scalar : f32) -> Quatf #must
{
	return quat (vec, scalar);
}

quatd :: inline (x : f64, y : f64, z : f64, w : f64) -> Quatd #must
{
	return quat (x, y, z, w);
}

quatd :: inline (vec : Vec3d, scalar : f64) -> Quatd #must
{
	return quat (vec, scalar);
}

quatf_look_at :: quatf_from_forward_up;
//quatd_look_at :: quatd_from_forward_up;
quat_look_at :: quat_from_forward_up;
quatf_from_forward_up :: #bake_constants quat_from_forward_up (T = f32);
//quatd_from_forward_up :: #bake_constants quat_from_forward_up (T = f64);
quat_from_forward_up :: inline (forward : Vec3 ($T), up : Vec3 (T)) -> Quat (T) #must
{
	f := normalized (forward, context.coordinate_axes.forward);
	u := normalized (up, context.coordinate_axes.up);
	d := dot (context.coordinate_axes.forward, f);
	if approx_equals (d, -1, 0.00001)
		return quat_from_axis_angle (u, PI);
	else if approx_equals (d, 1, 0.00001)
		return Quat (T).{};
	angle := math.acos (d);
	axis := cross (context.coordinate_axes.forward, f);
	axis = normalized (axis);

	return quat_from_axis_angle (axis, angle);
}

quatf_from_axis_angle :: #bake_constants quat_from_axis_angle (T = f32);
quatd_from_axis_angle :: #bake_constants quat_from_axis_angle (T = f64);
quat_from_axis_angle :: inline (axis : Vector ($T, 3), angle : T) -> Quat (T) #must
{
	a := normalized (axis);
	c := math.cos (angle / 2.0);
	s := math.sin (angle / 2.0);

	return quat (a.x * s, a.y * s, a.z * s, c);
}

quatf_from_euler_angles :: #bake_constants quat_from_euler_angles (T = f32);
quatd_from_euler_angles :: #bake_constants quat_from_euler_angles (T = f64);
quat_from_euler_angles :: inline (euler : Euler_Angles ($T)) -> Quat (T) #must
{
	yaw   := quat_from_axis_angle (Vec3 (T).{ 0, 1, 0 }, euler.yaw);
	pitch := quat_from_axis_angle (Vec3 (T).{ 1, 0, 0 }, euler.pitch);
	roll  := quat_from_axis_angle (Vec3 (T).{ 0, 0, 1 }, euler.roll);

	return roll * yaw * pitch;
}

quatf_from_matrix :: #bake_constants quat_from_matrix (T = f32);
quatd_from_matrix :: #bake_constants quat_from_matrix (T = f64);
quat_from_matrix :: inline (mat : Matrix ($T, $M, $N)) -> Quat (T) #must
{
	#assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

	result : Quat (T) = ---;
	t : T = ---;

	if mat.r2c2 < 0
	{
		if mat.r0c0 > mat.r1c1
		{
			t = 1 + mat.r0c0 - mat.r1c1 - mat.r2c2;
			result = quat (t, mat.r1c0 + mat.r0c1, mat.r0c2 + mat.r2c0, mat.r2c1 - mat.r1c2);
		}
		else
		{
			t = 1 - mat.r0c0 + mat.r1c1 - mat.r2c2;
			result = quat (mat.r1c0 + mat.r0c1, t, mat.r1c2 + mat.r2c1, mat.r2c0 - mat.r0c2);
		}
	}
	else
	{
		if mat.r0c0 < -mat.r1c1
		{
			t = 1 - mat.r0c0 - mat.r1c1 + mat.r2c2;
			result = quat (mat.r0c2 + mat.r2c0, mat.r2c1 + mat.r1c2, t, mat.r1c0 - mat.r0c1);
		}
		else
		{
			t = 1 + mat.r0c0 + mat.r1c1 + mat.r2c2;
			result = quat (mat.r2c1 - mat.r1c2, mat.r0c2 - mat.r2c0, mat.r1c0 - mat.r0c1, t);
		}
	}

	return result * 0.5 / math.sqrt (t);
}

to_axis_angle :: inline (q : $Q/Quat, fallback_axis : Q.V3 = .{ 1, 0, 0 }, epsilon : Q.T = 0.00001) -> axis : Q.V3, angle : Q.T
{
	w     := clamp (q.w, -1, 1);
	theta := math.acos (w);
	sint  := math.sin (theta);
	theta *= 2;
	if approx_zero (sint, epsilon)
		return fallback_axis, theta;

	return vec3 (q.x / sint, q.y / sint, q.z / sint), theta;
}

dot :: inline (left : $Q/Quat, right : Q) -> Q.T #must
{
	return left.x * right.x + left.y * right.y + left.z * right.z + left.w * right.w;
}

length :: inline (q : $Q/Quat) -> Q.T #must
{
	return math.sqrt (dot (q, q));
}

normalized :: inline (q : $Q/Quat, fallback : Q = .{}, epsilon : Q.T = 0.00001) -> Q #must, len : Q.T
{
	if approx_zero (q, epsilon)
		return fallback, 0;
	len := length (q);

	return q * (1.0 / len), len;
}

inverse :: inline (q : $Q/Quat, epsilon : Q.T = 0.00001) -> Q #must
{
	sqrd_length := dot (q, q);
	if approx_zero (sqrd_length, epsilon)
		return q;

	return quat (-q.x, -q.y, -q.z, -q.w) / sqrd_length;
}

conjugate :: inline (q : $Q/Quat) -> Q #must
{
	return quat (-q.x, -q.y, -q.z, q.w);
}

angle :: inline (q : $Q/Quat) -> Q.T #must
{
	return math.acos (q.w) * 2;
}

angle :: inline (a : $Q/Quat, b : Q) -> Q.T #must
{
	return angle (b * inverse (a));
}

rotate :: inline (vec : $V/Vec3, q : Quat (V.T)) -> V #must
{
	return (q * quat (vec, 0) * conjugate (q)).vec;
}

right_vector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
	return rotate (Vec3 (Q.T).{ 1, 0, 0 }, q);
}

up_vector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
	return rotate (Vec3 (Q.T).{ 0, 1, 0 }, q);
}

forward_vector :: inline (q : $Q/Quat) -> Vec3 (Q.T) #must
{
	return rotate (Vec3 (Q.T).{ 0, 0, 1 }, q);
}

nlerp :: inline (a : $Q/Quat, b : Q, t : $T2, $$shortest_path := true) -> Q #must
#modify { return is_floating_point (T2), "T2 must be a float type."; }
{
	first  := a;
	second := b;
	if shortest_path
	{
		if dot (first, second) < 0
			second = -second;
	}

	return normalized ((1 - t) * first + t * second);
}

add :: inline (left : $Q/Quat, right : Q) -> Q #must
{
	result : Q = ---;
	result.x = left.x + right.x;
	result.y = left.y + right.y;
	result.z = left.z + right.z;
	result.w = left.w + right.w;

	return result;
}

sub :: inline (left : $Q/Quat, right : Q) -> Q #must
{
	result : Q = ---;
	result.x = left.x - right.x;
	result.y = left.y - right.y;
	result.z = left.z - right.z;
	result.w = left.w - right.w;

	return result;
}

neg :: inline (q : $Q/Quat) -> Q #must
{
	result : Q = ---;
	result.x = -q.x;
	result.y = -q.y;
	result.z = -q.z;
	result.w = -q.w;

	return result;
}

mul :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	result : Q = ---;
	result.x = left.x * right;
	result.y = left.y * right;
	result.z = left.z * right;
	result.w = left.w * right;

	return result;
}

mul :: inline (left : $Q/Quat, right : Q) -> Q #must
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

div :: inline (left : $Q/Quat, right : $T) -> Q #must
{
	result : Q = ---;
	result.x = left.x / right;
	result.y = left.y / right;
	result.z = left.z / right;
	result.w = left.w / right;

	return result;
}

equals :: inline (left : $Q/Quat, right : Q) -> bool #must
{
	return left.x == right.x && left.y == right.y && left.z == right.z && left.w == right.w;
}

approx_zero :: inline (val : $Q/Quat, epsilon : Q.T) -> bool #must
{
	return approx_zero (val.x, epsilon) && approx_zero (val.y, epsilon) && approx_zero (val.z, epsilon) && approx_zero (val.w, epsilon);
}

approx_equals :: inline (left : $Q/Quat, right : Q, epsilon : Q.T) -> bool #must
{
	return approx_equals (left.x, right.x, epsilon) && approx_equals (left.y, right.y, epsilon) && approx_equals (left.z, right.z, epsilon) && approx_equals (left.w, right.w, epsilon);
}

operator *[] :: inline (q : $Q/Quat, index : s64) -> *Q.T #must
{
	return *q.n[index];
}

operator + :: inline (left : $Q/Quat, right : Q) -> Q #must
{
	return add (left, right);
}

operator - :: inline (left : $Q/Quat, right : Q) -> Q #must
{
	return sub (left, right);
}

operator - :: inline (q : $Q/Quat) -> Q #must
{
	return neg (q);
}

operator * :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return is_numeric (T), "T must be numeric."; }
#symmetric
{
	return mul (left, right);
}

operator * :: inline (left : $Q/Quat, right : Q) -> Q #must
{
	return mul (left, right);
}

operator / :: inline (left : $Q/Quat, right : $T) -> Q #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	return div (left, right);
}

operator == :: inline (left : $Q/Quat, right : Q) -> bool #must
{
	return equals (left, right);
}
