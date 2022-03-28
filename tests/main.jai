#import "Linalg";
#import "Type_Traits";
#import "Fmt";

// For now, the following tests are there to make sure things compile fine.

vector :: ($T : Type, $N : s64)
{
	VAL :: Vector (T, N).{};

	v := scale (VAL, VAL);
	t := dot (v, v);
	f := length (v);
	v = normalized (v);
	v = clamp (v, VAL, VAL);
	v = round (v);
	v = floor (v);
	v = ceil (v);
	v = min (v, v);
	v = max (v, v);
	v = lerp (v, v, 1.0);
	f = distance (v, v);
	v += v;
	v -= v;
#if #run is_signed (T)
	v = -v;
	v *= 10;
	v /= 10;
	b := v == v;
#if #run is_floating_point (T)
{
	b = approx_zero (v, 0.0001);
	b = approx_equals (v, v, 0.0001);
}
#if N == 3
{
	v = cross (v, v);
}
	t = v[1];
}

matrix :: ($T : Type, $M : s64, $N : s64)
{
	m : Matrix (T, M, N);
	println ("%", m);
	//identity (*m);
	r := get_row (m, 0);
	c := get_col (m, 0);
	set_row (*m, 0, r);
	set_col (*m, 0, c);
	mt := transposed (m);
	m2 := resize (m, T, 5, 5);
	m += m;
	m -= m;
#if #run is_signed (T)
	m = -m;
	m *= 10;
	vr := m * m.Right_Vector.{};
	vl := m.Left_Vector.{} * m;
	{
		m2 : Matrix (T, N, 2);
		m3 := m * m2;
	}
	m /= 10;
	b := m == m;
#if M >= 3 && N >= 3
	m = matrix_from_axis_angle (M, N, Vec3 (T).{}, 0);
}

quaternion :: ($T : Type)
{
	q : Quat (T);
	println ("%", q);
	q = quat_look_at (Vec3 (T).{ 0, 0, 1 }, Vec3 (T).{ 0, 1, 0 });
	q = quat_from_axis_angle (Vec3 (T).{ 0, 0, 1 }, 0);
	to_axis_angle (q);
	q = quat_from_euler_angles (Euler_Angles (T).{ 0, 0, 0 });
	euler := euler_angles_from_quat (q);
	t := dot (q, q);
	f := length (q);
	q = normalized (q);
	q = inverse (q);
	q = conjugate (q);
	f = angle (q);
	f = angle (q, q);
	m3 := mat3_from_quat (q);
	q = quat_from_matrix (m3);
	m4 := mat4_from_quat (q);
	q = quat_from_matrix (m4);
	v := rotate (Vec3 (T).{ 0, 0, 1 }, q);
	v = right_vector (q);
	v = up_vector (q);
	v = forward_vector (q);
	q = nlerp (q, q, 1.0);
	q += q;
	q -= q;
	q = -q;
	q *= 10;
	q *= q;
	q /= 10;
	b := q == q;
}

main :: ()
{
	vector (float, 2);
	vector (float, 3);
	vector (s32, 3);
	vector (u32, 4);

	matrix (float, 2, 2);
	matrix (float, 3, 3);
	matrix (float64, 3, 3);
	matrix (float, 4, 4);

	quaternion (float32);
	//quaternion (float64);
}
