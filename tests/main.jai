linalg :: #import "Linalg";
using, only (.["==","+","-","*","/","*[]"]) linalg;

#load "../utils.jai";

// For now, the following tests are there to make sure things compile fine.

vector :: ($T : Type, $N : s64)
{
	VAL :: linalg.Vector (T, N).{};

	v := linalg.scale (VAL, VAL);
	t := linalg.dot (v, v);
	f := linalg.length (v);
	v = linalg.normalized (v);
	v = linalg.clamp (v, VAL, VAL);
	v = linalg.round (v);
	v = linalg.floor (v);
	v = linalg.ceil (v);
	v = linalg.min (v, v);
	v = linalg.max (v, v);
	v = linalg.lerp (v, v, 1.0);
	f = linalg.distance (v, v);
	v += v;
	v -= v;
#if #run is_signed (T)
	v = -v;
	v *= 10;
	v /= 10;
	b := v == v;
#if #run is_floating_point (T)
{
	b = linalg.approx_zero (v, 0.0001);
	b = linalg.approx_equals (v, v, 0.0001);
}
#if N == 3
{
	v = linalg.cross (v, v);
}
	t = v[1];
	v[1] = 10;
	assert (v[1] == 10);
}

matrix :: ($T : Type, $M : s64, $N : s64)
{
	m : linalg.Matrix (T, M, N);
	//identity (*m);
	r := linalg.get_row (m, 0);
	c := linalg.get_col (m, 0);
	linalg.set_row (*m, 0, r);
	linalg.set_col (*m, 0, c);
	mt := linalg.transposed (m);
	m2 := linalg.resize (m, T, 5, 5);
	m += m;
	m -= m;
#if #run is_signed (T)
	m = -m;
	m *= 10;
	vr := m * m.Right_Vector.{};
	vl := m.Left_Vector.{} * m;
	{
		m2 : linalg.Matrix (T, N, 2);
		m3 := m * m2;
	}
	m /= 10;
	b := m == m;
#if M >= 3 && N >= 3
	m = linalg.matrix_from_axis_angle (M, N, linalg.Vec3 (T).{}, 0);
	m[1][1] = 100;
	assert (m[1][1] == 100);
	m[1] = m.Row.{};
	assert (m[1] == m.Row.{});
}

quaternion :: ($T : Type)
{
	q : linalg.Quat (T);
	q = linalg.quat_look_at (linalg.Vec3 (T).{ 0, 0, 1 }, linalg.Vec3 (T).{ 0, 1, 0 });
	q = linalg.quat_from_axis_angle (linalg.Vec3 (T).{ 0, 0, 1 }, 0);
	linalg.to_axis_angle (q);
	q = linalg.quat_from_euler_angles (linalg.Euler_Angles (T).{ 0, 0, 0 });
	euler := linalg.euler_angles_from_quat (q);
	t := linalg.dot (q, q);
	f := linalg.length (q);
	q = linalg.normalized (q);
	q = linalg.inverse (q);
	q = linalg.conjugate (q);
	f = linalg.angle (q);
	f = linalg.angle (q, q);
	m3 := linalg.mat3_from_quat (q);
	q = linalg.quat_from_matrix (m3);
	m4 := linalg.mat4_from_quat (q);
	q = linalg.quat_from_matrix (m4);
	v := linalg.rotate (linalg.Vec3 (T).{ 0, 0, 1 }, q);
	v = linalg.right_vector (q);
	v = linalg.up_vector (q);
	v = linalg.forward_vector (q);
	q = linalg.nlerp (q, q, 1.0);
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
