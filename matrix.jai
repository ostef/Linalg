// An M rows N columns matrix (usually we say N by M as
// N rows and M columns, but since M is before N in the
// alphabet, I use this convention).
// Our matrices are row-major in memory, meaning the first
// subscript index is the row, the second is the column.
// One reason for this is that matrices are printed as we
// write them mathematically, and not transposed like
// would happen with column-major ordering.
Matrix :: struct (T : Type, M : s64, N : s64)
{
	// @Note (stefan): We might want to also allow integer types, but
	// for now I want to keep things constrained.
	#assert #run is_floating_point (T) "T needs to be a floating point type.";
	#assert M > 0 && N > 0 "Invalid matrix dimensions.";

	IS_SQUARE :: M == N;

	Row :: Vector (T, N);	// A row has as many elements as there are columns
	Col :: Vector (T, M);	// A column has as many elements as there are rows
	Transposed :: Matrix (T, N, M);
	Left_Vector  :: Vector (T, M);	// The type of the lhs vector for multiplication
	Right_Vector :: Vector (T, N);	// The type of the rhs vector for multiplication

	// The following will insert:
	// * rIcJ : T, where I is the row and J is the column,
	// * cJrI : T, where I is the row and J is the column,
	// * rI : Vector (T), where I is the row.
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M - 1
	{
		for j : 0..N - 1
			if i == j
				print_to_builder (*builder, "r%c% : T = 1; ", i, j);
			else
				print_to_builder (*builder, "r%c% : T; ", i, j);
		println_to_builder (*builder, "@Fmt_Newline");
	}
	println_to_builder (*builder, "#place r0c0;");
	for i : 0..M - 1
	{
		for j : 0..N - 1
		{
			print_to_builder (*builder, "c%r% : T = ---; ", j, i);
		}
	}
	println_to_builder (*builder, "#place r0c0;");
	for i : 0..M - 1
		println_to_builder (*builder, "r% : Row = ---;", i);

	return to_string (builder);
};
	#place r0c0;
	rows : [M]Row = ---;
	#place r0c0;
	n : [M][N]T = ---;
}

Mat2   :: #bake_arguments Matrix (M = 2, N = 2);
Mat3   :: #bake_arguments Matrix (M = 3, N = 3);
Mat4   :: #bake_arguments Matrix (M = 4, N = 4);
Mat3x4 :: #bake_arguments Matrix (M = 3, N = 4);
Mat2f   :: Mat2 (f32);
Mat3f   :: Mat3 (f32);
Mat4f   :: Mat4 (f32);
Mat3x4f :: Mat3x4 (f32);
Mat2d   :: Mat2 (f64);
Mat3d   :: Mat3 (f64);
Mat4d   :: Mat4 (f64);
Mat3x4d :: Mat3x4 (f64);

matrix_from_quat :: inline ($M : s64, $N : s64, q : Quat ($T)) -> Matrix (T, M, N) #must
{
	#assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

	x_x := q.x * q.x;
	y_y := q.y * q.y;
	z_z := q.z * q.z;
	x_y := q.x * q.y;
	x_z := q.x * q.z;
	y_z := q.y * q.z;
	w_x := q.w * q.x;
	w_y := q.w * q.y;
	w_z := q.w * q.z;

	result : Matrix (T, M, N);
	result.r0c0 = cast (T) (1 - 2 * (y_y + z_z));
	result.r0c1 = cast (T)     (2 * (x_y - w_z));
	result.r0c2 = cast (T)     (2 * (x_z + w_y));

	result.r1c0 = cast (T)     (2 * (x_y + w_z));
	result.r1c1 = cast (T) (1 - 2 * (x_x + z_z));
	result.r1c2 = cast (T)     (2 * (y_z - w_x));

	result.r2c0 = cast (T)     (2 * (x_z - w_y));
	result.r2c1 = cast (T)     (2 * (y_z + w_x));
	result.r2c2 = cast (T) (1 - 2 * (x_x + y_y));

	return result;
}

matrix_from_axis_angle :: inline ($M : s64, $N : s64, axis : Vec3 ($T), angle : Vec3 (T).F) -> Matrix (T, M, N) #must
{
	#assert #run is_floating_point (T) "T needs to be a floating point type.";
	#assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

	n := normalized (axis);
	s := sin (angle);
	c := cos (angle);
	c2 := 1 - c;

	result : Matrix (T, M, N);
	result.r0c0 = cast (T) ((n.x * n.x * c2) + c);
	result.r0c1 = cast (T) ((n.y * n.x * c2) - (n.z * s));
	result.r0c2 = cast (T) ((n.z * n.x * c2) + (n.y * s));

	result.r1c0 = cast (T) ((n.x * n.y * c2) + (n.z * s));
	result.r1c1 = cast (T) ((n.y * n.y * c2) + c);
	result.r1c2 = cast (T) ((n.z * n.y * c2) - (n.x * s));

	result.r2c0 = cast (T) ((n.x * n.z * c2) - (n.y * s));
	result.r2c1 = cast (T) ((n.y * n.z * c2) + (n.x * s));
	result.r2c2 = cast (T) ((n.z * n.z * c2) + c);

	return result;
}

get_row :: inline (mat : $M/Matrix, index : s64) -> M.Row #must
{
	return mat.rows[index];
}

set_row :: inline (mat : *$M/Matrix, index : s64, row : M.Row)
{
	mat.rows[index] = row;
}

get_col :: inline (mat : $M/Matrix, index : s64) -> M.Col #must
{
	result : M.Col = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		println_to_builder (*builder, "result.n[{0}] = mat.n[{0}][index];", i);

	return to_string (builder);
};

	return result;
}

set_col :: inline (mat : *$M/Matrix, index : s64, col : M.Col)
{
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		println_to_builder (*builder, "mat.n[{0}][index] = col.n[{0}];", i);

	return to_string (builder);
};
}

transposed :: inline (mat : $M/Matrix) -> M.Transposed #must
{
	result : M.Transposed = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		for j : 0..M.N - 1
			println_to_builder (*builder, "result.n[{1}][{0}] = mat.n[{0}][{1}];", i, j);

	return to_string (builder);
};

	return result;
}

resize :: inline (mat : $M/Matrix, $T2 : Type, $M2 : s64, $N2 : s64) -> Matrix (T2, M2, N2) #must
{
	#assert #run is_numeric (T2) "T2 needs to be a numeric type.";
	#assert M2 > 0 && N2 > 0 "Invalid matrix dimensions.";

	result : Matrix (T2, M2, N2) = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	m := ifx M2 < M.M then M2 else M.M;
	n := ifx N2 < M.N then N2 else M.N;
	for i : 0..m - 1
		for j : 0..n - 1
			println_to_builder (*builder, "result.n[{0}][{1}] = cast (T2) mat.n[{0}][{1}];", i, j);
	for i : m..M2 - 1
		for j : n..N2 - 1
			if i == j
				println_to_builder (*builder, "result.n[%][%] = 1;", i, j);
			else
				println_to_builder (*builder, "result.n[%][%] = 0;", i, j);

	return to_string (builder);
};

	return result;
}

add :: inline (left : $M/Matrix, right : M) -> M #must
{
	result : M = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		for j : 0..M.N - 1
			println_to_builder (*builder, "result.n[{0}][{1}] = right.n[{0}][{1}] + left.n[{0}][{1}];", i, j);

	return to_string (builder);

};

	return result;
}

sub :: inline (left : $M/Matrix, right : M) -> M #must
{
	result : M = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		for j : 0..M.N - 1
			println_to_builder (*builder, "result.n[{0}][{1}] = right.n[{0}][{1}] - left.n[{0}][{1}];", i, j);

	return to_string (builder);

};

	return result;
}

neg :: inline (mat : $M/Matrix) -> M #must
{
	#assert #run is_signed (M.T) "Cannot negate an unsigned matrix.";

	result : M = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		for j : 0..M.N - 1
			println_to_builder (*builder, "result.n[{0}][{1}] = -mat.n[{0}][{1}];", i, j);

	return to_string (builder);

};

	return result;
}

mul :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	result : M = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		println_to_builder (*builder, "result.rows[{0}] = left.rows[{0}] * right;", i);

	return to_string (builder);
};

	return result;
}

mul :: inline (left : $M/Matrix, right : M.Right_Vector) -> M.Left_Vector #must
{
	result : M.Left_Vector = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
	{
		print_to_builder (*builder, "result.n[%] = ", i);
		for k : 0..M.N - 1
		{
			print_to_builder (*builder, "left.n[{0}][{1}] * right.n[{1}]", i, k);
			if k != M.N - 1
				print_to_builder (*builder, " + ");
		}
		println_to_builder (*builder, ";");
	}

	return to_string (builder);
};

	return result;
}

mul :: inline (left : M.Left_Vector, right : $M/Matrix) -> M.Right_Vector #must
{
	result : M.Right_Vector = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for j : 0..M.N - 1
	{
		print_to_builder (*builder, "result.n[%] = ", j);
		for k : 0..M.M - 1
		{
			print_to_builder (*builder, "left.n[{0}] * right.n[{0}][{1}]", k, j);
			if k != M.M - 1
				print_to_builder (*builder, " + ");
		}
		println_to_builder (*builder, ";");
	}

	return to_string (builder);
};

	return result;
}

mul :: inline (left : Matrix ($T, $M, $N), right : Matrix (T, N, $P)) -> Matrix (T, M, P) #must
{
	result : Matrix (T, M, P) = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M - 1
	{
		for j : 0..P - 1
		{
			print_to_builder (*builder, "result.n[%][%] = ", i, j);
			for k : 0..N - 1
			{
				print_to_builder (*builder, "left[{0}][{2}] * right[{2}][{1}]", i, j, k);
				if k != N - 1
					print_to_builder (*builder, " + ");
			}
			println_to_builder (*builder, ";");
		}
	}

	return to_string (builder);
};

	return result;
}

div :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	result : M = ---;
#insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
		println_to_builder (*builder, "result.rows[{0}] = left.rows[{0}] / right;", i);

	return to_string (builder);
};

	return result;
}

equals :: inline (left : $M/Matrix, right : M) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
	{
		print_to_builder (*builder, "equals (left.rows[{0}], right.rows[{0}])", i);
		if i != M.M - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

approx_zero :: inline (mat : $M/Matrix, epsilon : M.T) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..M.M - 1
	{
		print_to_builder (*builder, "approx_zero (mat.rows[{0}], epsilon)", i);
		if i != V.N - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

approx_equals :: inline (left : $M/Matrix, right : M, epsilon : M.T) -> bool #must
{
	return #insert -> string
{
	builder : String_Builder;
	builder.allocator = context.allocator;
	for i : 0..V.N - 1
	{
		print_to_builder (*builder, "approx_equals (left.rows[{0}], right.rows[{0}], epsilon)", i);
		if i != V.N - 1
			print_to_builder (*builder, " && ");
	}
	println_to_builder (*builder, ";");

	return to_string (builder);
};
}

operator [] :: inline (mat : $M/Matrix, row : s64) -> M.Row #must
{
	return mat.rows[row];
}

operator + :: inline (left : $M/Matrix, right : M) -> M #must
{
	return add (left, right);
}

operator - :: inline (left : $M/Matrix, right : M) -> M #must
{
	return sub (left, right);
}

operator - :: inline (mat : $M/Matrix) -> M #must
{
	#assert #run is_signed (M.T) "Cannot negate an unsigned matrix.";

	return neg (mat);
}

operator * :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return is_numeric (T), "T must be numeric."; }
#symmetric
{
	return mul (left, right);
}

operator * :: inline (left : $M/Matrix, right : M.Right_Vector) -> M.Left_Vector #must
{
	return mul (left, right);
}

operator * :: inline (left : M.Left_Vector, right : $M/Matrix) -> M.Right_Vector #must
{
	return mul (left, right);
}

operator * :: inline (left : Matrix ($T, $M, $N), right : Matrix (T, N, $P)) -> Matrix (T, M, P) #must
{
	return mul (left, right);
}

operator / :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	return div (left, right);
}

operator == :: inline (left : $M/Matrix, right : M) -> bool #must
{
	return equals (left, right);
}

operator != :: inline (left : $M/Matrix, right : M) -> bool #must
{
	return !equals (left, right);
}

/* Mat3 */

to_mat3f :: #bake_arguments resize (T2 = f32, M2 = 3, N2 = 3);
to_mat3d :: #bake_arguments resize (T2 = f64, M2 = 3, N2 = 3);
to_mat3 :: #bake_arguments resize (M2 = 3, N2 = 3);

mat3 :: inline (
	r0c0 : $T, r0c1 : T, r0c2 : T,
	r1c0 :  T, r1c1 : T, r1c2 : T,
	r2c0 :  T, r2c1 : T, r2c2 : T
) -> Mat3 (T) #must
{
	result : Mat3 (T) = ---;
	result.r0c0 = r0c0;
	result.r0c1 = r0c1;
	result.r0c2 = r0c2;

	result.r1c0 = r1c0;
	result.r1c1 = r1c1;
	result.r1c2 = r1c2;

	result.r2c0 = r2c0;
	result.r2c1 = r2c1;
	result.r2c2 = r2c2;

	return result;
}

mat3 :: inline (
	r0 : $V/Vec3,
	r1 : V,
	r2 : V
) -> Mat3 (V.T) #must
{
	result : Mat3 (V.T) = ---;
	result.r0 = r0;
	result.r1 = r1;
	result.r2 = r2;

	return result;
}

mat3f :: inline (
	r0c0 : f32, r0c1 : f32, r0c2 : f32,
	r1c0 : f32, r1c1 : f32, r1c2 : f32,
	r2c0 : f32, r2c1 : f32, r2c2 : f32
) -> Mat3f #must
{
	return mat3 (r0c0, r0c1, r0c2,
		r1c0, r1c1, r1c2,
		r2c0, r2c1, r2c2);
}

mat3f :: inline (
	r0 : Vec3f,
	r1 : Vec3f,
	r2 : Vec3f
) -> Mat3f #must
{
	return mat3 (r0, r1, r2);
}

mat3d :: inline (
	r0c0 : f64, r0c1 : f64, r0c2 : f64,
	r1c0 : f64, r1c1 : f64, r1c2 : f64,
	r2c0 : f64, r2c1 : f64, r2c2 : f64
) -> Mat3d #must
{
	return mat3 (r0c0, r0c1, r0c2,
		r1c0, r1c1, r1c2,
		r2c0, r2c1, r2c2);
}

mat3d :: inline (
	r0 : Vec3d,
	r1 : Vec3d,
	r2 : Vec3d
) -> Mat3d #must
{
	return mat3 (r0, r1, r2);
}

mat3f_from_quat :: #bake_constants mat3_from_quat (T = f32);
mat3d_from_quat :: #bake_constants mat3_from_quat (T = f64);
mat3_from_quat :: #bake_arguments matrix_from_quat (M = 3, N = 3);

mat3f_from_axis_angle :: #bake_constants mat3_from_axis_angle (T = f32);
mat3d_from_axis_angle :: #bake_constants mat3_from_axis_angle (T = f64);
mat3_from_axis_angle :: #bake_arguments matrix_from_axis_angle (M = 3, N = 3);

mat3f_rotate :: mat3f_from_axis_angle;
mat3d_rotate :: mat3d_from_axis_angle;
mat3_rotate :: mat3_from_axis_angle;

right_vector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
	#assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
	return get_col (mat, 0).xyz;
}

up_vector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
	#assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
	return get_col (mat, 1).xyz;
}

forward_vector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
	#assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
	return get_col (mat, 2).xyz;
}

scale_vector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
	#assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
	result : Vector (M.T, 3) = ---;
	result.x = length (right_vector (mat));
	result.y = length (up_vector (mat));
	result.z = length (forward_vector (mat));

	return result;
}

rotation_matrix :: inline (mat : $M/Matrix) -> M #must
{
	#assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
	result : M;
	set_col (*result, 0, normalized (right_vector (mat)));
	set_col (*result, 1, normalized (up_vector (mat)));
	set_col (*result, 2, normalized (forward_vector (mat)));

	return result;
}

inverse :: inline (mat : $M/Mat3, epsilon : M.T = 0.00001) -> M #must, exists : bool
{
	a := get_col (mat, 0);
	b := get_col (mat, 1);
	c := get_col (mat, 2);

	r0 := cross (b, c);
	r1 := cross (c, a);
	r2 := cross (a, b);

	det := dot (r2, c);
	inv_det := 1 / det;

	return mat3 (
		r0 * inv_det,
		r1 * inv_det,
		r2 * inv_det
	), !approx_zero (det, epsilon);
}

/* Mat4 */

to_mat4f :: #bake_arguments resize (T2 = f32, M2 = 4, N2 = 4);
to_mat4d :: #bake_arguments resize (T2 = f64, M2 = 4, N2 = 4);
to_mat4 :: #bake_arguments resize (M2 = 4, N2 = 4);

mat4 :: inline (
	r0c0 : $T, r0c1 : T, r0c2 : T, r0c3 : T,
	r1c0 :  T, r1c1 : T, r1c2 : T, r1c3 : T,
	r2c0 :  T, r2c1 : T, r2c2 : T, r2c3 : T,
	r3c0 :  T, r3c1 : T, r3c2 : T, r3c3 : T
) -> Mat4 (T) #must
{
	result : Mat4 (T) = ---;
	result.r0c0 = r0c0;
	result.r0c1 = r0c1;
	result.r0c2 = r0c2;
	result.r0c3 = r0c3;

	result.r1c0 = r1c0;
	result.r1c1 = r1c1;
	result.r1c2 = r1c2;
	result.r1c3 = r1c3;

	result.r2c0 = r2c0;
	result.r2c1 = r2c1;
	result.r2c2 = r2c2;
	result.r2c3 = r2c3;

	result.r3c0 = r3c0;
	result.r3c1 = r3c1;
	result.r3c2 = r3c2;
	result.r3c3 = r3c3;

	return result;
}

mat4 :: inline (
	r0 : $V/Vec4,
	r1 : V,
	r2 : V,
	r3 : V
) -> Mat4 (V.T) #must
{
	result : Mat4 (V.T) = ---;
	result.r0 = r0;
	result.r1 = r1;
	result.r2 = r2;
	result.r3 = r3;

	return result;
}

mat4f :: inline (
	r0c0 : f32, r0c1 : f32, r0c2 : f32, r0c3 : f32,
	r1c0 : f32, r1c1 : f32, r1c2 : f32, r1c3 : f32,
	r2c0 : f32, r2c1 : f32, r2c2 : f32, r2c3 : f32,
	r3c0 : f32, r3c1 : f32, r3c2 : f32, r3c3 : f32
) -> Mat4f #must
{
	return mat4 (r0c0, r0c1, r0c2, r0c3,
		r1c0, r1c1, r1c2, r1c3,
		r2c0, r2c1, r2c2, r2c3,
		r3c0, r3c1, r2c2, r3c3);
}

mat4f :: inline (
	r0 : Vec4f,
	r1 : Vec4f,
	r2 : Vec4f,
	r3 : Vec4f
) -> Mat4f #must
{
	return mat4 (r0, r1, r2, r3);
}

mat4d :: inline (
	r0c0 : f64, r0c1 : f64, r0c2 : f64, r0c3 : f64,
	r1c0 : f64, r1c1 : f64, r1c2 : f64, r1c3 : f64,
	r2c0 : f64, r2c1 : f64, r2c2 : f64, r2c3 : f64,
	r3c0 : f64, r3c1 : f64, r3c2 : f64, r3c3 : f64
) -> Mat4d #must
{
	return mat4 (r0c0, r0c1, r0c2, r0c3,
		r1c0, r1c1, r1c2, r1c3,
		r2c0, r2c1, r2c2, r2c3,
		r3c0, r3c1, r2c2, r3c3);
}

mat4d :: inline (
	r0 : Vec4d,
	r1 : Vec4d,
	r2 : Vec4d,
	r3 : Vec4d
) -> Mat4d #must
{
	return mat4 (r0, r1, r2, r3);
}

mat4f_from_quat :: #bake_constants mat4_from_quat (T = f32);
mat4d_from_quat :: #bake_constants mat4_from_quat (T = f64);
mat4_from_quat :: #bake_arguments matrix_from_quat (M = 4, N = 4);

mat4f_from_axis_angle :: #bake_constants mat4_from_axis_angle (T = f32);
mat4d_from_axis_angle :: #bake_constants mat4_from_axis_angle (T = f64);
mat4_from_axis_angle :: #bake_arguments matrix_from_axis_angle (M = 4, N = 4);

mat4f_rotate :: mat4f_from_axis_angle;
mat4d_rotate :: mat4d_from_axis_angle;
mat4_rotate :: mat4_from_axis_angle;

mat4f_translate :: #bake_constants mat4_translate (T = f32);
mat4d_translate :: #bake_constants mat4_translate (T = f64);
mat4_translate :: inline (vec : Vec3 ($T)) -> Mat4 (T) #must
{
	result : Mat4 (T);
	result.r0c3 = vec.x;
	result.r1c3 = vec.y;
	result.r2c3 = vec.z;

	return result;
}

mat4f_scale :: #bake_constants mat4_scale (T = f32);
mat4d_scale :: #bake_constants mat4_scale (T = f64);
mat4_scale :: inline (vec : Vec3 ($T)) -> Mat4 (T) #must
{
	result : Mat4 (T);
	result.r0c0 = vec.x;
	result.r1c1 = vec.y;
	result.r2c2 = vec.z;

	return result;
}

mat4f_transform :: #bake_constants mat4_transform (T = f32);
mat4d_transform :: #bake_constants mat4_transform (T = f64);
// @Compiler_Bug: polymorphic struct literals in procedure headers
mat4_transform :: inline (translation : Vec3 ($T), rotation : Quat (T), scale : Vec3 (T)) -> Mat4 (T) #must
{
	return mat4_scale (scale) * mat4_from_quat (rotation) * mat4_translate (translation);
}

mat4f_look_at :: #bake_constants mat4_look_at (T = f32);
mat4d_look_at :: #bake_constants mat4_look_at (T = f64);
mat4_look_at :: inline (position : Vec3 ($T), target : Vec3 (T), up : Vec3 (T)) -> Mat4 (T) #must
{
	V :: Vec3 (T);
	M :: Mat4 (T);
	align_z :: inline (axis : V, up : V) -> M #must
	{
		x, y : V;
		y = up - axis * dot (up, axis);
		ylen := length (y);

		if approx_zero (ylen, 0.000001)
		{
			k := V.{ 1, 0, 0 };
			y = k - axis * dot (k, axis);
			ylen = length (y);
			if approx_zero (ylen, 0.000001)
			{
				k := V.{ 0, 0, 1 };
				y = k - axis * dot (k, axis);
				ylen = length (y);
			}
		}

		y /= ylen;
		x = cross (y, axis);

		return mat4 (
			x.x, y.x, axis.x, 0,
			x.y, y.y, axis.y, 0,
			x.z, y.z, axis.z, 0,
			  0,   0,      0, 1
		);
	}

	direction := normalized (target - position, fallback = V.{ 0, 0, 1 });
	up_vector := normalized (up, fallback = V.{ 0, 1, 0 });
	result := align_z (direction, up_vector);
	set_col (*result, 3, vec4 (position, 1));

	return result;
}

mat4f_orthographic_projection :: #bake_constants mat4_orthographic_projection (T = f32);
mat4d_orthographic_projection :: #bake_constants mat4_orthographic_projection (T = f64);
mat4_orthographic_projection :: inline (l : $T, r : T, b : T, t : T, n : T, f : T) -> Mat4 (T) #must
{
	return mat4 (
		2 / (r - l),           0,           0, (r + l) / (l - r),
		          0, 2 / (t - b),           0, (t + b) / (b - t),
		          0,           0, 2 / (n / f), (n + f) / (n - f),
		          0,           0,           0,                 1
	);
}

mat4f_perspective_projection :: #bake_constants mat4_perspective_projection (T = f32);
mat4d_perspective_projection :: #bake_constants mat4_perspective_projection (T = f64);
mat4_perspective_projection :: inline (fov_in_degrees : $T, aspect : T, n : T, f : T, left_handed := true) -> Mat4 (T) #must
{
	view_z := ifx left_handed then cast (T) 1 else cast (T) -1;
	t := tan (to_rads (fov_in_degrees) * 0.5) * n;
	b := -t;
	r := aspect * t;
	l := -r;

	return mat4 (
		2 * n / (r - l),               0, -view_z * (r + l) / (r - l),      0,
		              0, 2 * n / (t - b), -view_z * (t + b) / (t - b),      0,
		              0,               0,                      view_z, -2 * n,
		              0,               0,                      view_z,      0
	);
}

translation_vector :: inline (mat : $M/Matrix) -> Vector (M.T, 3) #must
{
	#assert M.M >= 3 && M.N >= 4 "Invalid dimensions for matrix (need at least 3 rows and 4 columns).";
	return get_col (mat, 3).xyz;
}

// See: 1.7.5 Inverses of Small Matrices in 'Foundations of Game Development: Mathematics' by Eric Lengyel.
inverse :: inline (mat : Matrix ($T, 4, 4), epsilon : T = 0.00001) -> Matrix (T, 4, 4) #must, exists : bool
{
	a := get_col (mat, 0).xyz;
	b := get_col (mat, 1).xyz;
	c := get_col (mat, 2).xyz;
	d := get_col (mat, 3).xyz;

	x := mat.r3c0;
	y := mat.r3c1;
	z := mat.r3c2;
	w := mat.r3c3;

	s := cross (a, b);
	t := cross (c, d);
	u := a * y - b * x;
	v := c * w - d * z;

	det := dot (s, v) + dot (t, u);
	s /= det;
	t /= det;
	u /= det;
	v /= det;

	r0 := cross (b, v) + t * y;
	r1 := cross (v, a) - t * x;
	r2 := cross (d, u) + s * w;
	r3 := cross (u, c) - s * z;

	return mat4 (
		vec4 (r0, -dot (b, t)),
		vec4 (r1,  dot (a, t)),
		vec4 (r2, -dot (d, s)),
		vec4 (r3,  dot (c, s))
	), !approx_zero (det, epsilon);
}

// See: 1.7.5 Inverses of Small Matrices in 'Foundations of Game Development: Mathematics' by Eric Lengyel.
adjugate :: inline (mat : $M/Mat4) -> M #must
{
	a := get_col (mat, 0).xyz;
	b := get_col (mat, 1).xyz;
	c := get_col (mat, 2).xyz;
	d := get_col (mat, 3).xyz;

	x := mat.r3c0;
	y := mat.r3c1;
	z := mat.r3c2;
	w := mat.r3c3;

	s := cross (a, b);
	t := cross (c, d);
	u := a * y - b * x;
	v := c * w - d * z;

	r0 := cross (b, v) + t * y;
	r1 := cross (v, a) - t * x;
	r2 := cross (d, u) + s * w;
	r3 := cross (u, c) - s * z;

	return mat4 (
		vec4 (r0, -dot (b, t)),
		vec4 (r1,  dot (a, t)),
		vec4 (r2, -dot (d, s)),
		vec4 (r3,  dot (c, s))
	);
}

determinant :: inline (mat : Matrix ($T, 4, 4)) -> T #must
{
	a := get_col (mat, 0).xyz;
	b := get_col (mat, 1).xyz;
	c := get_col (mat, 2).xyz;
	d := get_col (mat, 3).xyz;

	x := mat.r3c0;
	y := mat.r3c1;
	z := mat.r3c2;
	w := mat.r3c3;

	s := cross (a, b);
	t := cross (c, d);
	u := a * y - b * x;
	v := c * w - d * z;

	return dot (s, v) + dot (t, u);
}
