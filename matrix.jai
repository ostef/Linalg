// An M rows N columns matrix (usually we say N by M as
// N rows and M columns, but since M is before N in the
// alphabet, I use this convention).
// Our matrices are row-major in memory, meaning the first
// subscript index is the row, the second is the column.
// One reason for this is that matrices are printed as we
// write them mathematically, and not Transposed like
// would happen with column-major ordering.
Matrix :: struct (T : Type, M : s64, N : s64)
{
    // @Note (stefan): We might want to also allow integer types, but
    // for now I want to keep things constrained.
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";
    #assert M > 0 && N > 0 "Invalid matrix dimensions.";

    Is_Square :: M == N;

    Row :: Vector (T, N);	// A row has as many elements as there are columns
    Col :: Vector (T, M);	// A column has as many elements as there are rows
    Transposed :: Matrix (T, N, M);
    LeftVector  :: Vector (T, M);	// The type of the lhs vector for multiplication
    RightVector :: Vector (T, N);	// The type of the rhs vector for multiplication

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
        print_to_builder (*builder, "\n");
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

    return to_string (*builder);
};
    #place r0c0;
    rows : [M]Row = ---;
    #place r0c0;
    n : [M][N]T = ---;

    #if T == float
    {
        #if M == 3 && N == 3
        {
            #place r0c0;
            #as _ : math.Matrix3 = ---;
        }
        else #if M == 4 && N == 4
        {
            #place r0c0;
            #as _ : math.Matrix4 = ---;
        }
    }
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

MatrixFromQuat :: inline ($M : s64, $N : s64, q : Quat ($T)) -> Matrix (T, M, N) #must
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

MatrixFromAxisAngle :: inline ($M : s64, $N : s64, axis : Vec3 ($T), angle : Vec3 (T).F) -> Matrix (T, M, N) #must
{
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";
    #assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

    n := Normalized (axis);
    s := math.sin (angle);
    c := math.cos (angle);
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

TransformPoint :: inline (mat : Matrix ($T, $M, $N), point : Vec3 (T)) -> Vec3 (T) #must
{
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";
    #assert M >= 4 && N >= 4 "Invalid dimensions for matrix (need at least 4 rows and 4 columns).";

    result : Vec3 (T);
    result.x = mat.r0c0 * point.x + mat.r0c1 * point.y + mat.r0c2 * point.z + mat.r0c3;
    result.y = mat.r1c0 * point.x + mat.r1c1 * point.y + mat.r1c2 * point.z + mat.r1c3;
    result.z = mat.r2c0 * point.x + mat.r2c1 * point.y + mat.r2c2 * point.z + mat.r2c3;

    return result;
}

TransformVector :: inline (mat : Matrix ($T, $M, $N), vec : Vec3 (T)) -> Vec3 (T) #must
{
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";
    #assert M >= 3 && N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";

    result : Vec3 (T);
    result.x = mat.r0c0 * vec.x + mat.r0c1 * vec.y + mat.r0c2 * vec.z;
    result.y = mat.r1c0 * vec.x + mat.r1c1 * vec.y + mat.r1c2 * vec.z;
    result.z = mat.r2c0 * vec.x + mat.r2c1 * vec.y + mat.r2c2 * vec.z;

    return result;
}

GetRow :: inline (mat : $M/Matrix, index : s64) -> M.Row #must
{
    return mat.rows[index];
}

SetRow :: inline (mat : *$M/Matrix, index : s64, row : M.Row)
{
    mat.rows[index] = row;
}

GetCol :: inline (mat : $M/Matrix, index : s64) -> M.Col #must
{
    result : M.Col = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        println_to_builder (*builder, "result.n[%1] = mat.n[%1][index];", i);

    return to_string (*builder);
};

    return result;
}

SetCol :: inline (mat : *$M/Matrix, index : s64, col : M.Col)
{
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        println_to_builder (*builder, "mat.n[%1][index] = col.n[%1];", i);

    return to_string (*builder);
};
}

Transposed :: inline (mat : $M/Matrix) -> M.Transposed #must
{
    result : M.Transposed = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        for j : 0..M.N - 1
            println_to_builder (*builder, "result.n[%2][%1] = mat.n[%1][%2];", i, j);

    return to_string (*builder);
};

    return result;
}

Resize :: inline (mat : $M/Matrix, $T2 : Type, $M2 : s64, $N2 : s64) -> Matrix (T2, M2, N2) #must
{
    #assert #run IsNumeric (T2) "T2 needs to be a numeric type.";
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
            println_to_builder (*builder, "result.n[%1][%2] = cast (T2) mat.n[%1][%2];", i, j);
    for i : m..M2 - 1
        for j : n..N2 - 1
            if i == j
                println_to_builder (*builder, "result.n[%][%] = 1;", i, j);
            else
                println_to_builder (*builder, "result.n[%][%] = 0;", i, j);

    return to_string (*builder);
};

    return result;
}

Add :: inline (left : $M/Matrix, right : M) -> M #must
{
    result : M = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        for j : 0..M.N - 1
            println_to_builder (*builder, "result.n[%1][%2] = right.n[%1][%2] + left.n[%1][%2];", i, j);

    return to_string (*builder);

};

    return result;
}

Sub :: inline (left : $M/Matrix, right : M) -> M #must
{
    result : M = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        for j : 0..M.N - 1
            println_to_builder (*builder, "result.n[%1][%2] = right.n[%1][%2] - left.n[%1][%2];", i, j);

    return to_string (*builder);

};

    return result;
}

Neg :: inline (mat : $M/Matrix) -> M #must
{
    #assert #run IsSigned (M.T) "Cannot negate an unsigned matrix.";

    result : M = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        for j : 0..M.N - 1
            println_to_builder (*builder, "result.n[%1][%2] = -mat.n[%1][%2];", i, j);

    return to_string (*builder);

};

    return result;
}

Mul :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    result : M = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        println_to_builder (*builder, "result.rows[%1] = left.rows[%1] * right;", i);

    return to_string (*builder);
};

    return result;
}

Mul :: inline (left : $M/Matrix, right : M.RightVector) -> M.LeftVector #must
{
    result : M.LeftVector = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
    {
        print_to_builder (*builder, "result.n[%] = ", i);
        for k : 0..M.N - 1
        {
            print_to_builder (*builder, "left.n[%1][%2] * right.n[%2]", i, k);
            if k != M.N - 1
                print_to_builder (*builder, " + ");
        }
        println_to_builder (*builder, ";");
    }

    return to_string (*builder);
};

    return result;
}

Mul :: inline (left : M.LeftVector, right : $M/Matrix) -> M.RightVector #must
{
    result : M.RightVector = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for j : 0..M.N - 1
    {
        print_to_builder (*builder, "result.n[%] = ", j);
        for k : 0..M.M - 1
        {
            print_to_builder (*builder, "left.n[%1] * right.n[%1][%2]", k, j);
            if k != M.M - 1
                print_to_builder (*builder, " + ");
        }
        println_to_builder (*builder, ";");
    }

    return to_string (*builder);
};

    return result;
}

Mul :: inline (left : Matrix ($T, $M, $N), right : Matrix (T, N, $P)) -> Matrix (T, M, P) #must
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
                print_to_builder (*builder, "left[%1][%3] * right[%3][%2]", i, j, k);
                if k != N - 1
                    print_to_builder (*builder, " + ");
            }
            println_to_builder (*builder, ";");
        }
    }

    return to_string (*builder);
};

    return result;
}

Div :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    result : M = ---;
#insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
        println_to_builder (*builder, "result.rows[%1] = left.rows[%1] / right;", i);

    return to_string (*builder);
};

    return result;
}

Equals :: inline (left : $M/Matrix, right : M) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
    {
        print_to_builder (*builder, "Equals (left.rows[%1], right.rows[%1])", i);
        if i != M.M - 1
            print_to_builder (*builder, " && ");
    }
    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

ApproxZero :: inline (mat : $M/Matrix, epsilon : M.T) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
    {
        for j : 0..M.N - 1
        {
            if i > 0 || j > 0
                print_to_builder (*builder, " && ");
            print_to_builder (*builder, "ApproxZero (left.r%1c%2, epsilon)", i, j);
        }
    }

    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

ApproxEquals :: inline (left : $M/Matrix, right : M, epsilon : M.T) -> bool #must
{
    return #insert -> string
{
    builder : String_Builder;
    builder.allocator = context.allocator;
    for i : 0..M.M - 1
    {
        for j : 0..M.N - 1
        {
            if i > 0 || j > 0
                print_to_builder (*builder, " && ");
            print_to_builder (*builder, "ApproxEquals (left.r%1c%2, right.r%1c%2, epsilon)", i, j);
        }
    }

    println_to_builder (*builder, ";");

    return to_string (*builder);
};
}

operator *[] :: inline (mat : $M/Matrix, row : s64) -> *M.Row #must
{
    return *mat.rows[row];
}

operator + :: inline (left : $M/Matrix, right : M) -> M #must
{
    return Add (left, right);
}

operator - :: inline (left : $M/Matrix, right : M) -> M #must
{
    return Sub (left, right);
}

operator - :: inline (mat : $M/Matrix) -> M #must
{
    #assert #run IsSigned (M.T) "Cannot negate an unsigned matrix.";

    return Neg (mat);
}

operator * :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return IsNumeric (T), "T must be numeric."; }
#symmetric
{
    return Mul (left, right);
}

operator * :: inline (left : $M/Matrix, right : M.RightVector) -> M.LeftVector #must
{
    return Mul (left, right);
}

operator * :: inline (left : M.LeftVector, right : $M/Matrix) -> M.RightVector #must
{
    return Mul (left, right);
}

operator * :: inline (left : Matrix ($T, $M, $N), right : Matrix (T, N, $P)) -> Matrix (T, M, P) #must
{
    return Mul (left, right);
}

operator / :: inline (left : $M/Matrix, right : $T) -> M #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return Div (left, right);
}

operator == :: inline (left : $M/Matrix, right : M) -> bool #must
{
    return Equals (left, right);
}

/* Mat3 */

ToMat3f :: #bake_arguments Resize (T2 = f32, M2 = 3, N2 = 3);
ToMat3d :: #bake_arguments Resize (T2 = f64, M2 = 3, N2 = 3);
ToMat3 :: #bake_arguments Resize (M2 = 3, N2 = 3);

MakeMat3 :: inline (
    r0 : Vector ($T, 3),
    r1 : Vector (T, 3),
    r2 : Vector (T, 3)
) -> Mat3 (T) #must
{
    result : Mat3 (T) = ---;
    result.r0 = r0;
    result.r1 = r1;
    result.r2 = r2;

    return result;
}

MakeMat3f :: inline (
    r0 : Vec3f,
    r1 : Vec3f,
    r2 : Vec3f
) -> Mat3f #must
{
    return MakeMat3 (r0, r1, r2);
}

MakeMat3d :: inline (
    r0 : Vec3d,
    r1 : Vec3d,
    r2 : Vec3d
) -> Mat3d #must
{
    return MakeMat3 (r0, r1, r2);
}

Mat3FromRightUpForward :: inline (right : $V/Vec3, up : V, forward : V) -> Mat3 (V.T) #must
{
    result : Mat3 (V.T) = ---;
    result.r0c0 = right.x;
    result.r1c0 = right.y;
    result.r2c0 = right.z;

    result.r0c1 = up.x;
    result.r1c1 = up.y;
    result.r2c1 = up.z;

    result.r0c2 = forward.x;
    result.r1c2 = forward.y;
    result.r2c2 = forward.z;

    return result;
}

Mat3fFromRightUpForward :: inline (right : Vec3f, up : Vec3f, forward : Vec3f) -> Mat3f #must
{
    return Mat3FromRightUpForward (right, up, forward);
}

Mat3dFromRightUpForward :: inline (right : Vec3d, up : Vec3d, forward : Vec3d) -> Mat3d #must
{
    return Mat3FromRightUpForward (right, up, forward);
}

Mat3fFromQuat :: #bake_constants Mat3FromQuat (T = f32);
Mat3dFromQuat :: #bake_constants Mat3FromQuat (T = f64);
Mat3FromQuat :: #bake_arguments MatrixFromQuat (M = 3, N = 3);

Mat3fFromAxisAngle :: #bake_constants Mat3FromAxisAngle (T = f32);
Mat3dFromAxisAngle :: #bake_constants Mat3FromAxisAngle (T = f64);
Mat3FromAxisAngle :: #bake_arguments MatrixFromAxisAngle (M = 3, N = 3);

Mat3fRotate :: Mat3fFromAxisAngle;
Mat3dRotate :: Mat3dFromAxisAngle;
Mat3Rotate :: Mat3FromAxisAngle;

Mat3fScale :: #bake_constants Mat3Scale (T = f32);
Mat3dScale :: #bake_constants Mat3Scale (T = f64);
Mat3Scale :: inline (vec : Vec3 ($T)) -> Mat3 (T) #must
{
    result : Mat3 (T);
    result.r0c0 = vec.x;
    result.r1c1 = vec.y;
    result.r2c2 = vec.z;

    return result;
}

RightVector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
    #assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
    result := GetCol (mat, 0);

    return Normalized (result.xyz);
}

UpVector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
    #assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
    result := GetCol (mat, 1);

    return Normalized (result.xyz);
}

ForwardVector :: inline (mat : $M/Matrix) -> Vec3 (M.T) #must
{
    #assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
    result := GetCol (mat, 2);

    return Normalized (result.xyz);
}

RotationMatrix :: inline (mat : $M/Matrix) -> M #must
{
    #assert M.M >= 3 && M.N >= 3 "Invalid dimensions for matrix (need at least 3 rows and 3 columns).";
    result : M;

    col : M.Col;
    (cast (*Vec3 (M.T)) *col).* = Normalized (RightVector (mat));
    SetCol (*result, 0, col);

    (cast (*Vec3 (M.T)) *col).* = Normalized (UpVector (mat));
    SetCol (*result, 1, col);

    (cast (*Vec3 (M.T)) *col).* = Normalized (ForwardVector (mat));
    SetCol (*result, 2, col);

    return result;
}

Inverse :: inline (mat : $M/Mat3, epsilon : M.T = 0.00001) -> M #must, exists : bool
{
    a := GetCol (mat, 0);
    b := GetCol (mat, 1);
    c := GetCol (mat, 2);

    r0 := Cross (b, c);
    r1 := Cross (c, a);
    r2 := Cross (a, b);

    det := Dot (r2, c);
    inv_det := 1 / det;

    return MakeMat3 (
        r0 * inv_det,
        r1 * inv_det,
        r2 * inv_det
    ), !ApproxZero (det, epsilon);
}

/* Mat4 */

ToMat4f :: #bake_arguments Resize (T2 = f32, M2 = 4, N2 = 4);
ToMat4d :: #bake_arguments Resize (T2 = f64, M2 = 4, N2 = 4);
ToMat4 :: #bake_arguments Resize (M2 = 4, N2 = 4);

MakeMat4 :: inline (
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

MakeMat4f :: inline (
    r0 : Vec4f,
    r1 : Vec4f,
    r2 : Vec4f,
    r3 : Vec4f
) -> Mat4f #must
{
    return MakeMat4 (r0, r1, r2, r3);
}

MakeMat4d :: inline (
    r0 : Vec4d,
    r1 : Vec4d,
    r2 : Vec4d,
    r3 : Vec4d
) -> Mat4d #must
{
    return MakeMat4 (r0, r1, r2, r3);
}

Mat4FromRightUpForwardOrigin :: inline (right : $V/Vec3, up : V, forward : V, origin := V.{}) -> Mat4 (V.T) #must
{
    result : Mat4 (V.T) = ---;
    result.r0c0 = right.x;
    result.r1c0 = right.y;
    result.r2c0 = right.z;
    result.r3c0 = 0;

    result.r0c1 = up.x;
    result.r1c1 = up.y;
    result.r2c1 = up.z;
    result.r3c1 = 0;

    result.r0c2 = forward.x;
    result.r1c2 = forward.y;
    result.r2c2 = forward.z;
    result.r3c2 = 0;

    result.r0c3 = origin.x;
    result.r1c3 = origin.y;
    result.r2c3 = origin.z;
    result.r3c3 = 0;

    return result;
}

Mat4fFromRightUpForwardOrigin :: inline (right : Vec3f, up : Vec3f, forward : Vec3f, origin := Vec3f.{}) -> Mat4f #must
{
    return Mat4FromRightUpForwardOrigin (right, up, forward, origin);
}

Mat4dFromRightUpForwardOrigin :: inline (right : Vec3d, up : Vec3d, forward : Vec3d, origin := Vec3d.{}) -> Mat4d #must
{
    return Mat4FromRightUpForwardOrigin (right, up, forward, origin);
}

Mat4fFromQuat :: #bake_constants Mat4FromQuat (T = f32);
Mat4dFromQuat :: #bake_constants Mat4FromQuat (T = f64);
Mat4FromQuat :: #bake_arguments MatrixFromQuat (M = 4, N = 4);

Mat4fFromAxisAngle :: #bake_constants Mat4FromAxisAngle (T = f32);
Mat4dFromAxisAngle :: #bake_constants Mat4FromAxisAngle (T = f64);
Mat4FromAxisAngle :: #bake_arguments MatrixFromAxisAngle (M = 4, N = 4);

Mat4fRotate :: Mat4fFromAxisAngle;
Mat4dRotate :: Mat4dFromAxisAngle;
Mat4Rotate :: Mat4FromAxisAngle;

Mat4fTranslate :: #bake_constants Mat4Translate (T = f32);
Mat4dTranslate :: #bake_constants Mat4Translate (T = f64);
Mat4Translate :: inline (vec : Vec3 ($T)) -> Mat4 (T) #must
{
    result : Mat4 (T);
    result.r0c3 = vec.x;
    result.r1c3 = vec.y;
    result.r2c3 = vec.z;

    return result;
}

Mat4fScale :: #bake_constants Mat4Scale (T = f32);
Mat4dScale :: #bake_constants Mat4Scale (T = f64);
Mat4Scale :: inline (vec : Vec3 ($T)) -> Mat4 (T) #must
{
    result : Mat4 (T);
    result.r0c0 = vec.x;
    result.r1c1 = vec.y;
    result.r2c2 = vec.z;

    return result;
}

Mat4fLookAt :: #bake_constants Mat4LookAt (T = f32);
Mat4dLookAt :: #bake_constants Mat4LookAt (T = f64);
Mat4LookAt :: inline (position : Vec3 ($T), target : Vec3 (T), up : Vec3 (T)) -> Mat4 (T) #must
{
    V :: Vec3 (T);
    M :: Mat4 (T);

    AlignZ :: inline (axis : V, up : V) -> M #must
    {
        x, y : V;
        y = up - axis * Dot (up, axis);
        ylen := Length (y);

        if ApproxZero (ylen, 0.000001)
        {
            k := V.{ 1, 0, 0 };
            y = k - axis * Dot (k, axis);
            ylen = Length (y);
            if ApproxZero (ylen, 0.000001)
            {
                k := V.{ 0, 0, 1 };
                y = k - axis * Dot (k, axis);
                ylen = Length (y);
            }
        }

        y /= ylen;
        x = Cross (y, axis);

        return M.{
            x.x, y.x, axis.x, 0,
            x.y, y.y, axis.y, 0,
            x.z, y.z, axis.z, 0,
              0,   0,      0, 1
        };
    }

    direction := Normalized (target - position, fallback = V.{ 0, 0, 1 });
    up_vector := Normalized (up, fallback = V.{ 0, 1, 0 });
    result := AlignZ (direction, up_vector);
    SetCol (*result, 3, MakeVec4 (position, 1));

    return result;
}

Mat4fOrthographicProjection :: #bake_constants Mat4OrthographicProjection (T = f32);
Mat4dOrthographicProjection :: #bake_constants Mat4OrthographicProjection (T = f64);
Mat4OrthographicProjection :: inline (l : $T, r : T, b : T, t : T, n : T, f : T) -> Mat4 (T) #must
{
    return Mat4 (T).{
        2 / (r - l),           0,           0, (r + l) / (l - r),
                  0, 2 / (t - b),           0, (t + b) / (b - t),
                  0,           0, 2 / (f - n), (n + f) / (n - f),
                  0,           0,           0,                 1
    };
}

Mat4fPerspectiveProjection :: #bake_constants Mat4PerspectiveProjection (T = f32);
Mat4dPerspectiveProjection :: #bake_constants Mat4PerspectiveProjection (T = f64);
Mat4PerspectiveProjection :: inline (fov_in_degrees : $T, aspect : T, n : T, f : T, left_handed := true) -> Mat4 (T) #must
{
    view_z := ifx left_handed then cast (T) 1 else cast (T) -1;
    t := math.tan (ToRads (fov_in_degrees) * 0.5) * n;
    b := -t;
    r := aspect * t;
    l := -r;

    return Mat4 (T).{
        2 * n / (r - l),               0, -view_z * (r + l) / (r - l),      0,
                      0, 2 * n / (t - b), -view_z * (t + b) / (t - b),      0,
                      0,               0,  view_z * (f + n) / (f - n), -(2 * f * n) / (f - n),
                      0,               0,                      view_z,      0
    };
}

Mat4fInfinitePerspectiveProjection :: #bake_constants Mat4InfinitePerspectiveProjection (T = f32);
Mat4dInfinitePerspectiveProjection :: #bake_constants Mat4InfinitePerspectiveProjection (T = f64);
Mat4InfinitePerspectiveProjection :: inline (fov_in_degrees : $T, aspect : T, n : T, left_handed := true) -> Mat4 (T) #must
{
    view_z := ifx left_handed then cast (T) 1 else cast (T) -1;
    t := math.tan (ToRads (fov_in_degrees) * 0.5) * n;
    b := -t;
    r := aspect * t;
    l := -r;

    return Mat4 (T).{
        2 * n / (r - l),               0, -view_z * (r + l) / (r - l),      0,
                      0, 2 * n / (t - b), -view_z * (t + b) / (t - b),      0,
                      0,               0,                      view_z, -2 * n,
                      0,               0,                      view_z,      0
    };
}

Mat4fTransformSRT :: #bake_constants Mat4TransformSRT (T = f32);
Mat4dTransformSRT :: #bake_constants Mat4TransformSRT (T = f64);
Mat4TransformSRT :: inline (position : Vec3 ($T), rotation : Quat (T), scale : Vec3 (T)) -> Mat4 (T) #must
{
    return Mat4Translate (position) * Mat4FromQuat (rotation) * Mat4Scale (scale);
}

TranslationVector :: inline (mat : $M/Matrix) -> Vector (M.T, 3) #must
{
    #assert M.M >= 3 && M.N >= 4 "Invalid dimensions for matrix (need at least 3 rows and 4 columns).";
    return GetCol (mat, 3).xyz;
}

// See: 1.7.5 Inverses of Small Matrices in 'Foundations of Game Development: Mathematics' by Eric Lengyel.
Inverse :: inline (mat : Matrix ($T, 4, 4), epsilon : T = 0.00001) -> Matrix (T, 4, 4) #must, exists : bool
{
    a := GetCol (mat, 0).xyz;
    b := GetCol (mat, 1).xyz;
    c := GetCol (mat, 2).xyz;
    d := GetCol (mat, 3).xyz;

    x := mat.r3c0;
    y := mat.r3c1;
    z := mat.r3c2;
    w := mat.r3c3;

    s := Cross (a, b);
    t := Cross (c, d);
    u := a * y - b * x;
    v := c * w - d * z;

    det := Dot (s, v) + Dot (t, u);
    s /= det;
    t /= det;
    u /= det;
    v /= det;

    r0 := Cross (b, v) + t * y;
    r1 := Cross (v, a) - t * x;
    r2 := Cross (d, u) + s * w;
    r3 := Cross (u, c) - s * z;

    return MakeMat4 (
        MakeVec4 (r0, -Dot (b, t)),
        MakeVec4 (r1,  Dot (a, t)),
        MakeVec4 (r2, -Dot (d, s)),
        MakeVec4 (r3,  Dot (c, s))
    ), !ApproxZero (det, epsilon);
}

// See: 1.7.5 Inverses of Small Matrices in 'Foundations of Game Development: Mathematics' by Eric Lengyel.
Adjugate :: inline (mat : $M/Mat4) -> M #must
{
    a := GetCol (mat, 0).xyz;
    b := GetCol (mat, 1).xyz;
    c := GetCol (mat, 2).xyz;
    d := GetCol (mat, 3).xyz;

    x := mat.r3c0;
    y := mat.r3c1;
    z := mat.r3c2;
    w := mat.r3c3;

    s := Cross (a, b);
    t := Cross (c, d);
    u := a * y - b * x;
    v := c * w - d * z;

    r0 := Cross (b, v) + t * y;
    r1 := Cross (v, a) - t * x;
    r2 := Cross (d, u) + s * w;
    r3 := Cross (u, c) - s * z;

    return MakeMat4 (
        MakeVec4 (r0, -Dot (b, t)),
        MakeVec4 (r1,  Dot (a, t)),
        MakeVec4 (r2, -Dot (d, s)),
        MakeVec4 (r3,  Dot (c, s))
    );
}

Determinant :: inline (mat : Matrix ($T, 4, 4)) -> T #must
{
    a := GetCol (mat, 0).xyz;
    b := GetCol (mat, 1).xyz;
    c := GetCol (mat, 2).xyz;
    d := GetCol (mat, 3).xyz;

    x := mat.r3c0;
    y := mat.r3c1;
    z := mat.r3c2;
    w := mat.r3c3;

    s := Cross (a, b);
    t := Cross (c, d);
    u := a * y - b * x;
    v := c * w - d * z;

    return Dot (s, v) + Dot (t, u);
}

PolarDecompose :: inline (mat : Matrix (float, $M, $N)) -> Q : Mat3f, S : Mat3f, det : float
{
    #assert M >= 3 && N >= 3;

    mat3 := ToMat3f (mat);
    Q, S, det := math.polar_decomp (mat3);

    return .{
            Q._11, Q._12, Q._13,
            Q._21, Q._22, Q._23,
            Q._31, Q._32, Q._33,
        },
        .{
            S._11, S._12, S._13,
            S._21, S._22, S._23,
            S._31, S._32, S._33,
        },
        det;
}
