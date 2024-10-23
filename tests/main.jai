Linalg :: #import "Linalg";
using, only(.["==","+","-","*","/","*[]"]) Linalg;

#load "../utils.jai";

// For now, the following tests are there to make sure things compile fine.

VectorTests :: ($T : Type, $N : s64)
{
    VAL :: Linalg.Vector(T, N).{};

    v := Linalg.Scale(VAL, VAL);
    t := Linalg.Dot(v, v);
    f := Linalg.Length(v);
    v = Linalg.Normalized(v);
    v = Linalg.Clamp(v, VAL, VAL);
    v = Linalg.Round(v);
    v = Linalg.Floor(v);
    v = Linalg.Ceil(v);
    v = Linalg.Min(v, v);
    v = Linalg.Max(v, v);
    v = Linalg.Lerp(v, v, 1.0);
    f = Linalg.Distance(v, v);
    v += v;
    v -= v;
#if #run IsSigned(T)
    v = -v;
    v *= 10;
    v /= 10;
    b := v == v;
#if #run IsFloatingPoint(T)
{
    b = Linalg.ApproxZero(v, 0.0001);
    b = Linalg.ApproxEquals(v, v, 0.0001);
}
#if N == 3
{
    v = Linalg.Cross(v, v);
}
    t = v[1];
    v[1] = 10;
    assert(v[1] == 10);
}

MatrixTests :: ($T : Type, $M : s64, $N : s64)
{
    m : Linalg.Matrix(T, M, N);
    //identity(*m);
    r := Linalg.GetRow(m, 0);
    c := Linalg.GetCol(m, 0);
    Linalg.SetRow(*m, 0, r);
    Linalg.SetCol(*m, 0, c);
    mt := Linalg.Transposed(m);
    m2 := Linalg.Resize(m, T, 5, 5);
    m += m;
    m -= m;
#if #run IsSigned(T)
    m = -m;
    m *= 10;
    vr := m * m.RightVector.{};
    vl := m.LeftVector.{} * m;
    {
        m2 : Linalg.Matrix(T, N, 2);
        m3 := m * m2;
    }
    m /= 10;
    b := m == m;
#if M >= 3 && N >= 3
    m = Linalg.MatrixFromAxisAngle(M, N, Linalg.Vec3(T).{}, 0);
    m[1][1] = 100;
    assert(m[1][1] == 100);
    m[1] = m.Row.{};
    assert(m[1] == m.Row.{});
}

QuaternionTests :: ($T : Type)
{
    q : Linalg.Quat(T);
    q = Linalg.QuatLookAt(Linalg.Vec3(T).{ 0, 0, 1 }, Linalg.Vec3(T).{ 0, 1, 0 });
    q = Linalg.QuatFromAxisAngle(Linalg.Vec3(T).{ 0, 0, 1 }, 0);
    Linalg.ToAxisAngle(q);
    q = Linalg.QuatFromEulerAngles(Linalg.EulerAngles(T).{ 0, 0, 0 });
    euler := Linalg.EulerAnglesFromQuat(q);
    t := Linalg.Dot(q, q);
    f := Linalg.Length(q);
    q = Linalg.Normalized(q);
    q = Linalg.Inverse(q);
    q = Linalg.Conjugate(q);
    f = Linalg.Angle(q);
    f = Linalg.Angle(q, q);
    m3 := Linalg.Mat3FromQuat(q);
    q = Linalg.QuatFromMatrix(m3);
    m4 := Linalg.Mat4FromQuat(q);
    q = Linalg.QuatFromMatrix(m4);
    v := Linalg.Rotate(Linalg.Vec3(T).{ 0, 0, 1 }, q);
    v = Linalg.RightVector(q);
    v = Linalg.UpVector(q);
    v = Linalg.ForwardVector (q);
    q = Linalg.NLerp(q, q, 1.0);
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
    VectorTests(float, 2);
    VectorTests(float, 3);
    VectorTests(s32, 3);
    VectorTests(u32, 4);

    MatrixTests(float, 2, 2);
    MatrixTests(float, 3, 3);
    MatrixTests(float64, 3, 3);
    MatrixTests(float, 4, 4);

    QuaternionTests(float32);
    //quaternion(float64);
}
