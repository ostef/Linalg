EulerAnglesRotationOrder :: enum
{
    XYZ;
    XZY;
    YXZ;
    YZX;
    ZXY;
    ZYX;
}

QuatfFromEulerAnglesOrdered :: #bake_constants QuatFromEulerAnglesOrdered(T=f32);
QuatdFromEulerAnglesOrdered :: #bake_constants QuatFromEulerAnglesOrdered(T=f64);

QuatdFromEulerAnglesXYZ :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.XYZ);
QuatdFromEulerAnglesXZY :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.XZY);
QuatdFromEulerAnglesYXZ :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.YXZ);
QuatdFromEulerAnglesYZX :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.YZX);
QuatdFromEulerAnglesZXY :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.ZXY);
QuatdFromEulerAnglesZYX :: #bake_arguments QuatdFromEulerAnglesOrdered(order=.ZYX);

QuatfFromEulerAnglesXYZ :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.XYZ);
QuatfFromEulerAnglesXZY :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.XZY);
QuatfFromEulerAnglesYXZ :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.YXZ);
QuatfFromEulerAnglesYZX :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.YZX);
QuatfFromEulerAnglesZXY :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.ZXY);
QuatfFromEulerAnglesZYX :: #bake_arguments QuatfFromEulerAnglesOrdered(order=.ZYX);

QuatFromEulerAnglesOrdered :: (euler : Vec3($T), $$order : EulerAnglesRotationOrder) -> Quat(T)
{
    a, b, c : Quat(T) = ---;
    X :: Vec3(T).{1,0,0};
    Y :: Vec3(T).{0,1,0};
    Z :: Vec3(T).{0,0,1};

    if #complete order ==
    {
    case .XYZ;
        a = QuatFromAxisAngle(X, euler.x);
        b = QuatFromAxisAngle(Y, euler.y);
        c = QuatFromAxisAngle(Z, euler.z);
    case .YXZ;
        a = QuatFromAxisAngle(Y, euler.y);
        b = QuatFromAxisAngle(X, euler.x);
        c = QuatFromAxisAngle(Z, euler.z);
    case .ZXY;
        a = QuatFromAxisAngle(Z, euler.z);
        b = QuatFromAxisAngle(X, euler.x);
        c = QuatFromAxisAngle(Y, euler.y);
    case .ZYX;
        a = QuatFromAxisAngle(Z, euler.z);
        b = QuatFromAxisAngle(Y, euler.y);
        c = QuatFromAxisAngle(X, euler.x);
    case .YZX;
        a = QuatFromAxisAngle(Y, euler.y);
        b = QuatFromAxisAngle(Z, euler.z);
        c = QuatFromAxisAngle(X, euler.x);
    case .XZY;
        a = QuatFromAxisAngle(X, euler.x);
        b = QuatFromAxisAngle(Z, euler.z);
        c = QuatFromAxisAngle(Y, euler.y);
    }

    return a * (b * c);
}

ToEulerAngles :: (q : Quat($T), $$order : EulerAnglesRotationOrder) -> Vec3(T)
{
    if #complete order ==
    {
    case .XYZ; return ToEulerAnglesXYZ(q);
    // case .XZY; return ToEulerAnglesXZY(q);
    case .YXZ; return ToEulerAnglesYXZ(q);
    // case .YZX; return ToEulerAnglesYZX(q);
    // case .ZXY; return ToEulerAnglesZXY(q);
    // case .ZYX; return ToEulerAnglesZYX(q);
    }
}

ToEulerAnglesXYZ :: (q : Quat($T)) -> Vec3(T)
{
    return ToEulerAnglesXYZ(Mat3FromQuat(q));
}

ToEulerAnglesYXZ :: (q : Quat($T)) -> Vec3(T)
{
    return ToEulerAnglesYXZ(Mat3FromQuat(q));
}

ToEulerAngles :: (m : Matrix($T, $M, $N), $$order : EulerAnglesRotationOrder) -> Vec3(T)
{
    if #complete order ==
    {
    case .XYZ; return ToEulerAnglesXYZ(m);
    // case .XZY; return ToEulerAnglesXZY(m);
    case .YXZ; return ToEulerAnglesYXZ(m);
    // case .YZX; return ToEulerAnglesYZX(m);
    // case .ZXY; return ToEulerAnglesZXY(m);
    // case .ZYX; return ToEulerAnglesZYX(m);
    }
}

ToEulerAnglesXYZ :: (m : Matrix($T, $M, $N)) -> Vec3(T)
{
    T1 := math.atan2(m.r1c2, m.r2c2);
    C2 := math.sqrt(m.r0c0 * m.r0c0 + m.r0c1 * m.r0c1);
    T2 := math.atan2(-m.r0c2, C2);
    S1 := math.sin(T1);
    C1 := math.cos(T1);
    T3 := math.atan2(S1 * m.r2c0 - C1 * m.r1c0, C1 * m.r1c1 - S1 * m.r2c1);

    return .{T1, T2, T3};
}

ToEulerAnglesYXZ :: (m : Matrix($T, $M, $N)) -> Vec3(T)
{
    T1 := math.atan2(m.r0c2, m.r2c2);
    C2 := math.sqrt(m.r1c0 * m.r1c0 + m.r1c1 * m.r1c1);
    T2 := math.atan2(-m.r1c2, C2);
    S1 := math.sin(T1);
    C1 := math.cos(T1);
    T3 := math.atan2(S1 * m.r2c1 - C1 * m.r0c1, C1 * m.r0c0 - S1 * m.r2c0);

    return .{T2, T1, T3};
}
