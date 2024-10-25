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

QuatFromEulerAnglesOrdered :: (euler : Vec3($T), $$order : EulerAnglesRotationOrder) -> Quat(T) #must
{
    cosx := math.cos(euler.x * 0.5);
    cosy := math.cos(euler.y * 0.5);
    cosz := math.cos(euler.z * 0.5);

    sinx := math.sin(euler.x * 0.5);
    siny := math.sin(euler.y * 0.5);
    sinz := math.sin(euler.z * 0.5);

    sinx_cosy := sinx * cosy;
    cosx_siny := cosx * siny;
    cosx_cosy := cosx * cosy;
    sinx_siny := sinx * siny;

    q : Quat(T) = ---;
    if order ==
    {
    case .XYZ;
        q.x = sinx_cosy * cosz + cosx_siny * sinz;
        q.y = cosx_siny * cosz - sinx_cosy * sinz;
        q.z = cosx_cosy * sinz + sinx_siny * cosz;
        q.w = cosx_cosy * cosz - sinx_siny * sinz;
    case .YXZ;
        q.x = sinx_cosy * cosz + cosx_siny * sinz;
        q.y = cosx_siny * cosz - sinx_cosy * sinz;
        q.z = cosx_cosy * sinz - sinx_siny * cosz;
        q.w = cosx_cosy * cosz + sinx_siny * sinz;
    case .ZXY;
        q.x = sinx_cosy * cosz - cosx_siny * sinz;
        q.y = cosx_siny * cosz + sinx_cosy * sinz;
        q.z = cosx_cosy * sinz + sinx_siny * cosz;
        q.w = cosx_cosy * cosz - sinx_siny * sinz;
    case .ZYX;
        q.x = sinx_cosy * cosz - cosx_siny * sinz;
        q.y = cosx_siny * cosz + sinx_cosy * sinz;
        q.z = cosx_cosy * sinz - sinx_siny * cosz;
        q.w = cosx_cosy * cosz + sinx_siny * sinz;
    case .YZX;
        q.x = sinx_cosy * cosz + cosx_siny * sinz;
        q.y = cosx_siny * cosz + sinx_cosy * sinz;
        q.z = cosx_cosy * sinz - sinx_siny * cosz;
        q.w = cosx_cosy * cosz - sinx_siny * sinz;
    case .XZY;
        q.x = sinx_cosy * cosz - cosx_siny * sinz;
        q.y = cosx_siny * cosz - sinx_cosy * sinz;
        q.z = cosx_cosy * sinz + sinx_siny * cosz;
        q.w = cosx_cosy * cosz + sinx_siny * sinz;
    }

    return q;
}

ToEulerAnglesXYZ :: (q : $Q/Quat) -> Vec3(Q.T) #must, flip : bool
{
    result : Vec3(Q.T) = ---;

    flip : bool = ---;
    test := q.y * q.w + q.x * q.z;
    if Abs(test) > 0.4999
    {
        sign := ifx test > 0 then 1.0 else -1.0;
        result.x = sign * 2 * math.atan2(q.z, q.w);
        result.y = sign * math.PI / 2;
        result.z = 0;
        flip = true;
    }
    else
    {
        sqy := q.y * q.y;
        result.x = math.atan2(2 * (q.x * q.w - q.y * q.z), 1 - 2 * (q.x * q.x + sqy));
        result.y = math.asin(2 * test);
        result.z = math.atan2(2 * (q.z * q.w - q.x * q.y), 1 - 2 * (q.z * q.z + sqy));
        flip = false;
    }

    return result, flip;
}
