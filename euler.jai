EulerAngles :: struct (T := f32)
{
    #assert #run IsFloatingPoint (T) "T needs to be a floating point type.";

    yaw, pitch, roll : T;

    #place yaw;
    n : [3]T;
}

EulerAnglesFromQuat :: inline (q : $Q/Quat) -> EulerAngles (Q.T) #must
{
    result : EulerAngles (Q.T) = ---;

    // Yaw
    siny_cosp :=     2 * (q.w * q.y + q.z * q.x);
    cosy_cosp := 1 - 2 * (q.x * q.x + q.y * q.y);
    result.yaw = math.atan2 (siny_cosp, cosy_cosp);

    // Pitch
    sinp := 2 * (q.w * q.x - q.y * q.z);
         if sinp >=  1 then result.pitch =  PI / 2;
    else if sinp <= -1 then result.pitch = -PI / 2;
    else                    result.pitch = math.asin (sinp);

    // Roll
    sinr_cosp :=     2 * (q.w * q.z + q.x * q.y);
    cosr_cosp := 1 - 2 * (q.z * q.z + q.x * q.x);
    result.roll = math.atan2 (sinr_cosp, cosr_cosp);

    return result;
}

Lerp :: inline (a : $E/EulerAngles, b : E, t : f32) -> E #must
{
    result : E = ---;
    result.yaw   = lerp_angle (a.yaw, b.yaw, t);
    result.pitch = lerp_angle (a.pitch, b.pitch, t);
    result.roll  = lerp_angle (a.roll, b.roll, t);

    return result;
}
