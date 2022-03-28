Euler_Angles :: struct (T := f32)
{
	#assert #run is_floating_point (T) "T needs to be a floating point type.";

	yaw, pitch, roll : T;

	#place yaw;
	n : [3]T;
}

euler_angles :: inline (yaw : $T, pitch : T, roll : T) -> Euler_Angles (T) #must
{
	result : Euler_Angles (T) = ---;
	result.yaw = yaw;
	result.pitch = pitch;
	result.roll = roll;

	return result;
}

euler_angles_from_quat :: inline (q : $Q/Quat) -> Euler_Angles (Q.T) #must
{
	result : Euler_Angles (Q.T) = ---;

	// Yaw
	siny_cosp :=     2 * (q.w * q.y + q.z * q.x);
	cosy_cosp := 1 - 2 * (q.x * q.x + q.y * q.y);
	result.yaw = atan2 (siny_cosp, cosy_cosp);

	// Pitch
	sinp := 2 * (q.w * q.x - q.y * q.z);
	     if sinp >=  1 then result.pitch =  PI / 2; 
	else if sinp <= -1 then result.pitch = -PI / 2;
	else                    result.pitch = asin (sinp); 

	// Roll
	sinr_cosp :=     2 * (q.w * q.z + q.x * q.y);
	cosr_cosp := 1 - 2 * (q.z * q.z + q.x * q.x);
	result.roll = atan2 (sinr_cosp, cosr_cosp);

	return result;
}

lerp :: inline (a : $E/Euler_Angles, b : E, t : f32) -> E #must
{
	result : E = ---;
	result.yaw   = lerp_angle (a.yaw, b.yaw, t);
	result.pitch = lerp_angle (a.pitch, b.pitch, t);
	result.roll  = lerp_angle (a.roll, b.roll, t);

	return result;
}
