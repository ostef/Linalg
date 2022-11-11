#scope_module

assert :: inline (assertion : bool, str := "", location := #caller_location)
{
	// Prevent assert ()->...->assert ()->...->assert ()->...->assert () infinite recursion
	if !assertion && !context.handling_assertion_failure
	{
		context.handling_assertion_failure = true;
		context.assertion_failed (location, str);
		context.handling_assertion_failure = false;
	}
}

abs :: (val : $T) -> T #expand
#modify { return is_numeric (T), "T must be numeric."; }
{
	if val < 0 then return -val;

	return val;
}

min :: (a : $T, b : T) -> T #expand
#modify { return is_numeric (T), "T must be numeric."; }
{
	return ifx a < b then a else b;
}

max :: (a : $T, b : T) -> T #expand
#modify { return is_numeric (T), "T must be numeric."; }
{
	return ifx a > b then a else b;
}

sign :: (val : $T) -> T #expand
#modify { return is_numeric (T), "T must be numeric."; }
{
	#if #run is_unsigned (T)
		return cast (T) 1;
	else
		return ifx val < 0 then cast (T) -1 else cast (T) 1;
}

clamp :: (x : $T, min : T, max : T) -> T #expand
#modify { return is_numeric (T), "T must be numeric."; }
{
	if x > max then x = max;
	if x < min then x = min;

	return x;
}

lerp :: inline (x : $T, y : T, t : $T2) -> T #must
#modify { return is_numeric (T), "T must be numeric."; }
{
	return x + cast (T) (t * (y - x));
}

round :: inline (x : $T) -> T #must
#modify { return is_numeric (T), "T must be numeric."; }
{
#if #run is_floating_point (T)
{
	return cast (T) (cast (int) (x + sign (x) * 0.5));
}
else
{
	return x;
}
}

floor :: inline (x : $T) -> T #must
#modify { return is_numeric (T), "T must be numeric."; }
{
#if #run is_floating_point (T)
{
	i := cast (int) round (x);
	diff := x - i;

	return cast (T) (i - cast (int) (diff < 0));
}
else
{
	return x;
}
}

ceil :: inline (x : $T) -> T #must
#modify { return is_numeric (T), "T must be numeric."; }
{
#if #run is_floating_point (T)
{
	i := cast (int) round (x);
	diff := i - x;

	return cast (T) (i + cast (int) (diff < 0));
}
else
{
	return x;
}
}

approx_zero :: inline (val : $T, epsilon : $T2) -> bool #must
#modify {
	return is_numeric (T) && is_floating_point (T2), "T must be numeric, T2 must be a float type.";
}
{
#if #run is_floating_point (T)
	return abs (val) <= epsilon;
else
	return val == 0;
}

approx_equals :: inline (left : $T, right : T, epsilon : $T2) -> bool #must
#modify {
	return is_numeric (T) && is_floating_point (T2), "T must be numeric, T2 must be a float type.";
}
{
#if #run is_floating_point (T)
	return abs (left - right) <= epsilon;
else
	return left == right;
}

to_rads :: inline (angle_in_degrees : $T) -> T #must
#modify { return is_floating_point (T), "T must be a float type."; }
{
	return angle_in_degrees * PI / 180.0;
}

// Type traits

devariantize :: inline (info : *Type_Info) -> *Type_Info #must
{
	if info.type == .VARIANT
		info = (cast (*Type_Info_Variant) info).variant_of;
	return info;
}

is_signed :: inline (type : Type) -> bool #must
{
	info := cast (*Type_Info) type;
	info = devariantize (info);
	if info.type != .INTEGER && info.type != .FLOAT
		return false;
	if info.type == .INTEGER
		return (cast (*Type_Info_Integer) info).signed;
	return true;
}

is_unsigned :: inline (type : Type) -> bool #must
{
	info := cast (*Type_Info) type;
	info = devariantize (info);
	if info.type != .INTEGER
		return false;
	return !(cast (*Type_Info_Integer) info).signed;
}

is_numeric :: inline (type : Type) -> bool #must
{
	info := cast (*Type_Info) type;
	info = devariantize (info);
	if info.type != .FLOAT && info.type != .INTEGER
		return false;
	return true;
}

is_floating_point :: inline (type : Type) -> bool #must
{
	info := cast (*Type_Info) type;
	info = devariantize (info);
	if info.type != .FLOAT
		return false;
	return true;
}

#scope_file

basic :: #import "Basic";

#scope_export

String_Builder :: basic.String_Builder;

to_string :: basic.builder_to_string;
print_to_builder :: basic.print_to_builder;

println_to_builder :: inline (builder : *String_Builder, fmt_str : string, args : ..Any)
{
	print_to_builder (builder, fmt_str, ..args);
	print_to_builder (builder, "\n");
}
