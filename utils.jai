#scope_module

Abs :: (val : $T) -> T #expand
#modify { return IsNumeric (T), "T must be numeric."; }
{
    if val < 0 then return -val;

    return val;
}

Min :: (a : $T, b : T) -> T #expand
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return ifx a < b then a else b;
}

Max :: (a : $T, b : T) -> T #expand
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return ifx a > b then a else b;
}

Sign :: (val : $T) -> T #expand
#modify { return IsNumeric (T), "T must be numeric."; }
{
    #if #run is_unsigned (T)
        return cast (T) 1;
    else
        return ifx val < 0 then cast (T) -1 else cast (T) 1;
}

Clamp :: (x : $T, min : T, max : T) -> T #expand
#modify { return IsNumeric (T), "T must be numeric."; }
{
    if x > max then x = max;
    if x < min then x = min;

    return x;
}

Lerp :: inline (x : $T, y : T, t : $T2) -> T #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
    return x + cast (T) (t * (y - x));
}

Round :: inline (x : $T) -> T #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
#if #run IsFloatingPoint (T)
{
    return cast (T) (cast (int) (x + Sign (x) * 0.5));
}
else
{
    return x;
}
}

Floor :: inline (x : $T) -> T #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
#if #run IsFloatingPoint (T)
{
    i := cast (int) Round (x);
    diff := x - i;

    return cast (T) (i - cast (int) (diff < 0));
}
else
{
    return x;
}
}

Ceil :: inline (x : $T) -> T #must
#modify { return IsNumeric (T), "T must be numeric."; }
{
#if #run IsFloatingPoint (T)
{
    i := cast (int) Round (x);
    diff := i - x;

    return cast (T) (i + cast (int) (diff < 0));
}
else
{
    return x;
}
}

#scope_export

ApproxZero :: inline (val : $T, epsilon : $T2) -> bool #must
#modify {
    return IsNumeric (T) && IsFloatingPoint (T2), "T must be numeric, T2 must be a float type.";
}
{
#if #run IsFloatingPoint (T)
    return Abs (val) <= epsilon;
else
    return val == 0;
}

ApproxEquals :: inline (left : $T, right : T, epsilon : $T2) -> bool #must
#modify {
    return IsNumeric (T) && IsFloatingPoint (T2), "T must be numeric, T2 must be a float type.";
}
{
#if #run IsFloatingPoint (T)
    return Abs (left - right) <= epsilon;
else
    return left == right;
}

#scope_module

ToRads :: inline (angle_in_degrees : $T) -> T #must
#modify { return IsFloatingPoint (T), "T must be a float type."; }
{
    return angle_in_degrees * PI / 180.0;
}

// Type traits

SharesSameSourcePolymorph :: inline (info : *Type_Info_Struct, of : *Type_Info_Struct) -> bool #must
{
    while info && info.polymorph_source_struct
    {
        info = info.polymorph_source_struct;
    }

    while of && of.polymorph_source_struct
    {
        of = of.polymorph_source_struct;
    }

    return info && info == of;
}

Devariantize :: inline (info : *Type_Info) -> *Type_Info #must
{
    if info.type == .VARIANT
        info = (cast (*Type_Info_Variant) info).variant_of;
    return info;
}

IsSigned :: inline (type : Type) -> bool #must
{
    info := cast (*Type_Info) type;
    info = Devariantize (info);
    if info.type != .INTEGER && info.type != .FLOAT
        return false;
    if info.type == .INTEGER
        return (cast (*Type_Info_Integer) info).signed;
    return true;
}

is_unsigned :: inline (type : Type) -> bool #must
{
    info := cast (*Type_Info) type;
    info = Devariantize (info);
    if info.type != .INTEGER
        return false;
    return !(cast (*Type_Info_Integer) info).signed;
}

IsNumeric :: inline (type : Type) -> bool #must
{
    info := cast (*Type_Info) type;
    info = Devariantize (info);
    if info.type != .FLOAT && info.type != .INTEGER
        return false;
    return true;
}

IsFloatingPoint :: inline (type : Type) -> bool #must
{
    info := cast (*Type_Info) type;
    info = Devariantize (info);
    if info.type != .FLOAT
        return false;
    return true;
}

Basic :: #import "Basic";

assert :: Basic.assert;

String_Builder :: Basic.String_Builder;

to_string :: Basic.builder_to_string;
print_to_builder :: Basic.print_to_builder;

println_to_builder :: inline (builder : *String_Builder, fmt_str : string, args : ..Any)
{
    print_to_builder (builder, fmt_str, ..args);
    print_to_builder (builder, "\n");
}
