// @Note (stefan): We import Math for trig functions and sqrt.
// I've moved a lot of the math functions from my Math module
// to utils.jai so that users can use any Math module, since
// any sensible Math module should provide trigonometry.

#scope_module

math :: #import "Math";

#scope_export

#load "utils.jai";
#load "vector.jai";
#load "matrix.jai";
#load "quaternion.jai";
#load "euler.jai";

CoordinateAxes :: struct
{
    right, up, forward : Vec3f;
}

#add_context coordinate_axes := CoordinateAxes.{ .{ 1, 0, 0 }, .{ 0, 1, 0 }, .{ 0, 0, 1 } };

MakeCoordinateAxes :: inline (right : string, up : string, forward : string) -> CoordinateAxes #must
{
    IsAxis :: inline (str : string) -> bool #must
    {
        if str.count != 2
            return false;
        if str[0] != #char "+" && str[0] != #char "-"
            return false;
        if str[1] != #char "X" && str[1] != #char "Y" && str[1] != #char "Z"
            return false;
        return true;
    }

    ToVector :: inline (axis : string) -> Vec3f #must
    {
        sign := ifx axis[0] == #char "-" then -1.0 else 1.0;
        vector : Vec3f;
        if axis[1] == #char "X"
            vector.x = sign;
        else if axis[1] == #char "Y"
            vector.y = sign;
        else if axis[1] == #char "Z"
            vector.z = sign;
        return vector;
    }

    assert (IsAxis (right), "Invalid parameter for right (must be [-+][XYZ]).");
    assert (IsAxis (up), "Invalid parameter for up (must be [-+][XYZ]).");
    assert (IsAxis (forward), "Invalid parameter for forward (must be [-+][XYZ]).");
    assert (right[1] != up[1] && up[1] != forward[1] && right[1] != forward[1], "Cannot have the same value for right, up and forward.");

    return .{ToVector (right), ToVector (up), ToVector (forward)};
}

IsVectorType :: inline (info : *Type_Info) -> bool #must
{
    return GetVectorTypeInfo (info);
}

IsQuaternionType :: inline (info : *Type_Info) -> bool #must
{
    return GetQuaternionTypeInfo (info);
}

IsMatrixType :: inline (info : *Type_Info) -> bool #must
{
    return GetMatrixTypeInfo (info);
}

GetQuaternionTypeInfo :: (info : *Type_Info) -> is_vector_type : bool, T : *Type_Info
{
    if info.type != .STRUCT
        return false, null;

    struct_info := cast (*Type_Info_Struct) info;
    if !SharesSameSourcePolymorph (struct_info, type_info (Quatf))
        return false, null;

    for struct_info.specified_parameters
    {
        if !(it.flags & .CONSTANT)
            continue;

        ptr := struct_info.constant_storage.data + it.offset_into_constant_storage;
        if it.name == "T"
            return true, (cast (**Type_Info) ptr).*;
    }

    return false, null;
}

GetVectorTypeInfo :: (info : *Type_Info) -> is_vector_type : bool, T : *Type_Info, N : int
{
    if info.type != .STRUCT
        return false, null, 0;

    struct_info := cast (*Type_Info_Struct) info;

    if !SharesSameSourcePolymorph (struct_info, type_info (Vec3f))
        return false, null, 0, ;

    T : *Type_Info;
    N : int;
    for struct_info.specified_parameters
    {
        if !(it.flags & .CONSTANT)
            continue;

        ptr := struct_info.constant_storage.data + it.offset_into_constant_storage;
        if it.name ==
        {
        case "N";
            N = (cast (*int) ptr).*;
        case "T";
            T = (cast (**Type_Info) ptr).*;
        }
    }

    return true, T, N;
}

GetMatrixTypeInfo :: (info : *Type_Info) -> is_matrix_type : bool, T : *Type_Info, M : int, N : int
{
    if info.type != .STRUCT
        return false, null, 0, 0;

    struct_info := cast (*Type_Info_Struct) info;
    if !SharesSameSourcePolymorph (struct_info, type_info (Mat3f))
        return false, null, 0, 0;

    T : *Type_Info;
    M : int;
    N : int;
    for struct_info.specified_parameters
    {
        if !(it.flags & .CONSTANT)
            continue;

        ptr := struct_info.constant_storage.data + it.offset_into_constant_storage;
        if it.name ==
        {
        case "M";
            M = (cast (*int) ptr).*;
        case "N";
            N = (cast (*int) ptr).*;
        case "T";
            T = (cast (**Type_Info) ptr).*;
        }
    }

    return true, T, M, N;
}

#scope_module

f32 :: float32;
f64 :: float64;

PI   : f32 : 3.1415927;
PI64 : f64 : 3.141592653589793;
