namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Base
open Aardvark.Rendering.Vulkan.NVRayTracing

open System.Runtime.InteropServices

[<StructLayout(LayoutKind.Explicit, Size = 3)>]
type uint24 =
    struct
        [<FieldOffset(0)>]
        val mutable public B0 : uint8
        [<FieldOffset(1)>]
        val mutable public B1 : uint8
        [<FieldOffset(2)>]
        val mutable public B2 : uint8

        member x.Value = uint32 (x.B0) ||| uint32(x.B1 <<< 8) ||| uint32 (x.B2 <<< 16)
        new(v : uint32) = { B0 = byte (v &&& 0xFFu); B1 = byte ((v >>> 8) &&& 0xFFu); B2= byte ((v >>> 16) &&& 0xFFu)}
    end

type VkGeometryInstance =
    struct
        val mutable public Transform : M34f
        val mutable public InstanceId : uint24
        val mutable public Mask : uint8
        val mutable public InstanceOffset : uint24
        val mutable public Flags : uint8
        val mutable public AccelerationStructureHandle : uint64

        new (transform : Trafo3d, instanceId : int, mask : uint8,
                instanceOffset : int, flags : VkGeometryInstanceFlagsNV, blAS : uint64) =
            {
                Transform = M34f.op_Explicit transform.Forward
                InstanceId = uint24 <| uint32 instanceId
                Mask = mask
                InstanceOffset = uint24 <| uint32 instanceOffset
                Flags = uint8 flags
                AccelerationStructureHandle = blAS
            }
    end