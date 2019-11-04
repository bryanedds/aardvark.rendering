namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

open Microsoft.FSharp.NativeInterop

module RaytracingProperties =
    
    let get (device : Device) =
        native {
            let! pRayTracingProperties =
                VkPhysicalDeviceRayTracingPropertiesNV(
                    VkStructureType.PhysicalDeviceRayTracingPropertiesNv, 0n,
                    0u, 0u, 0u, 0u, 0UL, 0UL, 0UL, 0u
                )

            let! pProperties =
                VkPhysicalDeviceProperties2(
                    VkStructureType.PhysicalDeviceProperties2, NativePtr.toNativeInt pRayTracingProperties,
                    VkPhysicalDeviceProperties()
                )

            VkRaw.vkGetPhysicalDeviceProperties2(device.PhysicalDevice.Handle, pProperties)
            let pNext = NativePtr.ofNativeInt<VkPhysicalDeviceRayTracingPropertiesNV> (!!pProperties).pNext

            return !!pNext
        }

    let shaderGroupHandleSize (device : Device) =
        (get device).shaderGroupHandleSize

    let maxRecursionDepth (device : Device) =
        (get device).maxRecursionDepth

