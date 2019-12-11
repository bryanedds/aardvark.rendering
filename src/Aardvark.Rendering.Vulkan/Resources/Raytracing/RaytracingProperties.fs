namespace Aardvark.Rendering.Vulkan.Raytracing

#nowarn "9"

open Aardvark.Rendering.Vulkan
open Aardvark.Rendering.Vulkan.NVRayTracing

open Microsoft.FSharp.NativeInterop

module RaytracingProperties =
    
    let get (device : Device) =
        native {
            let! pRayTracingProperties = VkPhysicalDeviceRayTracingPropertiesNV.Empty

            let! pProperties =
                VkPhysicalDeviceProperties2(
                    NativePtr.toNativeInt pRayTracingProperties,
                    VkPhysicalDeviceProperties.Empty
                )

            VkRaw.vkGetPhysicalDeviceProperties2(device.PhysicalDevice.Handle, pProperties)
            let pNext = NativePtr.ofNativeInt<VkPhysicalDeviceRayTracingPropertiesNV> (!!pProperties).pNext

            return !!pNext
        }

    let shaderGroupHandleSize (device : Device) =
        (get device).shaderGroupHandleSize

    let maxRecursionDepth (device : Device) =
        (get device).maxRecursionDepth

