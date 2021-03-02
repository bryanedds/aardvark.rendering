### 5.1.5
- fixed Silk.NET.Core depenedency
- fixed renderToColorCube 

### 5.1.4
- updated packages

### 5.1.3
- [Vulkan] CommandTask no longer disposes ResourceManager
- [Sg] Added instancing utilities for IndexedGeometry 

### 5.1.2
- fixed thread abort exn on linux

### 5.1.1
- https://hackmd.io/58CqcVmnRoGq-X5gIrNThg

### 5.0.17
 - [GL] OpenVR support for GL
 
### 5.0.15
 - [GL] replaced EXT_direct_state_access with ARB_direct_state_access
 - [GL] fixed crashes when using core profile
 - [GL] RenderTask.Dispose no longer needs a transaction (https://github.com/aardvark-platform/aardvark.rendering/issues/60)

### 5.0.14
 - [GL] More robust parsing of GL and GLSL versions

### 5.0.3
 - [Text] fixed compatibility with render passes

### 5.0.0
 - [Base] updated aardvark to v5
 - [Base] reworked Buffer API: added BufferUsage flags
 - [Base] added indirect draw stride
 - [Base] refactored RenderObject drawcalls
 - [Base] removed IResizeBuffer, IMappedBuffer, IMappedIndirectBuffer
 - [Sg] reworked ManagedPool

### 4.12.4
 - [GL] fixed mip level calculation in texture upload

### 4.12.3
 - [Base] updated base packages

### 4.12.2
 - [Base] fixed GLVM loading for all plattforms

### 4.12.1
 - [Base] updated GLVM for linux

### 4.12.0
 - [GL] removed warnings from LodRenderer
 - [GL] added support for NormalUV texture (2-channel float images)

### 4.11.15
 - [GL] fixed BufferRuntime.Clear

### 4.11.12
 - [GL] added quad-buffered stereo support to GameWindow

### 4.11.8
 - [Base] fixed RenderTask.custom
 - [GL] fixed size 0 UniformBuffer alloc

### 4.11.7
 - [Base] updated packages / fixed memory leak

### 4.11.5
 - [Base] reverted memory leak fix

### 4.11.4
 - [Base] rmeoved hooking mechanism of dynamic uniforms (no need anymore, allowed overwrite of view/proj trafo)
 - [Base] moved Caches (UnaryCache, BinaryCache) to Base.FSharp
 - [Sg] fixed memory leak when using derived attirbutes (e.g. ModelViewTrafo, ModelViewProjTrafo)
 - [GL] fixed texture array uniforms 

### 4.11.3
 - LoD Render: removed debug ouput

### 4.11.2
 - [GL] fixed buffer resource stats
 - [GL] fixed unmanaged memory leak of VAO
 