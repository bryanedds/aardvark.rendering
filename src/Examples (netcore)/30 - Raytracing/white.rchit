#version 460
#extension GL_NV_ray_tracing : require

struct RayPayload {
    vec4 color;
    int bounce;
};

layout(location = 0) rayPayloadInNV RayPayload payloadIn;

void main() {
    payloadIn.color = vec4(1.0, 1.0, 1.0, 1.0);
}