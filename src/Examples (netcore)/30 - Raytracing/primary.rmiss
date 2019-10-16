#version 460
#extension GL_NV_ray_tracing : require

struct RayPayload {
    vec4 color;
    int bounce;
};

layout(location = 0) rayPayloadInNV RayPayload payloadIn;

void main() {
    payloadIn.color = vec4(0.412f, 0.796f, 1.0f, 1.0f);
}