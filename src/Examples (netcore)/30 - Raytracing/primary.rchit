#version 460
#extension GL_NV_ray_tracing : require

struct RayPayload {
    vec4 color;
    int bounce;
};

layout(location = 0) rayPayloadInNV RayPayload payloadIn;
hitAttributeNV vec2 hitAttribs;

void main() {
    const vec3 bc = vec3(1.0f - hitAttribs.x - hitAttribs.y, hitAttribs.x, hitAttribs.y);
    payloadIn.color = vec4(bc, 1.0);
}