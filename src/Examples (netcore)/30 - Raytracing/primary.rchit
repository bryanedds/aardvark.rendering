#version 460
#extension GL_NV_ray_tracing : require
#extension GL_EXT_nonuniform_qualifier : require

struct RayPayload {
    vec4 color;
    int bounce;
};

layout(location = 0) rayPayloadInNV RayPayload payloadIn;
hitAttributeNV vec2 hitAttribs;

layout(set = 0, binding = 4, std430) readonly buffer ColorBuffer { 
    vec4 colors[];
} colorBuffer;

layout(set = 0, binding = 5) uniform sampler2D[] textures;

vec2 getTextureCoordinates() {
    const vec2 tc[3] = vec2[3](vec2(0,0), vec2(1,0), vec2(1,1));
    const vec3 bc = vec3(1.0f - hitAttribs.x - hitAttribs.y, hitAttribs.x, hitAttribs.y);
    return bc.x * tc[0] + bc.y * tc[1] + bc.z * tc[2];
}

void main() {
    vec2 tc = getTextureCoordinates();
    vec4 diffuse = texture(textures[gl_InstanceCustomIndexNV], tc);
    payloadIn.color = diffuse; //vec4(getTextureCoordinates(), 0.0, 1.0);//colorBuffer.colors[gl_InstanceCustomIndexNV];
}