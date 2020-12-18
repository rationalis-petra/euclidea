#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
  
out vec3 frag_pos;
out vec3 normal;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    vec4 pos = model * vec4(aPos, 1.0f);

    gl_Position = projection * view * pos;
    frag_pos = vec3(pos);
    normal = mat3(transpose(inverse(model))) * aNormal;
} 
