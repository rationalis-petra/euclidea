#version 330 core

layout (location = 0) in vec3 aPos;
  
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    vec3 pos = (aPos * 0.5) + vec3(0.0, 0.0, -3.0);
    gl_Position = projection * view * model * vec4(pos, 1.0f);
} 
