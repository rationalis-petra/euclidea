#version 330 core
out vec4 FragColor;

in vec2 tex_coord;

uniform vec3 light_pos;
uniform vec3 view_pos;
uniform sampler2D ourTexture;


void main() {
     FragColor = texture(ourTexture, tex_coord);
}