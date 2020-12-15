#version 330 core
out vec4 FragColor;

in vec2 tex_coord;

uniform vec3 light_pos;
uniform vec3 view_pos;
uniform sampler2D ourTexture;


void main() {
     //FragColor = vec4(1.0, 0.5, 0.31, 1.0);
     FragColor = texture(ourTexture, tex_coord);
}