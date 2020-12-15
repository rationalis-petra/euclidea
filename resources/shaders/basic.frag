#version 330 core
out vec4 FragColor;

uniform vec3 light_pos;
//uniform vec3 view_pos;
uniform vec3 object_color;

in vec3 frag_pos;
in vec3 normal;

void main()
{
    vec3 obj_colour = object_color;
    vec3 light_colour = vec3(1.0, 1.0, 1.0);

    float ambient_strength = 0.2;
    float specular_strength = 0.5;

    vec3 ambient = ambient_strength * light_colour;

    vec3 norm = normalize(normal);
    vec3 light_dir = normalize (light_pos - frag_pos);
    float diff = max(dot(norm, light_dir), 0.0);
    vec3 diffuse = diff * light_colour;

    vec3 result;
    result = (diffuse + ambient) * obj_colour;


    FragColor = vec4(result, 1.0f);
} 