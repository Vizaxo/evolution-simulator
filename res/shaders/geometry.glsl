#version 330 core

layout (points) in;
layout (triangle_strip, max_vertices = 18) out;

in vec3 col[];
out vec3 colOut;

uniform float scaleX;
uniform float scaleY;


float cos30 = 0.86602540378;
vec4 centre = gl_in[0].gl_Position;
vec4 v0 = centre+vec4(0, cos30, 0, 0);
vec4 v1 = centre+vec4(0.5, 0.5*cos30, 0, 0);
vec4 v2 = centre+vec4(0.5, -0.5*cos30, 0, 0);
vec4 v3 = centre+vec4(0, -cos30, 0, 0);
vec4 v4 = centre+vec4(-0.5, -0.5*cos30, 0, 0);
vec4 v5 = centre+vec4(-0.5, 0.5*cos30, 0, 0);

void tri(vec4 a, vec4 b) {
     gl_Position = centre * vec4(scaleX, scaleY, 1, 1);
     colOut = col[0];
     EmitVertex();

     gl_Position = a * vec4(scaleX, scaleY, 1, 1);
     colOut = col[0];
     EmitVertex();

     gl_Position = b * vec4(scaleX, scaleY, 1, 1);
     colOut = col[0];
     EmitVertex();

     EndPrimitive();
}
void main(void) {
     tri(v0, v1);
     tri(v1, v2);
     tri(v2, v3);
     tri(v3, v4);
     tri(v4, v5);
     tri(v5, v0);
}
