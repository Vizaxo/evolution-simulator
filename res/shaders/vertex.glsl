#version 330 core

in vec2 pos;
in vec3 colIn;
out vec3 col;

float cos30 = 0.86602540378;

void main(void) {
  gl_Position = vec4(pos.x + 0.5*pos.y, pos.y * 1.5*cos30, 0.0, 1.0);
  col = colIn;
}
