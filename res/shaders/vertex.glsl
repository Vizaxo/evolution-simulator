#version 330 core

in vec2 pos;
out vec3 col;

void main(void) {
  gl_Position = vec4(pos.xy, 0.0, 1.0);
  col = vec3(0,1,0);
}
