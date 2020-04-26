#version 330 core

in vec3 col;
out vec4 gl_FragColor;

void main(void) {
  gl_FragColor = vec4(col, 0.0);
}