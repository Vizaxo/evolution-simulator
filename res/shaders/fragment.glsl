#version 330 core

in vec3 colOut;
out vec4 gl_FragColor;

void main(void) {
  gl_FragColor = vec4(colOut, 0.0);
}
