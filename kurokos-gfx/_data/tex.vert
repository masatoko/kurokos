#version 400

in vec2 Position;
out vec2 TexCoord;

void main()
{
  gl_Position = vec4( Position, 0, 1 );
  TexCoord = Position * vec2(0.5) + vec2(0.5);
}
