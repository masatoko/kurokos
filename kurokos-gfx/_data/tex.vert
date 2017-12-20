#version 400

in vec2 VertexCoord;
in vec2 TexCoord;
out vec2 OTexCoord;

uniform mat4 MVP;

void main()
{
  gl_Position = MVP * vec4( VertexCoord, 0, 1 );
  OTexCoord = TexCoord;
}
