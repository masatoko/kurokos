#version 400

in vec2 VertexCoord;
out vec2 TexCoord;

void main()
{
  gl_Position = vec4( VertexCoord, 0, 1 );
  TexCoord = VertexCoord * vec2(0.5) + vec2(0.5);
}
