#version 400

in vec2 VertexCoord;
in vec2 TexCoord;
out vec2 OTexCoord;

uniform mat4 Projection;
uniform mat4 ModelView;

void main()
{
  gl_Position = Projection * ModelView * vec4( VertexCoord, 0, 1 );
  OTexCoord = TexCoord;
}
