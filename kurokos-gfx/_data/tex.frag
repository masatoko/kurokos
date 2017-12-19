#version 400

uniform sampler2D Texture;
in vec2 TexCoord;

out vec4 FragColor;

void main()
{
  FragColor = texture2D( Texture, TexCoord );
}
