#version 400

uniform sampler2D Texture;
in vec2 OTexCoord;

uniform vec3 BasisColor;

out vec4 FragColor;

void main()
{
  FragColor = vec4(BasisColor, texture2D( Texture, OTexCoord ).a);
}
