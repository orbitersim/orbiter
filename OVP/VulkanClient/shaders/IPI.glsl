
layout(binding = 0, row_major, scalar) uniform IPIVS	// uniform extern: vertex shader constants (the pixel shader's are binding 1)
{
mat4  mVP;
vec4    vTgtSize;
vec4    vPos;
};

struct OutputVS
{
	vec4 posH;     // POSITION0
	float  x;      // TEXCOORD0
	float  y;      // TEXCOORD1
};


OutputVS VSMain(vec3 posL, vec2 tex0)	// posL : POSITION0, tex0 : TEXCOORD0
{
	// Zero output.
	OutputVS outVS = OutputVS(vec4(0), 0.0, 0.0);

	posL.xy *= vPos.xy;
	posL.xy += vPos.zw;
	posL.xy *= vTgtSize.xy;
	
	outVS.posH = vec4(posL.xy, 0.0f, 1.0f) * mVP; // -0.5f left out: D3D9's half-pixel offset, Vulkan pixel centres are at .5
	outVS.x = tex0.x;
	outVS.y = tex0.y;
	return outVS;
}

#ifdef VS_VSMain
layout(location = 0) in vec3 iPosL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out float oX;	// TEXCOORDn → location n: the pixel shaders read x and y as separate inputs
layout(location = 1) out float oY;
void main() { OutputVS o = VSMain(iPosL, iTex0); gl_Position = o.posH; oX = o.x; oY = o.y; }
#endif




// Example of pixel shader

layout(binding = 4) uniform sampler2D mySmp;

vec4 PSMain(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	return texture(mySmp, vec2(x,y));
}

#ifdef PS_PSMain
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = PSMain(iX, iY); }
#endif
