// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

// ----------------------------------------------------------------------------
// D3D9Client generic scene rendering technique
// ----------------------------------------------------------------------------

layout(binding = 0, row_major, scalar) uniform FxParams	// the uniform extern parameters
{
mat4      gWVP;			    // Combined World, View and Projection matrix
mat4      gVP;			    // Combined World, View and Projection matrix
vec4      gColor;		    // Line Color
};
uniform extern texture   gTex0;			    // Diffuse texture

sampler2D Tex0S = sampler_state	// register (s0) left out: VkEffect picks the binding, the client sets gTex0
{
	Texture = <gTex0>;
	MinFilter = Anisotropic;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = WRAP;
	AddressV = WRAP;
};


struct LineOutputVS
{
	vec4 posH;     // POSITION0
};

// ----------------------------------------------------------------------------
// Line Tech Vertex/Pixel shader implementation
// ----------------------------------------------------------------------------

LineOutputVS LineTechVS(vec3 posL)	// posL : POSITION0
{
	// Zero output.
	LineOutputVS outVS = LineOutputVS(vec4(0));
	outVS.posH = vec4(posL, 1.0f) * gWVP;
	return outVS;
}

#ifdef VS_LineTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 0) out LineOutputVS oVS;
void main() { oVS = LineTechVS(iPosL VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 LineTechPS()
{
	return gColor;
}

#ifdef PS_LineTechPS
layout(location = 0) out vec4 oColor;
void main() { oColor = LineTechPS(); }
#endif

technique LineTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 LineTechVS();
		pixelShader  = compile ps_2_0 LineTechPS();
		ZEnable = false;
		AlphaBlendEnable = false;
	}
}



// ----------------------------------------------------------------------------
// Star rendering technique
// ----------------------------------------------------------------------------

struct StarOutputVS
{
	vec4 posH;     // POSITION0
	vec4 col;      // COLOR0
};

StarOutputVS StarTechVS(vec3 posL, vec4 col)	// posL : POSITION0, col : COLOR0
{
	// Zero output.
	StarOutputVS outVS = StarOutputVS(vec4(0), vec4(0));
	outVS.posH = vec4(posL, 1.0f) * gWVP;
	outVS.col  = col;
	return outVS;
}

#ifdef VS_StarTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 3) in vec4 iCol;
layout(location = 0) out StarOutputVS oVS;
void main() { oVS = StarTechVS(iPosL, iCol VS_ARGS); gl_Position = oVS.posH; gl_PointSize = 1.0; } // D3DRS_POINTSIZE default
#endif

vec4 StarTechPS(vec4 col)	// col : COLOR0
{
	return col;
}

#ifdef PS_StarTechPS
layout(location = 0) in StarOutputVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = StarTechPS(frg.col PS_ARGS); }
#endif

technique StarTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 StarTechVS();
		pixelShader  = compile ps_2_0 StarTechPS();
		ZEnable = false;
		AlphaBlendEnable = false;
		ZWriteEnable = false;
	}
}


struct LabelVS
{
	vec4 posH;     // POSITION0
	vec2 tex0;     // TEXCOORD0
};

LabelVS LabelTechVS(vec3 posL, vec2 tex0)	// posL : POSITION0, tex0 : TEXCOORD0
{
	// Zero output.
	LabelVS outVS = LabelVS(vec4(0), vec2(0));
	outVS.posH = vec4(posL, 1.0f) * gWVP;
	outVS.tex0 = tex0;
	return outVS;
}

#ifdef VS_LabelTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out LabelVS oVS;
void main() { oVS = LabelTechVS(iPosL, iTex0 VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 LabelTechPS(LabelVS frg)
{
	vec4 col;
	col = texture(Tex0S, frg.tex0);
	col.rgb = gColor.rgb;
	return col;
}

#ifdef PS_LabelTechPS
layout(location = 0) in LabelVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = LabelTechPS(frg PS_ARGS); }
#endif


technique LabelTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 LabelTechVS();
		pixelShader = compile ps_3_0 LabelTechPS();
		ZEnable = false;
		ZWriteEnable = false;
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
	}
}
