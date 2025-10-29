// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

// ----------------------------------------------------------------------------
// vkClient generic scene rendering technique
// ----------------------------------------------------------------------------

uniform extern float4x4  gWVP;			    // Combined World, View and Projection matrix
uniform extern float4x4  gVP;			    // Combined World, View and Projection matrix
uniform extern float4    gColor;		    // Line Color
uniform extern texture   gTex0;			    // Diffuse texture

sampler Tex0S : register (s0) = sampler_state
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
	float4 posH    : POSITION0;
};

// ----------------------------------------------------------------------------
// Line Tech Vertex/Pixel shader implementation
// ----------------------------------------------------------------------------

LineOutputVS LineTechVS(float3 posL : POSITION0)
{
	// Zero output.
	LineOutputVS outVS = (LineOutputVS)0;
	outVS.posH = mul(float4(posL, 1.0f),gWVP);
	return outVS;
}

float4 LineTechPS() : COLOR
{
	return gColor;
}

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
	float4 posH    : POSITION0;
	float4 col     : COLOR0;
};

StarOutputVS StarTechVS(float3 posL : POSITION0, float4 col : COLOR0)
{
	// Zero output.
	StarOutputVS outVS = (StarOutputVS)0;
	outVS.posH = mul(float4(posL, 1.0f), gWVP);
	outVS.col  = col;
	return outVS;
}

float4 StarTechPS(float4 col : COLOR0) : COLOR
{
	return col;
}

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
	float4 posH    : POSITION0;
	float2 tex0	   : TEXCOORD0;
};

LabelVS LabelTechVS(float3 posL : POSITION0, float2 tex0 : TEXCOORD0)
{
	// Zero output.
	LabelVS outVS = (LabelVS)0;
	outVS.posH = mul(float4(posL, 1.0f), gWVP);
	outVS.tex0 = tex0;
	return outVS;
}

float4 LabelTechPS(LabelVS frg) : COLOR
{
	float4 col;
	col = tex2D(Tex0S, frg.tex0);
	col.rgb = gColor.rgb;
	return col;
}


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

