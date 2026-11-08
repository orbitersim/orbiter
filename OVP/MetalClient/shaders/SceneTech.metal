// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of SceneTech.fx
// ==============================================================

#include <metal_stdlib>
using namespace metal;

struct Globals
{
	float4x4  gWVP;
	float4x4  gVP;
	float4    gColor;
};

constexpr sampler Tex0S(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(8));


struct LineInputVS
{
	float3 posL    [[attribute(0)]];
};

struct LineOutputVS
{
	float4 posH    [[position]];
};

// ----------------------------------------------------------------------------
// Line Tech Vertex/Pixel shader implementation
// ----------------------------------------------------------------------------

vertex LineOutputVS LineTechVS(LineInputVS vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	LineOutputVS outVS = {};
	outVS.posH = g.gWVP * float4(vrt.posL, 1.0f);
	return outVS;
}

fragment float4 LineTechPS(constant Globals &g [[buffer(0)]])
{
	return g.gColor;
}

// @technique LineTech
// @pass P0 vs=LineTechVS ps=LineTechPS ZEnable=false AlphaBlendEnable=false



// ----------------------------------------------------------------------------
// Star rendering technique
// ----------------------------------------------------------------------------

struct StarInputVS
{
	float3 posL    [[attribute(0)]];
	float4 col     [[attribute(12)]];
};

struct StarOutputVS
{
	float4 posH    [[position]];
	float4 col;
	float  psize   [[point_size]];
};

vertex StarOutputVS StarTechVS(StarInputVS vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	StarOutputVS outVS = {};
	outVS.posH = g.gWVP * float4(vrt.posL, 1.0f);
	outVS.col  = vrt.col;
	outVS.psize = 1.0f;
	return outVS;
}

fragment float4 StarTechPS(StarOutputVS frg [[stage_in]], constant Globals &g [[buffer(0)]])
{
	return frg.col;
}

// @technique StarTech
// @pass P0 vs=StarTechVS ps=StarTechPS ZEnable=false AlphaBlendEnable=false ZWriteEnable=false


struct LabelInputVS
{
	float3 posL    [[attribute(0)]];
	float2 tex0    [[attribute(4)]];
};

struct LabelVS
{
	float4 posH    [[position]];
	float2 tex0;
};

vertex LabelVS LabelTechVS(LabelInputVS vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	LabelVS outVS = {};
	outVS.posH = g.gWVP * float4(vrt.posL, 1.0f);
	outVS.tex0 = vrt.tex0;
	return outVS;
}

fragment float4 LabelTechPS(LabelVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 col;
	col = gTex0.sample(Tex0S, frg.tex0);
	col.rgb = g.gColor.rgb;
	return col;
}


// @technique LabelTech
// @pass P0 vs=LabelTechVS ps=LabelTechPS ZEnable=false ZWriteEnable=false AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha
