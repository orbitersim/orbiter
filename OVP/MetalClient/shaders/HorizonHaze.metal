// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of HorizonHaze.fx (included by D3D9Client.metal)
// ==============================================================

vertex HazeVS HazeTechVS(HZVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	HazeVS outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	outVS.tex0  = vrt.tex0;
	outVS.color = vrt.color;
	return outVS;
}


fragment float4 HazeTechPS(HazeVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	return frg.color * gTex0.sample(ClampS, frg.tex0);
}


// @technique HazeTech
// @pass P0 vs=HazeTechVS ps=HazeTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false
