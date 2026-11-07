// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of BeaconArray.fx (included by D3D9Client.metal)
// ==============================================================

struct BAVERTEX {
	float3 posL     [[attribute(0)]];
	float3 dirL     [[attribute(2)]];
	float4 data     [[attribute(4)]];
	float2 dataB    [[attribute(5)]];
	float4 colr     [[attribute(12)]];
};


struct BeaconVS
{
	float4 posH     [[position]];
	float4 colr;
	float2 tex0;
	float  size     [[point_size]];
	float  haze;
};

struct BeaconPS
{
	float4 posH     [[position]];
	float4 colr;
	float  haze;
};


vertex BeaconVS BeaconArrayVS(BAVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	BeaconVS outVS = {};

	float3 posW   = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	float3 dirW   = (g.gW * float4(vrt.dirL, 0.0f)).xyz;
	outVS.posH    = g.gVP * float4(posW, 1.0f);

	float fog     =  exp(-abs(outVS.posH.z * g.gFogDensity * 20.0f));

	float dist    = length(posW);

	float dota    = -dot(normalize(posW), dirW);
	float angl    = saturate((dota - vrt.data[1])/(1.0f-vrt.data[1]));
	float disp    = pow(angl, vrt.dataB[1]);

	float att0    = vrt.dataB[0] / (1.0f+dist*2e-4f);
	float att1    = saturate(pow(att0, 2.0f));

	if (g.gTime<vrt.data[2] || g.gTime>vrt.data[3]) disp = 0.0f;

	outVS.colr    = float4(vrt.colr.rgb, min(1.0f, vrt.colr.a * disp * att1 * 1.2f));
	outVS.size    = (5.5f + vrt.data[0] * g.gPointScale / dist) * disp;
	outVS.haze    = clamp(fog*1.8f, 0.3f, 1.8f);

	return outVS;
}

fragment float4 BeaconArrayPS(BeaconPS frg [[stage_in]], float2 tex0 [[point_coord]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 cTex = gTex0.sample(ClampS, tex0);
	return float4(frg.colr.rgb, pow(abs(cTex.a), frg.haze) * frg.colr.a);
}


// @technique BeaconArrayTech
// @pass P0 vs=BeaconArrayVS ps=BeaconArrayPS PointSpriteEnable=true AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false
