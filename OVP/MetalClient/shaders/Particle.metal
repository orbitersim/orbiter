// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of Particle.fx (included by D3D9Client.metal)
// ==============================================================

struct EPVERTEX {
	float3 posL     [[attribute(0)]];
	float2 tex0     [[attribute(4)]];
};

struct ParticleVS
{
	float4 posH     [[position]];
	float2 tex0;
	float  light;
};

vertex ParticleVS ParticleDiffuseVS(NTVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ParticleVS outVS = {};
	outVS.tex0    = vrt.tex0;
	outVS.light   = 1.0f;
	outVS.posH    = g.gVP * float4(vrt.posL, 1.0f);
	return outVS;
}

vertex ParticleVS ParticleEmissiveVS(EPVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	ParticleVS outVS = {};
	outVS.tex0   = vrt.tex0;
	outVS.posH   = g.gVP * float4(vrt.posL, 1.0f);
	return outVS;
}


fragment float4 ParticleDiffusePS(ParticleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 color = gTex0.sample(WrapS, frg.tex0);
	return float4(color.rgb*frg.light, color.a*g.gMix);
}

fragment float4 ParticleEmissivePS(ParticleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 color = gTex0.sample(WrapS, frg.tex0);
	return float4(color.rgb*g.gColor.rgb, color.a*g.gMix);
}

fragment float4 ParticleShadowPS(ParticleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 color = gTex0.sample(WrapS, frg.tex0);
	return float4(0.0f, 0.0f, 0.0f, color.a*g.gMix*2.0f);
}


// @technique ParticleDiffuseTech
// @pass P0 vs=ParticleDiffuseVS ps=ParticleDiffusePS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false

// @technique ParticleEmissiveTech
// @pass P0 vs=ParticleEmissiveVS ps=ParticleEmissivePS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false
// @pass P1 vs=ParticleEmissiveVS ps=ParticleShadowPS AlphaBlendEnable=true BlendOp=Add ZEnable=false SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false
