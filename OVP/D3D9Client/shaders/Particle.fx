// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

struct EPVERTEX {
	float3 posL     : POSITION0;
	float2 tex0     : TEXCOORD0;
};

struct ParticleVS
{
	float4 posH     : POSITION0;
	float2 tex0     : TEXCOORD0;
	float  light    : TEXCOORD1;
};

ParticleVS ParticleDiffuseVS(NTVERTEX vrt)
{
	ParticleVS outVS = (ParticleVS)0;
	outVS.tex0    = vrt.tex0;
	outVS.light   = 1.0f; // saturate(dot(-gSun.Dir, vrt.nrmL) * 2.0f);
	outVS.posH    = mul(float4(vrt.posL, 1.0f), gVP);
	return outVS;
}

ParticleVS ParticleEmissiveVS(EPVERTEX vrt)
{
	ParticleVS outVS = (ParticleVS)0;
	outVS.tex0   = vrt.tex0;
	outVS.posH   = mul(float4(vrt.posL, 1.0f), gVP);
	return outVS;
}



// ----------------------------------------------------------------------------
// gMix is the particle opacity computed from time and halflife
// gColor is hardcoded to [1,1,1] in exhaust streams and [1, 0.7, 0.5] in reentry streams
// frg.light is a sun light intensity level illuminating a particles. Light color is [1,1,1]
// ----------------------------------------------------------------------------


float4 ParticleDiffusePS(ParticleVS frg) : COLOR
{
	float4 color = tex2D(WrapS, frg.tex0);
	return float4(color.rgb*frg.light, color.a*gMix);
}

float4 ParticleEmissivePS(ParticleVS frg) : COLOR
{
	float4 color = tex2D(WrapS, frg.tex0);
	return float4(color.rgb*gColor.rgb, color.a*gMix);
}

float4 ParticleShadowPS(ParticleVS frg) : COLOR
{
	float4 color = tex2D(WrapS, frg.tex0);
	return float4(0,0,0,color.a*gMix*2.0);
}



technique ParticleDiffuseTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 ParticleDiffuseVS();
		pixelShader  = compile ps_3_0 ParticleDiffusePS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
	}
}


technique ParticleEmissiveTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 ParticleEmissiveVS();
		pixelShader  = compile ps_3_0 ParticleEmissivePS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
	}

	// ------------------------------------------------------------------------
	// Ground shadows are rendered only for DIFFUSE particles
	// Shadow rendering is defined here because of identical vertex declarations [EPVERTEX]
	// ------------------------------------------------------------------------
	pass P1
	{
		vertexShader = compile vs_3_0 ParticleEmissiveVS();
		pixelShader  = compile ps_3_0 ParticleShadowPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = false;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
	}
}