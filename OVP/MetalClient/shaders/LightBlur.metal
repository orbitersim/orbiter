// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016-2026 Jarmo Nikkanen
//				 2016 SolarLiner (Nathan Graule)
// Metal port of LightBlur.hlsl (ImageProcessing pixel shaders)
// ==============================================================

#include <metal_stdlib>
using namespace metal;

// Shader configurations -----------------------------------------------
//
#define radius			20
#define rate			0.7
#define fMinThreshold	1.1
#define fMaxThreshold	2.5


// Orher configurations ------------------------------------------------
//
#define fSunIntensity		3.14
#define fInvSunIntensity	(1.0/fSunIntensity)

// ---------------------------------------------------------------------
// Client configuration parameters
//
#define BufferDivider	2
#define PassCount		1
#define BufferFormat	1
// ---------------------------------------------------------------------

struct LightBlurPSConst
{
	float   fIntensity;
	float   fDistance;
	float   fThreshold;
	float   fGamma;
	float2  vSB;
	float2  vBB;
	int     bDir;
	int     bBlur;
	int     bBlendIn;
	int     bSample;
};

struct IPIData
{
	float4 posH     [[position]];
	float  x;
	float  y;
};

constant float3 cMult = float3(3.0f, 1.0f, 5.0f);

float Desaturate (float3 color)
{
	return dot(color, float3(0.2f, 0.7f, 0.1f) );
}



float3 HDRtoLDR(float3 hdr)
{
	float3 h2 = hdr*hdr;
	return hdr * pow(max(float3(0.0f), 1.0f + h2*h2), float3(-0.25f));
}


fragment float4 PSMain(IPIData frg [[stage_in]], constant LightBlurPSConst &cb [[buffer(0)]],
	texture2d<float> tBack [[texture(0)]], sampler tBackS [[sampler(0)]],
	texture2d<float> tBlur [[texture(1)]], sampler tBlurS [[sampler(1)]])
{
	float2 vPos = float2(frg.x, frg.y);

	float2 vX = float2(cb.vSB.x, 0.0f);
	float2 vY = float2(0.0f, cb.vSB.y);

	float2 sX = float2(cb.vBB.x, 0.0f);
	float2 sY = float2(0.0f, cb.vBB.y);

	float3 color = 0.0f;


	// Sample a backbuffer into a glow buffer --------------------------------
	//
	if (cb.bSample) {
		float3 res = tBack.sample(tBackS, vPos).rgb;
		float s = Desaturate(res);
		res *= smoothstep(cb.fThreshold, cb.fThreshold*1.5f, s) * 3.0f * rsqrt(1.0f + s*s);
		return float4(abs(res), 1.0f);
	}


	// Construct a glow gradient ---------------------------------------------
	//
	if (cb.bBlur) {

		if (cb.bDir) vX = vY;

		float2 pos = vPos;
		float  f = 1.0f;
		float  d = 1.0f;

		color += tBlur.sample(tBlurS, pos).rgb;

		for (int i = 1; i<radius; i++)
		{
			float2 vXi = i*vX;
			color += f * tBlur.sample(tBlurS, pos - vXi).rgb;
			color += f * tBlur.sample(tBlurS, pos + vXi).rgb;
			d += f * 2.0f;
			f *= cb.fDistance * 0.4f + 0.5f;
		}
		return float4(color/d, 1.0f);
	}


	// Blend the glow buffer back into a backbuffer ---------------------------
	//
	if (cb.bBlendIn) {

		float3 L = tBlur.sample(tBlurS, vPos).rgb * cb.fIntensity;
		float3 B = tBack.sample(tBackS, vPos).rgb;

		float q = Desaturate(L);


		L *= rsqrt(1.0f + q*q);

		color = B + L;

		color = HDRtoLDR(color);

		color = pow(abs(color), float3(cb.fGamma*0.6f + 0.4f));

		return float4(color, 1.0f);
	}

	return float4(0.0f);
}


// --------------------------------------------------------------
// Scale -2 to 3
//
float4 HeightToColor(float a)
{
	if (a < -2)		return float4(0.0f, 0.0f, 0.0f, 1.0f);
	if (a >  3)		return float4(0.0f, 0.0f, 0.0f, 1.0f);
	if (a < -1)		return saturate(float4(0.0f, 0.0f, 2.0f + a, 1.0f));
	else if (a < 0)	return saturate(float4(0.0f, 1.0f + a, 1.0f, 1.0f));
	else if (a < 1)	return saturate(float4(a, 1.0f, 1.0f - a, 1.0f));
	else if (a < 2)	return saturate(float4(1.0f, 2.0f - a, 0.0f, 1.0f));
	return saturate(float4(1.0f, a - 2.0f, a - 2.0f, 1.0f));
}


// Visualize screen depth
//
fragment float4 PSDepth(IPIData frg [[stage_in]], texture2d<float> tBack [[texture(0)]], sampler tBackS [[sampler(0)]])
{
	float z = tBack.sample(tBackS, float2(frg.x, frg.y)).a;
	if (z <= 0) return float4(0.0f, 0.0f, 0.0f, 1.0f);
	float q = 3.0f - log(1.0f + sqrt(max(0.0f, z - 5.0f)));
	return float4(HeightToColor(q).rgb, 1.0f);
}

// Visualize screen space normals
//
fragment float4 PSNormal(IPIData frg [[stage_in]], texture2d<float> tBack [[texture(0)]], sampler tBackS [[sampler(0)]])
{
	float3 q = tBack.sample(tBackS, float2(frg.x, frg.y)).xyz;
	float2 xy = (q.xy + 1.0f) * 0.5f;
	return float4(xy, abs(q.z), 1.0f);
}
