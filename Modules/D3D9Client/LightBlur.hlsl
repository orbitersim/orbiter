// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016 Jarmo Nikkanen
//				 2016 SolarLiner (Nathan Graule)
// ==============================================================

// Shader configurations -----------------------------------------------
//
#define fGlowIntensity	1.05	// Overal glow brightness multiplier
#define radius			8		// Radius of the glow
#define rate			0.95	// glow "linearity" [0.7 to 0.95]
#define fMinThreshold	0.85	// Glow starts to appear when back buffer intensity reaches this level
#define fMaxThreshold	3.00	// Glow reaches it's maximum intensity when backbuffer goes above this level 


// Orher configurations ------------------------------------------------
//
#define fSunIntensity		3.14	// Sunlight intensity
#define fInvSunIntensity	(1.0/fSunIntensity)

// ---------------------------------------------------------------------
// Client configuration parameters
//
#define BufferDivider	3		// Blur buffer size in pixels = ScreenSize / BufferDivider
#define PassCount		2		// Number of "bBlur" passes
#define BufferFormat	1		// Render buffer format, 0=RGB10A2, 1 = RGBA_16F
// ---------------------------------------------------------------------

uniform extern float2  vSB;
uniform extern float2  vBB;
uniform extern bool    bDir;
uniform extern bool    bBlur;
uniform extern bool    bBlendIn;
uniform extern bool    bSample;

uniform extern int     PassId;	// NOTE: CANNOT be used to toggle code section on and off efficiently, any code effected by PassId must be minimized

sampler tBack;
sampler tBlur;
sampler tCLUT;					// 2D D3D9Clut.dds texture
sampler tTone;					// 4x4 mipmap of backbuffer


float Desaturate (float3 color)
{
	return dot( color, float3(0.22, 0.707, 0.071) );
}


float4 PSMain(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float2 vPos = float2(x,y);

	float2 vX = float2(vSB.x, 0);		// Delta between two pixels in a "glow" buffer 
	float2 vY = float2(0, vSB.y);

	float2 sX = float2(vBB.x, 0);		// Delta between two pixels in backbuffer 
	float2 sY = float2(0, vBB.y);

	float3 color = 0;
	

	// Sample a backbuffer into a glow buffer --------------------------------
	//
	if (bSample) {
		float3 res = tex2D(tBack, vPos).rgb;
		res *= smoothstep(fMinThreshold, fMaxThreshold, Desaturate(res));
		return float4(res, 1);
	}


	// Construct a glow gradient ---------------------------------------------
	//
	if (bBlur) {

		if (bDir) vX = vY;

		float2 pos = vPos;
		float  f = 1.0f;

		for (int i = 0; i<radius; i++)
		{
			float2 vXi = i*vX;
			color += f * tex2D(tBlur, pos - vXi).rgb;
			color += f * tex2D(tBlur, pos + vXi).rgb;
			f *= rate;
		}

		color /= 2 * (1 - pow(rate,radius)) / (1 - rate);

		return float4(color * fGlowIntensity, 1);
	}


	// Blend the glow buffer back into a backbuffer ---------------------------
	//
	if (bBlendIn) {

		float3 L = tex2D(tBlur, vPos).rgb;
		float3 B = tex2D(tBack, vPos).rgb;
		
		color = 1 - ((1 - B)*(1 - L)); // Screen add
	
		return float4(color / max(1, max(color.r, max(color.g, color.b))), 1.0);			// Normalize color
	}

	return 0;
}