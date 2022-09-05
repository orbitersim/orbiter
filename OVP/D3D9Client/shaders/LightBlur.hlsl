// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016 Jarmo Nikkanen
//				 2016 SolarLiner (Nathan Graule)
// ==============================================================

// Shader configurations -----------------------------------------------
//
//#define fGlowIntensity	0.1		// Overal glow brightness multiplier
#define radius			20		// Radius of the glow
#define rate			0.7     // glow "linearity" [0.7 to 0.95]
#define fMinThreshold	1.1		// Glow starts to appear when back buffer intensity reaches this level
#define fMaxThreshold	2.5		// Glow reaches it's maximum intensity when backbuffer goes above this level


// Orher configurations ------------------------------------------------
//
#define fSunIntensity		3.14	// Sunlight intensity
#define fInvSunIntensity	(1.0/fSunIntensity)

// ---------------------------------------------------------------------
// Client configuration parameters
//
#define BufferDivider	2		// Blur buffer size in pixels = ScreenSize / BufferDivider
#define PassCount		1		// Number of "bBlur" passes
#define BufferFormat	1		// Render buffer format, 2=RGB10A2, 1 = RGBA_16F, 0=DEFAULT (RGBX8)
// ---------------------------------------------------------------------

uniform extern float   fIntensity;
uniform extern float   fDistance;
uniform extern float   fThreshold;
uniform extern float   fGamma;
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


static const float3 cMult = { 3.0f, 1.0f, 5.0f };

float Desaturate (float3 color)
{
	return dot(color, float3(0.2, 0.7, 0.1) );
}



float3 HDRtoLDR(float3 hdr)
{
	float3 h2 = hdr*hdr;
	return hdr * pow(max(0, 1.0f + h2*h2), -0.25);
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
		//res += tex2D(tBack, vPos + sX).rgb;
		//res += tex2D(tBack, vPos + sY).rgb;
		//res += tex2D(tBack, vPos + sX + sY).rgb;
		//res *= 0.25f;
		float s = Desaturate(res);
		res *= smoothstep(fThreshold, fThreshold*1.5f, s) * 3.0f * rsqrt(1.0f + s*s);
		return float4(abs(res), 1);
	}


	// Construct a glow gradient ---------------------------------------------
	//
	if (bBlur) {

		if (bDir) vX = vY;

		float2 pos = vPos;
		float  f = 1.0f;
		float  d = 1.0f;

		color += tex2D(tBlur, pos).rgb;

		for (int i = 1; i<radius; i++)
		{
			float2 vXi = i*vX;
			color += f * tex2D(tBlur, pos - vXi).rgb;
			color += f * tex2D(tBlur, pos + vXi).rgb;
			d += f * 2;
			f *= fDistance * 0.4f + 0.5;
		}
		return float4(color/d, 1);
	}


	// Blend the glow buffer back into a backbuffer ---------------------------
	//
	if (bBlendIn) {

		float3 L = tex2D(tBlur, vPos).rgb * fIntensity;
		float3 B = tex2D(tBack, vPos).rgb;

		//float w = Desaturate(B);
		float q = Desaturate(L);
		
		
		L *= rsqrt(1 + q*q);
		//B *= rsqrt(4 + w*w) * 2.24f;

		color = B + L;
		
		//float m = max(1, max(color.r, max(color.g, color.b)));
		//float k = max(0, m - 1);
		//color = 1 - ((1 - B)*(1 - L)); // Screen add
		//color = color / m; // lerp(color / m, float3(1, 1, 1), k * rsqrt(1 + k*k));
		
		color = HDRtoLDR(color);

		color = pow(abs(color), fGamma*0.6f + 0.4f);

		return float4(color, 1.0);
	}

	return 0;
}