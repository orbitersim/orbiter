// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016-2026 Jarmo Nikkanen
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

layout(binding = 1, row_major, scalar) uniform LightBlurPS	// uniform extern: pixel shader constants (IPI.glsl's are binding 0)
{
float   fIntensity;
float   fDistance;
float   fThreshold;
float   fGamma;
vec2    vSB;
vec2    vBB;
bool    bDir;
bool    bBlur;
bool    bBlendIn;
bool    bSample;

int     PassId;	// NOTE: CANNOT be used to toggle code section on and off efficiently, any code effected by PassId must be minimized
};

layout(binding = 4) uniform sampler2D tBack;	// s0
layout(binding = 5) uniform sampler2D tBlur;	// s1
layout(binding = 6) uniform sampler2D tCLUT;					// 2D D3D9Clut.dds texture
layout(binding = 7) uniform sampler2D tTone;					// 4x4 mipmap of backbuffer


const vec3 cMult = vec3( 3.0f, 1.0f, 5.0f );

float Desaturate (vec3 color)
{
	return dot(color, vec3(0.2, 0.7, 0.1) );
}



vec3 HDRtoLDR(vec3 hdr)
{
	vec3 h2 = hdr*hdr;
	return hdr * pow(max(vec3(0), 1.0f + h2*h2), vec3(-0.25));
}


vec4 PSMain(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	vec2 vPos = vec2(x,y);

	vec2 vX = vec2(vSB.x, 0);		// Delta between two pixels in a "glow" buffer
	vec2 vY = vec2(0, vSB.y);

	vec2 sX = vec2(vBB.x, 0);		// Delta between two pixels in backbuffer
	vec2 sY = vec2(0, vBB.y);

	vec3 color = vec3(0);


	// Sample a backbuffer into a glow buffer --------------------------------
	//
	if (bSample) {
		vec3 res = texture(tBack, vPos).rgb;
		//res += tex2D(tBack, vPos + sX).rgb;
		//res += tex2D(tBack, vPos + sY).rgb;
		//res += tex2D(tBack, vPos + sX + sY).rgb;
		//res *= 0.25f;
		float s = Desaturate(res);
		res *= smoothstep(fThreshold, fThreshold*1.5f, s) * 3.0f * inversesqrt(1.0f + s*s);
		return vec4(abs(res), 1);
	}


	// Construct a glow gradient ---------------------------------------------
	//
	if (bBlur) {

		if (bDir) vX = vY;

		vec2 pos = vPos;
		float  f = 1.0f;
		float  d = 1.0f;

		color += texture(tBlur, pos).rgb;

		for (int i = 1; i<radius; i++)
		{
			vec2 vXi = float(i)*vX;
			color += f * texture(tBlur, pos - vXi).rgb;
			color += f * texture(tBlur, pos + vXi).rgb;
			d += f * 2;
			f *= fDistance * 0.4f + 0.5;
		}
		return vec4(color/d, 1);
	}


	// Blend the glow buffer back into a backbuffer ---------------------------
	//
	if (bBlendIn) {

		vec3 L = texture(tBlur, vPos).rgb * fIntensity;
		vec3 B = texture(tBack, vPos).rgb;

		//float w = Desaturate(B);
		float q = Desaturate(L);
		
		
		L *= inversesqrt(1 + q*q);
		//B *= rsqrt(4 + w*w) * 2.24f;

		color = B + L;
		
		//float m = max(1, max(color.r, max(color.g, color.b)));
		//float k = max(0, m - 1);
		//color = 1 - ((1 - B)*(1 - L)); // Screen add
		//color = color / m; // lerp(color / m, float3(1, 1, 1), k * rsqrt(1 + k*k));
		
		color = HDRtoLDR(color);

		color = pow(abs(color), vec3(fGamma*0.6f + 0.4f));

		return vec4(color, 1.0);
	}

	return vec4(0);
}


// --------------------------------------------------------------
// Scale -2 to 3
//
vec4 HeightToColor(float a)
{
	if (a < -2)		return vec4(0, 0, 0, 1);
	if (a >  3)		return vec4(0, 0, 0, 1);
	if (a < -1)		return clamp(vec4(0, 0, 2 + a, 1.0), 0.0, 1.0);
	else if (a < 0)	return clamp(vec4(0, 1 + a, 1, 1.0), 0.0, 1.0);
	else if (a < 1)	return clamp(vec4(a, 1, 1 - a, 1.0), 0.0, 1.0);
	else if (a < 2)	return clamp(vec4(1, 2 - a, 0, 1.0), 0.0, 1.0);
	return clamp(vec4(1, a - 2, a - 2, 1.0), 0.0, 1.0);
}


// Visualize screen depth
//
vec4 PSDepth(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	float z = texture(tBack, vec2(x,y)).a;
	if (z <= 0) return vec4(0, 0, 0, 1);
	float q = 3 - log(1.0f + sqrt(max(0, z - 5.0)));
	return vec4(HeightToColor(q).rgb, 1);
}

// Visualize screen space normals
//
vec4 PSNormal(float tx, float ty)	// tx : TEXCOORD0, ty : TEXCOORD1
{
	vec3 q = texture(tBack, vec2(tx,ty)).xyz;
	vec2 xy = (q.xy + 1.0f) * 0.5f;
	return vec4(xy, abs(q.z), 1.0f);
}

#if defined(PS_PSMain) || defined(PS_PSDepth) || defined(PS_PSNormal)
layout(location = 0) in float iX;	// TEXCOORD0 of IPI.glsl
layout(location = 1) in float iY;	// TEXCOORD1
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_PSMain
void main() { oColor = PSMain(iX, iY); }
#endif
#ifdef PS_PSDepth
void main() { oColor = PSDepth(iX, iY); }
#endif
#ifdef PS_PSNormal
void main() { oColor = PSNormal(iX, iY); }
#endif
