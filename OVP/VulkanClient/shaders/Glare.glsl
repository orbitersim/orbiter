
// ===================================================
// Copyright (C) 2022-2026 Jarmo Nikkanen
// licensed under MIT
// ===================================================



// ====================================================================
// GPU based computation of local lights visibility (including the Sun)
// ====================================================================

float ilerp(float a, float b, float x)
{
	return clamp((x - a) / (b - a), 0.0, 1.0);
}

vec3 HDRtoLDR(vec3 hdr)
{
	vec3 h2 = hdr * hdr;
	return hdr * pow(max(vec3(0), 1.0f + h2 * h2), vec3(-0.25));
}

layout(binding = 4) uniform sampler2D tDepth;	// s0

#define LocalKernelSize 57
const float iLKS = 1.0f / LocalKernelSize;

struct cbPSType {
	mat4 mVP;
	mat4 mSVP;
	vec4 vSrc;
	vec3 vDir;
};

// uniform extern: cbPS is read by both stages (one block), cbKernel by VisibilityPS
layout(binding = 0, row_major, scalar) uniform cbPSBlock { cbPSType cbPS; };
layout(binding = 1, row_major, scalar) uniform cbKernelBlock { vec2 cbKernel[LocalKernelSize]; };

struct LData
{
	vec4 posH;	// POSITION0	// For rendering
	vec4 smpH;	// TEXCOORD0	// For sampling screen depth bufer
	vec3 posW;	// TEXCOORD1	// Camera centric location ECL 
	float  cone;	// TEXCOORD3	// Attennuation by light-cone
};

LData VisibilityVS(float posL, vec4 posW)	// posL : POSITION0, posW : TEXCOORD0
{
	LData outVS = LData(vec4(0), vec4(0), vec3(0), 0.0);
	outVS.posH = vec4(posL.x + 0.5f, 0.5f, 0.0f, 1.0f) * cbPS.mVP; // Render projection (y 0.0 → 0.5: Vulkan pixel centres are at .5)
	outVS.smpH = vec4(posW.xyz, 1.0f) * cbPS.mSVP; // Depth sampling projection 
	outVS.posW = posW.xyz;
	outVS.cone = posW.a;
	return outVS;
}

#ifdef VS_VisibilityVS
layout(location = 0) in float iPosL;
layout(location = 5) in vec4 iPosW;
layout(location = 0) out LData oVS;
void main() { oVS = VisibilityVS(iPosL, iPosW); gl_Position = oVS.posH; gl_PointSize = 1.0; } // D3DRS_POINTSIZE default
#endif

// Check sun/light "glare" visibility
//
vec4 VisibilityPS(LData frg)
{
	vec4 smpH = frg.smpH;
	smpH.xyz /= smpH.w;
	vec2 sp = smpH.xy * vec2(0.5f, -0.5f) + vec2(0.5f, 0.5f); // Scale and offset to 0-1 range

	if (sp.x < 0 || sp.y < 0) return vec4(0.0f);				// If a sample is outside border -> obscured
	if (sp.x > 1 || sp.y > 1) return vec4(0.0f);

	vec2 vScale = 40.0f * cbPS.vSrc.zz;				// Kernel scale factor (from unit kernel)
	float fDepth = dot(frg.posW, cbPS.vDir) - 0.25f;	// Depth to compare
	float fRet = 0;

	for (int i = 0; i < LocalKernelSize; i++) {
		vec2 s = sp + cbKernel[i].xy * vScale * 0.4f;
		if (s.x < 0 || s.x > 1) { fRet += iLKS;	continue; }
		if (s.y < 0 || s.y > 1) { fRet += iLKS;	continue; }
		float d = texture(tDepth, s).a;
		fRet += d > 0.1f && d < fDepth ? iLKS : 0;
	}

	return vec4(1.0f - fRet);
}

#ifdef PS_VisibilityPS
layout(location = 0) in LData frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = VisibilityPS(frg); }
#endif



// ====================================================================
// Rendering of glares for the Sun and local lights
// ====================================================================

layout(binding = 5) uniform sampler2D tVis;		// Pre-computed visibility factors
layout(binding = 6) uniform sampler2D tTex0;	// Main Glare
layout(binding = 7) uniform sampler2D tTex1;	// Atmospheric Glare 

struct ConstType {
	mat4	mVP;
	vec4		Pos;
	vec4		Color;
	float		GPUId;
	float		Alpha;
	float		Blend;
};

// uniform extern: Const is read by both stages (one block)
layout(binding = 2, row_major, scalar) uniform ConstBlock { ConstType Const; };


struct OutputVS
{
	vec4 posH;	// POSITION0
	vec3 uvi;	// TEXCOORD0
};


OutputVS GlareVS(vec3 posL, vec2 tex0)	// posL : POSITION0, tex0 : TEXCOORD0
{
	// Zero output.
	OutputVS outVS = OutputVS(vec4(0), vec3(0));

	float visibility = smoothstep(0.5f, 1.0f, textureLod(tVis, vec2(Const.GPUId, 0.5f), 0).r);

	posL.xy *= Const.Pos.zw * (0.01f + visibility);
	posL.xy += Const.Pos.xy;

	outVS.posH = vec4(posL.xy, 0.0f, 1.0f) * Const.mVP; // -0.5f left out: D3D9's half-pixel offset, Vulkan pixel centres are at .5
	outVS.uvi = vec3(tex0.xy, visibility);

	return outVS;
}

#ifdef VS_GlareVS
layout(location = 0) in vec3 iPosL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out OutputVS oVS;
void main() { oVS = GlareVS(iPosL, iTex0); gl_Position = oVS.posH; }
#endif


vec4 GlarePS(OutputVS frg)
{
	float t0 = max(0, texture(tTex0, frg.uvi.xy).r - 0.1f);  // Texture intensity
	//float t1 = max(0, tex2D(tTex1, frg.uvi.xy).r - 0.1f);  // Texture intensity
	//float t = lerp(t1, t0, Const.Blend);
	float a = clamp(1.0f - exp(-frg.uvi.z * Const.Alpha * t0), 0.0, 1.0);
	return vec4(HDRtoLDR(Const.Color.rgb * sqrt(t0 + 1.0f)), a);
}

#ifdef PS_GlarePS
layout(location = 0) in OutputVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = GlarePS(frg); }
#endif








// ====================================================================
// Creation of "glare" textures
// ====================================================================



// ======================================================================
// Render sun "Glare" (seen in space)
//
vec4 CreateSunGlarePS(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u = u * 2.0 - 1.0;	v = v * 2.0 - 1.0;

	float a = atan(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.3f * pow(abs(sin(3.0f * a)), 4.0f);	// abs: ps_3_0 pow takes |x|
	float w = 0.5f + 0.2f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	//float I = pow(max(0, 2.0f * (1 - r / q)), 12.0f);
	//float K = pow(max(0, 2.0f * (1 - r / w)), 12.0f);
	//float I = exp(max(0, 10.0f * (1 - r / q))) - 1.0f;
	//float K = exp(max(0, 10.0f * (1 - r / w))) - 1.0f;

	float L = pow(max(0, (1 - r / q)), 6.0f) * 3.0f;	// Low frequency spikes
	float H = pow(max(0, (1 - r / w)), 6.0f) * 5.0f;	// High frequency spikes
	float C = ilerp(0.03, 0.01, r) * 7.0f;				// Core
	float S = ilerp(1.7f, 0.35f, r);					// Skirt

	C *= C;
	C += S * S * 0.40f;

	return vec4(max(L + C, H + C), 0, 0, 1);
}



// ======================================================================
// Render sun "Glare" (seen in atmosphere)
//
vec4 CreateSunGlareAtmPS(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u = u * 2.0 - 1.0;	v = v * 2.0 - 1.0;

	float a = atan(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 1.0f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.8f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 4;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 8;

	float L = ilerp(0.05, 0.01, r) * 16.0f;
	float T = max(0, max(I + L, K + L)) * 2.0f;

	return vec4(T, 0, 0, 1);
}



// ======================================================================
// Render "Glare" for local light sources
//
vec4 CreateLocalGlarePS(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u = u * 2.0 - 1.0;	v = v * 2.0 - 1.0;

	float a = atan(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.3f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float L = pow(max(0, (1 - r / q)), 6.0f) * 4.0f;
	float H = pow(max(0, (1 - r / w)), 6.0f) * 8.0f;
	float C = ilerp(0.15, 0.10, r) * 4.0f;

	return vec4(max(L + C, H + C), 0, 0, 1);
}




// ======================================================================
// Render regular Sun texture [ NOT IN USE ]
//
vec4 CreateSunTexPS(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u = u * 2.0 - 1.0;	v = v * 2.0 - 1.0;

	float a = atan(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 1.0f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.8f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 1;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 3;

	float L = ilerp(0.08, 0.03, r) * 8.0f;
	float T = max(0, max(I + L, K + L));

	T = clamp(1.0f - exp(-T), 0.0, 1.0);
	return vec4(1, 1, 1, T);
}

#if defined(PS_CreateSunGlarePS) || defined(PS_CreateSunGlareAtmPS) || defined(PS_CreateLocalGlarePS) || defined(PS_CreateSunTexPS)
layout(location = 0) in float iX;	// TEXCOORD0 of IPI.glsl
layout(location = 1) in float iY;	// TEXCOORD1
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_CreateSunGlarePS
void main() { oColor = CreateSunGlarePS(iX, iY); }
#endif
#ifdef PS_CreateSunGlareAtmPS
void main() { oColor = CreateSunGlareAtmPS(iX, iY); }
#endif
#ifdef PS_CreateLocalGlarePS
void main() { oColor = CreateLocalGlarePS(iX, iY); }
#endif
#ifdef PS_CreateSunTexPS
void main() { oColor = CreateSunTexPS(iX, iY); }
#endif
