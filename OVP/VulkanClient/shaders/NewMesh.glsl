// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2022-2026 Jarmo Nikkanen
// ==============================================================

#define BOOL bool

// Constant Buffers --------------------------------------
//
struct VSConst {
	mat4 mVP;	// View Projection Matrix
	mat4 mW;	// World Matrix
};

struct PSConst {
	vec3 Cam_X;
	vec3 Cam_Y;
	vec3 Cam_Z;
};

struct PSBools {
	BOOL bOIT;		// Enable order independent transparency
};

// uniform extern struct X {...} x: the structs in blocks (vertex binding 0, pixel binding 1), set by name as the whole struct
layout(binding = 0, row_major, scalar) uniform VSConstBlock { VSConst vs_const; };
layout(binding = 1, row_major, scalar) uniform PSConstBlock { PSConst ps_const; PSBools ps_bools; };


layout(binding = 4) uniform sampler2D tDiff;	// s0

// Vertex data input layouts -----------------------------
//
struct MESH_VERTEX
{
	vec3 posL;	// POSITION0
	vec3 nrmL;	// NORMAL0
	vec3 tanL;	// TANGENT0
	vec3 tex0;	// TEXCOORD0	// Handiness in .z
};

struct POSTEX
{
	vec3 posL;	// POSITION0
	vec2 tex0;	// TEXCOORD0
};

struct SHADOW_VERTEX
{
	vec4 posL;	// POSITION0
};



// Internal data feeds between VS and PS ------------------
//
struct BShadowVS
{
	vec4 posH;	// POSITION0
	vec2 dstW;	// TEXCOORD0
};

struct ShadowTexVS
{
	vec4 posH;	// POSITION0
	vec4 tex0;	// TEXCOORD0	// distance in .zw
};

struct NormalTexVS
{
	vec4 posH;	// POSITION0
	vec3 posW;	// TEXCOORD0
	vec3 nrmW;	// TEXCOORD1
	vec2 tex0;	// TEXCOORD2
};

struct PBRData
{
	vec4 posH;     // POSITION0
	vec3 camW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
	vec4 tanW;     // TEXCOORD3	 // Handiness in .w
#if SHDMAP > 0
	vec4 shdH;     // TEXCOORD4
#endif
};

struct BasicData
{
	vec4 posH;     // POSITION0
	vec3 camW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
#if SHDMAP > 0
	vec4 shdH;     // TEXCOORD3
#endif
};



// -----------------------------------------------------------------------------------
// Shadow Map rendering with plain geometry (without texture) 
//
BShadowVS ShdMapVS(SHADOW_VERTEX vrt)
{
	// Zero output.
	BShadowVS outVS = BShadowVS(vec4(0), vec2(0));
	vec3 posW = (vec4(vrt.posL.xyz, 1.0f) * vs_const.mW).xyz;
	outVS.posH = vec4(posW, 1.0f) * vs_const.mVP;
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

#ifdef VS_ShdMapVS
layout(location = 0) in vec4 iPosL;
layout(location = 0) out BShadowVS oVS;
void main() { oVS = ShdMapVS(SHADOW_VERTEX(iPosL)); gl_Position = oVS.posH; }
#endif

vec4 ShdMapPS(BShadowVS frg)
{
	return vec4(1 - (frg.dstW.x / frg.dstW.y));
}

#ifdef PS_ShdMapPS
layout(location = 0) in BShadowVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ShdMapPS(frg); }
#endif




// -----------------------------------------------------------------------------------
// Shadow Map rendering with texture alpha included
//
ShadowTexVS ShdMapOIT_VS(POSTEX vrt)
{
	// Zero output.
	ShadowTexVS outVS = ShadowTexVS(vec4(0), vec4(0));
	vec3 posW = (vec4(vrt.posL.xyz, 1.0f) * vs_const.mW).xyz;
	outVS.posH = vec4(posW, 1.0f) * vs_const.mVP;
	outVS.tex0 = vec4(vrt.tex0.xy, outVS.posH.zw);
	return outVS;
}

#ifdef VS_ShdMapOIT_VS
layout(location = 0) in vec3 iPosL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out ShadowTexVS oVS;
void main() { oVS = ShdMapOIT_VS(POSTEX(iPosL, iTex0)); gl_Position = oVS.posH; }
#endif

vec4 ShdMapOIT_PS(ShadowTexVS frg)
{
	if (ps_bools.bOIT) {
		float alpha = texture(tDiff, frg.tex0.xy).a;
		if (alpha < 0.75f) return vec4(1.0f);
	}
	return vec4(1 - (frg.tex0.z / frg.tex0.w));
}

#ifdef PS_ShdMapOIT_PS
layout(location = 0) in ShadowTexVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ShdMapOIT_PS(frg); }
#endif




// -----------------------------------------------------------------------------------
// Render Normal and depth buffer
//
NormalTexVS NormalDepth_VS(MESH_VERTEX vrt)
{
	// Zero output.
	NormalTexVS outVS = NormalTexVS(vec4(0), vec3(0), vec3(0), vec2(0));

	outVS.posW = (vec4(vrt.posL.xyz, 1.0f) * vs_const.mW).xyz;
	outVS.nrmW = (vec4(vrt.nrmL, 0.0f) * vs_const.mW).xyz;
	outVS.posH = vec4(outVS.posW, 1.0f) * vs_const.mVP;
	outVS.tex0 = vrt.tex0.xy;
	return outVS;
}

#ifdef VS_NormalDepth_VS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 2) in vec3 iTanL;
layout(location = 5) in vec3 iTex0;
layout(location = 0) out NormalTexVS oVS;
void main() { oVS = NormalDepth_VS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0)); gl_Position = oVS.posH; }
#endif

#ifdef STAGE_PS
vec4 NormalDepth_PS(NormalTexVS frg)
{
	if (ps_bools.bOIT) {
		if (texture(tDiff, frg.tex0.xy).a < 0.75f) discard; // clip(-1)
	}
	//if (dot(frg.nrmW, ps_const.Cam_Z) > 0) clip(-1);

	float D = length(frg.posW);
	float x = dot(frg.nrmW, ps_const.Cam_X);
	float y = dot(frg.nrmW, ps_const.Cam_Y);
	float z = sqrt(clamp(1.0 - (x * x + y * y), 0.0, 1.0));
	return vec4(x, y, z, D);
}
#endif

#ifdef PS_NormalDepth_PS
layout(location = 0) in NormalTexVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = NormalDepth_PS(frg); }
#endif
