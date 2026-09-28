// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Licensed under LGPL v2
// Copyright (C) 2021-2026 Jarmo Nikkanen
// ==============================================================

#define BOOL bool

struct CelDataStruct
{
	mat4 mWorld;
	mat4 mViewProj;
	float	 fAlpha;
	float	 fBeta;
};

// Booleans go to sepatate structure. (not mixing different datatypes in a structure)
// Also bool is 32-bits in HLSL therefore must use BOOL in C++ structure
struct CelDataFlow
{
	BOOL	 bAlpha;
	BOOL	 bBeta;
};

struct TILEVERTEX					// (VERTEX_2TEX) Vertex declaration used for surface tiles and cloud layer
{
	vec3 posL;     // POSITION0
	vec3 normalL;  // NORMAL0
	vec2 tex0;     // TEXCOORD0
	float  elev;   // TEXCOORD1
};

struct CelSphereVS
{
	vec4 posH;     // POSITION0
	vec2 tex0;     // TEXCOORD0
};

// uniform extern structs: Const is read by both stages (the client sets it in both tables), Flow by the pixel shader
layout(binding = 0, row_major, scalar) uniform CelConstBlock { CelDataStruct Const; };
layout(binding = 1, row_major, scalar) uniform CelFlowBlock { CelDataFlow Flow; };

layout(binding = 4) uniform sampler2D tTexA;	// s0
layout(binding = 5) uniform sampler2D tTexB;	// s1

CelSphereVS CelVS(TILEVERTEX vrt)
{
	// Zero output.
	CelSphereVS outVS = CelSphereVS(vec4(0), vec2(0));
	vec3 posW = (vec4(vrt.posL, 1.0f) * Const.mWorld).xyz;
	outVS.posH = vec4(posW, 1.0f) * Const.mViewProj;
	outVS.tex0 = vrt.tex0;
	return outVS;
}

#ifdef VS_CelVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out CelSphereVS oVS;
void main() { oVS = CelVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev)); gl_Position = oVS.posH; }
#endif

vec4 CelPS(CelSphereVS frg)
{
	vec3 vColor = vec3(0);
	if (Flow.bAlpha) vColor += texture(tTexA, frg.tex0).rgb * Const.fAlpha;
	if (Flow.bBeta)  vColor += texture(tTexB, frg.tex0).rgb * Const.fBeta;
	return vec4(vColor, 1.0);
}

#ifdef PS_CelPS
layout(location = 0) in CelSphereVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = CelPS(frg); }
#endif
