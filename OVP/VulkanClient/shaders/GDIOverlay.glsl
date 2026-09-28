
// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016-2026 Jarmo Nikkanen
// ==============================================================

layout(binding = 1, row_major, scalar) uniform GDIOverlayPS	// uniform extern: pixel shader constants (IPI.glsl's are binding 0)
{
vec4  vColorKey;
};
layout(binding = 4) uniform sampler2D tSrc;	// s0

#define tol 0.02

#ifdef STAGE_PS
vec4 PSMain(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	vec3 vClr = texture(tSrc, vec2(x, y)).rgb;
	vec3 c = abs(vClr - vColorKey.rgb);
	if (c.r<tol && c.g<tol && c.b<tol) discard;	// clip(-1)
	return vec4(vClr, 1.0f);
}
#endif

#ifdef PS_PSMain
layout(location = 0) in float iX;	// TEXCOORD0 of IPI.glsl
layout(location = 1) in float iY;	// TEXCOORD1
layout(location = 0) out vec4 oColor;
void main() { oColor = PSMain(iX, iY); }
#endif
