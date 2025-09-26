// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2016 Jarmo Nikkanen
// ==============================================================

uniform extern float4  vColorKey;
sampler tSrc;

#define tol 0.02

float4 PSMain(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 vClr = tex2D(tSrc, float2(x, y)).rgb;
	float3 c = abs(vClr - vColorKey.rgb);
	if (c.r<tol && c.g<tol && c.b<tol) clip(-1);
	return float4(vClr, 1.0f);
}