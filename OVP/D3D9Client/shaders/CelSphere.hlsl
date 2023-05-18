// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Licensed under LGPL v2
// Copyright (C) 2021 Jarmo Nikkanen
// ==============================================================

#define BOOL bool

struct CelDataStruct
{
	float4x4 mWorld;
	float4x4 mViewProj;
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
	float3 posL     : POSITION0;
	float3 normalL  : NORMAL0;
	float2 tex0     : TEXCOORD0;
	float  elev		: TEXCOORD1;
};

struct CelSphereVS
{
	float4 posH    : POSITION0;
	float2 tex0    : TEXCOORD0;
};

uniform extern CelDataStruct Const;
uniform extern CelDataFlow Flow;

sampler2D tTexA;
sampler2D tTexB;

CelSphereVS CelVS(TILEVERTEX vrt)
{
	// Zero output.
	CelSphereVS outVS = (CelSphereVS)0;
	float3 posW = mul(float4(vrt.posL, 1.0f), Const.mWorld).xyz;
	outVS.posH = mul(float4(posW, 1.0f), Const.mViewProj);
	outVS.tex0 = vrt.tex0;
	return outVS;
}

float4 CelPS(CelSphereVS frg) : COLOR
{
	float3 vColor = 0;
	if (Flow.bAlpha) vColor += tex2D(tTexA, frg.tex0).rgb * Const.fAlpha;
	if (Flow.bBeta)  vColor += tex2D(tTexB, frg.tex0).rgb * Const.fBeta;
	return float4(vColor, 1.0);
}
