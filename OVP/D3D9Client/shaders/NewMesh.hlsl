// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2022 Jarmo Nikkanen
// ==============================================================

#define BOOL bool

// Constant Buffers --------------------------------------
//
uniform extern struct VSConst {
	float4x4 mVP;	// View Projection Matrix
	float4x4 mW;	// World Matrix
} vs_const;

uniform extern struct PSConst {
	float3 Cam_X;
	float3 Cam_Y;
	float3 Cam_Z;
} ps_const;

uniform extern struct PSBools {
	BOOL bOIT;		// Enable order independent transparency
} ps_bools;


sampler2D tDiff;

// Vertex data input layouts -----------------------------
//
struct MESH_VERTEX
{
	float3 posL	: POSITION0;
	float3 nrmL	: NORMAL0;
	float3 tanL	: TANGENT0;
	float3 tex0	: TEXCOORD0;	// Handiness in .z
};

struct POSTEX
{
	float3 posL	: POSITION0;
	float2 tex0	: TEXCOORD0;
};

struct SHADOW_VERTEX
{
	float4 posL	: POSITION0;
};



// Internal data feeds between VS and PS ------------------
//
struct BShadowVS
{
	float4 posH	 : POSITION0;
	float2 dstW	 : TEXCOORD0;
};

struct ShadowTexVS
{
	float4 posH	: POSITION0;
	float4 tex0	: TEXCOORD0;	// distance in .zw
};

struct NormalTexVS
{
	float4 posH	: POSITION0;
	float3 posW	: TEXCOORD0;
	float3 nrmW : TEXCOORD1;
	float2 tex0	: TEXCOORD2;
};

struct PBRData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
	float4 tanW     : TEXCOORD3;	 // Handiness in .w
#if SHDMAP > 0
	float4 shdH     : TEXCOORD4;
#endif
};

struct BasicData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
#if SHDMAP > 0
	float4 shdH     : TEXCOORD3;
#endif
};



// -----------------------------------------------------------------------------------
// Shadow Map rendering with plain geometry (without texture) 
//
BShadowVS ShdMapVS(SHADOW_VERTEX vrt)
{
	// Zero output.
	BShadowVS outVS = (BShadowVS)0;
	float3 posW = mul(float4(vrt.posL.xyz, 1.0f), vs_const.mW).xyz;
	outVS.posH = mul(float4(posW, 1.0f), vs_const.mVP);
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

float4 ShdMapPS(BShadowVS frg) : COLOR
{
	return 1 - (frg.dstW.x / frg.dstW.y);
}




// -----------------------------------------------------------------------------------
// Shadow Map rendering with texture alpha included
//
ShadowTexVS ShdMapOIT_VS(POSTEX vrt)
{
	// Zero output.
	ShadowTexVS outVS = (ShadowTexVS)0;
	float3 posW = mul(float4(vrt.posL.xyz, 1.0f), vs_const.mW).xyz;
	outVS.posH = mul(float4(posW, 1.0f), vs_const.mVP);
	outVS.tex0 = float4(vrt.tex0.xy, outVS.posH.zw);
	return outVS;
}

float4 ShdMapOIT_PS(ShadowTexVS frg) : COLOR
{
	if (ps_bools.bOIT) {
		float alpha = tex2D(tDiff, frg.tex0.xy).a;
		if (alpha < 0.75f) return 1.0f;
	}
	return 1 - (frg.tex0.z / frg.tex0.w);
}




// -----------------------------------------------------------------------------------
// Render Normal and depth buffer
//
NormalTexVS NormalDepth_VS(MESH_VERTEX vrt)
{
	// Zero output.
	NormalTexVS outVS = (NormalTexVS)0;

	outVS.posW = mul(float4(vrt.posL.xyz, 1.0f), vs_const.mW).xyz;
	outVS.nrmW = mul(float4(vrt.nrmL, 0.0f), vs_const.mW).xyz;
	outVS.posH = mul(float4(outVS.posW, 1.0f), vs_const.mVP);
	outVS.tex0 = vrt.tex0.xy;
	return outVS;
}

float4 NormalDepth_PS(NormalTexVS frg) : COLOR
{
	if (ps_bools.bOIT) {
		if (tex2D(tDiff, frg.tex0.xy).a < 0.75f) clip(-1);
	}
	//if (dot(frg.nrmW, ps_const.Cam_Z) > 0) clip(-1);

	float D = length(frg.posW);
	float x = dot(frg.nrmW, ps_const.Cam_X);
	float y = dot(frg.nrmW, ps_const.Cam_Y);
	float z = sqrt(saturate(1.0 - (x * x + y * y)));
	return float4(x, y, z, D);
}
