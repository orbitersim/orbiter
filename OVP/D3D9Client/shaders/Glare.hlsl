// ===================================================
// Copyright (C) 2022 Jarmo Nikkanen
// licensed under MIT
// ===================================================



// ====================================================================
// GPU based computation of local lights visibility (including the Sun)
// ====================================================================

sampler2D tDepth;

#define LocalKernelSize 40
static const float iLKS = 1.0f / LocalKernelSize;

uniform extern struct {
	float4x4 mVP;
	float4x4 mSVP;
	float4 vTgt;
	float3 vDir;
} cbPS;

uniform extern float2 cbKernel[LocalKernelSize];

struct LData
{
	float4 posH : POSITION0;	// For rendering
	float4 smpH : TEXCOORD0;	// For sampling screen depth bufer
	float3 posW : TEXCOORD1;	// Camera centric location ECL 
	float  cone : TEXCOORD3;	// Attennuation by light-cone
};

LData VisibilityVS(float posL : POSITION0, float4 posW : TEXCOORD0)
{
	LData outVS = (LData)0;
	outVS.posH = mul(float4(posL.x - 0.5f, 0.0f, 0.0f, 1.0f), cbPS.mVP); // Render projection
	outVS.smpH = mul(float4(posW.xyz, 1.0f), cbPS.mSVP); // Depth sampling projection 
	outVS.posW = posW.xyz;
	outVS.cone = posW.a;
	return outVS;
}

float4 VisibilityPS(LData frg) : COLOR
{
	float4 smpH = frg.smpH;
	smpH.xyz /= smpH.w;
	float2 sp = smpH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f); // Scale and offset to 0-1 range

	if (sp.x < 0 || sp.y < 0) return 0.0f;		// If a sample is outside border -> obscured
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	float2 vScale = 30.0f * cbPS.vTgt.zw;		// Kernel scale factor (from unit kernel)
	float fDepth = dot(frg.posW, cbPS.vDir);	// Depth to compare
	float fRet = 0;

	[unroll] for (int i = 0; i < LocalKernelSize; i++) if (tex2D(tDepth, sp + cbKernel[i].xy * vScale).r > fDepth) fRet += iLKS;

	return fRet;
}



// ====================================================================
// Rendering of glares for the Sun and local lights
// ====================================================================

sampler2D tVis;	// Pre-computed visibility factors
sampler2D tTex; 

uniform extern struct {
	float4x4	mVP;
	float4		Pos;
	float4		Color;
	float		GPUId;
	float		Scale;
} Const;
	

struct OutputVS
{
	float4 posH : POSITION0;
	float3 uvi  : TEXCOORD0;
};


OutputVS GlareVS(float3 posL : POSITION0, float2 tex0 : TEXCOORD0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;

	float v_factor = tex2Dlod(tVis, float4(Const.GPUId, 0, 0, 0)).r;

	posL.xy *= Const.Pos.zw * (0.2f + v_factor * 0.8f) * Const.Scale;
	posL.xy += Const.Pos.xy;

	outVS.posH = mul(float4(posL.xy - 0.5f, 0.0f, 1.0f), Const.mVP);
	outVS.uvi = float3(tex0.xy, v_factor);

	return outVS;
}


float4 GlarePS(OutputVS frg) : COLOR
{
	if (frg.uvi.z < 0.02f) discard;
	return float4(Const.Color.rgb, tex2D(tTex, frg.uvi.xy).r * Const.Scale);
}





// ======================================================================
// Render sun texture "Glare"
//
float4 CreateSunGlarePS(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u = u * 2.0 - 1.0;
	v = v * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(sin(3.0f * a), 4.0f);
	float w = 0.5f + 0.3f * pow(sin(30.0f * a), 2.0f) * pow(sin(41.0f * a), 2.0f);

	//float I = pow(max(0, 2.0f * (1 - r / q)), 12.0f);
	//float K = pow(max(0, 2.0f * (1 - r / w)), 12.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 8e4;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 16e4;

	//float I = exp(max(0, 10.0f * (1 - r / q))) - 1.0f;
	//float K = exp(max(0, 10.0f * (1 - r / w))) - 1.0f;

	float L = exp(max(0, 0.35f - r) * 43.0f);

	return float4(max(I + L,K + L), 0, 0, 1);
}



// ======================================================================
// Render "Glare" for local light sources
//
float4 CreateLocalGlarePS(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u = u * 2.0 - 1.0;
	v = v * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(sin(3.0f * a), 4.0f);
	float w = 0.5f + 0.3f * pow(sin(30.0f * a), 2.0f) * pow(sin(41.0f * a), 2.0f);

	//float I = pow(max(0, 2.0f * (1 - r / q)), 12.0f);
	//float K = pow(max(0, 2.0f * (1 - r / w)), 12.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 8e4;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 16e4;

	//float I = exp(max(0, 10.0f * (1 - r / q))) - 1.0f;
	//float K = exp(max(0, 10.0f * (1 - r / w))) - 1.0f;

	float L = exp(max(0, 0.35f - r) * 43.0f);

	return float4(max(I + L,K + L), 0, 0, 1);
}


