// Metal port of Glare.hlsl
#include <metal_stdlib>
using namespace metal;

static float ilerp(float a, float b, float x)
{
	return saturate((x - a) / (b - a));
}

static float3 HDRtoLDR(float3 hdr)
{
	float3 h2 = hdr * hdr;
	return hdr * pow(max(0.0f, 1.0f + h2 * h2), -0.25f);
}

#define LocalKernelSize 57
constant float iLKS = 1.0f / LocalKernelSize;

struct GlareCBPS
{
	float4x4 mVP;
	float4x4 mSVP;
	float4 vSrc;
	float3 vDir;
};

struct VisibilityVSConst
{
	GlareCBPS cbPS;
};

struct VisibilityPSConst
{
	GlareCBPS cbPS;
	float2 cbKernel[LocalKernelSize];
};

struct LDataIn
{
	float posL [[attribute(0)]];
	float4 posW [[attribute(4)]];
};

struct LData
{
	float4 posH [[position]];
	float4 smpH;
	float3 posW;
	float cone;
	float psize [[point_size]];
};

vertex LData VisibilityVS(LDataIn vin [[stage_in]], constant VisibilityVSConst &cb [[buffer(0)]])
{
	LData outVS = {};
	outVS.posH = cb.cbPS.mVP * float4(vin.posL + 0.5f, 0.0f, 0.0f, 1.0f);
	outVS.smpH = cb.cbPS.mSVP * float4(vin.posW.xyz, 1.0f);
	outVS.posW = vin.posW.xyz;
	outVS.cone = vin.posW.a;
	outVS.psize = 1.0f;
	return outVS;
}

fragment float4 VisibilityPS(LData frg [[stage_in]], constant VisibilityPSConst &cb [[buffer(0)]],
	texture2d<float> tDepth [[texture(1)]], sampler tDepthS [[sampler(1)]])
{
	float4 smpH = frg.smpH;
	smpH.xyz /= smpH.w;
	float2 sp = smpH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f);

	if (sp.x < 0 || sp.y < 0) return 0.0f;
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	float2 vScale = 40.0f * cb.cbPS.vSrc.zz;
	float fDepth = dot(frg.posW, cb.cbPS.vDir) - 0.25f;
	float fRet = 0;

	for (int i = 0; i < LocalKernelSize; i++) {
		float2 s = sp + cb.cbKernel[i].xy * vScale * 0.4f;
		if (s.x < 0 || s.x > 1) { fRet += iLKS; continue; }
		if (s.y < 0 || s.y > 1) { fRet += iLKS; continue; }
		float d = tDepth.sample(tDepthS, s).a;
		fRet += d > 0.1f && d < fDepth ? iLKS : 0;
	}

	return 1.0f - fRet;
}

struct GlareConst
{
	float4x4 mVP;
	float4 Pos;
	float4 Color;
	float GPUId;
	float Alpha;
	float Blend;
};

struct GlareVSConst
{
	GlareConst Const;
};

struct GlarePSConst
{
	GlareConst Const;
};

struct GlareVertex
{
	float3 posL [[attribute(0)]];
	float2 tex0 [[attribute(4)]];
};

struct GlareOut
{
	float4 posH [[position]];
	float3 uvi;
};

vertex GlareOut GlareVS(GlareVertex vin [[stage_in]], constant GlareVSConst &cb [[buffer(0)]],
	texture2d<float> tVis [[texture(0)]], sampler tVisS [[sampler(0)]])
{
	GlareOut outVS = {};
	float visibility = smoothstep(0.5f, 1.0f, tVis.sample(tVisS, float2(cb.Const.GPUId, 0.5f), level(0)).r);

	float3 posL = vin.posL;
	posL.xy *= cb.Const.Pos.zw * (0.01f + visibility);
	posL.xy += cb.Const.Pos.xy;

	outVS.posH = cb.Const.mVP * float4(posL.xy - 0.5f, 0.0f, 1.0f);
	outVS.uvi = float3(vin.tex0.xy, visibility);
	return outVS;
}

fragment float4 GlarePS(GlareOut frg [[stage_in]], constant GlarePSConst &cb [[buffer(0)]],
	texture2d<float> tTex0 [[texture(2)]], sampler tTex0S [[sampler(2)]])
{
	float t0 = max(0.0f, tTex0.sample(tTex0S, frg.uvi.xy).r - 0.1f);
	float a = saturate(1.0f - exp(-frg.uvi.z * cb.Const.Alpha * t0));
	return float4(HDRtoLDR(cb.Const.Color.rgb * sqrt(t0 + 1.0f)), a);
}

struct IPIFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

fragment float4 CreateSunGlarePS(IPIFrag frg [[stage_in]])
{
	float u = frg.x * 2.0 - 1.0;
	float v = frg.y * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.3f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.2f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float L = pow(max(0.0f, (1 - r / q)), 6.0f) * 3.0f;
	float H = pow(max(0.0f, (1 - r / w)), 6.0f) * 5.0f;
	float C = ilerp(0.03, 0.01, r) * 7.0f;
	float S = ilerp(1.7f, 0.35f, r);

	C *= C;
	C += S * S * 0.40f;

	return float4(max(L + C, H + C), 0, 0, 1);
}

fragment float4 CreateSunGlareAtmPS(IPIFrag frg [[stage_in]])
{
	float u = frg.x * 2.0 - 1.0;
	float v = frg.y * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 1.0f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.8f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float I = pow(max(0.0f, (1 - r / q)), 6.0f) * 4;
	float K = pow(max(0.0f, (1 - r / w)), 6.0f) * 8;

	float L = ilerp(0.05, 0.01, r) * 16.0f;
	float T = max(0.0f, max(I + L, K + L)) * 2.0f;

	return float4(T, 0, 0, 1);
}

fragment float4 CreateLocalGlarePS(IPIFrag frg [[stage_in]])
{
	float u = frg.x * 2.0 - 1.0;
	float v = frg.y * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.3f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float L = pow(max(0.0f, (1 - r / q)), 6.0f) * 4.0f;
	float H = pow(max(0.0f, (1 - r / w)), 6.0f) * 8.0f;
	float C = ilerp(0.15, 0.10, r) * 4.0f;

	return float4(max(L + C, H + C), 0, 0, 1);
}

fragment float4 CreateSunTexPS(IPIFrag frg [[stage_in]])
{
	float u = frg.x * 2.0 - 1.0;
	float v = frg.y * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 1.0f * pow(abs(sin(3.0f * a)), 4.0f);
	float w = 0.5f + 0.8f * pow(abs(sin(30.0f * a)), 2.0f) * pow(abs(sin(41.0f * a)), 2.0f);

	float I = pow(max(0.0f, (1 - r / q)), 6.0f) * 1;
	float K = pow(max(0.0f, (1 - r / w)), 6.0f) * 3;

	float L = ilerp(0.08, 0.03, r) * 8.0f;
	float T = max(0.0f, max(I + L, K + L));

	T = saturate(1.0f - exp(-T));
	return float4(1, 1, 1, T);
}
