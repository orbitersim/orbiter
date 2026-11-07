// Metal port of CelSphere.hlsl
#include <metal_stdlib>
using namespace metal;

struct CelDataStruct
{
	float4x4 mWorld;
	float4x4 mViewProj;
	float fAlpha;
	float fBeta;
};

struct CelDataFlow
{
	int bAlpha;
	int bBeta;
};

struct CelVSConst
{
	CelDataStruct Const;
};

struct CelPSConst
{
	CelDataStruct Const;
	CelDataFlow Flow;
};

struct TILEVERTEX
{
	float3 posL [[attribute(0)]];
	float3 normalL [[attribute(2)]];
	float2 tex0 [[attribute(4)]];
	float elev [[attribute(5)]];
};

struct CelSphereVS
{
	float4 posH [[position]];
	float2 tex0;
};

vertex CelSphereVS CelVS(TILEVERTEX vrt [[stage_in]], constant CelVSConst &cb [[buffer(0)]])
{
	CelSphereVS outVS = {};
	float3 posW = (cb.Const.mWorld * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH = cb.Const.mViewProj * float4(posW, 1.0f);
	outVS.tex0 = vrt.tex0;
	return outVS;
}

fragment float4 CelPS(CelSphereVS frg [[stage_in]], constant CelPSConst &cb [[buffer(0)]],
	texture2d<float> tTexA [[texture(0)]], sampler tTexAS [[sampler(0)]],
	texture2d<float> tTexB [[texture(1)]], sampler tTexBS [[sampler(1)]])
{
	float3 vColor = 0;
	if (cb.Flow.bAlpha) vColor += tTexA.sample(tTexAS, frg.tex0).rgb * cb.Const.fAlpha;
	if (cb.Flow.bBeta) vColor += tTexB.sample(tTexBS, frg.tex0).rgb * cb.Const.fBeta;
	return float4(vColor, 1.0);
}
