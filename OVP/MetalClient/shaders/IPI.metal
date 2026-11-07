// Metal port of IPI.hlsl
#include <metal_stdlib>
using namespace metal;

struct IPIVSConst
{
	float4x4 mVP;
	float4 vTgtSize;
	float4 vPos;
};

struct IPIVertex
{
	float3 posL [[attribute(0)]];
	float2 tex0 [[attribute(4)]];
};

struct OutputVS
{
	float4 posH [[position]];
	float x;
	float y;
};

vertex OutputVS VSMain(IPIVertex vin [[stage_in]], constant IPIVSConst &cb [[buffer(0)]])
{
	OutputVS outVS = {};
	float3 posL = vin.posL;
	posL.xy *= cb.vPos.xy;
	posL.xy += cb.vPos.zw;
	posL.xy *= cb.vTgtSize.xy;
	outVS.posH = cb.mVP * float4(posL.xy - 0.5f, 0.0f, 1.0f);
	outVS.x = vin.tex0.x;
	outVS.y = vin.tex0.y;
	return outVS;
}

fragment float4 PSMain(OutputVS frg [[stage_in]],
	texture2d<float> mySmp [[texture(0)]], sampler mySmpS [[sampler(0)]])
{
	return mySmp.sample(mySmpS, float2(frg.x, frg.y));
}
