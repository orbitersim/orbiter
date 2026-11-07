// Metal port of GDIOverlay.hlsl
#include <metal_stdlib>
using namespace metal;

#define tol 0.02

struct GDIPSConst
{
	float4 vColorKey;
};

struct IPIFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

fragment float4 PSMain(IPIFrag frg [[stage_in]], constant GDIPSConst &cb [[buffer(0)]],
	texture2d<float> tSrc [[texture(0)]], sampler tSrcS [[sampler(0)]])
{
	float3 vClr = tSrc.sample(tSrcS, float2(frg.x, frg.y)).rgb;
	float3 c = abs(vClr - cb.vColorKey.rgb);
	if (c.r < tol && c.g < tol && c.b < tol) discard_fragment();
	return float4(vClr, 1.0f);
}
