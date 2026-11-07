#include <metal_stdlib>
using namespace metal;

struct Globals {
	float4x4 gW;
	float4 gColor;
	int gUseTex;
};

struct VIn { float3 pos [[attribute(0)]]; float2 uv [[attribute(4)]]; };
struct VOut { float4 pos [[position]]; float2 uv; };

constexpr sampler gSmp(filter::nearest, address::clamp_to_edge);

vertex VOut TestVS(VIn v [[stage_in]], constant Globals &g [[buffer(0)]])
{
	VOut o;
	o.pos = g.gW * float4(v.pos, 1.0);
	o.uv = v.uv;
	return o;
}

fragment float4 TestPS(VOut in [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex [[texture(0)]])
{
	return g.gUseTex ? gTex.sample(gSmp, in.uv) : g.gColor;
}

// @technique Solid
// @pass P0 vs=TestVS ps=TestPS ZEnable=false CullMode=None
// @technique Blend
// @pass P0 vs=TestVS ps=TestPS ZEnable=false CullMode=None AlphaBlendEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha
