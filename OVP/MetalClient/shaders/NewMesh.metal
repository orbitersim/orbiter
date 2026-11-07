// Metal port of NewMesh.hlsl
#include <metal_stdlib>
using namespace metal;

struct VSConst
{
	float4x4 mVP;
	float4x4 mW;
};

struct PSConst
{
	float3 Cam_X;
	float3 Cam_Y;
	float3 Cam_Z;
};

struct PSBools
{
	int bOIT;
};

struct MeshVSConst
{
	VSConst vs_const;
};

struct OITPSConst
{
	PSBools ps_bools;
};

struct NormalDepthPSConst
{
	PSConst ps_const;
	PSBools ps_bools;
};

struct MESH_VERTEX
{
	float3 posL [[attribute(0)]];
	float3 nrmL [[attribute(2)]];
	float3 tanL [[attribute(14)]];
	float3 tex0 [[attribute(4)]];
};

struct POSTEX
{
	float3 posL [[attribute(0)]];
	float2 tex0 [[attribute(4)]];
};

struct SHADOW_VERTEX
{
	float4 posL [[attribute(0)]];
};

struct BShadowVS
{
	float4 posH [[position]];
	float2 dstW;
};

struct ShadowTexVS
{
	float4 posH [[position]];
	float4 tex0;
};

struct NormalTexVS
{
	float4 posH [[position]];
	float3 posW;
	float3 nrmW;
	float2 tex0;
};

struct PBRData
{
	float4 posH [[position]];
	float3 camW;
	float2 tex0;
	float3 nrmW;
	float4 tanW;
#if SHDMAP > 0
	float4 shdH;
#endif
};

struct BasicData
{
	float4 posH [[position]];
	float3 camW;
	float2 tex0;
	float3 nrmW;
#if SHDMAP > 0
	float4 shdH;
#endif
};

vertex BShadowVS ShdMapVS(SHADOW_VERTEX vrt [[stage_in]], constant MeshVSConst &cb [[buffer(0)]])
{
	BShadowVS outVS = {};
	float3 posW = (cb.vs_const.mW * float4(vrt.posL.xyz, 1.0f)).xyz;
	outVS.posH = cb.vs_const.mVP * float4(posW, 1.0f);
	outVS.dstW = outVS.posH.zw;
	return outVS;
}

fragment float4 ShdMapPS(BShadowVS frg [[stage_in]])
{
	return 1 - (frg.dstW.x / frg.dstW.y);
}

vertex ShadowTexVS ShdMapOIT_VS(POSTEX vrt [[stage_in]], constant MeshVSConst &cb [[buffer(0)]])
{
	ShadowTexVS outVS = {};
	float3 posW = (cb.vs_const.mW * float4(vrt.posL.xyz, 1.0f)).xyz;
	outVS.posH = cb.vs_const.mVP * float4(posW, 1.0f);
	outVS.tex0 = float4(vrt.tex0.xy, outVS.posH.zw);
	return outVS;
}

fragment float4 ShdMapOIT_PS(ShadowTexVS frg [[stage_in]], constant OITPSConst &cb [[buffer(0)]],
	texture2d<float> tDiff [[texture(0)]], sampler tDiffS [[sampler(0)]])
{
	if (cb.ps_bools.bOIT) {
		float alpha = tDiff.sample(tDiffS, frg.tex0.xy).a;
		if (alpha < 0.75f) return 1.0f;
	}
	return 1 - (frg.tex0.z / frg.tex0.w);
}

vertex NormalTexVS NormalDepth_VS(MESH_VERTEX vrt [[stage_in]], constant MeshVSConst &cb [[buffer(0)]])
{
	NormalTexVS outVS = {};
	outVS.posW = (cb.vs_const.mW * float4(vrt.posL.xyz, 1.0f)).xyz;
	outVS.nrmW = (cb.vs_const.mW * float4(vrt.nrmL, 0.0f)).xyz;
	outVS.posH = cb.vs_const.mVP * float4(outVS.posW, 1.0f);
	outVS.tex0 = vrt.tex0.xy;
	return outVS;
}

fragment float4 NormalDepth_PS(NormalTexVS frg [[stage_in]], constant NormalDepthPSConst &cb [[buffer(0)]],
	texture2d<float> tDiff [[texture(0)]], sampler tDiffS [[sampler(0)]])
{
	if (cb.ps_bools.bOIT) {
		if (tDiff.sample(tDiffS, frg.tex0.xy).a < 0.75f) discard_fragment();
	}

	float D = length(frg.posW);
	float x = dot(frg.nrmW, cb.ps_const.Cam_X);
	float y = dot(frg.nrmW, cb.ps_const.Cam_Y);
	float z = sqrt(saturate(1.0 - (x * x + y * y)));
	return float4(x, y, z, D);
}
