// Metal port of IrradianceInteg.hlsl
#include <metal_stdlib>
using namespace metal;

#define IKernelSize 150

struct PreIntegPSConst
{
	float2 fD;
};

struct IntegPSConst
{
	float4 Kernel[IKernelSize];
	float3 vNr;
	float3 vUp;
	float3 vCp;
	float fIntensity;
	int bUp;
};

struct PostBlurPSConst
{
	float2 fD;
};

struct IPIFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

static float3 Paraboloidal_to_World(float3 i, float3 vCp, float3 vNr, float3 vUp)
{
	i.xy *= 1.1f;
	float d = (1.0f - dot(i.xy, i.xy)) * 0.5f;
	float3 p = normalize(float3(i.xy, d));
	return (vCp * p.x) + (vNr * p.y) + (vUp * p.z * i.z);
}

fragment float4 PSPreInteg(IPIFrag frg [[stage_in]], constant PreIntegPSConst &cb [[buffer(0)]],
	texture2d<float> tSrc [[texture(1)]], sampler tSrcS [[sampler(1)]])
{
	float3 color = 0;
	float2 p = float2(frg.x, frg.y);
	for (int j = 0; j < 8; j++) {
		for (int i = 0; i < 8; i++) {
			color += tSrc.sample(tSrcS, p + (float2(i, j) - 4) * cb.fD).rgb;
		}
	}
	return float4(color * (1.0f / 64.0f), 1);
}

fragment float4 PSInteg(IPIFrag frg [[stage_in]], constant IntegPSConst &cb [[buffer(0)]],
	texturecube<float> tCube [[texture(0)]], sampler tCubeS [[sampler(0)]])
{
	float x = frg.x, y = frg.y;
	float2 qw = float2(x, y) * 2.0f - 1.0f;
	float3 vz = Paraboloidal_to_World(float3(qw.xy, (cb.bUp ? 1 : -1)), cb.vCp, cb.vNr, cb.vUp);

	float3 q = mix(cb.vUp, cb.vCp, fract(x * 21));
	float3 w = mix(q, cb.vNr, fract(y * 17));
	float3 vx = normalize(cross(vz, w));
	float3 vy = normalize(cross(vz, vx));

	float3 sum = 0;

	for (int i = 0; i < IKernelSize; i++) {
		float3 d = (vx * cb.Kernel[i].x) + (vy * cb.Kernel[i].y) + (vz * cb.Kernel[i].z);
		sum += tCube.sample(tCubeS, d).rgb * cb.Kernel[i].w;
	}

	return float4(sqrt(sum * cb.fIntensity * (0.7f / IKernelSize)), 1.0f);
}

fragment float4 PSPostBlur(IPIFrag frg [[stage_in]], constant PostBlurPSConst &cb [[buffer(0)]],
	texture2d<float> tSrc [[texture(1)]], sampler tSrcS [[sampler(1)]])
{
	float3 color = 0;
	float2 p = float2(frg.x, frg.y);
	color += tSrc.sample(tSrcS, p).rgb;
	color += tSrc.sample(tSrcS, p + float2(0, 1) * cb.fD).rgb;
	color += tSrc.sample(tSrcS, p + float2(0, -1) * cb.fD).rgb;
	color += tSrc.sample(tSrcS, p + float2(-1, 0) * cb.fD).rgb;
	color += tSrc.sample(tSrcS, p + float2(-1, 0) * cb.fD).rgb;
	return float4(color * 0.2, 1);
}
