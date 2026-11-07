// Metal port of EnvMapBlur.hlsl
#include <metal_stdlib>
using namespace metal;

struct BlurPSConst
{
	float3 vDir;
	float3 vUp;
	float3 vCp;
	float fD;
	int bDir;
};

struct IPIFrag
{
	float4 posH [[position]];
	float x;
	float y;
};

constant float coeff[8] = { 0.13298076, 0.125794409, 0.106482669, 0.080656908, 0.054670025, 0.033159046, 0.017996989, 0.00874063 };

fragment float4 PSBlur(IPIFrag frg [[stage_in]], constant BlurPSConst &cb [[buffer(0)]],
	texturecube<float> tCube [[texture(0)]], sampler tCubeS [[sampler(0)]])
{
	float x = frg.x * 2.0f - 1.0f;
	float y = frg.y * 2.0f - 1.0f;

	float3 vD;
	float3 dir = cb.vDir - cb.vUp * y + cb.vCp * x;

	if (cb.bDir) vD = cross(dir, cb.vCp);
	else vD = cross(dir, cb.vUp);

	vD = normalize(vD) * cb.fD;

	float3 color = tCube.sample(tCubeS, dir).rgb;
	float3 vX = 0;
	float f = 0.75f;
	float a = 0.5f;

	for (int i = 1; i < 16; i++) {
		vX += vD;
		color += f * tCube.sample(tCubeS, dir + vX).rgb;
		color += f * tCube.sample(tCubeS, dir - vX).rgb;
		a += f;
		f *= 0.75f;
	}
	color /= (a * 2.0f);
	return float4(color, 1);
}
