// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under MIT
// ==============================================================

uniform extern float3  fControl[10];
uniform extern int	   iCount;

sampler tMap[10] : register(s0);

float cmax(float3 a)
{
	return max(a.x, max(a.y, a.z));
}

float3 LightFX(float3 c)
{
	float q = cmax(c);
	return c * 1.2f * rsqrt(2 + q * q);
}

// Combine multiple baked lightmaps into a single map
//
float4 PSMain(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 color = 0;
	[unroll] for (int i = 0; i < iCount; i++) color += tex2D(tMap[i], float2(x, y)).rgb * fControl[i];
	return float4(LightFX(color), 1.0f);
}
