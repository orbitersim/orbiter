
#define IKernelSize 120

uniform extern float4  Kernel[IKernelSize];
uniform extern float3  vNr; // North
uniform extern float3  vUp; // Up
uniform extern float3  vCp; // Forward (East)
uniform extern float2  fD;
uniform extern float   fIntensity;
uniform extern bool    bUp;

sampler tCube;
sampler tSrc;
sampler tRandom;


float3 Paraboloidal_to_World(float3 i)
{
	i.xy *= 1.1f;
	float  d = (1.0f - dot(i.xy, i.xy)) * 0.5f;
	float3 p = normalize(float3(i.xy, d));
	return (vCp*p.x) + (vNr*p.y) + (vUp * p.z * i.z);
}


float4 PSInteg(float x : TEXCOORD0, float y : TEXCOORD1, float2 sc : VPOS) : COLOR
{
	float a = tex2D(tRandom, float2(x*8,y*4)).r * 6.283185307;
	float2 qw = float2(x, y) * 2.0f - 1.0f;
	float3 vz = Paraboloidal_to_World(float3(qw.xy, (bUp ? 1 : -1)));
	float3 qx = normalize(cross(vz, vNr));
	float3 qy = normalize(cross(vz, qx));
	float3 vx = qx * sin(a) + qy * cos(a);
	float3 vy = qx * cos(a) - qy * sin(a);

	float3 sum = 0;

	[unroll] for (int i = 0; i < IKernelSize; i++) {
		float3 d = (vx*Kernel[i].x) + (vy*Kernel[i].y) + (vz*Kernel[i].z);
		float3 k = texCUBElod(tCube, float4(d, 0)).rgb;
		sum += k * Kernel[i].w;
	}

	sum = sum * (2.0f / IKernelSize);
	sum = sum * fIntensity;

	return float4(sum, 1.0f);
}

/*
float4 PSPostBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 color = 0;
	float2 p = float2(x, y);
	color += tex2D(tSrc, p).rgb;
	color += tex2D(tSrc, p + float2(0,  1)*fD).rgb;
	color += tex2D(tSrc, p + float2(0, -1)*fD).rgb;
	color += tex2D(tSrc, p + float2( 1, 0)*fD).rgb;
	color += tex2D(tSrc, p + float2(-1, 0)*fD).rgb;
	color += tex2D(tSrc, p + float2(1,  1)*fD).rgb;
	color += tex2D(tSrc, p + float2(1, -1)*fD).rgb;
	color += tex2D(tSrc, p + float2(-1, 1)*fD).rgb;
	color += tex2D(tSrc, p + float2(-1,-1)*fD).rgb;
	color += tex2D(tSrc, p + float2(0, 2) * fD).rgb;
	color += tex2D(tSrc, p + float2(0, -2) * fD).rgb;
	color += tex2D(tSrc, p + float2( 2, 0) * fD).rgb;
	color += tex2D(tSrc, p + float2(-2, 0) * fD).rgb;
	return float4(color * 0.07, 1);
}*/

float4 PSPostBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 color = 0;
	float2 p = float2(x, y);
	[unroll] for (int k = -4; k < 5; k++) {
		[unroll] for (int i = -4; i < 5; i++) {
			color += tex2D(tSrc, p + float2(i+0.5, k+0.5) * fD).rgb;		
		}
	}
	return float4(color / 81, 1);
	//return float4(color / 49, 1);
}
