
uniform extern float3  Kernel[200];
uniform extern float3  vNr; // North
uniform extern float3  vUp; // Up
uniform extern float3  vCp; // Forward (East)
uniform extern float   fD;
uniform extern float   fIntensity;
uniform extern bool    bUp;

sampler tCube;
sampler tSrc;


float3 Paraboloidal_to_World(float3 i)
{
	i.xy *= 1.1f;
	float  d = (1.0f - dot(i.xy, i.xy)) * 0.5f;
	float3 p = normalize(float3(i.xy, d));
	return p = (vCp*p.x) + (vNr*p.y) + (vUp * p.z * i.z);
}


float4 PSPreInteg(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 color = 0;
	float2 p = float2(x, y);
	for (int j = 0; j < 8; j++) {
		for (int i = 0; i < 8; i++) {
			color += tex2D(tSrc, p + (float2(i, j) - 4) * fD).rgb;
		}
	}
	return float4(color * (1.0f/64.0f), 1);
}


float4 PSInteg(float x : TEXCOORD0, float y : TEXCOORD1, float2 sc : VPOS) : COLOR
{
	float2 qw = float2(x, y) * 2.0f - 1.0f;
	float3 vz = Paraboloidal_to_World(float3(qw.xy, (bUp ? 1 : -1)));
	
	float3 q = lerp(vUp, vNr, abs(dot(vUp, vz)));
	float3 w = lerp(vCp, q,   abs(dot(vCp, vz)));

	float3 vx = normalize(cross(vz, w));
	float3 vy = normalize(cross(vz, vx));
	float3 sum = 0;

	for (int i = 0; i < 200; i++) sum += texCUBE(tCube, (vx*Kernel[i].x) + (vy*Kernel[i].y) + (vz*Kernel[i].z) ).rgb * Kernel[i].z;
	
	return float4(sqrt(sum * fIntensity * 0.0045f), 1.0f);
}
