
uniform extern float3  vDir;
uniform extern float3  vUp;
uniform extern float3  vCp;
uniform extern float   fD;
uniform extern bool    bDir;

sampler tCube;
sampler tSrc;

float4 PSBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	x = x * 2.0f - 1.0f;
	y = y * 2.0f - 1.0f;

	float3 vD;

	if (bDir) vD = vUp * fD;
	else	  vD = vCp * fD;

	float3 dir = vDir - vUp*y + vCp*x;

	dir -= vD * 5;

	float3 color = 0;
	//---------------------------------------------------
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;
	color += texCUBE(tCube, dir).rgb; dir += vD;

	color *= 0.091f;

	return float4(color, 1);
}