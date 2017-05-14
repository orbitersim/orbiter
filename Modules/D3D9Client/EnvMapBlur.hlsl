
uniform extern float3  vDir;
uniform extern float3  vUp;
uniform extern float3  vCp;
uniform extern float   fD;
uniform extern bool    bDir;

sampler tCube;
sampler tSrc;

static float coeff[8] = { 0.13298076, 0.125794409, 0.106482669, 0.080656908, 0.054670025, 0.033159046, 0.017996989, 0.00874063 };

float4 PSBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	x = x * 2.0f - 1.0f;
	y = y * 2.0f - 1.0f;

	float3 vD;

	float3 dir = vDir - vUp*y + vCp*x;

	if (bDir) vD = cross(dir, vCp);
	else	  vD = cross(dir, vUp);

	vD = normalize(vD) * fD;

	float3 color = texCUBE(tCube, dir).rgb * coeff[0];
	float3 vX = 0;

	for (int i = 1; i < 8; i++) {
		vX += vD;
		color += texCUBE(tCube, dir + vX).rgb * coeff[i];
		color += texCUBE(tCube, dir - vX).rgb * coeff[i];
	};

	return float4(color, 1);
}