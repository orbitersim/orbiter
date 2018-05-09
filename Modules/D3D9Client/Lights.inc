

float3 Light_fx(float3 x)
{
	return saturate(x);  //1.5 - exp2(-x.rgb)*1.5f;
}



void LocalLights(
	out float3 diff_out, 
	out float3 spec_out, 
	in float3 nrmW, 
	in float3 posW, 
	in float sp,
	uniform int x,
	uniform bool bSpec)
{

	float3 posWN = normalize(-posW);
	float3 p[4];
	float4 spe;
	int i;

	// Relative positions
	[unroll] for (i = 0; i < 4; i++) p[i] = posW - gLights[i + x].position;

	// Square distances
	float4 sd;
	[unroll] for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	// Normalize
	sd = rsqrt(sd);
	[unroll] for (i = 0; i < 4; i++) p[i] *= sd[i];

	// Distances
	float4 dst = rcp(sd);

	// Attennuation factors
	float4 att;
	[unroll] for (i = 0; i < 4; i++) att[i] = dot(gLights[i + x].attenuation.xyz, float3(1.0, dst[i], dst[i] * dst[i]));

	att = rcp(att);

	// Spotlight factors
	float4 spt;
	[unroll] for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], gLights[i + x].direction) - gLights[i + x].param[Phi]) * gLights[i + x].param[Theta];
		if (gLights[i + x].type == 0) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	// Diffuse light factors
	float4 dif;
	[unroll] for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);
	dif *= (att*spt);

	// Specular lights factors
	if (bSpec) {

		[unroll] for (i = 0; i < 4; i++) spe[i] = dot(reflect(p[i], nrmW), posWN) * (dif[i] > 0);

		spe = pow(saturate(spe), sp);
		spe *= (att*spt);
	}

	diff_out = 0;
	spec_out = 0;

	[unroll] for (i = 0; i < 4; i++) diff_out += gLights[i].diffuse.rgb * dif[i];

	if (bSpec) {
		[unroll] for (i = 0; i < 4; i++) spec_out += gLights[i].diffuse.rgb * spe[i];
	}
}




void LocalLightsEx(out float3 cDiffLocal, out float3 cSpecLocal, in float3 nrmW, in float3 posW, in float sp)
{

#if LMODE !=0
	if (!gLightsEnabled) {
		cDiffLocal = 0;
		cSpecLocal = 0;
	}
#endif

#if LMODE == 1
	LocalLights(cDiffLocal, cSpecLocal, nrmW, posW, sp, 0, false);

#elif LMODE == 2
	LocalLights(cDiffLocal, cSpecLocal, nrmW, posW, sp, 0, true);

#elif LMODE == 3
	float3 dd, ss;
	LocalLights(cDiffLocal, cSpecLocal, nrmW, posW, sp, 0, false);
	LocalLights(dd, ss, nrmW, posW, sp, 4, false);
	cDiffLocal += dd;
	cSpecLocal += ss;

#elif LMODE == 4
	float3 dd, ss;
	LocalLights(cDiffLocal, cSpecLocal, nrmW, posW, sp, 0, true);
	LocalLights(dd, ss, nrmW, posW, sp, 4, true);
	cDiffLocal += dd;
	cSpecLocal += ss;
#else
	cDiffLocal = 0;
	cSpecLocal = 0;
#endif
}