// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Metal port of Common.hlsl (included by Vessel.metal)
// ==============================================================

#define KERNEL_RADIUS 2.0f
#define SHADOW_THRESHOLD 0.1f



// ============================================================================
//
float4 Paraboloidal_LVLH(constant Globals &g, texture2d<float> t, sampler s, float3 i)
{
	float z = dot(g.gCameraPos, i);
	float2 p = float2(dot(g.gEast, i), dot(g.gNorth, i)) / (1.0f + abs(z));
	p *= float2(0.2273f, 0.4545f);
	float4 A = t.sample(s, p + float2(0.25f, 0.5f));
	float4 B = t.sample(s, p + float2(0.75f, 0.5f));
	return mix(A, B, smoothstep(-0.03f, 0.03f, z));
}

float3 Sq(float3 x)
{
	return x * x;
}

float4 Sq(float4 x)
{
	return x * x;
}


// ==========================================================================================================
// Local light sources
// ==========================================================================================================


float3 Light_fx(float3 x)
{
	return saturate(x);
}

void LocalLights(
	constant Globals &g,
	thread float3 &diff_out,
	thread float3 &spec_out,
	float3 nrmW,
	float3 posW,
	float sp,
	int x,
	bool bSpec)
{

	float3 posWN = normalize(-posW);
	float3 p[4];
	float4 spe;
	int i;

	for (i = 0; i < 4; i++) p[i] = posW - g.gLights[i + x].position;

	float4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	sd = rsqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	float4 dst = 1.0f / sd;

	float4 att;
	for (i = 0; i < 4; i++) att[i] = dot(g.gLights[i + x].attenuation.xyz, float3(1.0f, dst[i], dst[i] * dst[i]));

	att = 1.0f / att;

	float4 spt;
	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], g.gLights[i + x].direction) - g.gLights[i + x].param[Phi]) * g.gLights[i + x].param[Theta];
		if (g.gLights[i + x].type == 0) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	float4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);
	dif *= (att * spt);

	if (bSpec) {

		for (i = 0; i < 4; i++) spe[i] = dot(reflect(p[i], nrmW), posWN) * float(dif[i] > 0);

		spe = pow(saturate(spe), float4(sp));
		spe *= (att * spt);
	}

	diff_out = 0.0f;
	spec_out = 0.0f;

	for (i = 0; i < 4; i++) diff_out += g.gLights[i + x].diffuse.rgb * dif[i];

	if (bSpec) {
		for (i = 0; i < 4; i++) spec_out += g.gLights[i + x].diffuse.rgb * spe[i];
	}
}


void LocalLightsBeckman(
	constant Globals &g,
	thread float3 &diff_out,
	thread float3 &spec_out,
	float3 nrmW,
	float3 posW,
	float fRgh,
	int x,
	bool bSpec)
{

	float3 camW = normalize(-posW);
	float3 p[4];
	float3 H[4];
	float4 spe;
	float4 dHN;
	int i;

	for (i = 0; i < 4; i++) p[i] = posW - g.gLights[i + x].position;

	float4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	sd = rsqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	float4 dst = 1.0f / sd;

	if (bSpec) {

		float4 hd;
		for (i = 0; i < 4; i++) H[i] = (camW - p[i]);
		for (i = 0; i < 4; i++) hd[i] = dot(H[i], H[i]);

		hd = rsqrt(hd);

		for (i = 0; i < 4; i++) H[i] *= hd[i];
		for (i = 0; i < 4; i++) dHN[i] = dot(H[i], nrmW);
	}



	float4 att;
	for (i = 0; i < 4; i++) att[i] = dot(g.gLights[i + x].attenuation.xyz, float3(1.0f, dst[i], dst[i] * dst[i]));

	att = 1.0f / att;

	float4 spt;
	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], g.gLights[i + x].direction) - g.gLights[i + x].param[Phi]) * g.gLights[i + x].param[Theta];
		if (g.gLights[i + x].type == 0) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	float4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);

	if (bSpec) {

		float r2 = fRgh * fRgh;
		float4 d2 = dHN * dHN;
		float4 w = 1.0f / (3.14f * r2 * d2 * d2);
		float4 q = 1.0f / (r2 * d2);

		spe = (att * spt * dif) * w * exp((d2 - 1.0f) * q);
	}

	dif *= (att * spt);

	diff_out = 0.0f;
	spec_out = 0.0f;

	for (i = 0; i < 4; i++) diff_out += Sq(g.gLights[i + x].diffuse.rgb * dif[i]);

	if (bSpec) {
		for (i = 0; i < 4; i++) spec_out += g.gLights[i + x].diffuse.rgb * spe[i];
	}
}



void LocalLightsEx(constant Globals &g, thread float3 &cDiffLocal, thread float3 &cSpecLocal, float3 nrmW, float3 posW, float sp, bool ubBeckman)
{
	cDiffLocal = 0.0f;
	cSpecLocal = 0.0f;

#if LMODE !=0
	if (!g.gLightsEnabled) {
		cDiffLocal = 0.0f;
		cSpecLocal = 0.0f;
	}
#endif

#if LMODE == 0
	cDiffLocal = 0.0f;
	cSpecLocal = 0.0f;
#elif (LMODE & 1) == 1
	float3 dd, ss;
	int i;
	if (ubBeckman) {
		for (i = 0; i < MAX_LIGHTS; i += 4) {
			LocalLightsBeckman(g, dd, ss, nrmW, posW, sp, i, false);
			cDiffLocal += dd;
			cSpecLocal += ss;
		}
	}
	else {
		for (i = 0; i < MAX_LIGHTS; i += 4) {
			LocalLights(g, dd, ss, nrmW, posW, sp, i, false);
			cDiffLocal += dd;
			cSpecLocal += ss;
		}
	}
#elif (LMODE & 1) == 0
	float3 dd, ss;
	int i;
	if (ubBeckman) {
		for (i = 0; i < MAX_LIGHTS; i += 4) {
			LocalLightsBeckman(g, dd, ss, nrmW, posW, sp, i, true);
			cDiffLocal += dd;
			cSpecLocal += ss;
		}
	}
	else {
		for (i = 0; i < MAX_LIGHTS; i += 4) {
			LocalLights(g, dd, ss, nrmW, posW, sp, i, true);
			cDiffLocal += dd;
			cSpecLocal += ss;
		}
	}
#endif
}



// ==========================================================================================================
// Object Self Shadows
// ==========================================================================================================

float SampleShadows(constant Globals &g, texture2d<float> gShadowMap, float2 sp, float pd)
{

	float2 dx = float2(g.gSHD[1], 0.0f) * 1.5f;
	float2 dy = float2(0.0f, g.gSHD[1]) * 1.5f;
	float  va = 0.0f;

	sp -= dy;
	if ((gShadowMap.sample(ShadowS, sp - dx).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((gShadowMap.sample(ShadowS, sp - dx).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((gShadowMap.sample(ShadowS, sp - dx).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp).r) > pd) va++;
	if ((gShadowMap.sample(ShadowS, sp + dx).r) > pd) va++;

	return va * 0.1111111f;
}


float SampleShadows2(constant Globals &g, texture2d<float> gShadowMap, float2 sp, float pd)
{

	float val = 0.0f;
	float m = KERNEL_RADIUS * g.gSHD[1];

	for (int i = 0; i < KERNEL_SIZE; i++) {
		if ((gShadowMap.sample(ShadowS, sp + g.kernel[i].xy * m).r) > pd) val += g.kernel[i].z;
	}

	return saturate(val * KERNEL_WEIGHT);
}


float SampleShadows3(constant Globals &g, texture2d<float> gShadowMap, float2 sp, float pd, float4 frame)
{

	float val = 0.0f;
	frame *= KERNEL_RADIUS * g.gSHD[1];

	for (int i = 0; i < KERNEL_SIZE; i++) {
		float2 ofs = frame.xy * g.kernel[i].x + frame.zw * g.kernel[i].y;
		if (gShadowMap.sample(ShadowS, sp + ofs).r > pd) val += g.kernel[i].z;
	}

	return saturate(val * KERNEL_WEIGHT);
}


float SampleShadowsEx(constant Globals &g, texture2d<float> gShadowMap, float2 sp, float pd, float4 sc)
{

#if SHDMAP == 1
	return SampleShadows(g, gShadowMap, sp, pd);
#elif SHDMAP == 2 || SHDMAP == 4
	return SampleShadows2(g, gShadowMap, sp, pd);
#else
	float si, co;
	sc += (g.gSHD[2] * 2.0f);
	si = sincos(sc.y + sc.x * 149.0f, co);
	return SampleShadows3(g, gShadowMap, sp, pd, float4(si, co, co, -si));
#endif
}


float ComputeShadow(constant Globals &g, texture2d<float> gShadowMap, float4 shdH, float dLN, float4 sc)
{
	if (!g.gShadowsEnabled) return 1.0f;

	shdH.xyz /= shdH.w;
	shdH.z = 1.0f - shdH.z;
	float2 sp = shdH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f);

	sp += g.gSHD[1] * 0.5f;

	if (sp.x < 0 || sp.y < 0) return 1.0f;
	if (sp.x > 1 || sp.y > 1) return 1.0f;

	float fShadow;

	float kr = g.gSHD[0] * KERNEL_RADIUS;
	float dx = rsqrt(1.0f - dLN * dLN);
	float ofs = kr / (dLN * dx);
	float omx = min(0.05f + ofs, 0.5f);

	float  pd = shdH.z + omx * g.gSHD[3];

	if (pd < 0) pd = 0.0f;
	if (pd > 1) pd = 1.0f;

	fShadow = SampleShadowsEx(g, gShadowMap, sp, pd, sc);

	return 1.0f - fShadow;
}
