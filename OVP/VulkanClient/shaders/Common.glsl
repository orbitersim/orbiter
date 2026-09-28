

#define KERNEL_RADIUS 2.0f
#define SHADOW_THRESHOLD 0.1f       // 0.3 to 7.0



// ============================================================================
//
vec4 Paraboloidal_LVLH(sampler2D s, vec3 i)
{
	float z = dot(gCameraPos, i);
	vec2 p = vec2(dot(gEast, i), dot(gNorth, i)) / (1.0f + abs(z));
	p *= vec2(0.2273f, 0.4545f);
	vec4 A = texture(s, p + vec2(0.25f, 0.5f));
	vec4 B = texture(s, p + vec2(0.75f, 0.5f));
	return mix(A, B, smoothstep(-0.03, 0.03, z));
}

vec3 Sq(vec3 x)
{
	return x * x;
}

vec4 Sq(vec4 x)
{
	return x * x;
}


// ==========================================================================================================
// Local light sources
// ==========================================================================================================


vec3 Light_fx(vec3 x)
{
	return clamp(x, 0.0, 1.0);  //1.5 - exp2(-x.rgb)*1.5f;
}

void LocalLights(
	out vec3 diff_out,
	out vec3 spec_out,
	in vec3 nrmW,
	in vec3 posW,
	in float sp,
	int x,
	bool bSpec)
{

	vec3 posWN = normalize(-posW);
	vec3 p[4];
	vec4 spe;
	int i;

	// Relative positions
	for (i = 0; i < 4; i++) p[i] = posW - gLights[i + x].position;

	// Square distances
	vec4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	// Normalize
	sd = inversesqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	// Distances
	vec4 dst = 1.0 / sd;

	// Attennuation factors
	vec4 att;
	for (i = 0; i < 4; i++) att[i] = dot(gLights[i + x].attenuation.xyz, vec3(1.0, dst[i], dst[i] * dst[i]));

	att = 1.0 / att;

	// Spotlight factors
	vec4 spt;
	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], gLights[i + x].direction) - gLights[i + x].param[Phi]) * gLights[i + x].param[Theta];
		if (gLights[i + x].type == 0) spt[i] = 1.0f;
	}

	spt = clamp(spt, 0.0, 1.0);

	// Diffuse light factors
	vec4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = clamp(dif, 0.0, 1.0);
	dif *= (att * spt);

	// Specular lights factors
	if (bSpec) {

		for (i = 0; i < 4; i++) spe[i] = dot(reflect(p[i], nrmW), posWN) * float(dif[i] > 0);

		spe = pow(clamp(spe, 0.0, 1.0), vec4(sp));
		spe *= (att * spt);
	}

	diff_out = vec3(0);
	spec_out = vec3(0);

	for (i = 0; i < 4; i++) diff_out += gLights[i + x].diffuse.rgb * dif[i];

	if (bSpec) {
		for (i = 0; i < 4; i++) spec_out += gLights[i + x].diffuse.rgb * spe[i];
	}
}


void LocalLightsBeckman(
	out vec3 diff_out,
	out vec3 spec_out,
	in vec3 nrmW,
	in vec3 posW,
	in float fRgh,
	int x,
	bool bSpec)
{

	vec3 camW = normalize(-posW);
	vec3 p[4];
	vec3 H[4];
	vec4 spe;
	vec4 dHN;
	int i;

	// Relative positions
	for (i = 0; i < 4; i++) p[i] = posW - gLights[i + x].position;

	// Square distances
	vec4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	// Normalize
	sd = inversesqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	// Distances
	vec4 dst = 1.0 / sd;

	if (bSpec) {

		// Halfway Vectors
		vec4 hd;
		for (i = 0; i < 4; i++) H[i] = (camW - p[i]);
		for (i = 0; i < 4; i++) hd[i] = dot(H[i], H[i]);

		hd = inversesqrt(hd);

		for (i = 0; i < 4; i++) H[i] *= hd[i];
		for (i = 0; i < 4; i++) dHN[i] = dot(H[i], nrmW);
	}



	// Attennuation factors
	vec4 att;
	for (i = 0; i < 4; i++) att[i] = dot(gLights[i + x].attenuation.xyz, vec3(1.0, dst[i], dst[i] * dst[i]));

	att = 1.0 / att;

	// Spotlight factors
	vec4 spt;
	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], gLights[i + x].direction) - gLights[i + x].param[Phi]) * gLights[i + x].param[Theta];
		if (gLights[i + x].type == 0) spt[i] = 1.0f;
	}

	spt = clamp(spt, 0.0, 1.0);

	// Diffuse light factors
	vec4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = clamp(dif, 0.0, 1.0);

	// Specular lights factors

	if (bSpec) {

		float r2 = fRgh * fRgh;
		vec4 d2 = dHN * dHN;
		vec4 w = 1.0 / (3.14 * r2 * d2 * d2);
		vec4 q = 1.0 / (r2 * d2);

		spe = (att * spt * dif) * w * exp((d2 - 1.0f) * q);
	}

	dif *= (att * spt);

	diff_out = vec3(0);
	spec_out = vec3(0);

	for (i = 0; i < 4; i++) diff_out += Sq(gLights[i + x].diffuse.rgb * dif[i]);

	if (bSpec) {
		for (i = 0; i < 4; i++) spec_out += gLights[i + x].diffuse.rgb * spe[i];
	}
}



void LocalLightsEx(out vec3 cDiffLocal, out vec3 cSpecLocal, in vec3 nrmW, in vec3 posW, in float sp, bool ubBeckman)
{
	cDiffLocal = vec3(0); // uninitialized in HLSL, where fxc started it at 0 (the += below)
	cSpecLocal = vec3(0);

#if LMODE !=0
    if (!gLightsEnabled) {
        cDiffLocal = vec3(0);
        cSpecLocal = vec3(0);
    }
#endif

#if LMODE == 0
    cDiffLocal = vec3(0);
    cSpecLocal = vec3(0);
#elif (LMODE & 1) == 1 // partial
    vec3 dd, ss;
	int i;
    if (ubBeckman) {
        for (i = 0; i < MAX_LIGHTS; i += 4) {
            LocalLightsBeckman(dd, ss, nrmW, posW, sp, i, false);
            cDiffLocal += dd;
            cSpecLocal += ss;
        }
    }
    else {
        for (i = 0; i < MAX_LIGHTS; i += 4) {
            LocalLights(dd, ss, nrmW, posW, sp, i, false);
            cDiffLocal += dd;
            cSpecLocal += ss;
        }
    }
#elif (LMODE & 1) == 0 // full
    vec3 dd, ss;
    int i;
    if (ubBeckman) {
        for (i = 0; i < MAX_LIGHTS; i += 4) {
            LocalLightsBeckman(dd, ss, nrmW, posW, sp, i, true);
            cDiffLocal += dd;
            cSpecLocal += ss;
        }
    }
    else {
        for (i = 0; i < MAX_LIGHTS; i += 4) {
            LocalLights(dd, ss, nrmW, posW, sp, i, true);
            cDiffLocal += dd;
            cSpecLocal += ss;
        }
    }
#endif
}







// ==========================================================================================================
// Object Self Shadows
// ==========================================================================================================
/*
float ProjectShadows(float2 sp)
{
	if (!gShadowsEnabled) return 0.0f;

	if (sp.x < 0 || sp.y < 0) return 0.0f;
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	float2 dx = float2(gSHD[1], 0) * 1.5f;
	float2 dy = float2(0, gSHD[1]) * 1.5f;
	float  va = 0;
	float  pd = 1e-4;

	sp -= dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;

	return va / 9.0f;
}*/


// ---------------------------------------------------------------------------------------------------
//
float SampleShadows(vec2 sp, float pd)
{

	vec2 dx = vec2(gSHD[1], 0) * 1.5f;
	vec2 dy = vec2(0, gSHD[1]) * 1.5f;
	float  va = 0;

	sp -= dy;
	if ((texture(ShadowS, sp - dx).r) > pd) va++;
	if ((texture(ShadowS, sp).r) > pd) va++;
	if ((texture(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((texture(ShadowS, sp - dx).r) > pd) va++;
	if ((texture(ShadowS, sp).r) > pd) va++;
	if ((texture(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((texture(ShadowS, sp - dx).r) > pd) va++;
	if ((texture(ShadowS, sp).r) > pd) va++;
	if ((texture(ShadowS, sp + dx).r) > pd) va++;

	return va * 0.1111111f;
}


// ---------------------------------------------------------------------------------------------------
//
float SampleShadows2(vec2 sp, float pd)
{

	float val = 0;
	float m = KERNEL_RADIUS * gSHD[1];

	for (int i = 0; i < KERNEL_SIZE; i++) {
		if ((texture(ShadowS, sp + kernel[i].xy * m).r) > pd) val += kernel[i].z;
	}

	return clamp(val * KERNEL_WEIGHT, 0.0, 1.0);
}


// ---------------------------------------------------------------------------------------------------
//
float SampleShadows3(vec2 sp, float pd, vec4 frame)
{

	float val = 0;
	frame *= KERNEL_RADIUS * gSHD[1];

	for (int i = 0; i < KERNEL_SIZE; i++) {
		vec2 ofs = frame.xy * kernel[i].x + frame.zw * kernel[i].y;
		if (texture(ShadowS, sp + ofs).r > pd) val += kernel[i].z;
	}

	return clamp(val * KERNEL_WEIGHT, 0.0, 1.0);
}


// ---------------------------------------------------------------------------------------------------
//
float SampleShadowsEx(vec2 sp, float pd, vec4 sc)
{

#if SHDMAP == 1
	return SampleShadows(sp, pd);
#elif SHDMAP == 2 || SHDMAP == 4
	return SampleShadows2(sp, pd);
#else
	float si, co;
	sc += (gSHD[2] * 2.0f);
	si = sin(sc.y + sc.x * 149.0f); co = cos(sc.y + sc.x * 149.0f); // sincos
	return SampleShadows3(sp, pd, vec4(si, co, co, -si));
#endif
}


// ---------------------------------------------------------------------------------------------------
//
float ComputeShadow(vec4 shdH, float dLN, vec4 sc)
{
	if (!gShadowsEnabled) return 1.0f;

	shdH.xyz /= shdH.w;
	shdH.z = 1 - shdH.z;
	vec2 sp = shdH.xy * vec2(0.5f, -0.5f) + vec2(0.5f, 0.5f);

	// sp += gSHD[1] * 0.5f left out: D3D9's half-texel offset, Vulkan pixel centres are at .5

	if (sp.x < 0 || sp.y < 0) return 1.0f;	// If a sample is outside border -> fully lit
	if (sp.x > 1 || sp.y > 1) return 1.0f;

	float fShadow;

	float kr = gSHD[0] * KERNEL_RADIUS;
	float dx = inversesqrt(1.0 - dLN * dLN);
	float ofs = kr / (dLN * dx);
	float omx = min(0.05 + ofs, 0.5);

	float  pd = shdH.z + omx * gSHD[3];

	if (pd < 0) pd = 0;
	if (pd > 1) pd = 1;

	fShadow = SampleShadowsEx(sp, pd, sc);

	return 1 - fShadow;
}
