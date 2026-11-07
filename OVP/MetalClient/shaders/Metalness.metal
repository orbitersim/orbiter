// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Metal port of Metalness.fx (included by Vessel.metal)
// ==============================================================

#define eps 0.001f

// ============================================================================
// Vertex shader for physics based rendering
//
vertex PBRData MetalnessVS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	PBRData outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	float3 nrmW = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;

#if SHDMAP > 0
	outVS.shdH = g.gLVP * float4(posW, 1.0f);
#endif

	outVS.nrmW = nrmW;
	outVS.tanW = float4((g.gW * float4(vrt.tanL, 0.0f)).xyz, vrt.tex0.z);
	outVS.posH = g.gVP * float4(posW, 1.0f);
	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

	return outVS;
}


// ============================================================================
//
float BeckmanNDF(float dHN, float rgh)
{
	float r2 = rgh*rgh;
	float dHN2 = dHN*dHN;
	float2 w = 1.0f / float2(3.14f * r2 * dHN2*dHN2, r2*dHN2);
	return w.x * exp((dHN2 - 1.0f) * w.y);
}

// ============================================================================
//
float GGX_NDF(float dHN, float rgh)
{
	float r2 = rgh*rgh;
	float dHN2 = dHN*dHN;
	float d = (r2 * dHN2) + (1.0f - dHN2);
	return r2 / (3.14f * d * d);
}


// ============================================================================
//
float SchlickBeckmanGSF(float dLN, float dCN, float rgh)
{
	float2 dots = clamp(float2(dLN, dCN), float2(eps), float2(1.0f));
	float  r2 = rgh * rgh;
	float2 e; e.xy = (1.0f - r2);
	float2 w = 1.0f / ((dots * e) + r2);
	return w.x*w.y;
}


// ============================================================================
//
float DiffuseRetroReflectance(float dLN, float dCN, float dLH, float rgh, float mtl)
{
	float2 q = (1.0f-float2(dLN, dCN));	q *= q*q;
	float z = 0.5f + 1.6f * dLH*dLH * rgh;
	return ((1.0f - q.x) + z*q.x) * ((1.0f - q.y) + z*q.y);
}


// ============================================================================
//
float3 LightFX(float3 c)
{
	float q = cmax(c);
	return c * rsqrt(2.0f + q*q) * 1.8f;
}

// ============================================================================
//
float3 LightFXSq(float3 c)
{
	c = sqrt(c);
	float q = cmax(c);
	return c * rsqrt(2.0f + q*q) * 1.8f;
}


// ============================================================================
//
void SampleEnvMap(texturecube<float> gEnvMapA, thread float3 &cE, float dCN, float fRgh, float fMetal, float3 rflW, float3 nrmW)
{
	fRgh = saturate(fRgh - 0.1f);
	float fLOD = fRgh * mix(dCN * 2.0f, (0.2f + dCN*0.8f) * 2.5f, fMetal);

	fLOD *= 5.0f * rsqrt(1.0f + fLOD*fLOD);

	cE = gEnvMapA.sample(EnvMapAS, rflW, level(fLOD)).rgb;
}


// ============================================================================
//
void Transmittance(constant Globals &g, texture2d<float> gTransmMap, texture2d<float> gTranslMap, thread float4 &cDiff, float uLN, float uLC, float2 uv, float3 cSun)
{
	float4 cTransm = float4(cDiff.rgb, 1.0f);
	float3 cTransl = cDiff.rgb;

	if (g.gCfg.Transm) {
		cTransm = gTransmMap.sample(TransmS, uv);
		cTransm.a *= 1024.0f;
	}

	if (g.gCfg.Transl) cTransl = gTranslMap.sample(TranslS, uv).rgb;

	float sunLightFromBehind = saturate(-uLN);
	float sunSpotFromBehind = saturate(pow(saturate(-uLC), cTransm.a) * 3.0f);

	cDiff.rgb += (1.0f - cDiff.rgb) * cTransl.rgb * saturate(cSun * sunLightFromBehind);
	cDiff.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
}


// ============================================================================
// A Shader for a typical "Metalness" PBR workflow.
// ============================================================================

fragment float4 MetalnessPS(PBRData frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]],
	texture2d<float> gTex3 [[texture(2)]],
	texture2d<float> gSpecMap [[texture(3)]],
	texture2d<float> gRghnMap [[texture(4)]],
	texture2d<float> gEmisMap [[texture(5)]],
	texturecube<float> gEnvMapA [[texture(6)]],
	texturecube<float> gEnvMapB [[texture(7)]],
	texture2d<float> gMetlMap [[texture(9)]],
	texture2d<float> gHeatMap [[texture(10)]],
	texture2d<float> gTranslMap [[texture(11)]],
	texture2d<float> gTransmMap [[texture(12)]],
	texture2d<float> gShadowMap [[texture(13)]],
	texture2d<float> gIrradianceMap [[texture(14)]])
{
	float4 sc = float4(floor(frg.posH.xy), 0.0f, 0.0f);
	float3 nrmT;
	float3 nrmW;
	float3 cEmis;
	float4 cSpecularMap;
	float4 cDiff;
	float  fHeat;
	float  fSmth, fMetal;
	float3 cDiffLocal;
	float3 cSpecLocal;

	// ======================================================================
	// Start fetching texture data
	// ======================================================================

	if (g.gTextured) cDiff = gTex0.sample(WrapS, frg.tex0.xy);
	else		     cDiff = 1.0f;

	if (g.gOITEnable) if (cDiff.a < 0.5f) discard_fragment();

	if (g.gCfg.Norm) nrmT = gTex3.sample(Nrm0S, frg.tex0.xy).rgb;

	if (g.gCfg.Rghn) fSmth = gRghnMap.sample(RghnS, frg.tex0.xy).g;
	else		     fSmth = 1.0f;

	if (g.gCfg.Metl) fMetal = gMetlMap.sample(MetlS, frg.tex0.xy).g;
	else		     fMetal = g.gMtrl.metalness;

	if (g.gCfg.Emis) cEmis = gEmisMap.sample(EmisS, frg.tex0.xy).rgb;
	else		     cEmis = 0.0f;

	if (g.gCfg.Spec) cSpecularMap = gSpecMap.sample(SpecS, frg.tex0.xy).rgba;

	if (g.gCfg.Heat) fHeat = saturate((gHeatMap.sample(HeatS, frg.tex0.xy).g) - 1.0f + (g.gMtrl.specialfx.x * g.gMtrl.specialfx.x * g.gMtrl.specialfx.x));
	else fHeat = (g.gMtrl.specialfx.x * g.gMtrl.specialfx.x * g.gMtrl.specialfx.x);

	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	float3 camW = normalize(frg.camW);
	float3 cSun = g.gSun.Color * mix(float3(1.1f, 1.1f, 0.9f), float3(1.0f, 1.0f, 1.0f), saturate(g.gRadius[3]*2e-5f));


	// ======================================================================
	// Construct a proper world space normal
	// ======================================================================

	float3 tanW = frg.tanW.xyz;
	float3 bitW = cross(tanW, frg.nrmW) * frg.tanW.w;

	if (g.gCfg.Norm) {
		nrmT.rg = nrmT.rg * 2.0f - 1.0f;
		nrmW = frg.nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;
	}
	else nrmW = frg.nrmW;

	nrmW = normalize(nrmW);



	// ======================================================================
	// Typical compatibility requirements
	// ======================================================================

	if (g.gNoColor) cDiff.rgb = 1.0f;
	cDiff = saturate(cDiff * float4(g.gMtrl.diffuse.rgb, g.gMtrlAlpha));


	// ======================================================================
	// Some Precomputations
	// ======================================================================

	float3 sunW = -g.gSun.Dir;
	float3 cEnv = 0.0f;

	float3 rflW = reflect(-camW, nrmW);
	float3 hlvW = normalize(camW + sunW);

	float uLN = dot(sunW, nrmW);
	float uLC = dot(sunW, camW);
	float dLN = saturate(uLN);
	float dLH = saturate(dot(sunW, hlvW));
	float dCN = saturate(dot(camW, nrmW));
	float dHN = saturate(dot(hlvW, nrmW));

	fSmth = pow(abs(fSmth), g.gMtrl.roughness.y) * g.gMtrl.roughness.x;

	fSmth = fSmth + ((1.0f - fSmth) * pow(abs(1.0f - dCN), 4.0f)) * pow(abs(fSmth), 0.5f);

	float fRgh = saturate(1.0f - fSmth);
	float fRgh3 = fRgh*fRgh*fRgh;


	// ======================================================================
	// Compute Local Light Sources
	// ======================================================================

	LocalLightsEx(g, cDiffLocal, cSpecLocal, nrmW, -frg.camW, fRgh3, true);


#if defined(_ENVMAP)

	if (g.gEnvMapEnable) {
		SampleEnvMap(gEnvMapA, cEnv, dCN, fRgh, fMetal, rflW, nrmW);
	}

#if defined(_IRRADIANCE)
	float3 cAmbient = Paraboloidal_LVLH(g, gIrradianceMap, IrradS, nrmW).rgb;
	cAmbient *= cAmbient;

	cAmbient = max(cAmbient, g.gSun.Ambient);
#endif
#endif

#if !defined(_ENVMAP) || !defined(_IRRADIANCE)
	float angl = saturate((-dot(g.gCameraPos, nrmW) - g.gProxySize) * g.gInvProxySize);
	float3 cAmbient = g.gAtmColor.rgb * max(0.0f, angl * g.gGlowConst) + g.gSun.Ambient;
#endif


	cAmbient *= (1.0f - fMetal);


	// ======================================================================
	// Add vessel self-shadows
	// ======================================================================
#if SHDMAP > 0
	cSun *= smoothstep(0.0f, 0.72f, ComputeShadow(g, gShadowMap, frg.shdH, dLN, sc));
#endif


	// ======================================================================
	// Main shader core MetalnessPS
	// ======================================================================
	float  fD = GGX_NDF(dHN, mix(0.01f, 1.0f, fRgh3));
	float  fG = SchlickBeckmanGSF(dLN, dCN, fRgh);
	float  fR = DiffuseRetroReflectance(dLN, dCN, dLH, fRgh, fMetal);

	float3 cS2 = 0.0f;
	float3 cSpec2 = 0.0f;

	float3 cSpec = mix(cDiff.rgb, float3(1.0f, 1.0f, 1.0f), (1.0f - fMetal) * (1.0f - fRgh3));

	float fFrs = pow(1.0f - dCN, fRgh*2.5f + 2.5f);

	fFrs *= saturate(0.3f - fRgh*fRgh) * 3.3f;

	float  fP = mix(0.1f + (1.0f - fRgh)*0.4f, 1.0f, fMetal);


	// ======================================================================
	// Add multilayer texture effect
	// ======================================================================
	if (g.gCfg.Spec) {
		cSpec2 = cSpecularMap.rgb;
		cSpecularMap.a = 1.0f - cSpecularMap.a;
		float fD2 = GGX_NDF(dHN, mix(0.01f, 1.0f, cSpecularMap.a));

		cS2 = (fD2 * cSpec2 * fP) * cSpecularMap.a ;
	}

	float3 cF = cSpec + (1.0f - cSpec) * fFrs;

	float3 cS = (fD * cF * fG * fP) * 0.25f;


	float  R = 0.1f * fSmth;
	float  frP = R + (1.0f - R) * fFrs;

	float3 cE = (cEnv * cF * mix(frP, 1.0f, fMetal));

	float  fA = (1.0f - fFrs) * (1.0f - fMetal);

	fA += fRgh * fMetal * 0.05f;

	float3 zD = cDiff.rgb * fA * LightFXSq(Sq(cSun * fR * dLN) + cDiffLocal + Sq(cAmbient) + Sq(g.gMtrl.emissive.rgb));

	float3 zS = cS * (cSun * dLN) + cSpec * LightFX(cSpecLocal) * 0.5f  + cS2 * (cSun * dLN) + cSpec2 * LightFX(cSpecLocal) * 0.5f;

	cDiff.rgb = zD + zS + cE;

	cDiff.a = saturate(cDiff.a + cmax(zS + cE));


	// ======================================================================
	// Add texture transmittance
	// ======================================================================

	if (g.gCfg.Transm || g.gCfg.Transl) Transmittance(g, gTransmMap, gTranslMap, cDiff, uLN, uLC, frg.tex0.xy, cSun);



	cDiff.rgb = max(cDiff.rgb, cEmis * g.gMtrl.emission2.rgb);

	float3 cHeat = pow(float3(abs(fHeat)), float3(0.5f, 1.5f, 8.0f));
	cDiff.rgb = cDiff.rgb + cHeat;

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - g.gColor*0.5f) + g.gColor;
#endif

#if defined(_LIGHTGLOW)
	cDiff.rgb *= g.gSun.Transmission;
	cDiff.rgb += g.gSun.Inscatter;
	return cDiff;
#else
	float3 h2 = cDiff.rgb * cDiff.rgb;
	cDiff.rgb *= pow(max(float3(0.0f), 1.0f + h2 * h2), float3(-0.25f));

	cDiff.rgb *= g.gSun.Transmission;
	cDiff.rgb += g.gSun.Inscatter;

	return cDiff;
#endif
}
