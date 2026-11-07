// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2018 Jarmo Nikkanen
// Metal port of PBR.fx (included by Vessel.metal)
// ==============================================================




struct PBRData
{
	float4 posH     [[position]];
	float3 camW;
	float2 tex0;
	float3 nrmW;
	float4 tanW;
#if SHDMAP > 0
	float4 shdH;
#endif
};



// ========================================================================================================================
// Vertex shader for physics based rendering
//
vertex PBRData PBR_VS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	PBRData outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	float3 nrmW = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;

	outVS.nrmW = nrmW;
	outVS.tanW = float4((g.gW * float4(vrt.tanL, 0.0f)).xyz, vrt.tex0.z);
	outVS.posH = g.gVP * float4(posW, 1.0f);

#if SHDMAP > 0
	outVS.shdH = g.gLVP * float4(posW, 1.0f);
#endif

	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

	return outVS;
}






// ============================================================================
//
fragment float4 PBR_PS(PBRData frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]],
	texture2d<float> gTex3 [[texture(2)]],
	texture2d<float> gSpecMap [[texture(3)]],
	texture2d<float> gRghnMap [[texture(4)]],
	texture2d<float> gEmisMap [[texture(5)]],
	texturecube<float> gEnvMapA [[texture(6)]],
	texture2d<float> gReflMap [[texture(8)]],
	texture2d<float> gShadowMap [[texture(13)]])
{
	float4 sc = float4(floor(frg.posH.xy), 0.0f, 0.0f);
	float3 nrmT;
	float3 nrmW;
	float3 cEmis;
	float3 cRefl, cRefl2, cRefl3;
	float3 cFrsl = 1.0f;
	float4 cDiff;
	float4 cSpec;
	float4 sMask = float4(1.0f, 1.0f, 1.0f, 1024.0f);
	float  fRghn;
	float3 cDiffLocal;
	float3 cSpecLocal;


	// ----------------------------------------------------------------------
	// Start fetching texture data
	// ----------------------------------------------------------------------

	if (g.gTextured) cDiff = gTex0.sample(WrapS, frg.tex0.xy);
	else		     cDiff = 1.0f;

	if (g.gOITEnable) if (cDiff.a < 0.5f) discard_fragment();

	if (g.gCfg.Norm) nrmT = gTex3.sample(Nrm0S, frg.tex0.xy).rgb;

	if (g.gCfg.Spec) cSpec = gSpecMap.sample(SpecS, frg.tex0.xy).rgba * sMask;
	else 		     cSpec = g.gMtrl.specular.rgba;

	if (g.gCfg.Refl) cRefl = gReflMap.sample(ReflS, frg.tex0.xy).rgb;
	else		     cRefl = g.gMtrl.reflect.rgb;

	if (g.gCfg.Rghn) fRghn = gRghnMap.sample(RghnS, frg.tex0.xy).g;
	else		     fRghn = g.gMtrl.roughness.r;

	if (g.gCfg.Emis) cEmis = gEmisMap.sample(EmisS, frg.tex0.xy).rgb;
	else		     cEmis = 0.0f;



	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	float3 CamD = normalize(frg.camW);
	float3 cSun = saturate(g.gSun.Color);


	// ----------------------------------------------------------------------
	// Texture tuning controls for add-on developpers
	// ----------------------------------------------------------------------

#if defined(_DEBUG)
	if (g.gTuneEnabled) {

		nrmT *= g.gTune.Norm.rgb;

		cDiff.rgb = pow(abs(cDiff.rgb), float3(g.gTune.Albe.a)) * g.gTune.Albe.rgb;
		cRefl.rgb = pow(abs(cRefl.rgb), float3(g.gTune.Refl.a)) * g.gTune.Refl.rgb;
		cEmis.rgb = pow(abs(cEmis.rgb), float3(g.gTune.Emis.a)) * g.gTune.Emis.rgb;
		fRghn = pow(abs(fRghn), g.gTune.Rghn.a) * g.gTune.Rghn.g;
		cSpec.rgba = cSpec.rgba * g.gTune.Spec.rgba;

		cDiff = saturate(cDiff);
		cRefl = saturate(cRefl);
		fRghn = saturate(fRghn);
		cSpec = min(cSpec, sMask);
	}
#endif


	cSpec.rgb *= saturate(cSpec.a);

	// ----------------------------------------------------------------------
	// "Legacy/PBR" switch
	// ----------------------------------------------------------------------

	if (g.gPBRSw) {
		cRefl2 = cRefl*cRefl;
		cRefl3 = cRefl2*cRefl;
		cSpec.rgb = cRefl2;
		cSpec.a = exp2(fRghn * 12.0f);
	}
	else {
		cRefl3 = cRefl2 = cRefl;
	}

	float fRefl = cmax(cRefl3);


	// ----------------------------------------------------------------------
	// cSpec.pwr to fRghn Converter
	// ----------------------------------------------------------------------

	if (g.gRghnSw) {
		fRghn = log2(cSpec.a+1.0f) * 0.1f;
	}




	// ----------------------------------------------------------------------
	// Construct a proper world space normal
	// ----------------------------------------------------------------------

	if (g.gCfg.Norm) {
		float3 bitW = cross(frg.tanW.xyz, frg.nrmW) * frg.tanW.w;
		nrmT.rg = nrmT.rg * 2.0f - 1.0f;
		nrmW = frg.nrmW*nrmT.z + frg.tanW.xyz*nrmT.x + bitW*nrmT.y;
	}
	else nrmW = frg.nrmW;

	nrmW = normalize(nrmW);



	// ----------------------------------------------------------------------
	// Compute reflection vector and some required dot products
	// ----------------------------------------------------------------------

	float3 RflW = reflect(-CamD, nrmW);
	float dRS = saturate(-dot(RflW, g.gSun.Dir));
	float dLN = saturate(-dot(g.gSun.Dir, nrmW));
	float dLNx = saturate(dLN * 80.0f);


	// ----------------------------------------------------------------------
	// Add vessel self-shadows
	// ----------------------------------------------------------------------

#if SHDMAP > 0
	cSun *= smoothstep(0.0f, 0.72f, ComputeShadow(g, gShadowMap, frg.shdH, dLN, sc));
#endif


	// ----------------------------------------------------------------------
	// Compute a fresnel terms fFrsl, iFrsl, fFLbe
	// ----------------------------------------------------------------------

	float fFrsl = 0.0f;
	float iFrsl = 0.0f;
	float fFLbe = 0.0f;

#if defined(_GLASS)

	if (g.gFresnel) {

		float dCN = saturate(dot(CamD, nrmW));

		fFrsl = pow(1.0f - dCN, g.gMtrl.fresnel.x);

		fFLbe = pow(dRS, g.gMtrl.fresnel.z) * dLNx * float(any(cRefl != 0.0f));

		cFrsl *= g.gMtrl.fresnel.y;

		iFrsl = cmax(cFrsl) * fFrsl;
	}
#endif




	// ----------------------------------------------------------------------
	// Compute a specular and diffuse lighting
	// ----------------------------------------------------------------------

	float fLobe = pow(dRS, cSpec.a) * dLNx;


	// ----------------------------------------------------------------------
	// Compute Local Light Sources
	// ----------------------------------------------------------------------

	LocalLightsEx(g, cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);


	// ----------------------------------------------------------------------
	// Compute Earth glow
	// ----------------------------------------------------------------------

	float angl = saturate((-dot(g.gCameraPos, nrmW) - g.gProxySize) * g.gInvProxySize);
	cDiffLocal += g.gAtmColor.rgb * max(0.0f, angl*g.gGlowConst);

	float3 diffBaked = Light_fx(g.gMtrl.diffuse.rgb * (dLN * cSun + cDiffLocal) + g.gMtrl.emissive.rgb + g.gMtrl.ambient.rgb*g.gSun.Ambient);

#if LMODE > 0
	cSun = Light_fx(cSun + cSpecLocal);
#endif

	if (g.gNoColor) cDiff.rgb = 1.0f;

	// ------------------------------------------------------------------------
	cDiff.rgb *= diffBaked;
	cDiff.a *= g.gMtrlAlpha;


	// ------------------------------------------------------------------------
	// Compute total reflected sun light from a material
	//
	float3 cBase = cSpec.rgb * (1.0f - iFrsl) * fLobe;

#if defined(_GLASS)
	cBase += cFrsl.rgb * fFrsl * fFLbe;
#endif

	cSpec.rgb = cSun * saturate(cBase);







	// ----------------------------------------------------------------------
	// Compute a environment reflections
	// ----------------------------------------------------------------------

	float3 cEnv = 0.0f;

#if defined(_ENVMAP)

	if (g.gEnvMapEnable) {

#if defined(_GLASS)

		if (g.gFresnel) {

			float fLOD = max(0.0f, (10.0f - log2(g.gMtrl.fresnel.z)));

			fLOD *= (1.0f - fFrsl);

			cEnv = (cFrsl * fFrsl) * gEnvMapA.sample(EnvMapAS, RflW, level(fLOD)).rgb;
		}
#endif

		float fLOD = (1.0f - fRghn) * 8.0f;

		cEnv += cRefl3 * (1.0f-iFrsl) * gEnvMapA.sample(EnvMapAS, RflW, level(fLOD)).rgb;
	}

#endif




	// ----------------------------------------------------------------------
	// Combine all results together
	// ----------------------------------------------------------------------

	float fTot = cmax(cEnv + cSpec.rgb);

	cDiff.rgb *= (1.0f - fTot);

#if defined(_ENVMAP)
	cDiff.rgb *= (1.0f - fRefl);

#if defined(_GLASS)
	cDiff.rgb *= (1.0f - iFrsl*iFrsl);
#endif
#endif

	cDiff.a = saturate(cDiff.a + fTot);

	cDiff.rgb += cEnv;

	cDiff.rgb += cSpec.rgb;

	cDiff.rgb = max(cDiff.rgb, cEmis * g.gMtrl.emission2.rgb);

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - g.gColor*0.5f) + g.gColor;
#endif

	cDiff.rgb *= g.gSun.Transmission;
	cDiff.rgb += g.gSun.Inscatter;

	return cDiff;
}







// ============================================================================
// Fast legacy Implementation no additional textures
// ============================================================================


struct FASTData
{
	float4 posH     [[position]];
	float3 camW;
	float2 tex0;
	float3 nrmW;
#if SHDMAP > 0
	float4 shdH;
#endif
};


// ============================================================================
// Vertex shader for physics based rendering
//
vertex FASTData FAST_VS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	FASTData outVS = {};

	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	float3 nrmW = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;

	outVS.nrmW = nrmW;
	outVS.posH = g.gVP * float4(posW, 1.0f);
	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

#if SHDMAP > 0
	outVS.shdH = g.gLVP * float4(posW, 1.0f);
#endif

	return outVS;
}


// ============================================================================
//
fragment float4 FAST_PS(FASTData frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]],
	texture2d<float> gEmisMap [[texture(5)]],
	texture2d<float> gShadowMap [[texture(13)]])
{
	float4 sc = float4(floor(frg.posH.xy), 0.0f, 0.0f);

	float3 cEmis;
	float4 cDiff;
	float3 cDiffLocal;
	float3 cSpecLocal;

	if (g.gTextured) cDiff = gTex0.sample(WrapS, frg.tex0.xy);
	else		     cDiff = 1.0f;

	if (g.gOITEnable) if (cDiff.a < 0.5f) discard_fragment();

	if (g.gFullyLit) {
		if (g.gNoColor) cDiff.rgb = 1.0f;
		cDiff.rgb *= saturate(g.gMtrl.diffuse.rgb + g.gMtrl.emissive.rgb);
	}
	else {

		if (g.gCfg.Emis) cEmis = gEmisMap.sample(EmisS, frg.tex0.xy).rgb;
		else		     cEmis = 0.0f;

		float3 nrmW  = normalize(frg.nrmW);
		float4 cSpec = g.gMtrl.specular.rgba;
		float3 cSun  = saturate(g.gSun.Color);
		float  dLN   = saturate(-dot(g.gSun.Dir, nrmW));

		if (g.gNoColor) cDiff.rgb = 1.0f;

		// ----------------------------------------------------------------------
		// Add vessel self-shadows
		// ----------------------------------------------------------------------

#if SHDMAP > 0
		float fShadow = smoothstep(0.0f, 0.72f, ComputeShadow(g, gShadowMap, frg.shdH, dLN, sc));
		dLN *= fShadow;
#endif

		LocalLightsEx(g, cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);

		float angl = saturate((-dot(g.gCameraPos, nrmW) - g.gProxySize) * g.gInvProxySize);
		cDiffLocal += g.gAtmColor.rgb * max(0.0f, angl*g.gGlowConst);

		cDiff.rgb *= saturate( (g.gMtrl.diffuse.rgb*(dLN * cSun + cDiffLocal)) + (g.gMtrl.ambient.rgb*g.gSun.Ambient) + g.gMtrl.emissive.rgb );

		float3 CamD = normalize(frg.camW);
		float3 HlfW = normalize(CamD - g.gSun.Dir);
		float  fSun = pow(saturate(dot(HlfW, nrmW)), g.gMtrl.specular.a);

#if SHDMAP > 0
		fSun *= fShadow;
#endif


		if (dLN == 0) fSun = 0.0f;

#if LMODE > 0
		float3 specLight = saturate((fSun * cSun) + cSpecLocal);
#else
		float3 specLight = (fSun * cSun);
#endif
		cDiff.rgb += (cSpec.rgb * specLight);

		cDiff.rgb += cEmis;
	}

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - g.gColor*0.5f) + g.gColor;
#endif

	cDiff.a *= g.gMtrlAlpha;

	cDiff.rgb *= g.gSun.Transmission;
	cDiff.rgb += g.gSun.Inscatter;

	return cDiff;
}



// ========================================================================================================================
//
fragment float4 XRHUD_PS(FASTData frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	return gTex0.sample(WrapS, frg.tex0.xy);
}
