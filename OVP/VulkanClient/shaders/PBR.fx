// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2018 Jarmo Nikkanen
// ==============================================================




struct PBRData
{
	vec4 posH;     // POSITION0
	vec3 camW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
	vec4 tanW;     // TEXCOORD3	 // Handiness in .w
#if SHDMAP > 0
	vec4 shdH;     // TEXCOORD4
#endif
};



// ========================================================================================================================
// Vertex shader for physics based rendering
//
PBRData PBR_VS(MESH_VERTEX vrt)
{
    // Zero output.
	PBRData outVS; // (PBRData)0: every member is set below

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	vec3 nrmW = (vec4(vrt.nrmL, 0.0f) * gW).xyz;

	outVS.nrmW = nrmW;
	outVS.tanW = vec4((vec4(vrt.tanL, 0.0f) * gW).xyz, vrt.tex0.z);
	outVS.posH = vec4(posW, 1.0f) * gVP;

#if SHDMAP > 0
	outVS.shdH = vec4(posW, 1.0f) * gLVP;
#endif

    outVS.camW = -posW;
    outVS.tex0 = vrt.tex0.xy;

    return outVS;
}

#if defined(VS_PBR_VS) || defined(VS_AdvancedVS) || defined(VS_MetalnessVS) || defined(VS_FAST_VS)
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 2) in vec3 iTanL;
layout(location = 5) in vec3 iTex0;
#endif
#ifdef VS_PBR_VS
layout(location = 0) out PBRData oVS;
void main() { oVS = PBR_VS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif






// ============================================================================
//
#ifdef STAGE_PS
vec4 PBR_PS(vec4 sc, PBRData frg)	// sc : VPOS
{
	vec3 nrmT;
	vec3 nrmW;
	vec3 cEmis;
	vec3 cRefl, cRefl2, cRefl3;
	vec3 cFrsl = vec3(1);
	vec4 cDiff;
	vec4 cSpec;
	vec4 sMask = vec4(1.0f, 1.0f, 1.0f, 1024.0f);
	float  fRghn;
	vec3 cDiffLocal;
	vec3 cSpecLocal;


	// ----------------------------------------------------------------------
	// Start fetching texture data
	// ----------------------------------------------------------------------

	if (gTextured) cDiff = texture(WrapS, frg.tex0.xy);
	else		   cDiff = vec4(1);

	if (gOITEnable) if (cDiff.a < 0.5f) discard; // clip(-1)

	// Fetch a normal map
	//
	if (gCfg.Norm) nrmT = texture(Nrm0S, frg.tex0.xy).rgb;


	// Sample specular map
	if (gCfg.Spec) cSpec = texture(SpecS, frg.tex0.xy).rgba * sMask;
	else 		   cSpec = gMtrl.specular.rgba;


	// Use _refl color for both
	if (gCfg.Refl) cRefl = texture(ReflS, frg.tex0.xy).rgb;
	else		   cRefl = gMtrl.reflect.rgb;


	// Roughness map
	if (gCfg.Rghn) fRghn = texture(RghnS, frg.tex0.xy).g;
	else		   fRghn = gMtrl.roughness.r;


	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	if (gCfg.Emis) cEmis = texture(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = vec3(0);



	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	vec3 CamD = normalize(frg.camW);
	vec3 cSun = clamp(gSun.Color, 0.0, 1.0);


	// ----------------------------------------------------------------------
	// Texture tuning controls for add-on developpers
	// ----------------------------------------------------------------------

#if defined(_DEBUG)
	if (gTuneEnabled) {

		nrmT *= gTune.Norm.rgb;

		cDiff.rgb = pow(abs(cDiff.rgb), vec3(gTune.Albe.a)) * gTune.Albe.rgb;
		cRefl.rgb = pow(abs(cRefl.rgb), vec3(gTune.Refl.a)) * gTune.Refl.rgb;
		cEmis.rgb = pow(abs(cEmis.rgb), vec3(gTune.Emis.a)) * gTune.Emis.rgb;
		fRghn = pow(abs(fRghn), gTune.Rghn.a) * gTune.Rghn.g;
		cSpec.rgba = cSpec.rgba * gTune.Spec.rgba;

		cDiff = clamp(cDiff, 0.0, 1.0);
		cRefl = clamp(cRefl, 0.0, 1.0);
		fRghn = clamp(fRghn, 0.0, 1.0);
		cSpec = min(cSpec, sMask);
	}
#endif


	// Use alpha zero to mask off specular reflections
	cSpec.rgb *= clamp(cSpec.a, 0.0, 1.0);

	// ----------------------------------------------------------------------
	// "Legacy/PBR" switch
	// ----------------------------------------------------------------------

	if (gPBRSw) {
		cRefl2 = cRefl*cRefl;
		cRefl3 = cRefl2*cRefl;
		cSpec.rgb = cRefl2;
		cSpec.a = exp2(fRghn * 12.0f);					// Compute specular power
	}
	else {
		cRefl3 = cRefl2 = cRefl;
	}

	float fRefl = cmax(cRefl3);


	// ----------------------------------------------------------------------
	// cSpec.pwr to fRghn Converter
	// ----------------------------------------------------------------------

	if (gRghnSw) {
		fRghn = log2(cSpec.a+1.0f) * 0.1f;
	}




	// ----------------------------------------------------------------------
	// Construct a proper world space normal
	// ----------------------------------------------------------------------

	if (gCfg.Norm) {
		vec3 bitW = cross(frg.tanW.xyz, frg.nrmW) * frg.tanW.w;
		nrmT.rg = nrmT.rg * 2.0f - 1.0f;
		nrmW = frg.nrmW*nrmT.z + frg.tanW.xyz*nrmT.x + bitW*nrmT.y;
	}
	else nrmW = frg.nrmW;

	nrmW = normalize(nrmW);



	// ----------------------------------------------------------------------
	// Compute reflection vector and some required dot products
	// ----------------------------------------------------------------------

	vec3 RflW = reflect(-CamD, nrmW);				// Reflection vector
	float dRS = clamp(-dot(RflW, gSun.Dir), 0.0, 1.0);		// Reflection/sun angle
	float dLN = clamp(-dot(gSun.Dir, nrmW), 0.0, 1.0);		// Diffuse lighting term
	float dLNx = clamp(dLN * 80.0f, 0.0, 1.0);				// Specular, Fresnel shadowing term


	// ----------------------------------------------------------------------
	// Add vessel self-shadows
	// ----------------------------------------------------------------------

#if SHDMAP > 0
	cSun *= smoothstep(0, 0.72, ComputeShadow(frg.shdH, dLN, sc));
#endif


	// ----------------------------------------------------------------------
	// Compute a fresnel terms fFrsl, iFrsl, fFLbe
	// ----------------------------------------------------------------------

	float fFrsl = 0;	// Fresnel angle co-efficiency factor
	float iFrsl = 0;	// Fresnel intensity
	float fFLbe = 0;	// Fresnel lobe

#if defined(_GLASS)

	if (gFresnel) {

		float dCN = clamp(dot(CamD, nrmW), 0.0, 1.0);

		// Compute a fresnel term
		fFrsl = pow(1.0f - dCN, gMtrl.fresnel.x);

		// Compute a specular lobe for fresnel reflection
		fFLbe = pow(dRS, gMtrl.fresnel.z) * dLNx * float(any(notEqual(cRefl, vec3(0))));

		// Modulate with material
		cFrsl *= gMtrl.fresnel.y;

		// Compute intensity term. Fresnel is always on a top of a multi-layer material
		// therefore it remains strong and attennuates other properties to maintain energy conservation using (1.0 - iFrsl)
		iFrsl = cmax(cFrsl) * fFrsl;
	}
#endif




	// ----------------------------------------------------------------------
	// Compute a specular and diffuse lighting
	// ----------------------------------------------------------------------

	// Compute a specular lobe for base material
	float fLobe = pow(dRS, cSpec.a) * dLNx;


	// ----------------------------------------------------------------------
	// Compute Local Light Sources
	// ----------------------------------------------------------------------

	LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);


	// ----------------------------------------------------------------------
	// Compute Earth glow
	// ----------------------------------------------------------------------

	float angl = clamp((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize, 0.0, 1.0);
	cDiffLocal += gAtmColor.rgb * max(0, angl*gGlowConst);

	// Bake material props and lights together
	vec3 diffBaked = Light_fx(gMtrl.diffuse.rgb * (dLN * cSun + cDiffLocal) + gMtrl.emissive.rgb + gMtrl.ambient.rgb*gSun.Ambient);

#if LMODE > 0
	cSun = Light_fx(cSun + cSpecLocal);	// Add local light sources
#endif

	// Special alpha only texture in use, set the .rgb to 1.0f
	// Used for panel background lighting in Delta Glider
	if (gNoColor) cDiff.rgb = vec3(1);

	// ------------------------------------------------------------------------
	cDiff.rgb *= diffBaked;				// Lit the texture
	cDiff.a *= gMtrlAlpha;				// Modulate material alpha


	// ------------------------------------------------------------------------
	// Compute total reflected sun light from a material
	//
	vec3 cBase = cSpec.rgb * (1.0f - iFrsl) * fLobe;

#if defined(_GLASS)
	cBase += cFrsl.rgb * fFrsl * fFLbe;
#endif

	cSpec.rgb = cSun * clamp(cBase, 0.0, 1.0);







	// ----------------------------------------------------------------------
	// Compute a environment reflections
	// ----------------------------------------------------------------------

	vec3 cEnv = vec3(0);

#if defined(_ENVMAP)

	if (gEnvMapEnable) {

#if defined(_GLASS)

		if (gFresnel) {

			// Compute LOD level for fresnel reflection
			float fLOD = max(0, (10.0f - log2(gMtrl.fresnel.z)));

			// Always mirror clear reflection for low angles
			fLOD *= (1.0f - fFrsl);

			// Fresnel based environment reflections
			cEnv = (cFrsl * fFrsl) * textureLod(EnvMapAS, RflW, fLOD).rgb;
		}
#endif

		// Compute LOD level for blur effect
		float fLOD = (1.0f - fRghn) * 8.0f;

		// Add a metallic reflections from a base material
		cEnv += cRefl3 * (1.0f-iFrsl) * textureLod(EnvMapAS, RflW, fLOD).rgb;
	}

#endif




	// ----------------------------------------------------------------------
	// Combine all results together
	// ----------------------------------------------------------------------

	// Compute total reflected light
	float fTot = cmax(cEnv + cSpec.rgb);

	// Attennuate diffuse surface beneath
	cDiff.rgb *= (1.0f - fTot);

#if defined(_ENVMAP)
	// Attennuate diffuse surface beneath
	cDiff.rgb *= (1.0f - fRefl);

#if defined(_GLASS)
	// Further attennuate diffuse surface beneath
	cDiff.rgb *= (1.0f - iFrsl*iFrsl);			// note: (1-iFrsl) goes black too quick
#endif
#endif

	// Re-compute output alpha for alpha blending stage
	cDiff.a = clamp(cDiff.a + fTot, 0.0, 1.0);

	// Add reflections to output
	cDiff.rgb += cEnv;

	// Add specular to output
	cDiff.rgb += cSpec.rgb;

	// Add emission texture to output, modulate with material
	cDiff.rgb = max(cDiff.rgb, cEmis * gMtrl.emission2.rgb);

#if defined(_DEBUG)
	//if (gDebugHL) cDiff = cDiff*0.5f + gColor;
	cDiff = cDiff * (1 - gColor*0.5f) + gColor;
#endif

	cDiff.rgb *= gSun.Transmission;
	cDiff.rgb += gSun.Inscatter;

	return cDiff;
}
#endif

#if defined(PS_PBR_PS) || defined(PS_AdvancedPS) || defined(PS_MetalnessPS)
layout(location = 0) in PBRData frg;
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_PBR_PS
void main() { oColor = PBR_PS(vec4(gl_FragCoord.xy - 0.5, 0, 0), frg PS_ARGS); } // VPOS: pixel coordinates without D3D9's centre offset
#endif







// ============================================================================
// Fast legacy Implementation no additional textures
// ============================================================================


struct FASTData
{
	vec4 posH;     // POSITION0
	vec3 camW;     // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
	vec3 nrmW;     // TEXCOORD2
#if SHDMAP > 0
	vec4 shdH;     // TEXCOORD4
#endif
};


// ============================================================================
// Vertex shader for physics based rendering
//
FASTData FAST_VS(MESH_VERTEX vrt)
{
	// Zero output.
	FASTData outVS; // (FASTData)0: every member is set below

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	vec3 nrmW = (vec4(vrt.nrmL, 0.0f) * gW).xyz;

	outVS.nrmW = nrmW;
	outVS.posH = vec4(posW, 1.0f) * gVP;
	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

#if SHDMAP > 0
	outVS.shdH = vec4(posW, 1.0f) * gLVP;
#endif

	return outVS;
}

#ifdef VS_FAST_VS
layout(location = 0) out FASTData oVS;
void main() { oVS = FAST_VS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


// ============================================================================
//
#ifdef STAGE_PS
vec4 FAST_PS(vec4 sc, FASTData frg)	// sc : VPOS
{

	vec3 cEmis;
	vec4 cDiff;
	vec3 cDiffLocal;
	vec3 cSpecLocal;

	// Start fetching texture data -------------------------------------------
	//
	if (gTextured) cDiff = texture(WrapS, frg.tex0.xy);
	else		   cDiff = vec4(1);

	if (gOITEnable) if (cDiff.a < 0.5f) discard; // clip(-1)

	if (gFullyLit) {
		if (gNoColor) cDiff.rgb = vec3(1);
		cDiff.rgb *= clamp(gMtrl.diffuse.rgb + gMtrl.emissive.rgb, 0.0, 1.0);
	}
	else {

		// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
		if (gCfg.Emis) cEmis = texture(EmisS, frg.tex0.xy).rgb;
		else		   cEmis = vec3(0);

		vec3 nrmW  = normalize(frg.nrmW);
		vec4 cSpec = gMtrl.specular.rgba;
		vec3 cSun  = clamp(gSun.Color, 0.0, 1.0);
		float  dLN   = clamp(-dot(gSun.Dir, nrmW), 0.0, 1.0);

		//cSpec.rgb *= 0.33333f;

		if (gNoColor) cDiff.rgb = vec3(1);

		// ----------------------------------------------------------------------
		// Add vessel self-shadows
		// ----------------------------------------------------------------------

#if SHDMAP > 0
		float fShadow = smoothstep(0, 0.72, ComputeShadow(frg.shdH, dLN, sc));
		dLN *= fShadow;
#endif

		// ----------------------------------------------------------------------
		// Compute Local Light Sources
		// ----------------------------------------------------------------------

		LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);


		// ----------------------------------------------------------------------
		// Compute Earth glow
		// ----------------------------------------------------------------------

		float angl = clamp((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize, 0.0, 1.0);
		cDiffLocal += gAtmColor.rgb * max(0, angl*gGlowConst);

		cDiff.rgb *= clamp( (gMtrl.diffuse.rgb*(dLN * cSun + cDiffLocal)) + (gMtrl.ambient.rgb*gSun.Ambient) + gMtrl.emissive.rgb , 0.0, 1.0);

		vec3 CamD = normalize(frg.camW);
		vec3 HlfW = normalize(CamD - gSun.Dir);
		float  fSun = pow(clamp(dot(HlfW, nrmW), 0.0, 1.0), gMtrl.specular.a);

#if SHDMAP > 0
		fSun *= fShadow;
#endif


		if (dLN == 0) fSun = 0;

#if LMODE > 0
		vec3 specLight = clamp((fSun * cSun) + cSpecLocal, 0.0, 1.0);
#else
		vec3 specLight = (fSun * cSun);
#endif
		cDiff.rgb += (cSpec.rgb * specLight);

		cDiff.rgb += cEmis;
	}

#if defined(_DEBUG)
	//if (gDebugHL) cDiff = cDiff*0.5f + gColor;
	cDiff = cDiff * (1 - gColor*0.5f) + gColor;
#endif

	cDiff.a *= gMtrlAlpha;

	cDiff.rgb *= gSun.Transmission;
	cDiff.rgb += gSun.Inscatter;

	return cDiff;
}
#endif



// ========================================================================================================================
//
vec4 XRHUD_PS(FASTData frg)
{
	return texture(WrapS, frg.tex0.xy);
}

#if defined(PS_FAST_PS) || defined(PS_XRHUD_PS)
layout(location = 0) in FASTData frg;
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_FAST_PS
void main() { oColor = FAST_PS(vec4(gl_FragCoord.xy - 0.5, 0, 0), frg PS_ARGS); } // VPOS
#endif
#ifdef PS_XRHUD_PS
void main() { oColor = XRHUD_PS(frg PS_ARGS); }
#endif
