// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
// ==============================================================

#define PHONG	0
#define BLINN   1
#define WARD	2


#if defined(_LIGHTS)
struct PBRData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
	float4 tanW     : TEXCOORD3;	 // Handiness in .w
	// Local vertex lights
	float4 locW     : TEXCOORD4;	 // Local light source average dir
	float3 cDif		: COLOR0;		 // Local lights diffuse color
	float3 cSpe		: COLOR1;		 // Local lights specular color
};
#else
struct PBRData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
	float4 tanW     : TEXCOORD3;	 // Handiness in .w
	float3 cDif		: COLOR0;		 // Local lights diffuse color
};
#endif

float3 cLuminosity = { 0.4, 0.7, 0.3 };

inline float cmax(float3 color)
{
	return max(max(color.r, color.g), color.b);
}

// ========================================================================================================================
// Vertex shader for physics based rendering
//
PBRData PBR_VS(MESH_VERTEX vrt)
{
    // Zero output.
	PBRData outVS = (PBRData)0;

	float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz;
	
	outVS.nrmW = nrmW;
	outVS.tanW = float4(mul(float4(vrt.tanL, 0.0f), gW).xyz, vrt.tex0.z);
	outVS.posH = mul(float4(posW, 1.0f), gVP);
    outVS.camW = -posW;
    outVS.tex0 = vrt.tex0.xy;
	
	// Local light sources ------------------------------------------------------
	//
#if defined(_LIGHTS)
	if (gLocalLights) {	
		float3 locW;
		LocalVertexLight(outVS.cDif, outVS.cSpe, locW, nrmW, posW, gMtrl.specular.a);
		outVS.locW = float4(-locW.xyz, 1.0f - saturate(dot(-locW.xyz, nrmW)));
	}
	else {
		outVS.cDif = 0;
		outVS.cSpe = 0;
		outVS.locW = float4(0, 0, 0, 1);
	}
#else
	outVS.cDif = 0;
#endif


	// Earth "glow" ------------------------------------------------------------
	//
	if (gGlow) {
		float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
		outVS.cDif += gAtmColor.rgb * max(0, angl*gGlowConst);
	}

    return outVS;
}






// ========================================================================================================================
//
float4 PBR_PS(PBRData frg) : COLOR
{
	float3 bitW;
	float3 nrmT;
	float3 cEmis;
	float3 cRefl;
	float3 nrmW;
	float4 cTex;
	float  fRghn;


	// Start fetching texture data -------------------------------------------
	//
	if (gTextured) cTex = tex2D(WrapS, frg.tex0.xy);
	else		   cTex = 1;


	// Fetch a normal map ----------------------------------------------------
	//
	if (gCfg.Norm) nrmT = tex2D(Nrm0S, frg.tex0.xy).rgb;

	// Use _refl color for both
	if (gCfg.Refl) cRefl = tex2D(ReflS, frg.tex0.xy).rgb;
	else		   cRefl = gMtrl.reflect.rgb;


	// Roughness map 
	if (gCfg.Rghn) fRghn = tex2D(RghnS, frg.tex0.xy).g;
	else		   fRghn = gMtrl.roughness;


	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	if (gCfg.Emis) cEmis = tex2D(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = 0;


	// Now do other calculations while textures are being fetched -----------
	//
	float4 cSpec = gMtrl.specular.rgba;	cSpec.rgb *= 0.33333f;
	float3 CamD = normalize(frg.camW);
	float3 cMrtlBase = (gMtrl.ambient.rgb*gSun.Ambient) + gMtrl.emissive.rgb;
	float3 cSun = saturate(gSun.Color) * fSunIntensity;

	float fN = 1.0f;

	// Construct a proper world space normal --------------------------------
	//
	if (gCfg.Norm) {
		nrmT.rg = nrmT.rg * 2.0f - 1.0f;
		bitW = cross(frg.tanW.xyz, frg.nrmW) * frg.tanW.w;
		nrmW = frg.nrmW*nrmT.z + frg.tanW.xyz*nrmT.x + bitW*nrmT.y;

	#if defined(_LIGHTS)
		fN = max(dot(frg.locW.xyz, nrmW) + frg.locW.w, 0.0f);
		fN *= fN;
	#endif

	}
	else nrmW = frg.nrmW;

	nrmW = normalize(nrmW);

	float3 RflW = reflect(-CamD, nrmW);
	float  dLN = saturate(-dot(gSun.Dir, nrmW));

	if (gCfg.Rghn) cSpec.a = exp2(fRghn * 10.0f);

	float fSun = pow(saturate(-dot(RflW, gSun.Dir)), cSpec.a);

	if (dLN == 0) fSun = 0;

	// Compute received diffuse light
	float3 diffLight = dLN * cSun * fDiffuseFactor + frg.cDif * fN;

	// Bake material props and lights together
	float3 diffBaked = (gMtrl.diffuse.rgb*diffLight) + cMrtlBase;

	// Special alpha only texture in use
	if (gNoColor) cTex.rgb = 1;

	
	


	// Compute sunlight reflection --------------------------------------------

	// Compute total received specular light
	float3 specLight = cSun * fSun;

#if defined(_LIGHTS)
	specLight += frg.cSpe;	// Add local light sources
#endif

	// Convert to sRGB (approximate)
	cRefl *= cRefl;

	// ------------------------------------------------------------------------
	if (gCfg.Refl) cSpec.rgb = cRefl.rgb;
	//else cSpec.rgb = cRefl * any(cRefl);    // Comment out to debound Refl and Spec
	// ------------------------------------------------------------------------

	// Compute maximum allowed cSpec
	//float3 cReflMax = (fSunIntensity - cTex.rgb) / fSunIntensity;
	//cSpec.rgb = min(cSpec.rgb, cReflMax);

	// ------------------------------------------------------------------------
	cTex.rgb *= diffBaked;				// Lit the texture
	cTex.a *= gMtrlAlpha;				// Modulate material alpha
	cSpec.rgb *= specLight;	
	// ------------------------------------------------------------------------


	float fFrsl = 1.0f;
	float fInt = 0.0f;
	
	// Compute reflectivity
	float fRefl = cmax(cRefl);

#if defined(_ENVMAP)

	if (gEnvMapEnable) {

		// Do we need fresnel code for this render pass ?

		if (gFresnel) {

			if (gCfg.Frsl) fFrsl = tex2D(FrslS, frg.tex0.xy).g;
			else 		   fFrsl = gMtrl.fresnel.y;

			// Get mirror reflection for fresnel
			float3 cEnvFres = texCUBElod(EnvMapAS, float4(RflW, 0)).rgb;

//#if (SHADE != WARD)
			float  dCN = saturate(dot(CamD, nrmW));
//#endif

			// Compute a fresnel term with compensations included
			fFrsl *= pow(1.0f - dCN, gMtrl.fresnel.z) * (1.0 - fRefl) * any(cRefl);


//#if (SHADE != WARD)
			// Sunlight reflection for fresnel material
			cSpec.rgb = saturate(cSpec.rgb + fSun * fFrsl * cSun);
//#endif

			// Compute total reflected light with fresnel reflection
			// and accummulate in cSpec
			cSpec.rgb = saturate(cSpec.rgb + fFrsl * cEnvFres);


			// Compute intensity
			fInt = saturate(dot(cSpec.rgb, cLuminosity));
			//fInt = cmax(cSpec);


			// Attennuate diffuse surface
			cTex.rgb *= (1.0f - fInt); // *fFrsl);
		}


		// Compute LOD level for blur effect 
		float fLOD = (1.0f - fRghn) * 10.0f;

		float3 cEnv = texCUBElod(EnvMapAS, float4(RflW, fLOD)).rgb;

		// Compute total reflected light, accummulate in cSpec 
		cSpec.rgb += cRefl.rgb * cEnv;
	}

#endif	

	// Attennuate diffuse surface
	cTex.rgb *= (1.0f - fRefl);

	// Re-compute output alpha for alpha blending stage
	// NOTE: Without fresnel fInt remains zero
	cTex.a = saturate(cTex.a + fInt);

	// Add reflections to output
	cTex.rgb += cSpec.rgb;

	// Add emissive textures to output
	cTex.rgb += cEmis;

#if defined(_DEBUG)	
	if (gDebugHL) cTex = cTex*0.5f + gColor;
#endif

	return cTex;
}







// ================================================================================================
// Fast legacy Implementation no additional textures
// ================================================================================================


#if defined(_LIGHTS)
struct FASTData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
	float3 cDif		: COLOR0;		 // Local lights diffuse color
	float3 cSpe		: COLOR1;		 // Local lights specular color
};
#else
struct FASTData
{
	float4 posH     : POSITION0;
	float3 camW     : TEXCOORD0;
	float2 tex0     : TEXCOORD1;
	float3 nrmW     : TEXCOORD2;
	float3 cDif		: COLOR0;		 // Local lights diffuse color
};
#endif


// ========================================================================================================================
// Vertex shader for physics based rendering
//
FASTData FAST_VS(MESH_VERTEX vrt)
{
	// Zero output.
	FASTData outVS = (FASTData)0;

	float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz;

	outVS.nrmW = nrmW;
	outVS.posH = mul(float4(posW, 1.0f), gVP);
	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

	// Local light sources ------------------------------------------------------
	//
#if defined(_LIGHTS)
	if (gLocalLights) {
		float3 locW;
		LocalVertexLight(outVS.cDif, outVS.cSpe, locW, nrmW, posW, gMtrl.specular.a);
	}
	else {
		outVS.cDif = 0;
		outVS.cSpe = 0;	
	}
#else
	outVS.cDif = 0;
#endif


	// Earth "glow" ------------------------------------------------------------
	//
	if (gGlow) {
		float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
		outVS.cDif += gAtmColor.rgb * max(0, angl*gGlowConst);
	}

	return outVS;
}


// ========================================================================================================================
//
float4 FAST_PS(FASTData frg) : COLOR
{

	float3 cEmis;
	float4 cTex;
	
	// Start fetching texture data -------------------------------------------
	//
	if (gTextured) cTex = tex2D(WrapS, frg.tex0.xy);
	else		   cTex = 1;

	if (gFullyLit) {
		if (gNoColor) cTex.rgb = 1;
		cTex.rgb *= saturate(gMtrl.diffuse.rgb + gMtrl.emissive.rgb);
	}
	else {

		// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
		if (gCfg.Emis) cEmis = tex2D(EmisS, frg.tex0.xy).rgb;
		else		   cEmis = 0;

		float3 nrmW  = normalize(frg.nrmW);
		float4 cSpec = gMtrl.specular.rgba;
		float3 cSun  = saturate(gSun.Color) * fSunIntensity;
		float  dLN   = saturate(-dot(gSun.Dir, nrmW));
		
		cSpec.rgb *= 0.33333f;

		if (gNoColor) cTex.rgb = 1;

		cTex.rgb *= (gMtrl.diffuse.rgb*(dLN * cSun * fDiffuseFactor + frg.cDif)) + (gMtrl.ambient.rgb*gSun.Ambient) + gMtrl.emissive.rgb;

		float3 CamD = normalize(frg.camW);
		float3 HlfW = normalize(CamD - gSun.Dir);
		float  fSun = pow(saturate(dot(HlfW, nrmW)), gMtrl.specular.a);

		if (dLN == 0) fSun = 0;

#if defined(_LIGHTS)
		float3 specLight = (fSun * cSun) + frg.cSpe;
#else
		float3 specLight = (fSun * cSun);
#endif
		cTex.rgb += (cSpec.rgb * specLight);
		
		cTex.rgb += cEmis;
	}

#if defined(_DEBUG)	
	if (gDebugHL) cTex = cTex*0.5f + gColor;
#endif

	cTex.a *= gMtrlAlpha;

	return cTex;
}