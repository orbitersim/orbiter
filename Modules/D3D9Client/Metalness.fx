// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#define eps 0.001f

// ============================================================================
// Vertex shader for physics based rendering
//
PBRData MetalnessVS(MESH_VERTEX vrt)
{
	// Zero output.
	PBRData outVS = (PBRData)0;

	float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	float3 nrmW = mul(float4(vrt.nrmL, 0.0f), gW).xyz;

#if SHDMAP > 0
	outVS.shdH = mul(float4(posW, 1.0f), gLVP);
#endif

	outVS.nrmW = nrmW;
	outVS.tanW = float4(mul(float4(vrt.tanL, 0.0f), gW).xyz, vrt.tex0.z);
	outVS.posH = mul(float4(posW, 1.0f), gVP);
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
	float2 w = rcp(float2(3.14*r2*dHN2*dHN2, r2*dHN2));
	return w.x * exp((dHN2 - 1.0f) * w.y);
}


// ============================================================================
//
float SchlickBeckmanGSF(float dLN, float dCN, float rgh) // pre-devided by dLN * dCN
{
	float  q = rgh * rgh;
	float2 e; e.xy = (1.0f - q);
	float2 w = rcp((float2(dLN, dCN) * e) + q);
	return w.x*w.y;
}


// ============================================================================
//
float DiffuseRetroReflectance(float dLN, float dCN, float dLH, float rgh, float mtl)
{
	float x = (1.0f - dLN); x *= x*x;
	float y = (1.0f - dCN); y *= y*y;
	float z = 0.5f + 1.6f * dLH*dLH * rgh;
	return ((1.0f - x) + z*x) * ((1.0f - y) + z*y);
}


// ============================================================================
//
float3 FresnelColorShift(float dCN, float3 c, float pwr, float rgh, float met)
{
	// Assume that plastics absorve 50-90% of specular light
	c *= lerp(0.1f + (1.0f - rgh)*0.4f, 1.0f, met);

	return c + (1.0f - c) * pow(1.0f - dCN, pwr);
}

// ============================================================================
//
float3 LightFX(float3 c)
{
	float q = cmax(c);
	return c * rsqrt(1 + q*q) * 1.4f;
}

// ============================================================================
//
float4 MetalnessPS(float4 sc : VPOS, PBRData frg) : COLOR
{
	float3 nrmT;
	float3 nrmW;
	float3 cEmis;
	float4 cDiff;
	float  fSmth, fMetal;
	float3 cDiffLocal;
	float3 cSpecLocal;


	// ======================================================================
	// Start fetching texture data
	// ======================================================================

	if (gTextured) cDiff = tex2D(WrapS, frg.tex0.xy);
	else		   cDiff = 1;

	// Fetch a normal map
	//
	if (gCfg.Norm) nrmT = tex2D(Nrm0S, frg.tex0.xy).rgb;

	// Fetch Smoothness map (i.e. *_rghn.dds)
	//
	if (gCfg.Rghn) fSmth = tex2D(RghnS, frg.tex0.xy).g;
	else		   fSmth = 1.0f;

	// Fetch Roughness map
	//
	if (gCfg.Metl) fMetal = tex2D(MetlS, frg.tex0.xy).g;
	else		   fMetal = gMtrl.metalness;


	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	//
	if (gCfg.Emis) cEmis = tex2D(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = 0;


	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	float3 camW = normalize(frg.camW);
	float3 cSun = saturate(gSun.Color);


	// ======================================================================
	// Construct a proper world space normal
	// ======================================================================

	float3 tanW = frg.tanW.xyz;
	float3 bitW = cross(tanW, frg.nrmW) * frg.tanW.w;
	
	if (gCfg.Norm) {
		nrmT.rg = nrmT.rg * 2.0f - 1.0f;
		nrmW = frg.nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;
	}
	else nrmW = frg.nrmW;

	nrmW = normalize(nrmW);
	


	// ======================================================================
	// Typical compatibility requirements
	// ======================================================================

	if (gNoColor) cDiff.rgb = 1;
	cDiff = saturate(cDiff * float4(gMtrl.diffuse.rgb, gMtrlAlpha));
	



	// ======================================================================
	// Some Precomputations
	// ======================================================================

	float3 sunW = -gSun.Dir;	
	float3 cEnv = 0;
	
	float3 rflW = reflect(-camW, nrmW);
	float3 hlvW = normalize(camW + sunW);

	// Dot Products
	float uLN = dot(sunW, nrmW);
	float dLN = saturate(uLN);
	float dLH = saturate(dot(sunW, hlvW));
	float dLR = saturate(dot(sunW, rflW));
	float dCN = saturate(dot(camW, nrmW));
	float dHN = saturate(dot(hlvW, nrmW));
	float iCN = sqrt(1.0f - dCN*dCN);
	//float iCN = (1.0f - dCN);

	//return float4(iCN, iCN, iCN, 1);

	if (dCN < eps) clip(-1);

	fSmth = clamp(fSmth*gMtrl.roughness, 0.01f, 0.999f);

	// Modulete roughness and convert
	float fRgh = saturate(1.0f - fSmth);


	// ======================================================================
	// Compute Local Light Sources
	// ======================================================================

	LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, fRgh, true);




	// ======================================================================
	// Compute Earth glow
	// ======================================================================

	float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
	float3 cAmbient = gAtmColor.rgb * max(0, angl * gGlowConst);


	// Base material color for reflections
	float3 cSpec = lerp(float3(1, 1, 1), cDiff.rgb, fMetal);


	// ======================================================================
	// Sample Env Map
	// ======================================================================

#if defined(_ENVMAP)
	if (gEnvMapEnable) {

		// Sharpen reflection at low angles 
		float b = lerp(dCN, 0.5f, fMetal);
		float fLOD = clamp(fRgh * b * 20.0f, 0, 6); // Compute LOD level for blur effect
		cEnv = (dCN > eps ? texCUBElod(EnvMapAS, float4(rflW, fLOD)).rgb : float3(0,0,0));

		cAmbient = texCUBElod(EnvMapAS, float4(nrmW, 6)).rgb;
		cAmbient += float3(1.0f, 1.0f, 1.0f) * cmax(cAmbient);
		cAmbient *= 0.5f;

		// No ambient for metals
		cAmbient *= (1.0f - fMetal);
	}
#endif


	// ======================================================================
	// Add vessel self-shadows
	// ======================================================================

#if SHDMAP > 0
	cSun *= smoothstep(0, 0.72, ComputeShadow(frg.shdH, dLN, sc));
#endif

	


	// ======================================================================
	// Main shader core
	// ======================================================================

	float  fD = BeckmanNDF(dHN, fRgh);
	float  fG = SchlickBeckmanGSF(dLN, dCN, fRgh);
	float3 fS = FresnelColorShift(dLN, cSpec, 3.0f, fRgh, fMetal);
	float  fR = DiffuseRetroReflectance(dLN, dCN, dLH, fRgh, fMetal); 
	
	// Specular Color
	float3 cS = (fD * fS * fG) * 0.25f;	//   / (4.0f*dLN*dCN) removed to avoid division by zero, compensation in GSF

	// Fresnel power 5.0 for glossy, 10.0 for rough
	float frs = pow(iCN, 10.0 * fRgh + 7.0);
	
	// How plastics reflect the environment
	float  frP = (frs * 0.95f + 0.05f) * fSmth;

	// How metals reflect the environment
	float  frM = 0.7f + fSmth * 0.3f;

	float4 cE = lerp(float4(float3(1, 1, 1), frP), float4(cDiff.rgb, frM), fMetal);

	cE.rgb *= (cEnv * cE.a);

	// Attennuate diffuse color for Metals & Fresnel
	float  fA = (1.0f - frP) * (1.0f - fMetal);

	// Add a faint diffuse hue for metals
	fA += fRgh * fMetal / (1.0f + fRgh * 5.0f);

	

	// Combine diffuse terms
	float3 zD = cDiff.rgb * fA * LightFX(cSun * fR * dLN + cDiffLocal + cAmbient) + (cDiff.rgb * gMtrl.emissive.rgb) + cE;

	// Combine specular terms
	float3 zS = cS * (cSun * dLN) + LightFX(cSpecLocal) * 1.5f;
	
	// Assume plastics absorving or transmitting 50% of incoming light or less
	zS *= lerp(0.1f + fSmth*fSmth*fSmth*0.5f, 1.0f, fMetal);

	cDiff.rgb = zD + zS;

	// Override material alpha to make reflections visible
	cDiff.a   = saturate(cDiff.a + cmax(cS + cE));

	
	// ======================================================================
	// Add texture transmittance 
	// ======================================================================

	if (gCfg.Transm || gCfg.Transl) 
	{
		float4 cTransm = float4(cDiff.rgb, 1.0f);
		float3 cTransl = cDiff.rgb;

		if (gCfg.Transm) {
			cTransm = tex2D(TransmS, frg.tex0.xy);
			cTransm.a *= 1024.0f;
		}
		if (gCfg.Transl) cTransl = tex2D(TranslS, frg.tex0.xy).rgb;

		float sunLightFromBehind = saturate(-uLN);
		float sunSpotFromBehind = saturate(pow(saturate(-dot(camW, sunW)), cTransm.a) * 3.0); // "3.0" Causes the transmittance (sun spot) effect to fall off at very shallow angles

		cDiff.rgb += (1.0f - cDiff.rgb) * cTransl.rgb * saturate(cSun * sunLightFromBehind);
		cDiff.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
	}

	// Add emission texture to output, modulate with material
	cDiff.rgb = max(cDiff.rgb, cEmis * gMtrl.emission2.rgb);

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - gColor*0.5f) + gColor;
#endif

	//float x = cmax(cDiff.rgb);
	//cDiff.rgb *= rsqrt(1.0f + x * x * gMtrl.glow);

	return cDiff;
}




/*
// ============================================================================
//
float4 TrslPS(float4 sc : VPOS, PBRData frg) : COLOR
{
	// Compute Transluciency effect --------------------------------------------------------------
	//
	if (gCfg.Transx) {
		float4 cTransm = float4(cTex.rgb, 1.0f);
		if (gCfg.Transm) {
			cTransm = tex2D(TransmS, frg.tex0.xy);
			cTransm.a *= 1024.0f;
		}

		float3 cTransl = cTex.rgb;

		if (gCfg.Transl) cTransl = tex2D(TranslS, frg.tex0.xy).rgb;
	
		float sunLightFromBehind = saturate(dot(gSun.Dir, nrmW));
		float sunSpotFromBehind = pow(saturate(dot(gSun.Dir, CamD)), cTransm.a);
		sunSpotFromBehind *= saturate(sunLightFromBehind * 3.0f);// Causes the transmittance (sun spot) effect to fall off at very shallow angles

		cTransl.rgb *= saturate(cSun * sunLightFromBehind);

		cTex.rgb += (1 - cTex.rgb) * cTransl.rgb;
		cTex.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
	}
}*/