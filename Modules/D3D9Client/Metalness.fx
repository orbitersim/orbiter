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
	float2 w = rcp(float2(3.14f * r2 * dHN2*dHN2, r2*dHN2));
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
float SchlickBeckmanGSF(float dLN, float dCN, float rgh) // pre-devided by dLN * dCN
{
	float  r2 = rgh * rgh;
	float2 e; e.xy = (1.0f - r2);
	float2 w = rcp((float2(dLN, dCN) * e) + r2);
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
float3 FresnelColorShift(float dCN, float3 c, float fFrs, float rgh, float met)
{
	// Assume that plastics absorve 40-70% of specular light
	c *= lerp(0.3f + (1.0f - rgh)*0.3f, 1.0f, met);
	return c + (1.0f - c) * fFrs;
}


// ============================================================================
//
float3 LightFX(float3 c)
{
	float q = cmax(c);
	return c * rsqrt(2 + q*q) * 1.8f;
}

// ============================================================================
//
float3 LightFXSq(float3 c)
{
	c = sqrt(c);
	float q = cmax(c);
	return c * rsqrt(2 + q*q) * 1.8f;
}


// ============================================================================
//
void SampleEnvMap(out float3 cE, float dCN, float fRgh, float fMetal, float3 rflW, float3 nrmW)
{
	// Sharpen reflection at low angles 
	fRgh = saturate(fRgh - 0.1f);
	float fLOD = fRgh * lerp(dCN * 2.0f, (0.2f + dCN*0.8f) * 2.5f, fMetal);	// Compute LOD level for blur effect

	fLOD *= 5.0f * rsqrt(1.0f + fLOD*fLOD);

	cE = (dCN > eps ? texCUBElod(EnvMapAS, float4(rflW, fLOD)).rgb : float3(0, 0, 0));
}


// ============================================================================
//
void Transmittance(in out float4 cDiff, float uLN, float uLC, float2 uv, float3 cSun)
{
	float4 cTransm = float4(cDiff.rgb, 1.0f);
	float3 cTransl = cDiff.rgb;
	
	if (gCfg.Transm) {
		cTransm = tex2D(TransmS, uv);
		cTransm.a *= 1024.0f;
	}

	if (gCfg.Transl) cTransl = tex2D(TranslS, uv).rgb;

	float sunLightFromBehind = saturate(-uLN);
	float sunSpotFromBehind = saturate(pow(saturate(-uLC), cTransm.a) * 3.0); // "3.0" Causes the transmittance (sun spot) effect to fall off at very shallow angles

	cDiff.rgb += (1.0f - cDiff.rgb) * cTransl.rgb * saturate(cSun * sunLightFromBehind);
	cDiff.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
}


// ============================================================================
// A Shader for a typical "Metalness" PBR workflow.
// ============================================================================

float4 MetalnessPS(float4 sc : VPOS, PBRData frg) : COLOR
{
	float3 nrmT;
	float3 nrmW;
	float3 cEmis;
	float4 cDiff;
	float  fHeat;
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

	// Fetch Heat map
	//
	if (gCfg.Heat) fHeat = tex2D(HeatS, frg.tex0.xy).g;
	else		   fHeat = gMtrl.specialfx.x;

	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	float3 camW = normalize(frg.camW);
	float3 cSun = gSun.Color;


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
	float uLC = dot(sunW, camW);
	float dLN = saturate(uLN);
	float dLH = saturate(dot(sunW, hlvW));
	float dCN = saturate(dot(camW, nrmW));
	float dHN = saturate(dot(hlvW, nrmW));
	
	//return float4(float3(1, 1, 1)*(1-dCN), 1);

	if (dCN < eps) clip(-1);

	// Apply a proper curve to a texture data, modulate with material value and clamp
	fSmth = clamp(pow(abs(fSmth), gMtrl.roughness.y) * gMtrl.roughness.x, 0.0005f, 0.9999f);

	float fRgh = saturate(1.0f - fSmth);
	float fRgh3 = clamp(fRgh*fRgh*fRgh, 0.0005f, 0.9999f);


	// ======================================================================
	// Compute Local Light Sources
	// ======================================================================

	LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, fRgh3, true);




	// ======================================================================
	// Compute Earth glow
	// ======================================================================

	float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
	float3 cAmbient = gAtmColor.rgb * max(0, angl * gGlowConst) + gSun.Ambient;


#if defined(_ENVMAP)
	if (gEnvMapEnable) {

		// ======================================================================
		// Sample Env Map
		SampleEnvMap(cEnv, dCN, fRgh, fMetal, rflW, nrmW);

		// ======================================================================
		// Sample Irradiance Map
		cAmbient = Paraboloidal_LVLH(IrradS, nrmW);
		cAmbient *= cAmbient;
		float fNight = saturate(1.0 - cSun.r);
		cAmbient = saturate(cAmbient * (1.0f + 30.0f * fNight));
	}
#endif

	//cAmbient = max(cAmbient, gSun.Ambient);
	cAmbient *= (1.0f - fMetal); // No ambient for metals


	// ======================================================================
	// Add vessel self-shadows
	// ======================================================================
#if SHDMAP > 0
	cSun *= smoothstep(0, 0.72, ComputeShadow(frg.shdH, dLN, sc));
#endif
	cSun *= (1.0f - gSun.Ambient);
	//cSun = 0;
	
	


	// ======================================================================
	// Main shader core MetalnessPS
	// ======================================================================

	float  fD = GGX_NDF(dHN, fRgh3);
	float  fG = SchlickBeckmanGSF(dLN, dCN, fRgh);
	float  fR = DiffuseRetroReflectance(dLN, dCN, dLH, fRgh, fMetal); 
	
	// Base material color for reflections. Use cDiff for metals and very rough plastics, white for the rest 
	float3 cSpec = lerp(cDiff.rgb, float3(1, 1, 1), (1.0f - fMetal) * (1.0f - fRgh3));

	// Fresnel power 2.5 for glossy, 5.0 for rough
	float fFrs = pow(1.0f - dCN, fRgh*2.5 + 2.5f);

	// Fresnel cut-off below X of fSmth
	fFrs *= saturate(0.5f - fRgh*fRgh) * 1.5f;

	// Assume that plastics absorve 50-90% of specular light
	float  fP = lerp(0.1f + (1.0f - fRgh)*0.4f, 1.0f, fMetal);

	// Fresnel color shift
	float3 cF = cSpec + (1.0f - cSpec) * fFrs;

	// Specular Color
	float3 cS = (fD * cF * fG * fP) * 0.25f; // (4.0f*dLN*dCN) removed to avoid division by zero, compensation in GSF

		
	// How plastics reflect the environment
	float  R = 0.1f * fSmth;
	float  frP = R + (1.0f - R) * fFrs;

	float3 cE = (cEnv * cF * lerp(frP, 1.0f, fMetal));

	// Attennuate diffuse color for Metals & Fresnel
	float  fA = (1.0f - fFrs) * (1.0f - fMetal);

	// Add a faint diffuse hue for rough metals. Rough metal doesn't look good if it's totally black
	fA += fRgh * fMetal * 0.05f;

	float3 zD = cDiff.rgb * fA * LightFXSq(Sq(cSun * fR * dLN) + cDiffLocal + Sq(cAmbient) + Sq(gMtrl.emissive.rgb));

	// Combine specular terms
	float3 zS = cS * (cSun * dLN) + cSpec * LightFX(cSpecLocal) * 0.5f;
	
	cDiff.rgb = zD + zS + cE;

	// Override material alpha to make reflections visible
	cDiff.a = saturate(cDiff.a + cmax(zS + cE));

	
	// ======================================================================
	// Add texture transmittance 
	// ======================================================================

	if (gCfg.Transm || gCfg.Transl) Transmittance(cDiff, uLN, uLC, frg.tex0.xy, cSun);
	


	// Add emission texture to output, modulate with material
	cDiff.rgb = max(cDiff.rgb, cEmis * gMtrl.emission2.rgb);

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - gColor*0.5f) + gColor;
#endif

#if defined(_LIGHTGLOW)
	return cDiff;
#else
	float3 h2 = cDiff.rgb*cDiff.rgb;
	return float4(cDiff.rgb * pow(max(0, 1.0f + h2*h2), -0.25), cDiff.a);
#endif
}










// ============================================================================
// A Shader for a typical "Specular" PBR workflow.
// ============================================================================

float4 SpecularPS(float4 sc : VPOS, PBRData frg) : COLOR
{
	float3 nrmT;
	float3 nrmW;
	float3 cEmis;
	float3 cSpec;
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


	// Fetch Reflection map
	//
	if (gCfg.Refl) cSpec = tex2D(ReflS, frg.tex0.xy).rgb;
	else		   cSpec = gMtrl.reflect.rgb;


	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	//
	if (gCfg.Emis) cEmis = tex2D(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = 0;


	// ----------------------------------------------------------------------
	// Now do other calculations while textures are being fetched
	// ----------------------------------------------------------------------

	float3 camW = normalize(frg.camW);
	float3 cSun = gSun.Color;


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
	float uLC = dot(sunW, camW);
	float dLN = saturate(uLN);
	float dLH = saturate(dot(sunW, hlvW));
	float dCN = saturate(dot(camW, nrmW));
	float dHN = saturate(dot(hlvW, nrmW));

	
	if (dCN < eps) clip(-1);

	// Apply a proper curve to a texture data, modulate with material value and clamp
	fSmth = clamp(pow(abs(fSmth), gMtrl.roughness.y) * gMtrl.roughness.x, 0.0005f, 0.9999f);

	float fRgh = saturate(1.0f - fSmth);
	float fRgh3 = clamp(fRgh*fRgh*fRgh, 0.0005f, 0.9999f);

	float fSpec = cmax(cSpec);

	// ======================================================================
	// Compute Local Light Sources
	// ======================================================================

	LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, fRgh3, true);




	// ======================================================================
	// Compute Earth glow
	// ======================================================================

	float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
	float3 cAmbient = gAtmColor.rgb * max(0, angl * gGlowConst) + gSun.Ambient;

#if defined(_ENVMAP)
	if (gEnvMapEnable) {

		// ======================================================================
		// Sample Env Map
		SampleEnvMap(cEnv, dCN, fRgh, fSpec, rflW, nrmW);

		// ======================================================================
		// Sample Irradiance Map
		cAmbient = Paraboloidal_LVLH(IrradS, nrmW);

		cAmbient *= cAmbient;
		float fNight = saturate(1.0 - cSun.r);
		cAmbient = saturate(cAmbient * (1.0f + 30.0f * fNight));
	}
#endif

	//cAmbient += gSun.Ambient;
	cAmbient *= (1.0f - fSpec); // No ambient for metals

	// ======================================================================
	// Add vessel self-shadows
	// ======================================================================
#if SHDMAP > 0
	cSun *= smoothstep(0, 0.72, ComputeShadow(frg.shdH, dLN, sc));
#endif
	cSun *= (1.0f - gSun.Ambient);


	// ======================================================================
	// Main shader core SpecularPS
	// ======================================================================

	float  fD = GGX_NDF(dHN, fRgh3);
	float  fG = SchlickBeckmanGSF(dLN, dCN, fRgh);
	float  fR = DiffuseRetroReflectance(dLN, dCN, dLH, fRgh, fSpec);

	// Fresnel power 2.5 for glossy, 5.0 for rough
	float fFrs = pow(1.0f - dCN, (fRgh*2.5 + 2.5f) * gMtrl.fresnel.x) * (1.0f - gMtrl.fresnel.y);

	// Fresnel cut-off below X of fSmth
	fFrs *= saturate(0.5f - fRgh*fRgh) * 1.5f;

	// Fresnel color shift
	float3 cF = cSpec + (1.0f - cSpec) * fFrs;

	// Specular Color
	float3 cS = (fD * cF * fG) * 0.25f;	//   / (4.0f*dLN*dCN) removed to avoid division by zero, compensation in GSF

	float3 cE = cEnv * cF;

	// Attennuate diffuse color for Fresnel
	float  fA = (1.0f - fFrs);
	
	// Safeguard diffuse from over-saturation
	if (gRghnSw) fA *= (1.0f - fSpec);

	// Combine diffuse terms
	float3 zD = cDiff.rgb * fA * LightFXSq(Sq(cSun * fR * dLN) + cDiffLocal + Sq(cAmbient) + Sq(gMtrl.emissive.rgb));

	// Combine specular terms
	float3 zS = cS * (cSun * dLN) + cSpec * LightFX(cSpecLocal) * 0.5f;

	cDiff.rgb = zD + zS + cE;

	// Override material alpha to make reflections visible
	cDiff.a = saturate(cDiff.a + cmax(zS + cE));


	// ======================================================================
	// Add texture transmittance 
	// ======================================================================

	if (gCfg.Transm || gCfg.Transl) Transmittance(cDiff, uLN, uLC, frg.tex0.xy, cSun);


	// Add emission texture to output, modulate with material
	cDiff.rgb = max(cDiff.rgb, cEmis * gMtrl.emission2.rgb);

#if defined(_DEBUG)
	cDiff = cDiff * (1.0f - gColor*0.5f) + gColor;
#endif

#if defined(_LIGHTGLOW)
	return cDiff;
#else
	float3 h2 = cDiff.rgb*cDiff.rgb;
	return float4(cDiff.rgb * pow(max(0, 1.0f + h2*h2), -0.25), cDiff.a);
#endif
}