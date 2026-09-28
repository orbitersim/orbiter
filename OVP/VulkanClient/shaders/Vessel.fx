// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================



const vec3 cLuminosity = vec3( 0.4, 0.7, 0.3 ); // HLSL global with a default value, never set by the client


float cmax(vec3 color)
{
	return max(max(color.r, color.g), color.b);
}

// Sun light brightness for diffuse and specular lighting
// LightBlur.glsl not included: nothing of it is used here and its constants and samplers have bindings of their own

// Incluse Light and Shadow
#include "Common.glsl"

// Must be included here
#include "PBR.fx"

// Must be included here
#include "Metalness.fx"

// ============================================================================
// Vertex shader for physics based rendering
//
PBRData AdvancedVS(MESH_VERTEX vrt)
{
	// Zero output.
	PBRData outVS; // (PBRData)0: every member is set below

	vec3 posW = (vec4(vrt.posL, 1.0f) * gW).xyz;
	vec3 nrmW = (vec4(vrt.nrmL, 0.0f) * gW).xyz;

#if SHDMAP > 0
	outVS.shdH = vec4(posW, 1.0f) * gLVP;
#endif

	outVS.nrmW = nrmW;
	outVS.tanW = vec4((vec4(vrt.tanL, 0.0f) * gW).xyz, vrt.tex0.z);
	outVS.posH = vec4(posW, 1.0f) * gVP;
	outVS.camW = -posW;
	outVS.tex0 = vrt.tex0.xy;

	return outVS;
}

#ifdef VS_AdvancedVS
layout(location = 0) out PBRData oVS;
void main() { oVS = AdvancedVS(MESH_VERTEX(iPosL, iNrmL, iTanL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif



// ============================================================================
//
#ifdef STAGE_PS
vec4 AdvancedPS(vec4 sc, PBRData frg)	// sc : VPOS
{
	vec3 bitW;
	vec3 nrmT;
	vec3 cRefl;
	vec3 cEmis;
	vec4 cSpec;
	vec4 cTex;

	vec3 cDiffLocal;
	vec3 cSpecLocal;

	if (gTextured) cTex = texture(WrapS, frg.tex0.xy);
	else		   cTex = vec4(1);

	if (gOITEnable) if (cTex.a < 0.5f) discard; // clip(-1)

	if (gCfg.Norm) nrmT  = texture(Nrm0S, frg.tex0.xy).rgb;

	if (gCfg.Spec) cSpec = texture(SpecS, frg.tex0.xy);
	else		   cSpec = gMtrl.specular;

	if (gCfg.Refl) cRefl = texture(ReflS, frg.tex0.xy).rgb;
	else		   cRefl = gMtrl.reflect.rgb;

	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	if (gCfg.Emis) cEmis = texture(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = vec3(0);


	vec3 nrmW = frg.nrmW;
	vec3 tanW = frg.tanW.xyz;
	vec3 cSun = clamp(gSun.Color, 0.0, 1.0);
	vec3 CamD = normalize(frg.camW);
	vec3 Base = (gMtrl.ambient.rgb*gSun.Ambient) + (gMtrl.emissive.rgb);


	// Compute World space normal -------------------------------------------
	//
	if (gCfg.Norm) {
		nrmT = nrmT * 2.0 - 1.0;
		bitW = cross(tanW, nrmW) * frg.tanW.w;
		nrmW = nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;
	}

	nrmW  = normalize(nrmW);

	vec3 TnrmW = -nrmW;
	vec3 RflW  = reflect(-CamD, nrmW);
	float  dLN   = clamp(-dot(gSun.Dir, nrmW), 0.0, 1.0);

	if (gCfg.Spec) cSpec.a *= 255.0f;

	// Approximate roughness
	float fRghn = log2(cSpec.a) * 0.1f;

	// Sunlight calculation
	float fSun = pow(clamp(-dot(RflW, gSun.Dir), 0.0, 1.0), cSpec.a) * clamp(cSpec.a, 0.0, 1.0);

	if (dLN == 0) fSun = 0;

	// Special alpha only texture in use
	if (gNoColor) cTex.rgb = vec3(1);


	// ----------------------------------------------------------------------
	// Add vessel self-shadows
	// ----------------------------------------------------------------------

#if SHDMAP > 0
	cSun.rgb *= ComputeShadow(frg.shdH, dLN, sc);
#endif



	// ----------------------------------------------------------------------
	// Compute Local Light Sources
	// ----------------------------------------------------------------------

	LocalLightsEx(cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);


	// Lit the diffuse texture
	cTex.rgb *= clamp(Base + gMtrl.diffuse.rgb * Light_fx(cDiffLocal + cSun * dLN), 0.0, 1.0);

	// Lit the specular surface
	cSpec.rgb *= clamp(cSpecLocal + fSun * cSun, 0.0, 1.0);


	// Compute Transluciency effect --------------------------------------------------------------
	//

	if (gCfg.Transm || gCfg.Transl) {

		vec4 cTransm = vec4(cTex.rgb, 1.0f);

		if (gCfg.Transm) {
			cTransm = texture(TransmS, frg.tex0.xy);
			cTransm.a *= 1024.0f;
		}

		vec3 cTransl = cTex.rgb;

		if (gCfg.Transl) {
			cTransl = texture(TranslS, frg.tex0.xy).rgb;
		}

		// Texture Tuning -------------------------------------------------------
		//
		if (gTuneEnabled) {
			cTransm *= gTune.Transm.rgba;
			cTransl *= gTune.Transl.rgb;
		}

		float sunLightFromBehind = clamp(dot(gSun.Dir, nrmW), 0.0, 1.0);
		float sunSpotFromBehind = pow(clamp(dot(gSun.Dir, CamD), 0.0, 1.0), cTransm.a);
		sunSpotFromBehind *= clamp(sunLightFromBehind * 3.0f, 0.0, 1.0);// Causes the transmittance (sun spot) effect to fall off at very shallow angles

		cTransl.rgb *= clamp(cSun * sunLightFromBehind, 0.0, 1.0);

		cTex.rgb += (1 - cTex.rgb) * cTransl.rgb;
		cTex.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
	}


	float fFrsl = 1.0f;
	float fInt = 0.0f;

	// Compute reflectivity
	float fRefl = cmax(cRefl);


#if defined(_ENVMAP)


	// Compute environment map/fresnel effects --------------------------------
	//
	if (gEnvMapEnable) {

		// Do we need fresnel code for this render pass ?

		if (gFresnel) {
		
			fFrsl = gMtrl.fresnel.y;

			// Get mirror reflection for fresnel
			vec3 cEnvFres = textureLod(EnvMapAS, RflW, 0).rgb;

			float  dCN = clamp(dot(CamD, nrmW), 0.0, 1.0);

			// Compute a fresnel term with compensations included
			fFrsl *= pow(1.0f - dCN, gMtrl.fresnel.x) * (1.0 - fRefl) * float(any(notEqual(cRefl, vec3(0))));

			// Sunlight reflection for fresnel material
			cSpec.rgb = clamp(cSpec.rgb + fSun * fFrsl * cSun, 0.0, 1.0);

			// Compute total reflected light with fresnel reflection
			// and accummulate in cSpec
			cSpec.rgb = clamp(cSpec.rgb + fFrsl * cEnvFres, 0.0, 1.0);

			// Compute intensity
			fInt = clamp(dot(cSpec.rgb, cLuminosity), 0.0, 1.0);

			// Attennuate diffuse surface
			cTex.rgb *= (1.0f - fInt);
		}

		// Compute LOD level for blur effect
		float fLOD = (1.0f - fRghn) * 10.0f;

		vec3 cEnv = textureLod(EnvMapAS, RflW, fLOD).rgb;

		// Compute total reflected light, accummulate in cSpec
		cSpec.rgb += cRefl.rgb * cEnv;
	}

#endif

	// Attennuate diffuse surface
	cTex.rgb *= (1.0f - fRefl);

	// Re-compute output alpha for alpha blending stage
	// NOTE: Without fresnel fInt remains zero
	cTex.a = clamp(cTex.a + fInt, 0.0, 1.0);

	// Add reflections to output
	cTex.rgb += cSpec.rgb;

	// Add emissive textures to output
	cTex.rgb += cEmis;

#if defined(_DEBUG)
	//if (gDebugHL) cTex = cTex*0.5f + gColor;
	cTex = cTex * (1 - gColor*0.5f) + gColor;
#endif

	cTex.rgb *= gSun.Transmission;
	cTex.rgb += gSun.Inscatter;

	return cTex;
}
#endif

#ifdef PS_AdvancedPS
void main() { oColor = AdvancedPS(vec4(gl_FragCoord.xy - 0.5, 0, 0), frg PS_ARGS); } // VPOS
#endif





// ============================================================================
// This is the default mesh rendering technique
//
technique VesselTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 PBR_VS();
		pixelShader = compile ps_3_0 PBR_PS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
	}

	pass P1
	{
		vertexShader = compile vs_3_0 AdvancedVS();
		pixelShader = compile ps_3_0 AdvancedPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
	}

	pass P2
	{
		vertexShader = compile vs_3_0 FAST_VS();
		pixelShader = compile ps_3_0 FAST_PS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
	}

	pass P3	// XR2 HUD PASS
	{
		vertexShader = compile vs_3_0 FAST_VS();
		pixelShader = compile ps_3_0 XRHUD_PS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
	}
	pass P4
	{
		vertexShader = compile vs_3_0 MetalnessVS();
		pixelShader = compile ps_3_0 MetalnessPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		ZEnable = true;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = true;
	}
}
