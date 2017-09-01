// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================



float3 cLuminosity = { 0.4, 0.7, 0.3 };


inline float cmax(float3 color)
{
	return max(max(color.r, color.g), color.b);
}

// Sun light brightness for diffuse and specular lighting
#include "LightBlur.hlsl"

// Must be included here
#include "PBR.fx"



// ============================================================================
// Vertex shader for physics based rendering
//
PBRData AdvancedVS(MESH_VERTEX vrt)
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

	// Local light sources ----------------------------------------------------
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


	// Earth "glow" -----------------------------------------------------------
	//
	if (gGlow) {
		float angl = saturate((-dot(gCameraPos, nrmW) - gProxySize) * gInvProxySize);
		outVS.cDif += gAtmColor.rgb * max(0, angl*gGlowConst);
	}

	return outVS;
}



// ============================================================================
//
float4 AdvancedPS(PBRData frg) : COLOR
{

	float  fN = 1;
	float3 bitW;
	float3 nrmT;
	float3 cRefl;
	float3 cEmis;
	float4 cSpec;
	float4 cTex;

	if (gCfg.Norm) nrmT  = tex2D(Nrm0S, frg.tex0.xy).rgb;

	if (gCfg.Spec) cSpec = tex2D(SpecS, frg.tex0.xy);
	else		   cSpec = gMtrl.specular;

	if (gTextured) cTex = tex2D(WrapS, frg.tex0.xy);
	else		   cTex = 1;

	if (gCfg.Refl) cRefl = tex2D(ReflS, frg.tex0.xy).rgb;
	else		   cRefl = gMtrl.reflect.rgb;

	// Sample emission map. (Note: Emissive materials and textures need to go different stages, material is added to light)
	if (gCfg.Emis) cEmis = tex2D(EmisS, frg.tex0.xy).rgb;
	else		   cEmis = 0;


	float3 nrmW = frg.nrmW;
	float3 tanW = frg.tanW.xyz;
	float3 cSun = saturate(gSun.Color);
	float3 CamD = normalize(frg.camW);
	float3 Base = (gMtrl.ambient.rgb*gSun.Ambient) + (gMtrl.emissive.rgb);


	// Compute World space normal ---------------------------------------------
	//
	if (gCfg.Norm) {
		nrmT = nrmT * 2.0 - 1.0;
		bitW = cross(tanW, nrmW) * frg.tanW.w;
		nrmW = nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;

#if defined(_LIGHTS)
		fN = max(dot(frg.locW.xyz, nrmW) + frg.locW.w, 0.0f);
		fN *= fN;
#endif
	}

	nrmW  = normalize(nrmW);

	float3 TnrmW = -nrmW;
	float3 RflW  = reflect(-CamD, nrmW);
	float  dLN   = saturate(-dot(gSun.Dir, nrmW));

	if (gCfg.Spec) cSpec.a *= 255.0f;

	// Approximate roughness
	float fRghn = log2(cSpec.a) * 0.1f;

	// Sunlight calculation
	float fSun = pow(saturate(-dot(RflW, gSun.Dir)), cSpec.a) * saturate(cSpec.a);

	if (dLN == 0) fSun = 0;

	// Special alpha only texture in use
	if (gNoColor) cTex.rgb = 1;

	// Lit the diffuse texture
	cTex.rgb *= saturate(Base + gMtrl.diffuse.rgb * (frg.cDif.rgb * fN + cSun * dLN));

	// Lit the specular surface
#if defined(_LIGHTS)
	cSpec.rgb *= (frg.cSpe.rgb + fSun * cSun);
#else
	cSpec.rgb *= (fSun * cSun);
#endif


	// Compute Transluciency effect -------------------------------------------
	//
	if (gCfg.Transx) {

		float4 cTransm = float4(cTex.rgb, 1.0f);

		if (gCfg.Transm) {
			cTransm = tex2D(TransmS, frg.tex0.xy);
			cTransm.a *= 1024.0f;
		}

		float3 cTransl = cTex.rgb;

		if (gCfg.Transl) {
			cTransl = tex2D(TranslS, frg.tex0.xy).rgb;
		}

		// Texture Tuning -----------------------------------------------------
		//
		if (gTuneEnabled) {
			cTransm *= gTune.Transm.rgba;
			cTransl *= gTune.Transl.rgb;
		}

		float Ts = pow(saturate(dot(gSun.Dir, CamD)), cTransm.a) * saturate(cTransm.a);
		Ts *= saturate(dLN * (-2.0f));  // Causes the transmittance effect to fall off at very shallow angles
		float3 Tdiff = Base + gMtrl.diffuse.rgb * (frg.cDif.rgb - cSun * dLN);
		cTransl.rgb *= saturate(Tdiff);

		cTex.rgb += (1 - cTex.rgb) * cTransl.rgb;
		cTex.rgb += cTransm.rgb * (Ts * cSun);
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

			if (gCfg.Frsl) fFrsl = tex2D(FrslS, frg.tex0.xy).g;
			else 		   fFrsl = gMtrl.fresnel.y;

			// Get mirror reflection for fresnel
			float3 cEnvFres = texCUBElod(EnvMapAS, float4(RflW, 0)).rgb;

			float  dCN = saturate(dot(CamD, nrmW));

			// Compute a fresnel term with compensations included
			fFrsl *= pow(1.0f - dCN, gMtrl.fresnel.x) * (1.0 - fRefl) * any(cRefl);

			// Sunlight reflection for fresnel material
			cSpec.rgb = saturate(cSpec.rgb + fSun * fFrsl * cSun);

			// Compute total reflected light with fresnel reflection
			// and accummulate in cSpec
			cSpec.rgb = saturate(cSpec.rgb + fFrsl * cEnvFres);

			// Compute intensity
			fInt = saturate(dot(cSpec.rgb, cLuminosity));

			// Attennuate diffuse surface
			cTex.rgb *= (1.0f - fInt);
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
		pixelShader  = compile ps_3_0 AdvancedPS();

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
}