// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Metal port of Vessel.fx (included by D3D9Client.metal)
// LightBlur.hlsl is not included: nothing in the effect uses it, its scalar
// uniforms are members of Globals and its samplers would exceed the 16 effect slots.
// ==============================================================



constant float3 cLuminosity = float3(0.4f, 0.7f, 0.3f);


inline float cmax(float3 color)
{
	return max(max(color.r, color.g), color.b);
}

#include "Common.metal"

#include "PBR.metal"

#include "Metalness.metal"

// ============================================================================
// Vertex shader for physics based rendering
//
vertex PBRData AdvancedVS(MESH_VERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
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
fragment float4 AdvancedPS(PBRData frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]],
	texture2d<float> gTex3 [[texture(2)]],
	texture2d<float> gSpecMap [[texture(3)]],
	texture2d<float> gEmisMap [[texture(5)]],
	texturecube<float> gEnvMapA [[texture(6)]],
	texture2d<float> gReflMap [[texture(8)]],
	texture2d<float> gTranslMap [[texture(11)]],
	texture2d<float> gTransmMap [[texture(12)]],
	texture2d<float> gShadowMap [[texture(13)]])
{
	float4 sc = float4(floor(frg.posH.xy), 0.0f, 0.0f);
	float3 bitW;
	float3 nrmT;
	float3 cRefl;
	float3 cEmis;
	float4 cSpec;
	float4 cTex;

	float3 cDiffLocal;
	float3 cSpecLocal;

	if (g.gTextured) cTex = gTex0.sample(WrapS, frg.tex0.xy);
	else		     cTex = 1.0f;

	if (g.gOITEnable) if (cTex.a < 0.5f) discard_fragment();

	if (g.gCfg.Norm) nrmT  = gTex3.sample(Nrm0S, frg.tex0.xy).rgb;

	if (g.gCfg.Spec) cSpec = gSpecMap.sample(SpecS, frg.tex0.xy);
	else		     cSpec = g.gMtrl.specular;

	if (g.gCfg.Refl) cRefl = gReflMap.sample(ReflS, frg.tex0.xy).rgb;
	else		     cRefl = g.gMtrl.reflect.rgb;

	if (g.gCfg.Emis) cEmis = gEmisMap.sample(EmisS, frg.tex0.xy).rgb;
	else		     cEmis = 0.0f;


	float3 nrmW = frg.nrmW;
	float3 tanW = frg.tanW.xyz;
	float3 cSun = saturate(g.gSun.Color);
	float3 CamD = normalize(frg.camW);
	float3 Base = (g.gMtrl.ambient.rgb*g.gSun.Ambient) + (g.gMtrl.emissive.rgb);


	// Compute World space normal -------------------------------------------
	//
	if (g.gCfg.Norm) {
		nrmT = nrmT * 2.0f - 1.0f;
		bitW = cross(tanW, nrmW) * frg.tanW.w;
		nrmW = nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;
	}

	nrmW  = normalize(nrmW);

	float3 TnrmW = -nrmW;
	float3 RflW  = reflect(-CamD, nrmW);
	float  dLN   = saturate(-dot(g.gSun.Dir, nrmW));

	if (g.gCfg.Spec) cSpec.a *= 255.0f;

	float fRghn = log2(cSpec.a) * 0.1f;

	float fSun = pow(saturate(-dot(RflW, g.gSun.Dir)), cSpec.a) * saturate(cSpec.a);

	if (dLN == 0) fSun = 0.0f;

	if (g.gNoColor) cTex.rgb = 1.0f;


	// ----------------------------------------------------------------------
	// Add vessel self-shadows
	// ----------------------------------------------------------------------

#if SHDMAP > 0
	cSun.rgb *= ComputeShadow(g, gShadowMap, frg.shdH, dLN, sc);
#endif



	// ----------------------------------------------------------------------
	// Compute Local Light Sources
	// ----------------------------------------------------------------------

	LocalLightsEx(g, cDiffLocal, cSpecLocal, nrmW, -frg.camW, cSpec.a, false);


	cTex.rgb *= saturate(Base + g.gMtrl.diffuse.rgb * Light_fx(cDiffLocal + cSun * dLN));

	cSpec.rgb *= saturate(cSpecLocal + fSun * cSun);


	// Compute Transluciency effect --------------------------------------------------------------
	//

	if (g.gCfg.Transm || g.gCfg.Transl) {

		float4 cTransm = float4(cTex.rgb, 1.0f);

		if (g.gCfg.Transm) {
			cTransm = gTransmMap.sample(TransmS, frg.tex0.xy);
			cTransm.a *= 1024.0f;
		}

		float3 cTransl = cTex.rgb;

		if (g.gCfg.Transl) {
			cTransl = gTranslMap.sample(TranslS, frg.tex0.xy).rgb;
		}

		if (g.gTuneEnabled) {
			cTransm *= g.gTune.Transm.rgba;
			cTransl *= g.gTune.Transl.rgb;
		}

		float sunLightFromBehind = saturate(dot(g.gSun.Dir, nrmW));
		float sunSpotFromBehind = pow(saturate(dot(g.gSun.Dir, CamD)), cTransm.a);
		sunSpotFromBehind *= saturate(sunLightFromBehind * 3.0f);

		cTransl.rgb *= saturate(cSun * sunLightFromBehind);

		cTex.rgb += (1.0f - cTex.rgb) * cTransl.rgb;
		cTex.rgb += cTransm.rgb * (sunSpotFromBehind * cSun);
	}


	float fFrsl = 1.0f;
	float fInt = 0.0f;

	float fRefl = cmax(cRefl);


#if defined(_ENVMAP)


	if (g.gEnvMapEnable) {

		if (g.gFresnel) {

			fFrsl = g.gMtrl.fresnel.y;

			float3 cEnvFres = gEnvMapA.sample(EnvMapAS, RflW, level(0.0f)).rgb;

			float  dCN = saturate(dot(CamD, nrmW));

			fFrsl *= pow(1.0f - dCN, g.gMtrl.fresnel.x) * (1.0f - fRefl) * float(any(cRefl != 0.0f));

			cSpec.rgb = saturate(cSpec.rgb + fSun * fFrsl * cSun);

			cSpec.rgb = saturate(cSpec.rgb + fFrsl * cEnvFres);

			fInt = saturate(dot(cSpec.rgb, cLuminosity));

			cTex.rgb *= (1.0f - fInt);
		}

		float fLOD = (1.0f - fRghn) * 10.0f;

		float3 cEnv = gEnvMapA.sample(EnvMapAS, RflW, level(fLOD)).rgb;

		cSpec.rgb += cRefl.rgb * cEnv;
	}

#endif

	cTex.rgb *= (1.0f - fRefl);

	cTex.a = saturate(cTex.a + fInt);

	cTex.rgb += cSpec.rgb;

	cTex.rgb += cEmis;

#if defined(_DEBUG)
	cTex = cTex * (1.0f - g.gColor*0.5f) + g.gColor;
#endif

	cTex.rgb *= g.gSun.Transmission;
	cTex.rgb += g.gSun.Inscatter;

	return cTex;
}



// ============================================================================
// This is the default mesh rendering technique
//
// @technique VesselTech
// @pass P0 vs=PBR_VS ps=PBR_PS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true
// @pass P1 vs=AdvancedVS ps=AdvancedPS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true
// @pass P2 vs=FAST_VS ps=FAST_PS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true
// @pass P3 vs=FAST_VS ps=XRHUD_PS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true
// @pass P4 vs=MetalnessVS ps=MetalnessPS AlphaBlendEnable=true BlendOp=Add ZEnable=true SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=true
