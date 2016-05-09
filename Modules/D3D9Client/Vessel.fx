// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#define fDiffuseFactor  0.32	// Diffuse surface multiplier  1 / PI

// Sun light brightness for diffuse and specular lighting
#include "LightBlur.hlsl"

// Must be included here
#include "PBR.fx"


// ========================================================================================================================
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
float4 AdvancedPS(PBRData frg) : COLOR
{
	
	float  fN = 1;
	float4 cRefl;
	float3 bitW;
	float4 cTex = tex2D(WrapS, frg.tex0.xy);
	float4 cSpec = gMtrl.specular;
	float3 nrmW = frg.nrmW;
	float3 tanW = frg.tanW.xyz;
	float3 cSun = saturate(gSun.Color) * fSunIntensity;

	if (gCfg.Norm) {
	
		float3 nrmT = tex2D(Nrm0S, frg.tex0.xy).rgb * 2.0 - 1.0;       //Sampler for R8G8B8, DXT1
		nrmT.z = cos(nrmT.x * nrmT.y * 1.570796f);
		bitW = cross(tanW, nrmW) * frg.tanW.w;
		nrmW = nrmW*nrmT.z + tanW*nrmT.x + bitW*nrmT.y;

#if defined(_LIGHTS)
		fN = max(dot(frg.locW.xyz, nrmW) + frg.locW.w, 0.0f);
		fN *= fN;
#endif
	}
	
	nrmW = normalize(nrmW);

	float3 TnrmW = -nrmW;
	float3 CamD  = normalize(frg.camW);
	float3 RflW  = reflect(-CamD, nrmW);
	float  dLN   = -dot(gSun.Dir, nrmW);
	
	if (gCfg.Spec) {
		cSpec = tex2D(SpecS, frg.tex0.xy);
		cSpec.a *= 255.0f;
	}

	float4 cTransm = float4(cTex.rgb, 1.0f);

	if (gCfg.Transm) {
		cTransm = tex2D(TransmS, frg.tex0.xy);
		cTransm.a *= 1024.0f;
	}

	float3 cTransl = cTex.rgb;

	if (gCfg.Transl) {
		cTransl = tex2D(TranslS, frg.tex0.xy).rgb;
	}

	// Sunlight calculation
	float s = pow(saturate(-dot(RflW, gSun.Dir)), cSpec.a) * saturate(cSpec.a);

	if (dLN == 0) s = 0;

	float3 Base = (gMtrl.ambient.rgb*gSun.Ambient) + (gMtrl.emissive.rgb);
	float3 diff = Base + gMtrl.diffuse.rgb * (frg.cDif.rgb * fN + cSun * dLN * fDiffuseFactor);
	
	cTex.rgb *= saturate(diff);

#if defined(_LIGHTS)
	float3 cTot = cSpec.rgb * (frg.cSpe.rgb + s * cSun);
#else
	float3 cTot = cSpec.rgb * (s * cSun);
#endif

	if (gCfg.Transx) {

		float Ts = pow(saturate(dot(gSun.Dir, CamD)), cTransm.a) * saturate(cTransm.a);

		Ts *= saturate(dLN * (-2.0f));  // Causes the transmittance effect to fall off at very shallow angles

		float3 Tdiff = Base + gMtrl.diffuse.rgb * (frg.cDif.rgb - cSun * dLN * fDiffuseFactor);
	
		cTransl.rgb *= saturate(Tdiff);// * 0.5f;

		cTex.rgb += (1 - cTex.rgb) * cTransl.rgb;

		cTot += cTransm.rgb * (Ts * cSun * fDiffuseFactor);
	}

#if defined(_ENVMAP)

	if (gEnvMapEnable) {

		if (gCfg.Refl) {
			cRefl = tex2D(ReflS, frg.tex0.xy);
			cRefl.a = max(cRefl.r, max(cRefl.g, cRefl.b));
		}
		else cRefl = gMtrl.reflect;

		float fresnel = gMtrl.fresnel.y * pow(1.0f - saturate(dot(CamD, nrmW)), gMtrl.fresnel.z);

		cRefl = saturate(cRefl + (cRefl.a>0)*fresnel);

		cTex.rgb *= (1.0f - cRefl.a);
		cTot.rgb += cRefl.rgb * texCUBE(EnvMapAS, RflW).rgb;
	}
#endif

	cTex.rgb += cTot.rgb;								// Apply reflections

	if (gCfg.Emis) cTex.rgb += tex2D(EmisS, frg.tex0.xy).rgb;

#if defined(_DEBUG)	
	if (gDebugHL) cTex = cTex*0.5f + gColor;
#endif

	cTex.a = 1.0f;

	return cTex;
}





// ========================================================================================================================
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