// Metal port of NewPlanet.hlsl
#include "Scatter.metal"

struct _Light
{
	float3 position[4];
	float3 direction[4];
	float3 diffuse[4];
	float3 attenuation[4];
	float4 param[4];
};

#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3

#define ATMNOISE 0.25
#define GLARE_SIZE 5

struct FlowControlPS
{
	int bInSpace;
	int bBelowClouds;
	int bOverlay;
	int bShadows;
	int bLocals;
	int bMicroNormals;
	int bCloudShd;
	int bMask;
	int bRipples;
	int bMicroTex;
	int bPlanetShadow;
	int bEclipse;
	int bTexture;
};

struct FlowControlVS
{
	int bInSpace;
	int bSpherical;
	int bElevOvrl;
};

struct PerObjectParams
{
	float4x4 mWorld;
	float4x4 mLVP;
	float4 vSHD;
	float4 vMSc[3];
	float4 vTexOff;
	float4 vCloudOff;
	float4 vMicroOff;
	float4 vOverlayOff;
	float4 vOverlayCtrl[4];
	float3 vEclipse;
	float fEclipse;
	float fAlpha;
	float fBeta;
	float fTgtScale;
};

struct PlanetVSConst
{
	PerObjectParams Prm;
	AtmoParams Const;
	FlowControlVS FlowVS;
};

struct PlanetPSConst
{
	PerObjectParams Prm;
	AtmoParams Const;
	FlowControlPS Flow;
	_Light Lights;
	int Spotlight[4];
};

#define T_DIFF texture2d<float> tDiff [[texture(0)]], sampler tDiffS [[sampler(0)]]
#define T_MASK texture2d<float> tMask [[texture(1)]], sampler tMaskS [[sampler(1)]]
#define T_CLOUD texture2d<float> tCloud [[texture(2)]], sampler tCloudS [[sampler(2)]]
#define T_CLOUD2 texture2d<float> tCloud2 [[texture(3)]], sampler tCloud2S [[sampler(3)]]
#define T_MICROC texture2d<float> tMicroC [[texture(3)]], sampler tMicroCS [[sampler(3)]]
#define T_OCEAN texture2d<float> tOcean [[texture(4)]], sampler tOceanS [[sampler(4)]]
#define T_MICROA texture2d<float> tMicroA [[texture(4)]], sampler tMicroAS [[sampler(4)]]
#define T_SKYRAY texture2d<float> tSkyRayColor [[texture(4)]], sampler tSkyRayColorS [[sampler(4)]]
#define T_AMBIENT texture2d<float> tAmbient [[texture(5)]], sampler tAmbientS [[sampler(5)]]
#define T_MICROB texture2d<float> tMicroB [[texture(5)]], sampler tMicroBS [[sampler(5)]]
#define T_SKYMIE texture2d<float> tSkyMieColor [[texture(5)]], sampler tSkyMieColorS [[sampler(5)]]
#define T_SUN texture2d<float> tSun [[texture(6)]], sampler tSunS [[sampler(6)]]
#define T_SHADOWMAP texture2d<float> tShadowMap [[texture(7)]], sampler tShadowMapS [[sampler(7)]]
#define T_OVERLAY texture2d<float> tOverlay [[texture(8)]], sampler tOverlayS [[sampler(8)]]
#define T_MSKOVERLAY texture2d<float> tMskOverlay [[texture(9)]], sampler tMskOverlayS [[sampler(9)]]
#define T_NOISE texture2d<float> tNoise [[texture(10)]], sampler tNoiseS [[sampler(10)]]
#define T_LNDRAY texture2d<float> tLndRay [[texture(11)]], sampler tLndRayS [[sampler(11)]]
#define T_LNDMIE texture2d<float> tLndMie [[texture(12)]], sampler tLndMieS [[sampler(12)]]
#define T_LNDATN texture2d<float> tLndAtn [[texture(13)]], sampler tLndAtnS [[sampler(13)]]
#define T_ECLIPSE texture2d<float> tEclipse [[texture(14)]], sampler tEclipseS [[sampler(14)]]
#define T_GLARE texture2d<float> tGlare [[texture(15)]], sampler tGlareS [[sampler(15)]]
#define T_CLOUDMICRO texture2d<float> tCloudMicro [[texture(1)]], sampler tCloudMicroS [[sampler(1)]]
#define T_CLOUDMICRONORM texture2d<float> tCloudMicroNorm [[texture(2)]], sampler tCloudMicroNormS [[sampler(2)]]
#define T_ELVOVERLAY texture2d<float> tElvOverlay [[texture(0)]], sampler tElvOverlayS [[sampler(0)]]

#define LANDVIEW_ARGS tLndRay, tLndRayS, tLndMie, tLndMieS, tLndAtn, tLndAtnS

struct TILEVERTEX
{
	float3 posL [[attribute(0)]];
	float3 normalL [[attribute(2)]];
	float2 tex0 [[attribute(4)]];
	float elev [[attribute(5)]];
};

struct HAZEVERTEX
{
	float3 posL [[attribute(0)]];
};

struct TileVS
{
	float4 posH [[position]];
	float2 texUV;
	float4 camW;
	float3 nrmW;
#if defined(_SHDMAP)
	float4 shdH;
#endif
};

struct CldVS
{
	float4 posH [[position]];
	float2 texUV;
	float3 nrmW;
	float3 posW;
};

struct HazeVS
{
	float4 posH [[position]];
	float2 texUV;
	float3 posW;
	float alpha;
};

static float SampleShadows(constant PerObjectParams &Prm, TEX_P(tShadowMap), float2 sp, float pd)
{
	if (sp.x < 0 || sp.y < 0) return 0.0f;
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	if (pd < 0) pd = 0;
	if (pd > 2) pd = 2;

	float2 dx = float2(Prm.vSHD[1], 0) * 1.5f;
	float2 dy = float2(0, Prm.vSHD[1]) * 1.5f;
	float va = 0;

	sp -= dy;
	if ((tShadowMap.sample(tShadowMapS, sp - dx).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tShadowMap.sample(tShadowMapS, sp - dx).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tShadowMap.sample(tShadowMapS, sp - dx).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp).r) > pd) va++;
	if ((tShadowMap.sample(tShadowMapS, sp + dx).r) > pd) va++;

	return va * 0.1111111f;
}

static void LocalLights(constant FlowControlPS &Flow, constant _Light &Lights, constant int *Spotlight,
	thread float3 &diff_out, float3 nrmW, float3 posW)
{
	diff_out = 0;

	if (!Flow.bLocals) return;
	int i;

	float3 p[4];
	for (i = 0; i < 4; i++) p[i] = posW - Lights.position[i];

	float4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	sd = rsqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	float4 dst = 1.0f / sd;

	float4 att;
	for (i = 0; i < 4; i++) att[i] = dot(Lights.attenuation[i].xyz, float3(1.0, dst[i], dst[i] * dst[i]));

	att = 1.0f / att;

	float4 spt = 1;

	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], Lights.direction[i]) - Lights.param[i][Phi]) * Lights.param[i][Theta];
		if (!Spotlight[i]) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	float4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);
	dif *= (att * spt);

	for (i = 0; i < 4; i++) diff_out += Lights.diffuse[i].rgb * dif[i];
}

static float GetEclipse(constant FlowControlPS &Flow, CONST_P, constant PerObjectParams &Prm, TEX_P(tEclipse), float3 vVrt)
{
	if (Flow.bEclipse)
	{
		float3 b = vVrt - Const.toSun * dot(vVrt, Const.toSun);
		float x = length(Prm.vEclipse - b) * Prm.fEclipse;
		return tEclipse.sample(tEclipseS, float2(saturate(x), 0.0f)).r;
	}
	return 1.0;
}

vertex HazeVS HorizonVS(HAZEVERTEX vin [[stage_in]], constant PlanetVSConst &cb [[buffer(0)]])
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	HazeVS outVS = {};
	float3 posL = vin.posL;

	outVS.texUV = posL.xy * 10.0;

	posL.xz *= mix(Prm.vTexOff[0], Prm.vTexOff[1], posL.y);
	posL.y = mix(Prm.vTexOff[2], Prm.vTexOff[3], posL.y);

	outVS.posW = (Prm.mWorld * float4(posL, 1.0f)).xyz;
	outVS.posH = Const.mVP * float4(outVS.posW, 1.0f);

	return outVS;
}

fragment float4 HorizonPS(HazeVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_NOISE, T_SKYRAY, T_SKYMIE, T_GLARE)
{
	constant AtmoParams &Const = cb.Const;
	float fNoise = (tNoise.sample(tNoiseS, frg.texUV, level(0)).r - 0.5f) * 0.03;

	float3 uDir = normalize(frg.posW);

	SkyOut sky = GetSkyColor(Const, tSkyRayColor, tSkyRayColorS, tSkyMieColor, tSkyMieColorS, uDir);

	float ph = dot(uDir, Const.toSun);

	float2 guv = float2(dot(uDir, Const.ZeroAz), dot(uDir, Const.Up)) * GLARE_SIZE + 0.5f;
	float cGlr = tGlare.sample(tGlareS, guv).r * saturate(ph) * Const.SunVis;

	float3 color = HDR(Const, sky.ray.rgb * RayPhase(ph) + (sky.mie.rgb + 0.0008f) * MiePhase(Const, ph) * (0.75f + cGlr * Const.cGlare));

	return float4(color + fNoise, sky.ray.a);
}

fragment float4 HorizonRingPS(HazeVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_SKYRAY, T_SKYMIE, T_ECLIPSE)
{
	constant AtmoParams &Const = cb.Const;
	float3 uDir = normalize(frg.posW);
	float3 uOrt = normalize(uDir - Const.toCam * dot(uDir, Const.toCam));
	float3 vVrt = Const.CamPos + frg.posW;
	float d = dot(uDir, frg.posW);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float r = length(vVrt);
	float q = (r - Const.PlanetRad) / Const.AtmoAlt;

	float2 uv = float2(x, q > 0 ? sqrt(q) : 0);

	float4 cRay = tSkyRayColor.sample(tSkyRayColorS, uv).rgba;
	float3 cMie = tSkyMieColor.sample(tSkyMieColorS, uv).rgb;

	float ph = dot(uDir, Const.toSun);

	float3 color = HDR(Const, cRay.rgb * RayPhase(ph) + cMie * MiePhase(Const, ph));

	color *= GetEclipse(cb.Flow, Const, cb.Prm, tEclipse, tEclipseS, vVrt);

	return float4(color, cRay.a);
}

#define AUX_DIST		0
#define AUX_NIGHT		1
#define AUX_SLOPE		2
#define AUX_RAYDEPTH	3

vertex TileVS TerrainVS(TILEVERTEX vrt [[stage_in]], constant PlanetVSConst &cb [[buffer(0)]],
	T_ELVOVERLAY)
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	constant FlowControlVS &FlowVS = cb.FlowVS;

	TileVS outVS = {};
	float4 vElev = 0;
	float3 vNrmW;

	float3 vPosW = (Prm.mWorld * float4(vrt.posL, 1.0f)).xyz;
	float3 vVrt = Const.CamPos + vPosW;
	float3 vPlN = normalize(vVrt);

	if (FlowVS.bElevOvrl)
	{
		float2 vUVOvl = vrt.tex0.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;

		vElev = tElvOverlay.sample(tElvOverlayS, vUVOvl, level(0));

		vNrmW = float3(vElev.xy, sqrt(saturate(1.0f - dot(vElev.xy, vElev.xy))));
		vNrmW = (Prm.mWorld * float4(vNrmW, 0.0f)).xyz;

		vPosW += normalize(Const.CamPos + vPosW) * (vElev.z - vrt.elev) * vElev.w;
	}
	else {
		vNrmW = (Prm.mWorld * float4(vrt.normalL, 0.0f)).xyz;
	}

	if (FlowVS.bSpherical) {
		vPosW = (normalize(Const.CamPos + vPosW) * Const.PlanetRad) - Const.CamPos;
		vNrmW = vPlN;
	}

	outVS.posH = Const.mVP * float4(vPosW, 1.0f);

#if defined(_SHDMAP)
	outVS.shdH = Prm.mLVP * float4(vPosW, 1.0f);
#endif

	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = float4(-vPosW, dot(vVrt, vPlN));
	outVS.nrmW = vNrmW;

	return outVS;
}

static bool InRange(float2 a)
{
	return (a.x > 0.0f && a.x < 1.0f) && (a.y > 0.0f && a.y < 1.0f);
}

static float GGX_NDF(float dHN, float rgh)
{
	float r2 = rgh * rgh;
	float dHN2 = dHN * dHN;
	float d = (r2 * dHN2) + (1.0f - dHN2);
	return r2 / (3.14f * d * d);
}

fragment float4 TerrainPS(TileVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_DIFF, T_MASK, T_SHADOWMAP, T_OVERLAY, T_MSKOVERLAY, T_NOISE, T_SUN, T_LNDRAY, T_LNDMIE, T_LNDATN, T_ECLIPSE
#if defined(_RIPPLES)
	, T_OCEAN
#endif
#if defined(_WATER) && defined(_RIPPLES)
	, T_AMBIENT
#endif
#if defined(_CLOUDSHD)
	, T_CLOUD, T_CLOUD2
#endif
#if defined(_MICROTEX)
	, T_MICROA, T_MICROB, T_MICROC
#endif
	)
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	constant FlowControlPS &Flow = cb.Flow;

	float2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;
	float2 vUVWtr = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	float2 vUVCld = frg.texUV.xy * Prm.vCloudOff.zw + Prm.vCloudOff.xy;

	vUVWtr.x += Const.Time / 180.0f;

	float3 cNrm = float3(0.5, 0.5, 1.0);
	float fChA = 0.0f, fChB = 0.0f;

#if defined(_RIPPLES)
	if (Flow.bTexture) cNrm = tOcean.sample(tOceanS, vUVWtr).xyz;
#endif

	float4 cTex = float4(0.5, 0.5, 0.5, 1.0);
	if (Flow.bTexture) cTex = tDiff.sample(tDiffS, vUVSrf);

	float4 cMsk = float4(0, 0, 0, 1);
	if (Flow.bMask) cMsk = tMask.sample(tMaskS, vUVSrf);

#if defined(_DEVTOOLS)
	if (Flow.bOverlay) {
		float2 vUVOvl = frg.texUV.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;
		if (InRange(vUVOvl)) {
			float4 cOvl = tOverlay.sample(tOverlayS, vUVOvl);
			float4 cWtr = tMskOverlay.sample(tMskOverlayS, vUVOvl);
			cTex.rgb = mix(cTex.rgb, cOvl.rgb, cOvl.a * Prm.vOverlayCtrl[0].rgb);
			cMsk.rgb = mix(cMsk.rgb, cWtr.rgb, cOvl.a * Prm.vOverlayCtrl[1].rgb);
			cMsk.a = mix(cMsk.a, cWtr.a, Prm.vOverlayCtrl[1].a);
		}
	}
#endif

#if defined(_CLOUDSHD)
	if (Flow.bCloudShd) {
		fChA = tCloud.sample(tCloudS, vUVCld).a;
		fChB = tCloud2.sample(tCloud2S, vUVCld - float2(1, 0)).a;
	}
#endif

	float fShadow = 1.0f;

#if defined(_SHDMAP)
	if (Flow.bShadows) {
		float4 shdH = frg.shdH;
		shdH.xyz /= shdH.w;
		shdH.z = 1 - shdH.z;
		float2 sp = shdH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f);
		float pd = shdH.z + 0.05f * Prm.vSHD[3];
		fShadow = 1.0f - SampleShadows(Prm, tShadowMap, tShadowMapS, sp, pd);
	}
#endif

	float3 cFar, cMed, cLow;

#if defined(_MICROTEX)
	float2 UV = frg.texUV.xy;
	if (Flow.bMicroTex)
	{
		if (Flow.bMicroNormals) {
			cFar = tMicroC.sample(tMicroCS, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).agb;
			cMed = tMicroB.sample(tMicroBS, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).agb;
			cLow = tMicroA.sample(tMicroAS, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).agb;
		}
		else {
			cFar = tMicroC.sample(tMicroCS, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).rgb;
			cMed = tMicroB.sample(tMicroBS, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).rgb;
			cLow = tMicroA.sample(tMicroAS, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).rgb;
		}
	}
#endif

	float3 cRfl = 0;
	float3 nvrW = normalize(frg.nrmW);
	float3 vRay = normalize(frg.camW.xyz);
	float3 vVrt = Const.CamPos - frg.camW.xyz;
	float3 vPlN = normalize(vVrt);
	float3 hlvW = normalize(vRay + Const.toSun);
	float dst = dot(vRay, frg.camW.xyz);
	float rad = frg.camW.w;
	float alt = rad - Const.PlanetRad;
	float fSrf = (1.0 - Const.CamSpace);
	float fMask = (1.0 - cMsk.a);
	float fSpe = 0;
	float fAmpf = 1.0f;
	float fDRS = dot(vRay, Const.toSun);
	float fDPS = dot(vPlN, Const.toSun);

#if defined(_WATER)
#if defined(_RIPPLES)

	cNrm.xy = (cNrm.xy - 0.5f) * 2.0f;
	cNrm.z *= Const.wNrmStr;
	cNrm = normalize(cNrm);

	float3 wnrmW = (Const.vTangent * cNrm.r) + (Const.vBiTangent * cNrm.g) + (vPlN * cNrm.b);
	wnrmW = mix(nvrW, wnrmW, fMask);
	float fDWS = dot(wnrmW, Const.toSun);

	float fDCH = saturate(dot(vRay, hlvW));
	float fDCN = saturate(dot(vRay, wnrmW));
	float fDHN = dot(hlvW, wnrmW);

	float3 f = 1.0 - float3(fDCH, fDCN, fDWS);
	float3 fFresnel4 = f * f * f;
	float3 fF = (0.15f + fFresnel4 * 0.85f) * fMask * Const.wSpec;

	fSpe = GGX_NDF(fDHN, 0.1f + saturate(fDWS) * 0.1f) * fF.y;
	fSpe /= (4.0f * fDCH * max(fDWS, fDCN) + 1e-3);

	if (!Flow.bInSpace)
	{
		cRfl = GetAmbient(Const, tAmbient, tAmbientS, reflect(-vRay, wnrmW)) * fF.y * fSrf;
		cTex.rgb *= saturate(1.0f - f.y * fSrf * fMask) * saturate(1.0f - f.z * fSrf * fMask);
	}

	cTex.rgb = saturate(cTex.rgb + float3(0, 0.55, 1.0) * Const.wBrightness * fMask);

#else
	float fDHN = dot(hlvW, nvrW);
	fSpe = pow(saturate(fDHN), 60.0f) * fMask * 5.0f;
#endif
#endif

	float3 nrmW = nvrW;

#if defined(_MICROTEX)

	if (Flow.bMicroTex)
	{
		float step1 = smoothstep(15000.0f, 3000.0f, dst);
		step1 *= (step1 * step1);
		float3 cFnl = max(0.0f, min(2.0f, 1.333f * (cFar + cMed + cLow) - 1));

		if (Flow.bMicroNormals)
		{
			cFnl = cFnl.bbb;

#if defined(_SOFT)
			float2 cMix = (cFar.rg + cMed.rg + cLow.rg) * 0.6666f;
#endif
#if defined(_MED)
			float2 cMix = (cFar.rg + 0.5f) * (cMed.rg + 0.5f) * (cLow.rg + 0.5f);
			fAmpf = 2.0f;
#endif
#if defined(_HARD)
			float2 cMix = cFar.rg * cMed.rg * cLow.rg * 8.0f;
			fAmpf = 4.0f;
#endif

			float3 cNrm = float3((cMix - 1.0f) * 2.0f, 0) * step1;
			cNrm.z = cos(cNrm.x * cNrm.y * 1.57);

			nrmW = normalize((Const.vTangent * cNrm.x) + (Const.vBiTangent * cNrm.y) + (nvrW * cNrm.z));

			nrmW = normalize(nrmW + Const.toSun * 0.06f);
		}

		cTex.rgb *= mix(float3(1.0f), cFnl, step1);
	}
#endif

	float fECL = GetEclipse(Flow, Const, Prm, tEclipse, tEclipseS, vVrt);

	float3 cDiffLocal = 0;

#if defined(_LOCALLIGHTS)
	LocalLights(Flow, cb.Lights, cb.Spotlight, cDiffLocal, nrmW, -frg.camW.xyz);
#endif

#if defined(_NO_ATMOSPHERE)

	float fDNS = saturate(dot(nvrW, Const.toSun));
	float fDCN2 = saturate(dot(nvrW, Const.toCam));
	float fLvl = 2.0f * fDNS / (fDNS + fDCN2 + 0.5f);
	float fSHD = 1.0f;

	if (Flow.bPlanetShadow) {
		float palt = sqrt(saturate(1.0f - fDPS * fDPS)) * rad - Const.PlanetRad;
		fSHD = fDPS > 0 ? 1.0f : ilerp(Const.MinAlt, Const.MaxAlt, palt);
	}

	fLvl += dot(nvrW - vPlN, Const.toSun) * fLvl * Const.trLS;

	fLvl += pow(saturate(fDRS), 4.0f) * 0.3f * fDNS;

#if defined(_MICROTEX)
	fLvl += dot(nrmW - nvrW, Const.toSun) * ilerp(0.0, 0.03, fLvl) * fAmpf;
#endif

	fLvl *= fSHD;
	fLvl *= fECL;

	float3 color = cTex.rgb * LightFX(max(fLvl, 0.0f) * fShadow + cDiffLocal);
	return float4(pow(saturate(color * Const.TrExpo), Const.TrGamma), 1.0f);
#else

	float fShd = 1.0f;

#if defined(_CLOUDSHD)
	if (Flow.bCloudShd) {
		fShd = (vUVCld.x < 1.0 ? fChA : fChB);
		fShd = saturate(1.0 - fShd * Prm.fAlpha);
	}
#endif

	float3 cNgt = 0;
	float3 cNgt2 = 0;
	float fDNS = dot(nvrW, Const.toSun);

#if defined(_NIGHTLIGHTS)

	float fNgt = saturate(-fDPS * 4.0f + 0.05f) * Prm.fBeta;

	cMsk.b = (cMsk.b > 0.15f ? cMsk.b : 0.0f);

	cNgt = cMsk.rgb * (1 - Const.CamSpace) * fNgt;
	cNgt2 = cMsk.rgb * Const.CamSpace * 4.0f * fNgt;
#endif

	float fNoise = (tNoise.sample(tNoiseS, frg.texUV.xy * 4.0f * Prm.fTgtScale, level(0)).r - 0.5f) * ATMNOISE;

	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;

	float4 cAmb = AmbientApprox(Const, vPlN, false);

	LandOut sct = GetLandView(Const, LANDVIEW_ARGS, rad, vPlN);

	float3 cSun = GetSunColor(Const, tSun, tSunS, fDPS, alt);
	float3 cSF = cSun * Const.cSun;
	float fMx = max(max(cSF.r, cSF.g), cSF.b);
	cSF = fMx > 1.0 ? cSF / fMx : cSF;

	float fL = Const.trLS * 0.3f;
	float fZ = clamp(dot(nvrW - vPlN, Const.toSun) * Const.trLS, -fL, fL);
	float fX = 1.0f - pow(1.0f - saturate(fDPS), 2.0f);

	fZ = fZ > 0 ? fZ * 2.0f : fZ;

#if defined(_MICROTEX)
	float fG = dot(nrmW - nvrW, Const.toSun) * fAmpf;
#else
	float fG = 0.0f;
#endif

	float fD = mix(fX + (fG + fZ) * fX, fDPS * fDPS, fMask);

	float fM = 0.5f - fMask * 0.25f;

	float3 cA = normalize(cAmb.rgb + cSF * 4.0f) * cAmb.a * cAmb.g * fM * exp(-alt * Const.iH.r) * Const.rmI.r * 6e5 * Const.TW_Terrain;

	fShd = saturate(fShd + (1.0f - fX));

	float3 cL = cSF * fD * fShadow * fShd;

	cTex.rgb *= cL * 2.0f + (cA + cDiffLocal + Const.cAmbient * Const.Ambient) * saturate(1.0f + fG + fZ) + cNgt;

	cTex.rgb = max(float3(0, 0, 0), cTex.rgb);

	cTex.rgb += cRfl * 0.75f;

	cTex.rgb += cSun * fSpe * smoothstep(-0.001f, 0.03f, fDPS);

	float fOrbShd = 1.0f - (1.0f - fShd) * Const.CamSpace * 0.5f;

	cTex.rgb *= sct.atn.rgb;
	cTex.rgb += (sct.ray.rgb * RayPhase(-fDRS) + sct.mie.rgb * MiePhase(Const, -fDRS)) * fOrbShd * (1.0f + fNoise);

	cTex.rgb *= fECL;
	cTex.rgb += cNgt2;

	return float4(HDR(Const, cTex.rgb), 1.0f);
#endif
}

vertex CldVS CloudVS(TILEVERTEX vrt [[stage_in]], constant PlanetVSConst &cb [[buffer(0)]])
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	CldVS outVS = {};

	float3 vPosW = (Prm.mWorld * float4(vrt.posL, 1.0f)).xyz;
	float3 vNrmW = (Prm.mWorld * float4(vrt.normalL, 0.0f)).xyz;

	outVS.posH = Const.mVP * float4(vPosW, 1.0f);
	outVS.nrmW = vNrmW;
	outVS.posW = vPosW;
	outVS.texUV.xy = vrt.tex0.xy;

	return outVS;
}

fragment float4 CloudPS(CldVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_DIFF, T_ECLIPSE, T_SUN, T_LNDRAY, T_LNDMIE, T_LNDATN
#if defined(_CLOUDMICRO)
	, T_CLOUDMICRO
#if defined(_CLOUDNORMALS)
	, T_CLOUDMICRONORM
#endif
#endif
	)
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	constant FlowControlPS &Flow = cb.Flow;

	float2 vUVTex = frg.texUV.xy;
	float4 cTex = tDiff.sample(tDiffS, vUVTex);
	float3 vRay;
	float3 vPxl;
	float dRC;
	float fNrm = 1.0f;

	if (Flow.bBelowClouds) {
		float rRef = Const.PlanetRad + Const.smi * 0.5f;
		float3 vRef = Const.toCam * rRef;
		vRay = normalize(Const.toCam * (Const.CamRad - rRef) + frg.posW);
		dRC = dot(vRay, Const.toCam);
		float fEca2 = 1.0f - dRC * dRC;
		float fD = Const.smi * rsqrt(1.0f - Const.ecc * Const.ecc * fEca2);
		vPxl = vRef + vRay * fD;
	}
	else {
		vRay = normalize(frg.posW);
		dRC = dot(vRay, Const.toCam);
		vPxl = Const.CamPos + frg.posW;
	}

	float3 vPlN = normalize(vPxl);
	float3 vVrt = Const.CamPos + frg.posW.xyz;
	float3 nrm = vPlN;

	float dRS = dot(vRay, Const.toSun);
	float dMNus = dot(vPlN, Const.toSun);
	float dMN = saturate(dMNus);
	float fPxR = dot(vPxl, vPlN);
	float fPxA = fPxR - Const.PlanetRad;

	if (!Flow.bBelowClouds) fPxA = Const.CloudAlt;

#if defined(_CLOUDMICRO)
	float2 vUVMic = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	float4 cMic = tCloudMicro.sample(tCloudMicroS, vUVMic);
#endif

#if defined(_CLOUDNORMALS)
#if defined(_CLOUDMICRO)

	float4 cMicNorm = tCloudMicroNorm.sample(tCloudMicroNormS, vUVMic);

	float d = 2.0 / 512.0;

	float x1 = tDiff.sample(tDiffS, vUVTex + float2(-d, 0)).a;
	float x2 = tDiff.sample(tDiffS, vUVTex + float2(+d, 0)).a;
	nrm.x = (x1 * x1 - x2 * x2);

	float y1 = tDiff.sample(tDiffS, vUVTex + float2(0, -d)).a;
	float y2 = tDiff.sample(tDiffS, vUVTex + float2(0, +d)).a;
	nrm.y = (y1 * y1 - y2 * y2);

	nrm.xy = (nrm.xy + saturate((cTex.a * 10.0f) - 3.0f) * saturate(((1.0f - cTex.a) * 10.0f) - 1.0f) * (cMicNorm.rg - 0.5f));

	nrm.xyz = nrm.xyz * (1.0f + (0.5f * dMN));

	nrm.z = sqrt(1.0f - saturate(nrm.x * nrm.x + nrm.y * nrm.y));

	nrm = normalize((Const.vTangent * nrm.x) + (Const.vBiTangent * nrm.y) + (vPlN * nrm.z));

	float dCS = dot(nrm, Const.toSun);

	dCS = saturate((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS);
	dCS = saturate((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS);

	dCS = mix(0.2f * dMN, 1.0f, dCS);

	fNrm = dCS + ((1.0f - dCS) * 0.2f);
#endif
#endif

#if defined(_CLOUDMICRO)
	float f = cTex.a;
	float g = mix(1.0f, cMic.a, 1.0f - abs(dot(Const.vPolarAxis, vPlN)));
	float h = (g + 4.0f) * 0.2f;
	cTex.a = saturate(mix(g, h, f) * f);
#endif

	float fECL = GetEclipse(Flow, Const, Prm, tEclipse, tEclipseS, vVrt);

	if (Flow.bBelowClouds)
	{
		float3 cSun = GetSunColor(Const, tSun, tSunS, dMNus, fPxA);

		float4 cMlt = AmbientApprox(Const, vPlN);

		cSun *= saturate(dRS + 1.3f);
		float fPh = pow(saturate(1.0f - dRC), 32.0f) * pow(saturate(dRS), 10.0f);
		cSun *= 1.0f + fPh * 8.0f;

		cSun *= Const.cSun * fNrm;
		cSun *= Const.Clouds;
		cSun += cMlt.rgb * cMlt.a * 0.2f;

		LandOut sct = GetLandView(Const, LANDVIEW_ARGS, fPxA + Const.PlanetRad, vPlN);

		cTex.rgb *= cSun;
		cTex.rgb *= sct.atn.rgb;
		cTex.rgb += sct.ray.rgb * 2.0f;
		cTex.rgb *= fECL;

		return float4(HDR(Const, cTex.rgb), saturate(cTex.a));
	}
	else {
		float3 cSun = GetSunColor(Const, tSun, tSunS, dMN, fPxA);

		float4 cAmb = AmbientApprox(Const, dMNus);
		float3 cMSC = Const.RayWave * Const.RayWave * Const.Clouds;

		cSun = sqrt(cMSC * cMSC + cSun * cSun * fNrm) * cAmb.a;

		LandOut sct = GetLandView(Const, LANDVIEW_ARGS, fPxA + Const.PlanetRad, vPlN);

		cTex.rgb *= cSun;
		cTex.rgb *= sct.atn.rgb;
		cTex.rgb += sct.ray.rgb;
		cTex.rgb *= fECL;

		return float4(sqr(HDR(Const, cTex.rgb * 4.0f)), cTex.a * cAmb.a * cAmb.a);
	}
}

vertex TileVS GiantVS(TILEVERTEX vrt [[stage_in]], constant PlanetVSConst &cb [[buffer(0)]])
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;
	TileVS outVS = {};

	float3 vPosW = (Prm.mWorld * float4(vrt.posL, 1.0f)).xyz;
	float3 vNrmW = (Prm.mWorld * float4(vrt.normalL, 0.0f)).xyz;

	outVS.posH = Const.mVP * float4(vPosW, 1.0f);
	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = float4(-vPosW, 0);
	outVS.nrmW = vNrmW;

	return outVS;
}

fragment float4 GiantPS(TileVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_DIFF, T_ECLIPSE)
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;

	float2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;

	float4 cTex = tDiff.sample(tDiffS, vUVSrf);

	float3 nrmW = normalize(frg.nrmW);
	float3 vRay = normalize(frg.camW.xyz);
	float3 vVrt = Const.CamPos - frg.camW.xyz;
	float3 vPlN = normalize(vVrt);
	float fDPS = dot(vPlN, Const.toSun);
	float3 cSun = saturate((fDPS + 0.1) * 5.0);

	cSun *= GetEclipse(cb.Flow, Const, Prm, tEclipse, tEclipseS, vVrt);

	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;

	float3 color = cTex.rgb * LightFX(cSun + float3(0.9, 0.9, 1.0) * Const.Ambient);

	return float4(HDR(Const, color), 1.0f);
}

fragment float4 GiantCloudPS(CldVS frg [[stage_in]], constant PlanetPSConst &cb [[buffer(0)]],
	T_DIFF, T_ECLIPSE)
{
	constant PerObjectParams &Prm = cb.Prm;
	constant AtmoParams &Const = cb.Const;

	float4 cTex = tDiff.sample(tDiffS, frg.texUV.xy);
	float3 vPlN = normalize(frg.nrmW);
	float3 vRay = normalize(frg.posW);
	float3 vVrt = Const.CamPos + frg.posW.xyz;
	float fDPS = dot(vPlN, Const.toSun);

	float3 cSun = saturate((fDPS + 0.1) * 5.0);

	float fECL = GetEclipse(cb.Flow, Const, Prm, tEclipse, tEclipseS, vVrt);

	cTex.rgb *= LightFX(cSun + float3(1.0, 1.0, 1.0) * Const.Ambient);
	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;
	cTex.rgb *= fECL;

	return float4(HDR(Const, cTex.rgb), saturate(cTex.a));
}
