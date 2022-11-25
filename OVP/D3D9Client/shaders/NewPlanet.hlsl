// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under LGPL v2
// Copyright (C) 2022 Jarmo Nikkanen
// ============================================================================

#include "Scatter.hlsl"

struct _Light
{
	float3   position[4];         /* position in world space */
	float3   direction[4];        /* direction in world space */
	float3   diffuse[4];          /* diffuse color of light */
	float3   attenuation[4];      /* Attenuation */
	float4   param[4];            /* range, falloff, theta, phi */
};

#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3

// ----------------------------------------------------------------------------
// Vertex input layouts from Vertex buffers to vertex shader
// ----------------------------------------------------------------------------

struct TILEVERTEX					// (VERTEX_2TEX) Vertex declaration used for surface tiles and cloud layer
{
	float3 posL     : POSITION0;
	float3 normalL  : NORMAL0;
	float2 tex0     : TEXCOORD0;
	float  elev     : TEXCOORD1;
};

// ----------------------------------------------------------------------------
// Vertex Shader to Pixel Shader datafeeds
// ----------------------------------------------------------------------------

struct TileVS
{
	float4 posH     : POSITION0;
	float2 texUV    : TEXCOORD0;  // Texture coordinate
	float4 camW		: TEXCOORD1;  // Radius in .w
	float3 nrmW		: TEXCOORD2;
#if defined(_SHDMAP)
	float4 shdH     : TEXCOORD3;
#endif
};

struct CldVS
{
	float4 posH     : POSITION0;
	float2 texUV    : TEXCOORD0;  // Texture coordinate
	float3 nrmW		: TEXCOORD1;
	float3 posW		: TEXCOORD2;
};

struct HazeVS
{
	float4 posH    : POSITION0;
	float2 texUV   : TEXCOORD0;
	float3 posW	   : TEXCOORD1;
	float  alpha   : COLOR0;
};


// Note: "bool" is 32-bits in a shaders (max count 16) 
//
struct FlowControlPS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bOverlay;				// Overlay on/off	
	BOOL bShadows;				// Shadow Map on/off
	BOOL bLocals;				// Local Lights on/off
	BOOL bMicroNormals;			// Micro texture has normals
	BOOL bCloudShd;
	BOOL bMask;
	BOOL bRipples;
};

struct FlowControlVS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bSpherical;			// Ignore elevation, render as sphere
	BOOL bElevOvrl;				// ElevOverlay on/off			
};

struct PerObjectParams
{
	float4x4 mWorld;			// World Matrix
	float4x4 mLVP;				// Light-View-Projection
	float4   vSHD;				// Shadow Map Parameters
	float4   vMSc[3];			// Micro Texture offset-scale
	float4	 vTexOff;			// Texture offset-scale
	float4   vCloudOff;			// Cloud texture offset-scale
	float4   vMicroOff;			// Micro texture offset-scale
	float4   vOverlayOff;       // Overlay texture offset-scale
	float4   vOverlayCtrl[4];
	float	 fAlpha;
	float	 fBeta;
};

uniform extern PerObjectParams Prm;
uniform extern FlowControlPS Flow;
uniform extern FlowControlVS FlowVS;
uniform extern _Light Lights;			// Note: DX9 doesn't tolerate structure arrays outside FX framework
uniform extern bool Spotlight[4];


sampler tDiff;				// Diffuse texture
sampler tMask;				// Nightlights / Specular mask texture
sampler tCloud;				// 1st Cloud shadow texture
sampler tCloud2;			// 2nd Cloud shadow texture
sampler tCloudMicro;		
sampler tCloudMicroNorm;
sampler tNoise;				//
sampler	tOcean;				// Ocean Normal Map Texture
sampler	tMicroA;
sampler	tMicroB;
sampler	tMicroC;
sampler	tShadowMap;
sampler	tOverlay;
sampler	tMskOverlay;
sampler	tElvOverlay;

static const float4 vW = float4(0.34786, 0.65215, 0.65215, 0.34786);
static const float4 vP = float4(0.06944, 0.33001, 0.66999, 0.93057);

static const float3 cSky = { 0.7f, 0.9f, 1.2f };
static const float3 cHazeColor = { 1.0f, 1.0f, 1.05f };


// ---------------------------------------------------------------------------------------------------
//
float SampleShadows(float2 sp, float pd)
{
	if (sp.x < 0 || sp.y < 0) return 0.0f;	// If a sample is outside border -> fully lit
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	if (pd < 0) pd = 0;
	if (pd > 2) pd = 2;

	float2 dx = float2(Prm.vSHD[1], 0) * 1.5f;
	float2 dy = float2(0, Prm.vSHD[1]) * 1.5f;
	float  va = 0;

	sp -= dy;
	if ((tex2D(tShadowMap, sp - dx).r) > pd) va++;
	if ((tex2D(tShadowMap, sp).r) > pd) va++;
	if ((tex2D(tShadowMap, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(tShadowMap, sp - dx).r) > pd) va++;
	if ((tex2D(tShadowMap, sp).r) > pd) va++;
	if ((tex2D(tShadowMap, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(tShadowMap, sp - dx).r) > pd) va++;
	if ((tex2D(tShadowMap, sp).r) > pd) va++;
	if ((tex2D(tShadowMap, sp + dx).r) > pd) va++;

	return va * 0.1111111f;
}

// -------------------------------------------------------------------------------------------------------------
// Local light sources
//
void LocalLights(
	out float3 diff_out,
	in float3 nrmW,
	in float3 posW)
{
	diff_out = 0;

	if (!Flow.bLocals) return;
	int i;

	// Relative positions
	float3 p[4];
	[unroll] for (i = 0; i < 4; i++) p[i] = posW - Lights.position[i];

	// Square distances
	float4 sd;
	[unroll] for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	// Normalize
	sd = rsqrt(sd);
	[unroll] for (i = 0; i < 4; i++) p[i] *= sd[i];

	// Distances
	float4 dst = rcp(sd);

	// Attennuation factors
	float4 att;
	[unroll] for (i = 0; i < 4; i++) att[i] = dot(Lights.attenuation[i].xyz, float3(1.0, dst[i], dst[i] * dst[i]));

	att = rcp(att);

	// Spotlight factors
	float4 spt = 1;
	
	[unroll] for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], Lights.direction[i]) - Lights.param[i][Phi]) * Lights.param[i][Theta];
		if (!Spotlight[i]) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	// Diffuse light factors
	float4 dif;
	[unroll] for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);
	dif *= (att * spt);

	[unroll] for (i = 0; i < 4; i++) diff_out += Lights.diffuse[i].rgb * dif[i];
}


// ============================================================================
// Render SkyDome and Horizon
// ============================================================================

HazeVS HorizonVS(float3 posL : POSITION0)
{
	// Zero output.
	HazeVS outVS = (HazeVS)0;

	outVS.texUV = posL.xy*10.0;

	posL.xz *= lerp(Prm.vTexOff[0], Prm.vTexOff[1], posL.y);
	posL.y   = lerp(Prm.vTexOff[2], Prm.vTexOff[3], posL.y);

	outVS.posW = mul(float4(posL, 1.0f), Prm.mWorld).xyz;
	outVS.posH = mul(float4(outVS.posW, 1.0f), Const.mVP);

	return outVS;
}


// SkyDome Shader, Renders the sky from with-in atmosphere
//
float4 HorizonPS(HazeVS frg) : COLOR
{
	float3 uDir = normalize(frg.posW);

	SkyOut sky = GetSkyColor(uDir);
	
	float ph = dot(uDir, Const.toSun);

	float3 color = HDR(sky.ray.rgb * RayPhase(ph) + sky.mie.rgb * MiePhase(ph));
	
	return float4(color, sky.ray.a);
}


// Renders the horizon "ring" from space
//
float4 HorizonRingPS(HazeVS frg) : COLOR
{
	float3 uDir = normalize(frg.posW);
	float3 uOrt = normalize(uDir - Const.toCam * dot(uDir, Const.toCam));

	float d = dot(uDir, frg.posW);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float r = length(Const.CamPos + frg.posW);
	float q = (r - Const.PlanetRad) / Const.AtmoAlt;

	float2 uv = float2(x, q > 0 ? sqrt(q) : 0);

	float4 cRay = tex2D(tSkyRayColor, uv).rgba;
	float3 cMie = tex2D(tSkyMieColor, uv).rgb;

	float ph = dot(uDir, Const.toSun);

	float3 color = HDR(cRay.rgb * RayPhase(ph) + cMie * MiePhase(ph));

	return float4(color, cRay.a);
}



// ============================================================================
// Planet Surface Renderer
// ============================================================================

#define AUX_DIST		0	// Vertex distance
#define AUX_NIGHT		1	// Night lights intensity
#define AUX_SLOPE		2   // Terrain slope factor 0.0=flat, 1.0=sloped
#define AUX_RAYDEPTH	3   // Optical depth of a ray

TileVS TerrainVS(TILEVERTEX vrt)
{
	// Zero output.
	TileVS outVS = (TileVS)0;
	float4 vElev = 0;
	float3 vNrmW;
	
	// Apply a world transformation matrix
	float3 vPosW = mul(float4(vrt.posL, 1.0f), Prm.mWorld).xyz;
	float3 vVrt = Const.CamPos + vPosW;
	float3 vPlN = normalize(vVrt);

	if (FlowVS.bElevOvrl)
	{
		// ----------------------------------------------------------
		// Elevation Overlay
		//
		float2 vUVOvl = vrt.tex0.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;

		// Sample Elevation Map
		vElev = tex2Dlod(tElvOverlay, float4(vUVOvl, 0, 0));

		// Construct world space normal
		vNrmW = float3(vElev.xy, sqrt(saturate(1.0f - dot(vElev.xy, vElev.xy))));
		vNrmW = mul(float4(vNrmW, 0.0f), Prm.mWorld).xyz;

		// Reconstruct Elevation
		vPosW += normalize(Const.CamPos + vPosW) * (vElev.z - vrt.elev) * vElev.w;	
	}
	else {
		vNrmW = mul(float4(vrt.normalL, 0.0f), Prm.mWorld).xyz;
	}

	// Disrecard elevation and make the surface spherical
	if (FlowVS.bSpherical) {
		vPosW = (normalize(Const.CamPos + vPosW) * Const.PlanetRad) - Const.CamPos;
		vNrmW = vPlN;
	}

	outVS.posH = mul(float4(vPosW, 1.0f), Const.mVP);

#if defined(_SHDMAP)
	outVS.shdH = mul(float4(vPosW, 1.0f), Prm.mLVP);
#endif

	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = float4(-vPosW, dot(vVrt, vPlN));
	outVS.nrmW = vNrmW;
	
	return outVS;
}



bool InRange(float2 a)
{
	return (a.x > 0.0f && a.x < 1.0f) && (a.y > 0.0f && a.y < 1.0f);
}



float4 TerrainPS(TileVS frg) : COLOR
{

	float2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;
	float2 vUVWtr = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	float2 vUVCld = frg.texUV.xy * Prm.vCloudOff.zw + Prm.vCloudOff.xy;

	vUVWtr.x += Const.Time / 180.0f;

	float3 cNrm;
	float fChA = 0.0f, fChB = 0.0f;

#if defined(_RIPPLES)
	cNrm = tex2D(tOcean, vUVWtr).xyz;
#endif

	// Fetch Main Textures
	float4 cTex = tex2D(tDiff, vUVSrf);
	float4 cMsk = 0;
	if (Flow.bMask) cMsk = tex2D(tMask, vUVSrf);

#if defined(_DEVTOOLS)
	if (Flow.bOverlay) {
		float2 vUVOvl = frg.texUV.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;
		if (InRange(vUVOvl)) {
			float4 cOvl = tex2D(tOverlay, vUVOvl);
			float4 cWtr = tex2D(tMskOverlay, vUVOvl);
			cTex.rgb = lerp(cTex.rgb, cOvl.rgb, cOvl.a * Prm.vOverlayCtrl[0].rgb);
			cMsk.rgb = lerp(cMsk.rgb, cWtr.rgb, cOvl.a * Prm.vOverlayCtrl[1].rgb);
			cMsk.a = lerp(cMsk.a, cWtr.a, Prm.vOverlayCtrl[1].a);
		}
	}
#endif

#if defined(_CLOUDSHD)
	if (Flow.bCloudShd) {
		fChA = tex2D(tCloud, vUVCld).a;
		fChB = tex2D(tCloud2, vUVCld - float2(1, 0)).a;
	}
#endif

	float fShadow = 1.0f;

#if defined(_SHDMAP)
	if (Flow.bShadows) {
		frg.shdH.xyz /= frg.shdH.w;
		frg.shdH.z = 1 - frg.shdH.z;
		float2 sp = frg.shdH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f);
		float  pd = frg.shdH.z + 0.05f * Prm.vSHD[3];
		fShadow = 1.0f - SampleShadows(sp, pd);
	}
#endif

	float3 cFar, cMed, cLow;

#if defined(_MICROTEX)
	float2 UV = frg.texUV.xy;
	// Create normals
	if (Flow.bMicroNormals) {
		// Normal in .ag luminance in .b
		cFar = tex2D(tMicroC, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).agb;	// High altitude micro texture C
		cMed = tex2D(tMicroB, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).agb;	// Medimum altitude micro texture B
		cLow = tex2D(tMicroA, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).agb;	// Low altitude micro texture A
	}
	else {
		// Color in .rgb no normals
		cFar = tex2D(tMicroC, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).rgb;	// High altitude micro texture C
		cMed = tex2D(tMicroB, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).rgb;	// Medimum altitude micro texture B
		cLow = tex2D(tMicroA, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).rgb;	// Low altitude micro texture A
	}
#endif

	float3 cRfl = 0;
	float3 cSpe = 0;
	float3 cMlt = 0;
	float3 nrmW = normalize(frg.nrmW);			// Per-pixel surface normal vector
	float3 nvrW = nrmW;							// Per-pixel surface normal vector
	float3 vRay = normalize(frg.camW.xyz);		// Unit viewing ray
	float3 vVrt = Const.CamPos - frg.camW.xyz;	// Geo-centric pixel position
	float3 vPlN = normalize(vVrt);				// Planet mean normal
	float   dst = dot(vRay, frg.camW.xyz);		// Pixel to camera distance
	float   rad = frg.camW.w;					// Pixel geo-distance
	float   alt = rad - Const.PlanetRad;		// Pixel altitude over mean radius


	// Render with specular ripples and fresnel water -------------------------
	//
#if defined(_WATER)

	// Specular Mask
	float m = (1.0 - cMsk.a);
	// Camera colse to surface ?
	float fSrf = (1.0 - Const.CamSpace);

#if defined (_RIPPLES)
	// Apply specular ripples
	cNrm.xy = clamp((cNrm.xy - 0.5f) * fSrf * 5.0f, -1, 1);
	cNrm.z = cos(cNrm.x * cNrm.y * 1.570796);
	// Compute world space normal
	nrmW = (Const.vTangent * cNrm.r) + (Const.vBiTangent * cNrm.g) + (vPlN * cNrm.b);
	nrmW = lerp(nvrW, nrmW, m);
#endif

	// Compute specular reflection intensity
	float3 vRfl = reflect(-vRay, nrmW);
	float s = dot(Const.toSun, vRfl);

	cSpe = m * pow(saturate(s), max(30.0f, fSrf * 180.0f)) * max(3.0f, fSrf * 20.0f) * fShadow;

	// Apply fresnel water only if close enough to a surface
	// 
	if (!Flow.bInSpace)
	{
		// Compute Fresnel term
		float f = 1.0 - saturate(dot(vRay, nrmW));
		float f2 = f * f;
		float f4 = f2 * f2;

		cRfl = GetAmbient(vRfl) * f4 * m * fSrf;

		// Attennuate diffuse texture for fresnel refl.
		cTex.rgb *= saturate(1.0f - m * f4 * fSrf);

		float eq = (Const.HrzDst * 1.2f - dst) * 0.01 + dot(Const.toSun, Const.toCam) * Const.HrzDst * 0.2f;
		float qe = 1.0f + abs(dot(frg.camW.xyz, Const.ZeroAz));
		float qq = max(0, (eq - qe)) / (qe + eq);

		cSpe *= lerp(1.0f, min(15.0f, qq * qq * 15.0f) + 0.2f, fSrf * fSrf);
	}

#endif


	// Render with surface microtextures --------------------------------------
	//
#if defined(_MICROTEX)


	float step1 = smoothstep(15000, 3000, dst);
	step1 *= (step1 * step1);
	float3 cFnl = max(0, min(2, 1.333f * (cFar + cMed + cLow) - 1));

	// Create normals
	if (Flow.bMicroNormals)
	{
		cFnl = cFnl.bbb;

#if defined(_SOFT)
		float2 cMix = (cFar.rg + cMed.rg + cLow.rg) * 0.6666f;			// SOFT BLEND
#endif
#if defined(_MED)
		float2 cMix = (cFar.rg + 0.5f) * (cMed.rg + 0.5f) * (cLow.rg + 0.5f);	// MEDIUM BLEND
#endif
#if defined(_HARD)
		float2 cMix = cFar.rg * cMed.rg * cLow.rg * 8.0f;				// HARD BLEND
#endif

		float3 cNrm = float3((cMix - 1.0f) * 2.0f, 0) * step1;
		cNrm.z = cos(cNrm.x * cNrm.y * 1.57);

		// Approximate world space normal
		nrmW = normalize((Const.vTangent * cNrm.x) + (Const.vBiTangent * cNrm.y) + (nvrW * cNrm.z));

		// Bend the normal towards sun a bit
		nrmW = normalize(nrmW + Const.toSun * 0.06f);
	}

	// Apply luminance
	cTex.rgb *= lerp(1.0f, cFnl, step1);
#endif


	float3 cDiffLocal = 0;

#if defined(_LOCALLIGHTS)
	LocalLights(cDiffLocal, nrmW, -frg.camW.xyz);
#endif

#if defined(_NO_ATMOSPHERE)
	//
	// Do we have an atmosphere or not ?
	//
	float fTrS = saturate(dot(nvrW, Const.toSun) * 10.0f);		// Shadowing by terrain
	float fPlS = saturate(dot(vPlN, Const.toSun) * 10.0f);		// Shadowing by planet

	float fDNS = dot(nrmW, Const.toSun);
	float fDRS = dot(vRay, Const.toSun);

	float fX = pow(saturate(fDNS), 0.33f);						// Lambertian
	float fZ = pow(abs(fDRS), 5.0f) * 0.5f;						// Shadow compensation
	float fLvl = fX * (fZ + 1.0f) * Const.TrExpo;				// Bake all together

	fLvl *= (fTrS * fPlS);										// Apply shadows

	float3 color = cTex.rgb * LightFX(Const.cSun * max(fLvl, 0) * fShadow + cDiffLocal);

	return float4(pow(saturate(color), Const.TrGamma), 1.0f);		// Gamma corrention
#else

	float fShd = 1.0f;

#if defined(_CLOUDSHD)
	// Do we render cloud shadows ?
	if (Flow.bCloudShd) {
		fShd = (vUVCld.x < 1.0 ? fChA : fChB);
		fShd = saturate(1.0 - fShd * Prm.fAlpha);
	}
#endif

	float3 cNgt = 0;
	float3 cNgt2 = 0;

	float fOrbShd = 1.0f;
	float fDRS = dot(vRay, Const.toSun);
	float fDPS = dot(vPlN, Const.toSun);
	float fS = sqrt(saturate(fDPS + 0.05f));	// Day-Night scaling term 1.0 at daytime
	float3 cSun = GetSunColor(fDPS, alt) * Const.cSun * fShd * fShadow;
	cSun *= pow(saturate(dot(vPlN, Const.toSun)), 0.5f);


#if defined(_NIGHTLIGHTS)

	// Night lights ?
	float fNgt = saturate(-fDPS * 4.0f + 0.05f) * Prm.fBeta; // Night lights intensity and 'on' time

	// Why is this in _NIGHTLIGHTS ??
	fOrbShd -= (1 - fShd) * Const.CamSpace * 0.5f; // Amplify cloud shadows for orbital views

	cMsk.b = (cMsk.b > 0.15f ? cMsk.b : 0.0f); // Blue dirt filter

	cNgt = cMsk.rgb * (1 - Const.CamSpace) * fNgt; // Nightlights surface texture illumination term
	cNgt2 = cMsk.rgb * Const.CamSpace * 4.0f * fNgt; // Nightlights orbital visibility
#endif

	// Terrain with gamma correction and attennuation
	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;

	// Evaluate multiscatter approximation
	cMlt = MultiScatterApprox(vPlN) * exp(-alt * Const.iH.r * 0.5f);

	LandOut sct = GetLandView(rad, vPlN);

	float3 color = cTex.rgb * LightFX(cSun + cMlt + cDiffLocal + cNgt + float3(0.9, 0.9, 1.0) * Const.Ambient);

	// Add Reflection
	color += cRfl * fS * 0.8f;

	// Add Specular component
	color += cSpe * fShd * cSun * fS;

	// Add Haze
	color *= sct.atn.rgb;
	color += (sct.ray.rgb + sct.mie.rgb);
	//color += (sct.ray.rgb * RayPhase(-fDRS) + sct.mie.rgb * MiePhase(-fDRS)) * fOrbShd;
	color += cNgt2;

	return float4(HDR(color), 1.0f);
#endif
}






// ============================================================================
// Planet Cloud Renderer
// ============================================================================

CldVS CloudVS(TILEVERTEX vrt)
{
	// Zero output.
	CldVS outVS = (CldVS)0;

	// Apply a world transformation matrix
	float3 vPosW = mul(float4(vrt.posL, 1.0f), Prm.mWorld).xyz;
	float3 vNrmW = mul(float4(vrt.normalL, 0.0f), Prm.mWorld).xyz;

	outVS.posH = mul(float4(vPosW, 1.0f), Const.mVP);
	outVS.nrmW = vNrmW;
	outVS.posW = vPosW;
	outVS.texUV.xy = vrt.tex0.xy;						// Note: vrt.tex0 is un-used (hardcoded in Tile::CreateMesh and varies per tile)

	return outVS;
}


// ============================================================================
// 
float4 CloudPS(CldVS frg) : COLOR
{

	float2 vUVMic = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	float2 vUVTex = frg.texUV.xy;

	//float a = (tex2Dlod(NoiseTexS, float4(vUVTex,0,0)).r - 0.5f) * ATMNOISE;

	float4 cTex = tex2D(tDiff, vUVTex);
	float3 vPlN = normalize(frg.nrmW);
	float3 vRay = normalize(frg.posW);

	float dMN = dot(vPlN, Const.toSun);    // Planet mean normal sun angle

	// -----------------------------------------------
	// Cloud layer rendering for Earth
	// -----------------------------------------------

#if defined(_CLOUDMICRO)
	float4 cMic = tex2D(tCloudMicro, vUVMic);
#endif


#if defined(_CLOUDNORMALS)
#if defined(_CLOUDMICRO)

	float4 cMicNorm = tex2D(tCloudMicroNorm, vUVMic);  // Filename "cloud1_norm.dds"

	// Filter width
	float d = 2.0 / 512.0;
	float3 nrm = 0;

	float x1 = tex2D(tDiff, vUVTex + float2(-d, 0)).a;
	float x2 = tex2D(tDiff, vUVTex + float2(+d, 0)).a;
	nrm.x = (x1 * x1 - x2 * x2);

	float y1 = tex2D(tDiff, vUVTex + float2(0, -d)).a;
	float y2 = tex2D(tDiff, vUVTex + float2(0, +d)).a;
	nrm.y = (y1 * y1 - y2 * y2);

	// Blend in cloud normals only on moderately thick clouds, allowing the highest cloud tops to be smooth.
	nrm.xy = (nrm.xy + saturate((cTex.a * 10.0f) - 3.0f) * saturate(((1.0f - cTex.a) * 10.0f) - 1.0f) * (cMicNorm.rg - 0.5f)); // new

	// Increase normals contrast based on sun-earth angle.
	nrm.xyz = nrm.xyz * (1.0f + (0.5f * dMN));

	nrm.z = sqrt(1.0f - saturate(nrm.x * nrm.x + nrm.y * nrm.y));

	// Approximate world space normal from local tangent space
	nrm = normalize((Const.vTangent * nrm.x) + (Const.vBiTangent * nrm.y) + (frg.nrmW * nrm.z));

	float dCS = dot(nrm, Const.toSun); // Cloud normal sun angle

	// Brighten the lighting model for clouds, based on sun-earth angle. Twice is better.
	// Low sun angles = greater effect. No modulation leads to washed out normals at high sun angles.
	dCS = saturate((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS);
	dCS = saturate((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS);

	// With a high sun angle, don't let the dCS go below 0.2 to avoid unnaturally dark edges.
	dCS = lerp(0.2f * dMN, 1.0f, dCS);

	// Effect of normal/sun angle to color
	// Add some brightness (borrowing red channel from sunset attenuation)
	// Adding it to the sun illumination factor, taking care to keep from saturating
	cTex.rgb *= dCS + ((1.0f - dCS) * 0.2f);
#endif
#endif

#if defined(_CLOUDMICRO)
	float f = cTex.a;
	float g = lerp(1.0f, cMic.a, 1.0f - abs(dot(Const.vPolarAxis, vPlN)));
	float h = (g + 4.0f) * 0.2f;
	cTex.a = saturate(lerp(g, h, f) * f);
#endif

	float phase = dot(Const.toSun, vRay);
	float3 sc = GetSunColor(dMN, Const.CloudAlt);
	float3 color = cTex.rgb * sc * Const.cSun * 8.0f;

	LandOut sct = GetLandView(Const.CloudAlt + Const.PlanetRad, vPlN);

	float alf = pow(abs((1.0f - cTex.a) * sqrt(cTex.a)), Const.Clouds);
	float mie = pow(saturate(phase), 60.0f) * alf * 3.0f;

	color *= exp(-(Const.RayWave * sct.ray.a + Const.MieWave * sct.mie.a));
	color += sct.ray.rgb * RayPhase(phase) + mie * sc;
	
	return float4(HDR(color), saturate(cTex.a * Prm.fBeta * Prm.fBeta * (1.0f + mie)));
}








// ============================================================================
// Gas Giant Renderer
// ============================================================================

TileVS GiantVS(TILEVERTEX vrt)
{
	// Zero output.
	TileVS outVS = (TileVS)0;
	
	// Apply a world transformation matrix
	float3 vPosW = mul(float4(vrt.posL, 1.0f), Prm.mWorld).xyz;
	float3 vNrmW = mul(float4(vrt.normalL, 0.0f), Prm.mWorld).xyz;
	
	outVS.posH = mul(float4(vPosW, 1.0f), Const.mVP);
	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = float4(-vPosW, 0);
	outVS.nrmW = vNrmW;

	return outVS;
}

// ============================================================================
//
float4 GiantPS(TileVS frg) : COLOR
{

	float2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;
	
	// Fetch Main Textures
	float4 cTex = tex2D(tDiff, vUVSrf);
	
	float3 nrmW = normalize(frg.nrmW);			// Per-pixel surface normal vector
	float3 vRay = normalize(frg.camW.xyz);		// Unit viewing ray
	float3 vVrt = Const.CamPos - frg.camW.xyz;	// Geo-centric pixel position
	float3 vPlN = normalize(vVrt);				// Planet mean normal
	//float  dst = dot(vRay, frg.camW);	// Pixel to camera distance
	//float  rad = dot(vVrt, vPlN);
	//float fDRS = dot(vRay, Const.toSun);
	float fDPS = dot(vPlN, Const.toSun);

	float3 cSun = Const.cSun * saturate((fDPS + 0.1) * 5.0);

	// Terrain with gamma correction and attennuation
	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;

	float3 color = cTex.rgb * LightFX(cSun + float3(0.9, 0.9, 1.0) * Const.Ambient);

	return float4(HDR(color), 1.0f);
}


// ============================================================================
// Gas giant cloud layer renderer
// ============================================================================

float4 GiantCloudPS(CldVS frg) : COLOR
{
	float4 cTex = tex2D(tDiff, frg.texUV.xy);
	float3 vPlN = normalize(frg.nrmW);
	float3 vRay = normalize(frg.posW);
	float  fDPS = dot(vPlN, Const.toSun);    // Planet mean normal sun angle

	float3 cSun = Const.cSun * saturate((fDPS + 0.1) * 5.0);

	cTex.rgb *= LightFX(cSun + float3(1.0, 1.0, 1.0) * Const.Ambient);
	cTex.rgb = pow(saturate(cTex.rgb), Const.TrGamma) * Const.TrExpo;

	return float4(HDR(cTex.rgb), saturate(cTex.a));
}
