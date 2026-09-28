
// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under LGPL v2
// Copyright (C) 2022-2026 Jarmo Nikkanen
// ============================================================================

#include "Scatter.glsl"

struct _Light
{
	vec3   position[4];         /* position in world space */
	vec3   direction[4];        /* direction in world space */
	vec3   diffuse[4];          /* diffuse color of light */
	vec3   attenuation[4];      /* Attenuation */
	vec4   param[4];            /* range, falloff, theta, phi */
};

#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3

#define ATMNOISE 0.25
#define GLARE_SIZE 5	// Larger value -> smaller

// ----------------------------------------------------------------------------
// Vertex input layouts from Vertex buffers to vertex shader
// ----------------------------------------------------------------------------

struct TILEVERTEX					// (VERTEX_2TEX) Vertex declaration used for surface tiles and cloud layer
{
	vec3 posL;     // POSITION0
	vec3 normalL;  // NORMAL0
	vec2 tex0;     // TEXCOORD0
	float  elev;   // TEXCOORD1
};

// ----------------------------------------------------------------------------
// Vertex Shader to Pixel Shader datafeeds
// ----------------------------------------------------------------------------

struct TileVS
{
	vec4 posH;     // POSITION0
	vec2 texUV;    // TEXCOORD0  // Texture coordinate
	vec4 camW;     // TEXCOORD1  // Radius in .w
	vec3 nrmW;     // TEXCOORD2
#if defined(_SHDMAP)
	vec4 shdH;     // TEXCOORD3
#endif
};

struct CldVS
{
	vec4 posH;     // POSITION0
	vec2 texUV;    // TEXCOORD0  // Texture coordinate
	vec3 nrmW;     // TEXCOORD1
	vec3 posW;     // TEXCOORD2
};

struct HazeVS
{
	vec4 posH;     // POSITION0
	vec2 texUV;    // TEXCOORD0
	vec3 posW;     // TEXCOORD1
	float  alpha;  // COLOR0
};


// Note: "bool" is 32-bits in a shaders (max count 16) 
//
struct FlowControlPS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bBelowClouds;			// Camera is below cloud layer
	BOOL bOverlay;				// Overlay on/off	
	BOOL bShadows;				// Shadow Map on/off
	BOOL bLocals;				// Local Lights on/off
	BOOL bMicroNormals;			// Micro texture has normals
	BOOL bCloudShd;				// Cloud shadow textures valid and enabled
	BOOL bMask;					// Nightlights/water mask texture is enabled
	BOOL bRipples;				// Water riples texture is enabled
	BOOL bMicroTex;				// Micro textures exists and enabled
	BOOL bPlanetShadow;			// Use spherical approximation for shadow
	BOOL bEclipse;				// Eclipse is occuring
	BOOL bTexture;				// Surface texture exists
};

struct FlowControlVS
{
	BOOL bInSpace;				// Camera in space (not in atmosphere)
	BOOL bSpherical;			// Ignore elevation, render as sphere
	BOOL bElevOvrl;				// ElevOverlay on/off			
};

struct PerObjectParams
{
	mat4 mWorld;			// World Matrix
	mat4 mLVP;				// Light-View-Projection
	vec4   vSHD;				// Shadow Map Parameters
	vec4   vMSc[3];			// Micro Texture offset-scale
	vec4	 vTexOff;			// Texture offset-scale
	vec4   vCloudOff;			// Cloud texture offset-scale
	vec4   vMicroOff;			// Micro texture offset-scale
	vec4   vOverlayOff;       // Overlay texture offset-scale
	vec4   vOverlayCtrl[4];
	vec3	 vEclipse;			// Eclipse caster position (geocentric)
	float	 fEclipse;			// Eclipse data addressing scale factor. (to access tExlipse)
	float	 fAlpha;
	float	 fBeta;
	float	 fTgtScale;
};

// uniform extern: vertex shader constants at binding 0 (the pixel shaders read Prm too), pixel shader constants at 2 (Scatter.glsl's are 1)
layout(binding = 0, row_major, scalar) uniform PlanetVSBlock { PerObjectParams Prm; FlowControlVS FlowVS; };
layout(binding = 2, row_major, scalar) uniform PlanetPSBlock {
	FlowControlPS Flow;
	_Light Lights;			// Note: DX9 doesn't tolerate structure arrays outside FX framework
	BOOL Spotlight[4];
};


layout(binding = 13) uniform sampler2D tDiff;				// Diffuse texture
layout(binding = 14) uniform sampler2D tMask;				// Nightlights / Specular mask texture
layout(binding = 15) uniform sampler2D tCloud;				// 1st Cloud shadow texture
layout(binding = 16) uniform sampler2D tCloud2;			// 2nd Cloud shadow texture
layout(binding = 17) uniform sampler2D tCloudMicro;		
layout(binding = 18) uniform sampler2D tCloudMicroNorm;
layout(binding = 19) uniform sampler2D tNoise;				//
layout(binding = 20) uniform sampler2D	tOcean;				// Ocean Normal Map Texture
layout(binding = 21) uniform sampler2D	tMicroA;
layout(binding = 22) uniform sampler2D	tMicroB;
layout(binding = 23) uniform sampler2D	tMicroC;
layout(binding = 24) uniform sampler2D tGlare;
layout(binding = 25) uniform sampler2D	tShadowMap;
layout(binding = 26) uniform sampler2D	tOverlay;
layout(binding = 27) uniform sampler2D	tMskOverlay;
layout(binding = 28) uniform sampler2D	tElvOverlay;
layout(binding = 29) uniform sampler2D	tEclipse;




// ---------------------------------------------------------------------------------------------------
//
float SampleShadows(vec2 sp, float pd)
{
	if (sp.x < 0 || sp.y < 0) return 0.0f;	// If a sample is outside border -> fully lit
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	if (pd < 0) pd = 0;
	if (pd > 2) pd = 2;

	vec2 dx = vec2(Prm.vSHD[1], 0) * 1.5f;
	vec2 dy = vec2(0, Prm.vSHD[1]) * 1.5f;
	float  va = 0;

	sp -= dy;
	if ((texture(tShadowMap, sp - dx).r) > pd) va++;
	if ((texture(tShadowMap, sp).r) > pd) va++;
	if ((texture(tShadowMap, sp + dx).r) > pd) va++;
	sp += dy;
	if ((texture(tShadowMap, sp - dx).r) > pd) va++;
	if ((texture(tShadowMap, sp).r) > pd) va++;
	if ((texture(tShadowMap, sp + dx).r) > pd) va++;
	sp += dy;
	if ((texture(tShadowMap, sp - dx).r) > pd) va++;
	if ((texture(tShadowMap, sp).r) > pd) va++;
	if ((texture(tShadowMap, sp + dx).r) > pd) va++;

	return va * 0.1111111f;
}

// -------------------------------------------------------------------------------------------------------------
// Local light sources
//
void LocalLights(
	out vec3 diff_out,
	in vec3 nrmW,
	in vec3 posW)
{
	diff_out = vec3(0);

	if (!Flow.bLocals) return;
	int i;

	// Relative positions
	vec3 p[4];
	for (i = 0; i < 4; i++) p[i] = posW - Lights.position[i];

	// Square distances
	vec4 sd;
	for (i = 0; i < 4; i++) sd[i] = dot(p[i], p[i]);

	// Normalize
	sd = inversesqrt(sd);
	for (i = 0; i < 4; i++) p[i] *= sd[i];

	// Distances
	vec4 dst = 1.0 / sd;

	// Attennuation factors
	vec4 att;
	for (i = 0; i < 4; i++) att[i] = dot(Lights.attenuation[i].xyz, vec3(1.0, dst[i], dst[i] * dst[i]));

	att = 1.0 / att;

	// Spotlight factors
	vec4 spt = vec4(1);
	
	for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], Lights.direction[i]) - Lights.param[i][Phi]) * Lights.param[i][Theta];
		if (!Spotlight[i]) spt[i] = 1.0f;
	}

	spt = clamp(spt, 0.0, 1.0);

	// Diffuse light factors
	vec4 dif;
	for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = clamp(dif, 0.0, 1.0);
	dif *= (att * spt);

	for (i = 0; i < 4; i++) diff_out += Lights.diffuse[i].rgb * dif[i];
}


// Render Eclipse ------------------------------------------------------------
//
float GetEclipse(vec3 vVrt)
{
	if (Flow.bEclipse)
	{
		vec3 b = vVrt - Const.toSun * dot(vVrt, Const.toSun); // Flatten
		float  x = length(Prm.vEclipse - b) * Prm.fEclipse;
		return texture(tEclipse, vec2(clamp(x, 0.0, 1.0), 0.5)).r;	// tex1D: the 512x1 table is a 2D texture here
	}
	return 1.0;
}
	


// ============================================================================
// Render SkyDome and Horizon
// ============================================================================

HazeVS HorizonVS(vec3 posL)	// posL : POSITION0
{
	// Zero output.
	HazeVS outVS = HazeVS(vec4(0), vec2(0), vec3(0), 0.0);

	outVS.texUV = posL.xy*10.0;

	posL.xz *= mix(Prm.vTexOff[0], Prm.vTexOff[1], posL.y);
	posL.y   = mix(Prm.vTexOff[2], Prm.vTexOff[3], posL.y);

	outVS.posW = (vec4(posL, 1.0f) * Prm.mWorld).xyz;
	outVS.posH = vec4(outVS.posW, 1.0f) * Const.mVP;

	return outVS;
}

#ifdef VS_HorizonVS
layout(location = 0) in vec3 iPosL;
layout(location = 0) out HazeVS oVS;
void main() { oVS = HorizonVS(iPosL); gl_Position = oVS.posH; }
#endif


// SkyDome Shader, Renders the sky from with-in atmosphere
//
vec4 HorizonPS(HazeVS frg)
{
	float fNoise = (textureLod(tNoise, frg.texUV, 0.0).r - 0.5f) * 0.03;

	vec3 uDir = normalize(frg.posW);

	SkyOut sky = GetSkyColor(uDir);
	
	float ph = dot(uDir, Const.toSun);

	vec2  guv = vec2(dot(uDir, Const.ZeroAz), dot(uDir, Const.Up)) * GLARE_SIZE + 0.5f;
	float  cGlr = texture(tGlare, guv).r * clamp(ph, 0.0, 1.0) * Const.SunVis;
	
	vec3 color = HDR(sky.ray.rgb * RayPhase(ph) + (sky.mie.rgb + 0.0008f) * MiePhase(ph) * (0.75f + cGlr * Const.cGlare));

	return vec4(color + fNoise, sky.ray.a);
}

#ifdef PS_HorizonPS
layout(location = 0) in HazeVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = HorizonPS(frg); }
#endif


// Renders the horizon "ring" from space
//
vec4 HorizonRingPS(HazeVS frg)
{
	vec3 uDir = normalize(frg.posW);
	vec3 uOrt = normalize(uDir - Const.toCam * dot(uDir, Const.toCam));
	vec3 vVrt = Const.CamPos + frg.posW;
	float d = dot(uDir, frg.posW);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float r = length(vVrt);
	float q = (r - Const.PlanetRad) / Const.AtmoAlt;

	vec2 uv = vec2(x, q > 0 ? sqrt(q) : 0.0);

	vec4 cRay = texture(tSkyRayColor, uv).rgba;
	vec3 cMie = texture(tSkyMieColor, uv).rgb;

	float ph = dot(uDir, Const.toSun);

	vec3 color = HDR(cRay.rgb * RayPhase(ph) + cMie * MiePhase(ph));

	color *= GetEclipse(vVrt);

	return vec4(color, cRay.a);
}

#ifdef PS_HorizonRingPS
layout(location = 0) in HazeVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = HorizonRingPS(frg); }
#endif




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
	TileVS outVS = TileVS(vec4(0), vec2(0), vec4(0), vec3(0)
#if defined(_SHDMAP)
		, vec4(0)
#endif
	);
	vec4 vElev = vec4(0);
	vec3 vNrmW;
	
	// Apply a world transformation matrix
	vec3 vPosW = (vec4(vrt.posL, 1.0f) * Prm.mWorld).xyz;
	vec3 vVrt = Const.CamPos + vPosW;
	vec3 vPlN = normalize(vVrt);

	if (FlowVS.bElevOvrl)
	{
		// ----------------------------------------------------------
		// Elevation Overlay
		//
		vec2 vUVOvl = vrt.tex0.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;

		// Sample Elevation Map
		vElev = textureLod(tElvOverlay, vUVOvl, 0.0);

		// Construct world space normal
		vNrmW = vec3(vElev.xy, sqrt(clamp(1.0f - dot(vElev.xy, vElev.xy), 0.0, 1.0)));
		vNrmW = (vec4(vNrmW, 0.0f) * Prm.mWorld).xyz;

		// Reconstruct Elevation
		vPosW += normalize(Const.CamPos + vPosW) * (vElev.z - vrt.elev) * vElev.w;	
	}
	else {
		vNrmW = (vec4(vrt.normalL, 0.0f) * Prm.mWorld).xyz;
	}

	// Disrecard elevation and make the surface spherical
	if (FlowVS.bSpherical) {
		vPosW = (normalize(Const.CamPos + vPosW) * Const.PlanetRad) - Const.CamPos;
		vNrmW = vPlN;
	}

	outVS.posH = vec4(vPosW, 1.0f) * Const.mVP;

#if defined(_SHDMAP)
	outVS.shdH = vec4(vPosW, 1.0f) * Prm.mLVP;
#endif

	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = vec4(-vPosW, dot(vVrt, vPlN));
	outVS.nrmW = vNrmW;
	
	return outVS;
}

#ifdef VS_TerrainVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out TileVS oVS;
void main() { oVS = TerrainVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev)); gl_Position = oVS.posH; }
#endif



bool InRange(vec2 a)
{
	return (a.x > 0.0f && a.x < 1.0f) && (a.y > 0.0f && a.y < 1.0f);
}

float GGX_NDF(float dHN, float rgh)
{
	float r2 = rgh * rgh;
	float dHN2 = dHN * dHN;
	float d = (r2 * dHN2) + (1.0f - dHN2);
	return r2 / (3.14f * d * d);
}


vec4 TerrainPS(TileVS frg)
{

	vec2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;
	vec2 vUVWtr = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	vec2 vUVCld = frg.texUV.xy * Prm.vCloudOff.zw + Prm.vCloudOff.xy;

	vUVWtr.x += Const.Time / 180.0f;

	vec3 cNrm = vec3(0.5, 0.5, 1.0);
	float fChA = 0.0f, fChB = 0.0f;

#if defined(_RIPPLES)
	if (Flow.bTexture) cNrm = texture(tOcean, vUVWtr).xyz;
#endif

	// Fetch Main Textures
	vec4 cTex = vec4(0.5, 0.5, 0.5, 1.0);
	if (Flow.bTexture) cTex = texture(tDiff, vUVSrf);

	vec4 cMsk = vec4(0, 0, 0, 1);
	if (Flow.bMask) cMsk = texture(tMask, vUVSrf);

#if defined(_DEVTOOLS)
	if (Flow.bOverlay) {
		vec2 vUVOvl = frg.texUV.xy * Prm.vOverlayOff.zw + Prm.vOverlayOff.xy;
		if (InRange(vUVOvl)) {
			vec4 cOvl = texture(tOverlay, vUVOvl);
			vec4 cWtr = texture(tMskOverlay, vUVOvl);
			cTex.rgb = mix(cTex.rgb, cOvl.rgb, cOvl.a * Prm.vOverlayCtrl[0].rgb);
			cMsk.rgb = mix(cMsk.rgb, cWtr.rgb, cOvl.a * Prm.vOverlayCtrl[1].rgb);
			cMsk.a = mix(cMsk.a, cWtr.a, Prm.vOverlayCtrl[1].a);
		}
	}
#endif

#if defined(_CLOUDSHD)
	if (Flow.bCloudShd) {
		fChA = texture(tCloud, vUVCld).a;
		fChB = texture(tCloud2, vUVCld - vec2(1, 0)).a;
	}
#endif

	float fShadow = 1.0f;

#if defined(_SHDMAP)
	if (Flow.bShadows) {
		frg.shdH.xyz /= frg.shdH.w;
		frg.shdH.z = 1 - frg.shdH.z;
		vec2 sp = frg.shdH.xy * vec2(0.5f, -0.5f) + vec2(0.5f, 0.5f);
		float  pd = frg.shdH.z + 0.05f * Prm.vSHD[3];
		fShadow = 1.0f - SampleShadows(sp, pd);
	}
#endif

	vec3 cFar, cMed, cLow;

#if defined(_MICROTEX)
	vec2 UV = frg.texUV.xy;
	// Create normals
	if (Flow.bMicroTex)
	{
		if (Flow.bMicroNormals) {
			// Normal in .ag luminance in .b
			cFar = texture(tMicroC, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).agb;	// High altitude micro texture C
			cMed = texture(tMicroB, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).agb;	// Medimum altitude micro texture B
			cLow = texture(tMicroA, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).agb;	// Low altitude micro texture A
		}
		else {
			// Color in .rgb no normals
			cFar = texture(tMicroC, UV * Prm.vMSc[2].zw + Prm.vMSc[2].xy).rgb;	// High altitude micro texture C
			cMed = texture(tMicroB, UV * Prm.vMSc[1].zw + Prm.vMSc[1].xy).rgb;	// Medimum altitude micro texture B
			cLow = texture(tMicroA, UV * Prm.vMSc[0].zw + Prm.vMSc[0].xy).rgb;	// Low altitude micro texture A
		}
	}
#endif

	vec3 cRfl = vec3(0);
	vec3 nvrW = normalize(frg.nrmW);			// Per-pixel surface normal vector
	vec3 vRay = normalize(frg.camW.xyz);		// Unit viewing ray
	vec3 vVrt = Const.CamPos - frg.camW.xyz;	// Geo-centric pixel position
	vec3 vPlN = normalize(vVrt);				// Planet mean normal
	vec3 hlvW = normalize(vRay + Const.toSun);
	float   dst = dot(vRay, frg.camW.xyz);		// Pixel to camera distance
	float   rad = frg.camW.w;					// Pixel geo-distance
	float   alt = rad - Const.PlanetRad;		// Pixel altitude over mean radius
	float  fSrf = (1.0 - Const.CamSpace);		// Camera colse to surface ?
	float fMask = (1.0 - cMsk.a);				// Specular Mask
	float  fSpe = 0;
	float fAmpf = 1.0f;
	float fDRS = dot(vRay, Const.toSun);
	float fDPS = dot(vPlN, Const.toSun);		// Mean normal dot sun


#if defined(_WATER)
#if defined(_RIPPLES)

	// Compute world space normal for water rendering
	//
	cNrm.xy = (cNrm.xy - 0.5f) * 2.0f;
	cNrm.z *= Const.wNrmStr;
	cNrm = normalize(cNrm);

	vec3 wnrmW = (Const.vTangent * cNrm.r) + (Const.vBiTangent * cNrm.g) + (vPlN * cNrm.b);
	wnrmW = mix(nvrW, wnrmW, fMask);
	float fDWS = dot(wnrmW, Const.toSun); // Water normal dot sun

	// Render with specular ripples and fresnel water -------------------------
	//
	float fDCH = clamp(dot(vRay, hlvW), 0.0, 1.0);
	float fDCN = clamp(dot(vRay, wnrmW), 0.0, 1.0);
	float fDHN = dot(hlvW, wnrmW);

	vec3 f = 1.0 - vec3(fDCH, fDCN, fDWS);
	vec3 fFresnel4 = f * f * f;
	vec3 fF = (0.15f + fFresnel4 * 0.85f) * fMask * Const.wSpec;

	// Compute specular reflection intensity
	fSpe = GGX_NDF(fDHN, 0.1f + clamp(fDWS, 0.0, 1.0) * 0.1f) * fF.y;
	fSpe /= (4.0f * fDCH * max(fDWS, fDCN) + 1e-3);

	// Apply fresnel water only if close enough to a surface
	//
	if (!Flow.bInSpace)
	{
		cRfl = GetAmbient(reflect(-vRay, wnrmW)) * fF.y * fSrf;	
		// Attennuate diffuse texture for fresnel refl.
		cTex.rgb *= clamp(1.0f - f.y * fSrf * fMask, 0.0, 1.0) * clamp(1.0f - f.z * fSrf * fMask, 0.0, 1.0);
	}

	cTex.rgb = clamp(cTex.rgb + vec3(0, 0.55, 1.0) * Const.wBrightness * fMask, 0.0, 1.0);

#else
	// Fallback to simple specular reflection
	float fDHN = dot(hlvW, nvrW);
	fSpe = pow(clamp(fDHN, 0.0, 1.0), 60.0f) * fMask * 5.0f;
#endif
#endif

	vec3 nrmW = nvrW; // Micro normal defaults to vertex normal

	// Render with surface microtextures --------------------------------------
	//
#if defined(_MICROTEX)

	if (Flow.bMicroTex)
	{
		float step1 = 1.0 - smoothstep(3000, 15000, dst);	// smoothstep(15000, 3000, dst): GLSL leaves edge0 > edge1 undefined
		step1 *= (step1 * step1);
		vec3 cFnl = max(vec3(0), min(vec3(2), 1.333f * (cFar + cMed + cLow) - 1));

		// Create normals
		if (Flow.bMicroNormals)
		{
			cFnl = cFnl.bbb;

#if defined(_SOFT)
			vec2 cMix = (cFar.rg + cMed.rg + cLow.rg) * 0.6666f;			// SOFT BLEND
#endif
#if defined(_MED)
			vec2 cMix = (cFar.rg + 0.5f) * (cMed.rg + 0.5f) * (cLow.rg + 0.5f);	// MEDIUM BLEND
			fAmpf = 2.0f;
#endif
#if defined(_HARD)
			vec2 cMix = cFar.rg * cMed.rg * cLow.rg * 8.0f;				// HARD BLEND
			fAmpf = 4.0f;
#endif

			vec3 cNrm = vec3((cMix - 1.0f) * 2.0f, 0) * step1;
			cNrm.z = cos(cNrm.x * cNrm.y * 1.57);

			// Approximate world space normal
			nrmW = normalize((Const.vTangent * cNrm.x) + (Const.vBiTangent * cNrm.y) + (nvrW * cNrm.z));

			// Bend the normal towards sun a bit
			nrmW = normalize(nrmW + Const.toSun * 0.06f);
		}

		// Apply luminance
		cTex.rgb *= mix(vec3(1.0f), cFnl, step1);
	}
#endif


	// Render Eclipse ------------------------------------------------------------
	//
	float fECL = GetEclipse(vVrt);

	vec3 cDiffLocal = vec3(0);

#if defined(_LOCALLIGHTS)
	LocalLights(cDiffLocal, nrmW, -frg.camW.xyz);
#endif

#if defined(_NO_ATMOSPHERE)

	float fDNS = clamp(dot(nvrW, Const.toSun), 0.0, 1.0);
	float fDCN = clamp(dot(nvrW, Const.toCam), 0.0, 1.0);
	float fLvl = 2.0f * fDNS / (fDNS + fDCN + 0.5f);
	float fSHD = 1.0f;

	// Shadowing by planet
	if (Flow.bPlanetShadow) {
		float palt = sqrt(clamp(1.0f - fDPS * fDPS, 0.0, 1.0)) * rad - Const.PlanetRad;
		fSHD = fDPS > 0 ? 1.0f : ilerp(Const.MinAlt, Const.MaxAlt, palt);
	}

	// Amplify light and shadows
	fLvl += dot(nvrW - vPlN, Const.toSun) * fLvl * Const.trLS;

	// Add opposition surge
	fLvl += pow(clamp(fDRS, 0.0, 1.0), 4.0f) * 0.3f * fDNS;

#if defined(_MICROTEX)
	fLvl += dot(nrmW - nvrW, Const.toSun) * ilerp(0.0, 0.03, fLvl) * fAmpf;
#endif

	fLvl *= fSHD;	// Apply planet shadow
	fLvl *= fECL;	// Apply eclipse

	vec3 color = cTex.rgb * LightFX(max(fLvl, 0.0) * fShadow + cDiffLocal);
	return vec4(pow(clamp(color * Const.TrExpo, 0.0, 1.0), vec3(Const.TrGamma)), 1.0f);		// Gamma corrention
#else

	float fShd = 1.0f;

#if defined(_CLOUDSHD)
	// Do we render cloud shadows ?
	if (Flow.bCloudShd) {
		fShd = (vUVCld.x < 1.0 ? fChA : fChB);
		fShd = clamp(1.0 - fShd * Prm.fAlpha, 0.0, 1.0);
	}
#endif

	vec3 cNgt = vec3(0);
	vec3 cNgt2 = vec3(0);
	float fDNS = dot(nvrW, Const.toSun); // Vertex normal dot sun

#if defined(_NIGHTLIGHTS)

	// Night lights ?
	float fNgt = clamp(-fDPS * 4.0f + 0.05f, 0.0, 1.0) * Prm.fBeta; // Night lights intensity and 'on' time

	cMsk.b = (cMsk.b > 0.15f ? cMsk.b : 0.0f); // Blue dirt filter

	cNgt = cMsk.rgb * (1 - Const.CamSpace) * fNgt; // Nightlights surface texture illumination term
	cNgt2 = cMsk.rgb * Const.CamSpace * 4.0f * fNgt; // Nightlights orbital visibility
#endif

	float fNoise = (textureLod(tNoise, frg.texUV.xy * 4.0f * Prm.fTgtScale, 0.0).r - 0.5f) * ATMNOISE;

	// Terrain with gamma correction and attennuation
	cTex.rgb = pow(clamp(cTex.rgb, 0.0, 1.0), vec3(Const.TrGamma)) * Const.TrExpo;

	// Evaluate ambient approximation
	vec4 cAmb = AmbientApprox(vPlN, false);
	
	LandOut sct = GetLandView(rad, vPlN);

	// Get the color of sunlight and set maximum intensity to 1.0
	vec3 cSun = GetSunColor(fDPS, alt);
	vec3 cSF = cSun * Const.cSun;
	float fMx = max(max(cSF.r, cSF.g), cSF.b);
	cSF = fMx > 1.0 ? cSF / fMx : cSF;

	float  fL = Const.trLS * 0.3f;
	float  fZ = clamp(dot(nvrW - vPlN, Const.toSun) * Const.trLS, -fL, fL);
	float  fX = 1.0f - pow(1.0f - clamp(fDPS, 0.0, 1.0), 2.0f);

	fZ = fZ > 0 ? fZ * 2.0f : fZ;

#if defined(_MICROTEX)
	float  fG = dot(nrmW - nvrW, Const.toSun) * fAmpf;
#else
	float  fG = 0.0f;
#endif

	// Diffuse "lambertian" shading term
	float  fD = mix(fX + (fG + fZ) * fX, fDPS * fDPS, fMask);

	// Water masking
	float  fM = 0.5f - fMask * 0.25f;

	// Ambient light for terrain
	//					  Color					   Distance				  Altitude factor		   Particle Density		
	vec3 cA = normalize(cAmb.rgb + cSF * 4.0f) * cAmb.a * cAmb.g * fM * exp(-alt * Const.iH.r) * Const.rmI.r * 6e5 * Const.TW_Terrain;

	fShd = clamp(fShd + (1.0f - fX), 0.0, 1.0);

	// Bake light and shadow terms
	vec3 cL = cSF * fD * fShadow * fShd;

	// Lit the texture with various things
	cTex.rgb *= cL * 2.0f + (cA + cDiffLocal + Const.cAmbient * Const.Ambient) * clamp(1.0f + fG + fZ, 0.0, 1.0) + cNgt;

	cTex.rgb = max(vec3(0, 0, 0), cTex.rgb);

	// Add Reflection
	cTex.rgb += cRfl * 0.75f;

	// Add Specular component
	cTex.rgb += cSun * fSpe * smoothstep(-0.001f, 0.03f, fDPS);

	// Amplify cloud shadows for orbital views
	float fOrbShd = 1.0f - (1.0f - fShd) * Const.CamSpace * 0.5f;

	// Add Haze and night lights
	cTex.rgb *= sct.atn.rgb;
	cTex.rgb += (sct.ray.rgb * RayPhase(-fDRS) + sct.mie.rgb * MiePhase(-fDRS)) * fOrbShd * (1.0f + fNoise);

	cTex.rgb *= fECL;	// Apply eclipse
	cTex.rgb += cNgt2;

	return vec4(HDR(cTex.rgb), 1.0f);
#endif
}

#ifdef PS_TerrainPS
layout(location = 0) in TileVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = TerrainPS(frg); }
#endif







// ============================================================================
// Planet Cloud Renderer
// ============================================================================

CldVS CloudVS(TILEVERTEX vrt)
{
	// Zero output.
	CldVS outVS = CldVS(vec4(0), vec2(0), vec3(0), vec3(0));

	// Apply a world transformation matrix
	vec3 vPosW = (vec4(vrt.posL, 1.0f) * Prm.mWorld).xyz;
	vec3 vNrmW = (vec4(vrt.normalL, 0.0f) * Prm.mWorld).xyz;

	outVS.posH = vec4(vPosW, 1.0f) * Const.mVP;
	outVS.nrmW = vNrmW;
	outVS.posW = vPosW;
	outVS.texUV.xy = vrt.tex0.xy;						// Note: vrt.tex0 is un-used (hardcoded in Tile::CreateMesh and varies per tile)

	return outVS;
}

#ifdef VS_CloudVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out CldVS oVS;
void main() { oVS = CloudVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev)); gl_Position = oVS.posH; }
#endif


// ============================================================================
// 
vec4 CloudPS(CldVS frg)
{
	vec2 vUVTex = frg.texUV.xy;
	vec4 cTex = texture(tDiff, vUVTex);
	vec3 vRay;
	vec3 vPxl;
	float  dRC;
	float  fNrm = 1.0f;

	if (Flow.bBelowClouds) {
		float  rRef = Const.PlanetRad + Const.smi * 0.5f;	// Reference altitude
		vec3 vRef = Const.toCam * rRef;
		vRay = normalize(Const.toCam * (Const.CamRad - rRef) + frg.posW); // Viewing ray to the pixel
		dRC = dot(vRay, Const.toCam);
		float  fEca2 = 1.0f - dRC * dRC;			// Ray horizon angle^2
		float  fD = Const.smi * inversesqrt(1.0f - Const.ecc * Const.ecc * fEca2); // Distance to ellipse threshold
		vPxl = vRef + vRay * fD;					// Pretend the pixel being closer and lower
	}
	else {
		vRay = normalize(frg.posW);					// Viewing ray to the pixel
		dRC = dot(vRay, Const.toCam);
		vPxl = Const.CamPos + frg.posW;				// Pixel's geocentric location
	}

	vec3 vPlN = normalize(vPxl);					// Mean Normal at pixel's locatin
	vec3 vVrt = Const.CamPos + frg.posW.xyz;	// Geo-centric pixel position
	vec3 nrm = vPlN;

	float dRS = dot(vRay, Const.toSun);
	float dMNus = dot(vPlN, Const.toSun);
	float dMN = clamp(dMNus, 0.0, 1.0);					// Mean normal sun angle
	float fPxR = dot(vPxl, vPlN);					// Pixel geo distance
	float fPxA = fPxR - Const.PlanetRad;			// Pixel altitude

	if (!Flow.bBelowClouds) fPxA = Const.CloudAlt;


	// -----------------------------------------------
	// Cloud layer rendering for Earth
	// -----------------------------------------------

#if defined(_CLOUDMICRO)
	vec2 vUVMic = frg.texUV.xy * Prm.vMicroOff.zw + Prm.vMicroOff.xy;
	vec4 cMic = texture(tCloudMicro, vUVMic);
#endif


#if defined(_CLOUDNORMALS)
#if defined(_CLOUDMICRO)

	vec4 cMicNorm = texture(tCloudMicroNorm, vUVMic);  // Filename "cloud1_norm.dds"

	// Extract normal from transparency (height) data
	// Filter width
	float d = 2.0 / 512.0;

	float x1 = texture(tDiff, vUVTex + vec2(-d, 0)).a;
	float x2 = texture(tDiff, vUVTex + vec2(+d, 0)).a;
	nrm.x = (x1 * x1 - x2 * x2);

	float y1 = texture(tDiff, vUVTex + vec2(0, -d)).a;
	float y2 = texture(tDiff, vUVTex + vec2(0, +d)).a;
	nrm.y = (y1 * y1 - y2 * y2);

	// Blend in cloud normals only on moderately thick clouds, allowing the highest cloud tops to be smooth.
	nrm.xy = (nrm.xy + clamp((cTex.a * 10.0f) - 3.0f, 0.0, 1.0) * clamp(((1.0f - cTex.a) * 10.0f) - 1.0f, 0.0, 1.0) * (cMicNorm.rg - 0.5f)); // new

	// Increase normals contrast based on sun-earth angle.
	nrm.xyz = nrm.xyz * (1.0f + (0.5f * dMN));

	nrm.z = sqrt(1.0f - clamp(nrm.x * nrm.x + nrm.y * nrm.y, 0.0, 1.0));

	// Approximate world space normal from local tangent space
	nrm = normalize((Const.vTangent * nrm.x) + (Const.vBiTangent * nrm.y) + (vPlN * nrm.z));

	float dCS = dot(nrm, Const.toSun); // Cloud normal sun angle

	// Brighten the lighting model for clouds, based on sun-earth angle. Twice is better.
	// Low sun angles = greater effect. No modulation leads to washed out normals at high sun angles.
	dCS = clamp((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS, 0.0, 1.0);
	dCS = clamp((1.0f - dMN) * (dCS * (1.0f - dCS)) + dCS, 0.0, 1.0);

	// With a high sun angle, don't let the dCS go below 0.2 to avoid unnaturally dark edges.
	dCS = mix(0.2f * dMN, 1.0f, dCS);

	// Effect of normal/sun angle to color
	// Add some brightness (borrowing red channel from sunset attenuation)
	// Adding it to the sun illumination factor, taking care to keep from saturating
	fNrm = dCS +((1.0f - dCS) * 0.2f);
#endif
#endif

#if defined(_CLOUDMICRO)
	float f = cTex.a;
	float g = mix(1.0f, cMic.a, 1.0f - abs(dot(Const.vPolarAxis, vPlN)));
	float h = (g + 4.0f) * 0.2f;
	cTex.a = clamp(mix(g, h, f) * f, 0.0, 1.0);
#endif

	// Render Eclipse ------------------------------------------------------------
	//
	float fECL = GetEclipse(vVrt);

	if (Flow.bBelowClouds)
	{
		// Get sunlight color
		vec3 cSun = GetSunColor(dMNus, fPxA);

		// Get ambient information
		vec4 cMlt = AmbientApprox(vPlN);

		cSun *= clamp(dRS + 1.3f, 0.0, 1.0);
		float fPh = pow(clamp(1.0f - dRC, 0.0, 1.0), 32.0f) * pow(clamp(dRS, 0.0, 1.0), 10.0f); // Boost near horizon and close the sun
		cSun *= 1.0f + fPh * 8.0f;

		cSun *= Const.cSun * fNrm;
		cSun *= Const.Clouds;
		cSun += cMlt.rgb * cMlt.a * 0.2f;

		LandOut sct = GetLandView(fPxA + Const.PlanetRad, vPlN);

		cTex.rgb *= cSun;
		cTex.rgb *= sct.atn.rgb;
		cTex.rgb += sct.ray.rgb * 2.0f;
		cTex.rgb *= fECL;

		return vec4(HDR(cTex.rgb), clamp(cTex.a, 0.0, 1.0));
	}
	else {

		// Get sunlight color
		vec3 cSun = GetSunColor(dMN, fPxA);
	
		// Get ambient information
		vec4 cAmb = AmbientApprox(dMNus);
		vec3 cMSC = Const.RayWave * Const.RayWave * Const.Clouds; // Multiscatter color

		cSun = sqrt(cMSC * cMSC + cSun * cSun * fNrm) * cAmb.a;
		
		LandOut sct = GetLandView(fPxA + Const.PlanetRad, vPlN);

		cTex.rgb *= cSun;
		cTex.rgb *= sct.atn.rgb;
		cTex.rgb += sct.ray.rgb;
		cTex.rgb *= fECL;

		return vec4(sqr(HDR(cTex.rgb * 4.0f)), cTex.a * cAmb.a * cAmb.a);
	}	
}

#ifdef PS_CloudPS
layout(location = 0) in CldVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = CloudPS(frg); }
#endif








// ============================================================================
// Gas Giant Renderer
// ============================================================================

TileVS GiantVS(TILEVERTEX vrt)
{
	// Zero output.
	TileVS outVS = TileVS(vec4(0), vec2(0), vec4(0), vec3(0)
#if defined(_SHDMAP)
		, vec4(0)
#endif
	);
	
	// Apply a world transformation matrix
	vec3 vPosW = (vec4(vrt.posL, 1.0f) * Prm.mWorld).xyz;
	vec3 vNrmW = (vec4(vrt.normalL, 0.0f) * Prm.mWorld).xyz;
	
	outVS.posH = vec4(vPosW, 1.0f) * Const.mVP;
	outVS.texUV.xy = vrt.tex0.xy;
	outVS.camW = vec4(-vPosW, 0);
	outVS.nrmW = vNrmW;

	return outVS;
}

#ifdef VS_GiantVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 6) in float iElev;
layout(location = 0) out TileVS oVS;
void main() { oVS = GiantVS(TILEVERTEX(iPosL, iNrmL, iTex0, iElev)); gl_Position = oVS.posH; }
#endif

// ============================================================================
//
vec4 GiantPS(TileVS frg)
{

	vec2 vUVSrf = frg.texUV.xy * Prm.vTexOff.zw + Prm.vTexOff.xy;
	
	// Fetch Main Textures
	vec4 cTex = texture(tDiff, vUVSrf);
	
	vec3 nrmW = normalize(frg.nrmW);			// Per-pixel surface normal vector
	vec3 vRay = normalize(frg.camW.xyz);		// Unit viewing ray
	vec3 vVrt = Const.CamPos - frg.camW.xyz;	// Geo-centric pixel position
	vec3 vPlN = normalize(vVrt);				// Planet mean normal
	float  fDPS = dot(vPlN, Const.toSun);
	vec3 cSun = vec3(clamp((fDPS + 0.1) * 5.0, 0.0, 1.0));


	// Render Eclipse ------------------------------------------------------------
	//
	cSun *= GetEclipse(vVrt);

	// Terrain with gamma correction and attennuation
	cTex.rgb = pow(clamp(cTex.rgb, 0.0, 1.0), vec3(Const.TrGamma)) * Const.TrExpo;

	vec3 color = cTex.rgb * LightFX(cSun + vec3(0.9, 0.9, 1.0) * Const.Ambient);

	return vec4(HDR(color), 1.0f);
}

#ifdef PS_GiantPS
layout(location = 0) in TileVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = GiantPS(frg); }
#endif


// ============================================================================
// Gas giant cloud layer renderer
// ============================================================================

vec4 GiantCloudPS(CldVS frg)
{
	vec4 cTex = texture(tDiff, frg.texUV.xy);
	vec3 vPlN = normalize(frg.nrmW);
	vec3 vRay = normalize(frg.posW);
	vec3 vVrt = Const.CamPos + frg.posW.xyz;	// Geo-centric pixel position
	float  fDPS = dot(vPlN, Const.toSun);    // Planet mean normal sun angle

	vec3 cSun = vec3(clamp((fDPS + 0.1) * 5.0, 0.0, 1.0));

	// Render Eclipse ------------------------------------------------------------
	//
	float fECL = GetEclipse(vVrt);

	cTex.rgb *= LightFX(cSun + vec3(1.0, 1.0, 1.0) * Const.Ambient);
	cTex.rgb = pow(clamp(cTex.rgb, 0.0, 1.0), vec3(Const.TrGamma)) * Const.TrExpo;
	cTex.rgb *= fECL;

	return vec4(HDR(cTex.rgb), clamp(cTex.a, 0.0, 1.0));
}

#ifdef PS_GiantCloudPS
layout(location = 0) in CldVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = GiantCloudPS(frg); }
#endif
