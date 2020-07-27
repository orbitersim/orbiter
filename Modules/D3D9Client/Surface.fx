// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
// ============================================================================


// ============================================================================
// Shader File for TileManager2 and 3D Terrain implementation
// Contains a light weight implementation
// ============================================================================

struct Light
{
	int		 type;			   /* Type of the light emitter 0=point, 1=spot */
	float    dst2;			   /* Camera-Light Emitter distance squared */
	float4   diffuse;          /* diffuse color of light */
	float3   position;         /* position in world space */
	float3   direction;        /* direction in world space */
	float3   attenuation;      /* Attenuation */
	float4   param;            /* range, falloff, theta, phi */
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
};


// ----------------------------------------------------------------------------
// Vertex Shader to Pixel Shader datafeeds
// ----------------------------------------------------------------------------

struct TileVS
{
	float4 posH     : POSITION0;
	float2 texUV    : TEXCOORD0;  // Texture coordinate
	float4 aux      : TEXCOORD1;  // Night lights
	float3 camW		: TEXCOORD2;
	float3 nrmW		: TEXCOORD3;
	float4 sunlight : COLOR0;     // Color of the sunlight received by terrain
	float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color
#if defined(_SHDMAP)
	float4 shdH     : TEXCOORD4;
#endif
};

struct CloudVS
{
	float4 posH     : POSITION0;
	float2 texUV    : TEXCOORD0;  // Texture coordinate
	float3 atten    : COLOR0;     // Attennuation
	float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color
	float2 fade     : TEXCOORD1;
	float3 nrmW		: TEXCOORD2;
};

struct HazeVS
{
	float4 posH    : POSITION0;
	float2 texUV   : TEXCOORD0;
	float3 insca   : COLOR0;
	float  alpha   : COLOR1;
};

struct CelSphereVS
{
	float4 posH    : POSITION0;
	float2 tex0    : TEXCOORD0;
};




// ----------------------------------------------------------------------------
// Global shader variables
// ----------------------------------------------------------------------------

uniform extern Light	 sLights[4];		// Local light sources

uniform extern float4x4  mLVP;				// Light View Projection
uniform extern float4x4  mWorld;		    // World matrix
uniform extern float4x4  mViewProj;			// Combined View and Projection matrix
// ------------------------------------------------------------
uniform extern float4    vMSc0;				// Micro Texture A scale factors
uniform extern float4    vMSc1;				// Micro Texture B scale factors
uniform extern float4    vMSc2;				// Micro texture C scale factors
uniform extern float4    vTexOff;			// Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vCloudOff;         // Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vMicroOff;         // Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vOverlayOff;       // Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vWater;			// Water material input structure (specular rgb, power)
uniform extern float3    vSunDir;			// Unit Vector towards the Sun
uniform extern float3    vTangent;			// Unit Vector
uniform extern float3    vBiTangent;		// Unit Vector
uniform extern float3    vPolarAxis;		// North Pole
uniform extern float4    vSHD;				// ShadowMap Params
// ------------------------------------------------------------
uniform extern float     fDistScale;		// UNUSED: Scale factor
uniform extern float 	 fAlpha;			// Cloud shodow alpha
uniform extern float 	 fNight;			// Nightlights intensity
// ------------------------------------------------------------
uniform extern bool      bCloudSh;			// Enable cloud shadows
uniform extern bool      bLights;			// Enable night-lights
uniform extern bool      bEnvEnable;		// Enable environment maps
uniform extern bool      bMicroNormals;		// Enable micro texture normal maps
uniform extern int		 iTileLvl;			// Surface tile level being rendered
uniform extern int		 iDebug;			// Debug Mode identifier
uniform extern bool		 bDebug;			// Debug Mode enabled
uniform extern bool		 bLocals;			// Local light sources enabled for this tile
uniform extern bool		 bShadows;			// Enable shadow projection
uniform extern bool		 bOverlay;			// Enable shadow projection
uniform extern bool		 bSpherical;		// Force Spherical planet (discard elevation) 
uniform extern bool		 bCloudNorm;
// Textures ---------------------------------------------------
uniform extern texture   tDiff;				// Diffuse texture
uniform extern texture   tMask;				// Nightlights / Specular mask texture
uniform extern texture   tCloud;
uniform extern texture   tCloud2;
uniform extern texture   tCloudMicro;
uniform extern texture   tCloudMicroNorm;
uniform extern texture   tNoise;			//
uniform extern texture	 tOcean;			// Ocean Texture
uniform extern texture	 tEnvMap;
uniform extern texture	 tMicroA;
uniform extern texture	 tMicroB;
uniform extern texture	 tMicroC;
uniform extern texture	 tShadowMap;
uniform extern texture	 tOverlay;


// ----------------------------------------------------------------------------
// Texture Sampler implementations
// ----------------------------------------------------------------------------

sampler OverlayS = sampler_state
{
	Texture = <tOverlay>;
	MinFilter = ANISOTROPIC;
	MagFilter = ANISOTROPIC;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler ShadowS = sampler_state
{
	Texture = <tShadowMap>;
	MinFilter = POINT;
	MagFilter = POINT;
	MipFilter = NONE;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler DiffTexS = sampler_state
{
	Texture = <tDiff>;
	MinFilter = ANISOTROPIC;
	MagFilter = ANISOTROPIC;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0.0;
	AddressU = CLAMP;
	AddressV = CLAMP;
};


sampler CloudTexS = sampler_state	// Cloud shadow sampler
{
	Texture = <tCloud>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 2;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler Cloud2TexS = sampler_state
{
	Texture = <tCloud2>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler MaskTexS = sampler_state
{
	Texture = <tMask>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler NoiseTexS = sampler_state
{
	Texture = <tNoise>;
	MinFilter = POINT;
	MagFilter = POINT;
	MipFilter = POINT;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler OceaTexS = sampler_state
{
	Texture = <tOcean>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler EnvMapS = sampler_state
{
	Texture = <tEnvMap>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler CloudMicroS = sampler_state
{
	Texture = <tCloudMicro>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler CloudMicroNormS = sampler_state
{
	Texture = <tCloudMicroNorm>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler MicroAS = sampler_state
{
	Texture = <tMicroA>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
	MipMapLodBias = MICRO_BIAS;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler MicroBS = sampler_state
{
	Texture = <tMicroB>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
	MipMapLODBias = MICRO_BIAS;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler MicroCS = sampler_state
{
	Texture = <tMicroC>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
	MipMapLODBias = MICRO_BIAS;
	AddressU = WRAP;
	AddressV = WRAP;
};

// ----------------------------------------------------------------------------
// Atmospheric scattering model
// ----------------------------------------------------------------------------

uniform extern float4	vMPhase;			// Pre-computed factors used in Mie phase function
uniform extern float4	vODCoEff;			// Optical Depth Taylor co-efficients
uniform extern float4	vODCoEffEx;			// Optical Depth Taylor co-efficients
uniform extern float3	vMieInSct;			// Mie scattering factor = Mie/(lambda^m) (slider)
uniform extern float3	vRayInSct;			// Rayleigh in-scattering = (ROut*RIn)/(lambda^r) (slider)
uniform extern float3	vTotOutSct;			// Total out-scattering = ROut/(lambda^r) + Mie/(lambda^m) (slider)
uniform extern float3	vHazeMax;			// Horizon haze limitter (slider)
uniform extern float3	vColorShift;		// lerp([1,1,1], 1.0/lambda^r, Aux3)
uniform extern float3   vCameraPos;         // Geo-centric camera position
uniform extern float3   vUnitCameraPos;     // Geo-centric camera position (Unit vector)
uniform extern float	fCloudInts;			// Cloud layer intensity
uniform extern float	fScaleHeight;		// Atmosphere scaleheight
uniform extern float	fInvScaleHeight;	// Inverse Scale Height 1.0f/fScaleHeight
uniform extern float	fSunAlt;			// Altitude of sunlight in the horizon ring, [meters]
uniform extern float    fRadius;            // PlanetRad
uniform extern float    fCameraAlt;         // Camera Altitude
uniform extern float    fHorizonAlt;        // Horizon (i.e. Skydome, Atmosphere) Altitude/Height
uniform extern float	fAtmRad2;			// Atmosphere upper radius squared (fRadius+fHorizonAlt)^2
uniform extern float	fRPhase;			// Rayleigh phase factor
uniform extern float	fHorizonDst;		// Camera to horizon distance sqrt(dot(vCameraPos,vCameraPos) - fRadius*fRadius)
uniform extern float	fExposure;			// Terrain brightness
uniform extern float	fAux1;				// Bound to Aux1 (slider)
uniform extern float	fAux2;				// Bound to Aux2 (slider)
uniform extern float	fInvAux1;			// Inverse of fAux1 (slider)
uniform extern float	fTrGamma;			// Terrain gamma control
uniform extern float	fAtmGamma;			// Atmosphere gamma control
uniform extern float	fInvParameter;		// Inverse of optical parameter 1.0/AngleCoEff(0)
uniform extern float	fAmbient;			// Planet specific ambient level pre-multiplied with camera altitude factor
uniform extern float	fGlobalAmb;
uniform extern float	fTime;				// Loop from 0 to 3600 (sec)
uniform extern bool		bInSpace;			// Camera in the space (i.e. fCameraAlt>fHorizonAlt)
uniform extern bool		bOnOff;

// Numeric integration points and weights for Gauss-Lobatto integral
//
static const float4 vWeight4 = {0.167, 0.833, 0.833, 0.167};
static const float4 vPoints4 = {0.0f, 0.27639f, 0.72360f, 1.0f};
static const float3 vWeight3 = {0.33333f, 1.33333f, 0.33333f};
static const float3 vPoints3 = {0.0f, 0.5f, 1.0f};
static const float3 cSky = {0.7f, 0.9f, 1.2f};
static const float3 cSun = { 1.0f, 1.0f, 1.0f };
static const float srfoffset = -0.2;
static const float ATMNOISE = 0.02;



// -------------------------------------------------------------------------------------------------------------
// Project shadows on surface
//
/*
float ProjectShadows(float2 sp)
{
	if (sp.x < 0 || sp.y < 0) return 1.0f;
	if (sp.x > 1 || sp.y > 1) return 1.0f;

	float2 dx = float2(vSHD[1], 0) * 1.5f;
	float2 dy = float2(0, vSHD[1]) * 1.5f;
	float  va = 0;
	float  pd = 1e-4;

	sp -= dy;
	if ((tex2D(ShadowS, sp - dx).r) < pd) va++;
	if ((tex2D(ShadowS, sp).r) < pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) < pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) < pd) va++;
	if ((tex2D(ShadowS, sp).r) < pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) < pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) < pd) va++;
	if ((tex2D(ShadowS, sp).r) < pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) < pd) va++;

	return va / 9.0f;
}*/



// ---------------------------------------------------------------------------------------------------
//
float SampleShadows(float2 sp, float pd)
{
	if (sp.x < 0 || sp.y < 0) return 0.0f;	// If a sample is outside border -> fully lit
	if (sp.x > 1 || sp.y > 1) return 0.0f;

	if (pd < 0) pd = 0;
	if (pd > 2) pd = 2;

	float2 dx = float2(vSHD[1], 0) * 1.5f;
	float2 dy = float2(0, vSHD[1]) * 1.5f;
	float  va = 0;

	sp -= dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;
	sp += dy;
	if ((tex2D(ShadowS, sp - dx).r) > pd) va++;
	if ((tex2D(ShadowS, sp).r) > pd) va++;
	if ((tex2D(ShadowS, sp + dx).r) > pd) va++;

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

	if (!bLocals) return;

	float3 posWN = normalize(-posW);
	float3 p[4];
	int i;

	// Relative positions
	[unroll] for (i = 0; i < 4; i++) p[i] = posW - sLights[i].position;

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
	[unroll] for (i = 0; i < 4; i++) att[i] = dot(sLights[i].attenuation.xyz, float3(1.0, dst[i], dst[i] * dst[i]));

	att = rcp(att);

	// Spotlight factors
	float4 spt;
	[unroll] for (i = 0; i < 4; i++) {
		spt[i] = (dot(p[i], sLights[i].direction) - sLights[i].param[Phi]) * sLights[i].param[Theta];
		if (sLights[i].type == 0) spt[i] = 1.0f;
	}

	spt = saturate(spt);

	// Diffuse light factors
	float4 dif;
	[unroll] for (i = 0; i < 4; i++) dif[i] = dot(-p[i], nrmW);

	dif = saturate(dif);
	dif *= (att*spt);

	diff_out = 0;

	[unroll] for (i = 0; i < 4; i++) diff_out += sLights[i].diffuse.rgb * dif[i];
}


// -------------------------------------------------------------------------------------------------------------
// "HDR"
//
float3 Light_fx2(float3 x)
{
	//return saturate(x);
	//return x * rsqrt(1.0f + x*x) * 1.4f;
	return 2.0*x / (1.0f + x);
}


// -------------------------------------------------------------------------------------------------------------
// Henyey-Greenstein Phase function
// x = (1-g^2)/(4pi), y = 1+g^2, w = -2*g
//
float MPhase(float cw)
{
	return max(1, vMPhase.x * pow(abs(vMPhase.y-vMPhase.w*cw), -1.5f));
}

// ----------------------------------------------------------------------------
// Rayleigh Phase function
//
float RPhase(float cw)
{

	//return (1.0 + cw*fRPhase) * (0.75 + cw*cw*0.5f);
	//return (1.0 + cw*cw*fRPhase);
	return (1.0 + cw*fRPhase);
}

// ----------------------------------------------------------------------------
// Optical depth angle co-efficiency factor. (Accurate and valid for angles 0 to 96deg)
// c = cosine of the ray direction (1.0 is up)
//
float AngleCoEff(float c)
{
	float  c1 = rcp(max(0.0954, c+0.2));
	float  c2 = c1*c1;
	float4 v1 = float4(1.0f, c1, c2, c2*c1);
	return dot(v1, vODCoEff) + dot(v1*(c2*c2), vODCoEffEx);
}

float Shadow(float c, float f)
{
	float fC = saturate((c+fAux1+f)*fInvAux1);
	return (3.0 - 2.0 * fC) * (fC*fC);
}

float4 DebugProg(in float3 nrmW, in float3 color, in float fRad)
{
	float a = (fRad - fRadius) / 7000.0f;

	if (iDebug==1) return float4((nrmW + 1.0) * 0.5, 1.0);

	if (iDebug==2) {
		float x = dot(nrmW, vTangent) + 1;
		float y = dot(nrmW, vBiTangent) + 1;
		float z = dot(nrmW, vUnitCameraPos) + 1;
		return float4(x, y, z, 2.0) * 0.5;
	}

	if (iDebug==3) {
		if (a<-1)		return saturate(float4(0,   0, 2+a, 1.0));
		else if (a<0)	return saturate(float4(0, 1+a,   1, 1.0));
		else if (a<1)	return saturate(float4(a,   1, 1-a, 1.0));
		else if (a<2)	return saturate(float4(1, 2-a,	 0, 1.0));
		else 			return saturate(float4(1, a-2, a-2, 1.0));
	}

	if (iDebug==4) {
		a = saturate(a*0.5 + 0.5);
		return float4(a, a, a, 1.0);
	}

	if (iDebug==5) {
		return float4(color, 1);
	}

	return float4(0,0,0,1);
}


// ============================================================================
// Atmospheric scattering implementation. (Will render horizon and sky-color)
// ----------------------------------------------------------------------------
// vIns = Light in scattering
// vUnitRay = Unit Vector pointing in ray direction. (away from the camera)
// ============================================================================

void SkyColor(out float3 vIns, in float3 vUnitRay)
{
	float  fDRC = dot(vUnitRay, vUnitCameraPos);
	float  fCam = fCameraAlt + fRadius;
	float  fCm2 = fCam * fCam;
	float  fPrm = fCam * fDRC;
	float  fRd2 = fCm2 - fPrm*fPrm;
	float  fRay = sqrt(fAtmRad2 - fRd2);

	if (fRd2>fAtmRad2) {
		vIns = 0;
		return;
	}

	float3 vRay;

	if (fCm2 < fAtmRad2) {
		fRay -= fPrm;
		vRay = vUnitRay * (fRay * 0.5f);
	}
	else {
		fRay *= 2.0f;
		vRay = vUnitRay * abs(fPrm);
	}


	float  fDRS = -dot(vUnitRay, vSunDir);
	float3 vPos = vCameraPos + vRay;
	float3 vNr1 = normalize(vPos);
	float  fAlt = dot(vNr1, vPos)-fRadius;
	float  fDNS = dot(vNr1, vSunDir);
	float  fDNR = dot(vNr1, vUnitRay);

	// Setup altitudes for all sample points
	float3 vAlt;

	if (fCm2 < fAtmRad2) vAlt = float3(fCameraAlt, fAlt, fHorizonAlt);
	else				 vAlt = float3(fHorizonAlt, fAlt, fHorizonAlt);

	// Atmospheric densities for sample points
	float3 vDns = exp2(-vAlt*fInvScaleHeight);

	// Mean atmospheric density for a viewing ray
	float  fMnD = dot(vDns, vWeight3);

	// Evaluate a Gauss-Lobatto integral (from camera to skydome). Will give optical depth for the ray
	float  fDep = fMnD * (fRay * fInvScaleHeight) * 0.3465735903f;

	float  fASn = AngleCoEff(fDNS);

	// Color of inscattered sunlight
	float3 vSun = cSun * exp2(-vTotOutSct * fAux2*((vDns[0]+vDns[1]) * 0.5f * fASn)) * fDep * Shadow(fDNS, srfoffset);
	//float3 vSun = cSun * exp2(-vTotOutSct * fAux2*(vDns[1] * fASn)) * fDep * Shadow(fDNS, srfoffset);

	// Compute in-scattering
	vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun;

	float fNgt = saturate(fDNS*2.924f+0.657f);

	// Compute ambient light level for the sky
	float3 vAmb = (vRayInSct + 0.5f) * (3.0*fAmbient * fNgt);

	vIns = 1.0f - exp2(-(vAmb + vIns) * vColorShift);
	//vIns = 1.0f - exp2(-(vIns) * vColorShift);

	vIns = pow(abs(vIns*vHazeMax), fAtmGamma);
}





// ============================================================================
// Planet Surface Renderer
// ============================================================================

#define AUX_DIST		0	// Vertex distance
#define AUX_NIGHT		1	// Night lights intensity
#define AUX_SLOPE		2   // Terrain slope factor 0.0=flat, 1.0=sloped
#define AUX_RAYDEPTH	3   // Optical depth of a ray


TileVS SurfaceTechVS(TILEVERTEX vrt,
	uniform bool sbRipples,
	uniform bool sbNoAtmo)
{
	// Zero output.
	TileVS outVS = (TileVS)0;

	// Apply a world transformation matrix
	float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;

	// Disrecard elevation and make the surface spherical
	if (bSpherical) vPosW = (normalize(vCameraPos + vPosW) * fRadius) - vCameraPos;
	
	float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);

#if defined(_SHDMAP)
	outVS.shdH = mul(float4(vPosW, 1.0f), mLVP);
#endif

	outVS.texUV.xy = vrt.tex0.xy;						// Note: vrt.tex0 is un-used (hardcoded in Tile::CreateMesh and varies per tile)

	float3 vVrt  = vCameraPos + vPosW;					// Geo-centric vertex position
	float3 vPlN  = normalize(vVrt);
	float3 vRay  = normalize(-vPosW);					// Unit viewing ray
	float  fDPS  = dot(vPlN,  vSunDir);					// Dot mean normal, sun direction
	float  fRay  = abs(dot(vPosW, vRay));				// Vertex camera distance
	float  fNgt	 = fDPS * 4.0f - 0.05f;
	float  fTrA  = saturate(fDPS*2.924f + 0.657f);
	outVS.camW   = -vPosW;
	outVS.nrmW   = vNrmW;

	outVS.aux[AUX_NIGHT] = -fNgt;
	outVS.aux[AUX_DIST]  =  fRay;



	// If no atmosphere skip the rest
	if (sbNoAtmo) return outVS;


	// Camara altitude dependency multiplier for ambient color of atmosphere
	//float fAmb = max(saturate(fNgt+0.9f)*fAmbient, fGlobalAmb) * 0.08f;

	float fAmb = max(saturate(fTrA*fAmbient), fGlobalAmb);

	float  fDPR  = dot(vPlN,  vRay);					// Dot mean normal, viewing ray
	float  fDNR	 = dot(vNrmW, vRay);
	float  fDNS  = dot(vNrmW, vSunDir);					// Dot vertex normal, sun direction
	float  fDRS  = dot(vRay,  vSunDir);					// Dot viewing ray, sun direction
	float  fDst  = dot(vVrt, vPlN);						// Vertex geo-distance
	float  fAlt  = fDst - fRadius;						// Vertex altitude

	float  fCamAlt = fCameraAlt;

	if (bInSpace) {
		float  fCm2 = fDst * fDst;
		float  fPrm = fDst * fDPR;
		fCamAlt = fHorizonAlt;
		fRay = sqrt(fAtmRad2 - (fCm2 - fPrm*fPrm)) - fPrm;	// Length of the ray inside atmosphere
	}


	// Altitude vector for sample points
	float3 vAlt = float3(fAlt, (fAlt + fCamAlt)*0.5, fCamAlt);

	// Compute atmospheric density for all sample points
	float3 vDns = exp2(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points

	// Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
	float fDRay = dot(vDns, vWeight3) * (fRay * fInvScaleHeight) * 0.3465735903f;

	float3 vSunLight = exp2(-vTotOutSct * (vDns[0] * 0.12f * AngleCoEff(fDPS))) * Shadow(fDPS, srfoffset);

	outVS.aux[AUX_RAYDEPTH] = clamp(fDRay, 0, 10);

	// Multiply in-coming light with phase and light scattering factors
	outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;
	outVS.insca = (1.0f - exp2(-outVS.insca));

	outVS.sunlight = float4(vSunLight, fAmb);

	return outVS;
}






float4 SurfaceTechPS(TileVS frg,
	uniform bool sbSpecular,
	uniform bool sbRipples,
	uniform bool sbMicro,
	uniform bool sbNoAtmos,
	uniform bool sbShadows) : COLOR
{

	float2 vUVSrf = frg.texUV.xy * vTexOff.zw + vTexOff.xy;
	float2 vUVWtr = frg.texUV.xy * vMicroOff.zw + vMicroOff.xy;
	float2 vUVCld = frg.texUV.xy * vCloudOff.zw + vCloudOff.xy;

	vUVWtr.x += fTime/180.0f;

	float4 cMsk = tex2D(MaskTexS, vUVSrf);
	float3 cNrm;
	float fChA, fChB;

	if (sbRipples) cNrm = tex2D(OceaTexS, vUVWtr).xyz;

	float4 cTex = tex2D(DiffTexS, vUVSrf);

	if (bOverlay) {
		float2 vUVOvl = frg.texUV.xy * vOverlayOff.zw + vOverlayOff.xy;
		float4 cOvl = tex2D(OverlayS, vUVOvl);
		cTex.rgb = lerp(cTex.rgb, cOvl.rgb, cOvl.a);
	}

	if (sbShadows) {
		if (bCloudSh) {
			fChA = tex2D(CloudTexS, vUVCld).a;
			fChB = tex2D(Cloud2TexS, vUVCld - float2(1, 0)).a;
		}
	}

	float fShadow = 1.0f;

#if defined(_SHDMAP)
	if (bShadows) {
		frg.shdH.xyz /= frg.shdH.w;
		frg.shdH.z = 1 - frg.shdH.z;
		float2 sp = frg.shdH.xy * float2(0.5f, -0.5f) + float2(0.5f, 0.5f);
		float  pd = frg.shdH.z + 0.05f * vSHD[3];
		fShadow = 1.0f - SampleShadows(sp, pd);
	}
#endif

	float3 cFar, cMed, cLow;

	if (sbMicro) {
		float2 UV = frg.texUV.xy;
		// Create normals
		if (bMicroNormals) {
			// Normal in .ag luminance in .b
			cFar = tex2D(MicroCS, UV*vMSc2.zw + vMSc2.xy).agb;	// High altitude micro texture C
			cMed = tex2D(MicroBS, UV*vMSc1.zw + vMSc1.xy).agb;	// Medimum altitude micro texture B
			cLow = tex2D(MicroAS, UV*vMSc0.zw + vMSc0.xy).agb;	// Low altitude micro texture A
		}
		else {
			// Color in .rgb no normals
			cFar = tex2D(MicroCS, UV*vMSc2.zw + vMSc2.xy).rgb;	// High altitude micro texture C
			cMed = tex2D(MicroBS, UV*vMSc1.zw + vMSc1.xy).rgb;	// Medimum altitude micro texture B
			cLow = tex2D(MicroAS, UV*vMSc0.zw + vMSc0.xy).rgb;	// Low altitude micro texture A
		}
	}


	float3 cSpe = 0;
	float3 nrmW = normalize(frg.nrmW);		// Per-pixel surface normal vector
	float3 nvrW = nrmW;						// Per-pixel surface normal vector
	float3 camW = normalize(frg.camW);		// Unit viewing ray
	float3 vVrt = vCameraPos - frg.camW;		// Geo-centric pixel position
	//float3 vPlN = normalize(vVrt);			// Planet mean normal
	//float  fRad = dot(vVrt, vPlN);			// Pixel Geo-distance
	float3 cNgt = saturate(frg.aux[AUX_NIGHT]) * fNight;


	// Render with specular ripples and fresnel water -------------------------
	//
	if (sbSpecular) {

		// Specular Mask
		float m = (1.0 - cMsk.a) * saturate(0.2f - frg.aux[AUX_NIGHT] * 4.0f);
		float f4 = 0;

		// Specular intensity
		float fInts = saturate(1.0 - fCameraAlt * 0.2e-4);

		if (sbRipples) {

			float3 vPlN = normalize(vVrt);			// Planet mean normal

			cNrm.xy = clamp((cNrm.xy - 0.5f) * fInts * 5.0f, -1, 1);

			cNrm.z = cos(cNrm.x * cNrm.y * 1.570796);

			// Compute world space normal
			nrmW = (vTangent * cNrm.r) + (vBiTangent * cNrm.g) + (vPlN * cNrm.b);
			nrmW = lerp(nvrW, nrmW, m);
		}

		// Compute Fresnel term
		float f = 1.0 - saturate(dot(camW, nrmW));
		float f2 = f*f;
		f4 = f2*f2;

		// Compute specular reflection intensity
		float s = dot(reflect(-vSunDir, nrmW), camW);

		cSpe = m * float3(1.2f, 1.1f, 1.0f) * pow(saturate(s), 35.0f) * max(0.7, fInts*1.5) * fShadow;

		// Apply fresnel reflection
		cTex.rgb = lerp(cTex.rgb, cSky, m * f4);
	}


	// Render with surface microtextures --------------------------------------
	//
	if (sbMicro) {

		float dist  = frg.aux[AUX_DIST];
		float step1 = smoothstep(15000, 3000, dist);
		step1 *= (step1*step1);
		float3 cFnl = max(0, min(2, 1.333f*(cFar+cMed+cLow)-1));

		// Create normals
		if (bMicroNormals) {

			cFnl = cFnl.bbb;

			#if BLEND==0
				float2 cMix = (cFar.rg + cMed.rg + cLow.rg) * 0.6666f;			// SOFT BLEND
			#elif BLEND==1
				float2 cMix = (cFar.rg+0.5f) * (cMed.rg+0.5f) * (cLow.rg+0.5f);	// MEDIUM BLEND
			#else
				float2 cMix = cFar.rg * cMed.rg * cLow.rg * 8.0f;				// HARD BLEND
			#endif

			float3 cNrm  = float3((cMix - 1.0f) * 2.0f, 0) * step1;
			cNrm.z = cos(cNrm.x * cNrm.y * 1.57);
			// Approximate world space normal
			nrmW = normalize((vTangent * cNrm.x) + (vBiTangent * cNrm.y) + (nvrW * cNrm.z));

			// Bend the normal towards sun a bit
			nrmW = normalize(nrmW + vSunDir * 0.06f);
		}
		// Apply luminance
		cTex.rgb *= lerp(1.0f, cFnl, step1);
	}



	// Is Debug mode enabled
	/*
	// ----------------------------------------------------------------------
	if (bDebug) {
		float3 vPlN = normalize(vVrt);
		float  fRad = dot(vVrt, vPlN);
		return DebugProg(nrmW, cTex.rgb, fRad);
	}
	// ----------------------------------------------------------------------
	*/
	float3 cDiffLocal = 0;

	LocalLights(cDiffLocal, nrmW, -frg.camW);

	// Do we have an atmosphere or not ?
	//
	if (sbNoAtmos) {

		float3 vPlN = normalize(vVrt);								// Planet mean normal
		float fTrS = saturate(dot(nvrW, vSunDir)*10.0f);			// Shadowing by terrain
		float fPlS = saturate(dot(vPlN, vSunDir)*10.0f);			// Shadowing by planet

		float fDNS = dot(nrmW, vSunDir);
		float fDRS = dot(camW, vSunDir);

		float fX = pow(saturate(fDNS), 0.33f);						// Lambertian
		float fZ = pow(abs(fDRS), 5.0f) * 0.5f;						// Shadow compensation
		float fLvl = fX * (fZ + 1.0f) * fExposure;					// Bake all together
		
		fLvl *= (fTrS * fPlS);										// Apply shadows

		float3 color = cTex.rgb * Light_fx2(cSun*max(fLvl, 0) * fShadow + cDiffLocal);

		return float4(pow(saturate(color), fTrGamma), 1.0f);			// Gamma corrention
	}
	else {

		float a = (tex2Dlod(NoiseTexS, float4(frg.texUV.xy, 0, 0)).r - 0.5f) * ATMNOISE;
		float fShd = 1.0f;

		// Do we render cloud shadows ?
		//
		if (sbShadows) {
			if (bCloudSh) {
				fShd = (vUVCld.x < 1.0 ? fChA : fChB);
				fShd *= fShd;
				fShd = saturate(1.0 - fShd*fAlpha);
			}
		}

		// Night lights ?
		if (bLights) {
			cMsk.b = (cMsk.b > 0.15f ? cMsk.b : 0.0f); // Blue dirt filter
			cNgt *= cMsk.rgb;
		}

		float q = saturate(fCameraAlt * 1e-5);


		// Terrain, Specular and Night lights
		float3 sun = max(frg.sunlight.rgb * fShadow, float3(0.9, 0.9, 1.0) * frg.sunlight.w);
		float3 color = cTex.rgb * Light_fx2(sun*fShd + cDiffLocal + cNgt*(1-q)) + (cNgt*q*3.0f);
		float3 atten = exp2(-vTotOutSct * frg.aux[AUX_RAYDEPTH]);

		// Terrain with gamma correction and attennuation
		color = pow(abs(color * fExposure), fTrGamma) * atten;

		// Add atmosphere with gamma correction
		color += pow(abs(frg.insca.rgb * vHazeMax), fAtmGamma);

		// Add Specular component
		color += cSpe * atten * fShd;

		return float4(color+a, 1.0f);
	}
}



// ============================================================================
// Cloud Layer Renderer
// ============================================================================

CloudVS CloudTechVS(TILEVERTEX vrt)
{
	// Zero output.
	CloudVS outVS = (CloudVS)0;

	// Apply a world transformation matrix
	float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
	float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;
	outVS.posH = mul(float4(vPosW, 1.0f), mViewProj);
	outVS.nrmW = vNrmW;
	outVS.texUV.xy = vrt.tex0.xy;						// Note: vrt.tex0 is un-used (hardcoded in Tile::CreateMesh and varies per tile)

	float3 vVrt = vCameraPos + vPosW;					// Geo-centric vertex position
	float3 vPlN = normalize(vVrt);
	float3 vRay = normalize(-vPosW);					// Unit viewing ray
	float  fDPS = dot(vPlN, vSunDir);					// Dot mean normal, sun direction
	float  fRay = abs(dot(vPosW, vRay));				// Vertex camera distance
	float  fTrA = saturate(fDPS*2.924f + 0.657f);

	// Camara altitude dependency multiplier for ambient color of atmosphere

	float fAmb = max(saturate(fTrA*8.0*fAmbient), fGlobalAmb) * 0.25f;

	float  fDPR = dot(vPlN, vRay);						// Dot mean normal, viewing ray
	float  fDNR = dot(vNrmW, vRay);
	float  fDNS = dot(vNrmW, vSunDir);					// Dot vertex normal, sun direction
	float  fDRS = dot(vRay, vSunDir);					// Dot viewing ray, sun direction
	float  fDst = dot(vVrt, vPlN);						// Vertex geo-distance
	float  fAlt = fDst - fRadius;						// Vertex altitude

	float  fCamAlt = fCameraAlt;

	if (bInSpace) {
		float  fCm2 = fDst * fDst;
		float  fPrm = fDst * fDPR;
		fCamAlt = fHorizonAlt;
		fRay = sqrt(fAtmRad2 - (fCm2 - fPrm*fPrm)) - fPrm;	// Length of the ray inside atmosphere
	}


	// Altitude vector for sample points
	float3 vAlt = float3(fAlt, (fAlt + fCamAlt)*0.5, fCamAlt);

	// Compute atmospheric density for all sample points
	float3 vDns = exp2(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points

													// Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
	float fDRay = dot(vDns, vWeight3) * (fRay * fInvScaleHeight) * 0.3465735903f;

	float3 vSunLight = exp2(-vTotOutSct * (vDns[0] * 0.25f * AngleCoEff(fDPS))) * Shadow(fDPS, srfoffset);

	// Compute surface texture color attennuation (i.e. extinction term)
	outVS.atten = exp2(-vTotOutSct * fDRay * 0.33f);
	outVS.fade.x = exp2(-fDRay * 0.5f) * 2.0f;
	outVS.fade.y = 1.0f - abs(dot(vPolarAxis, vPlN));

	// Multiply in-coming light with phase and light scattering factors
	outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;

	outVS.insca = (1.0f - exp2(-outVS.insca));
	outVS.atten *= max(vSunLight, (vRayInSct + 1.0f) * fAmb) * 1.3f;

	return outVS;
}


float4 CloudTechPS(CloudVS frg) : COLOR
{
	float2 vUVMic = frg.texUV.xy * vMicroOff.zw + vMicroOff.xy;
	float2 vUVTex = frg.texUV.xy;

	float a = (tex2Dlod(NoiseTexS, float4(vUVTex,0,0)).r - 0.5f) * ATMNOISE;

	float4 cTex = tex2D(DiffTexS, vUVTex);
	float4 cMic = tex2D(CloudMicroS, vUVMic);
	
#if defined(_CLOUDNORMALS)

	if (bCloudNorm) {

/*#if defined(_CLOUDMICRO)
		float4 cMicNorm = tex2D(CloudMicroNormS, vUVMic);  // Filename "cloud1_norm.dds" (does not exists in distribution)
#endif*/

		// Filter width
		float d = 2.0 / 512.0;
		float3 nrm = 0;

		float x1 = tex2D(DiffTexS, vUVTex + float2(-d, 0)).a;
		float x2 = tex2D(DiffTexS, vUVTex + float2(+d, 0)).a;
		nrm.x = (x1*x1 - x2*x2);

		float y1 = tex2D(DiffTexS, vUVTex + float2(0, -d)).a;
		float y2 = tex2D(DiffTexS, vUVTex + float2(0, +d)).a;
		nrm.y = (y1*y1 - y2*y2);

		float m = max(abs(nrm.x), abs(nrm.y));

		// Bump magnitude/contrast function
		float contrast = m * 1.0f;

/*#if defined(_CLOUDMICRO)	
		nrm.xy = nrm.xy * cMicNorm.rg * 4.0f; // Blend Normals
#endif*/

		nrm = normalize(nrm) * contrast;
		nrm.z = sqrt(1.0f - nrm.x*nrm.x - nrm.y*nrm.y);

		// Approximate world space normal from local tangent space
		nrm = normalize((vTangent * nrm.x) + (vBiTangent * nrm.y) + (frg.nrmW * nrm.z));

		float dCS = dot(nrm, vSunDir);			// Cloud normal sun angle
		//float dMN = dot(frg.nrmW, vSunDir);	// Planet mean normal sun angle
		
		//float y = abs(dCS*dCS);
		float y = pow(abs(dCS), 0.3f);
		if (dCS > 0) dCS = y; else dCS = -y;

		// Effect of normal/sun angle to color
		cTex.rgb *= saturate(0.6f + dCS*0.4f);		
	}
#endif

#if defined(_CLOUDMICRO)
		float f = cTex.a;
		float g = lerp(1, cMic.a, frg.fade.y);
		float h = (g + 4.0f)*0.2f;
		cTex.a = saturate(lerp(g, h, f) * f);
#endif

	float3 color = cTex.rgb*frg.atten.rgb*fCloudInts + frg.insca.rgb*vHazeMax;

	return float4(saturate(color + a), cTex.a*saturate(fCloudInts*fCloudInts));
	//return float4(saturate(color + a), cTex.a*saturate(frg.fade.x*fCloudInts*fCloudInts));
}


technique CloudTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 CloudTechVS();
		pixelShader  = compile ps_3_0 CloudTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}


#if defined(_CLOUDSHADOWS)
#define _CLDSHD true
#else
#define _CLDSHD false
#endif


// This is used for high resolution base tiles --------------------------------
//
technique TileTech
{

	pass P0	// Earth
	{
												 //Spec, Rippl, Mic,   NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(true, true,  false, false, _CLDSHD);
		vertexShader = compile vs_3_0 SurfaceTechVS(true, false);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}

	pass P1	// Earth, NoRipples
	{
												 //Spec,  Rippl, Mic,   NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(true, false, false, false, _CLDSHD);
		vertexShader = compile vs_3_0 SurfaceTechVS(false, false);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}

	pass P2	// Mars
	{
												 //Spec,  Rippl, Mic,   NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(false, false, true,  false, false);
		vertexShader = compile vs_3_0 SurfaceTechVS(false, false);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}

	pass P3	// Mars, no micro
	{
												 //Spec,  Rippl, Mic,   NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(false, false, false, false, false);
		vertexShader = compile vs_3_0 SurfaceTechVS(false, false);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}

	pass P4	// Luna
	{
												 //Spec,  Rippl, Mic,  NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(false, false, true, true,  false);
		vertexShader = compile vs_3_0 SurfaceTechVS(false, true);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}

	pass P5	// Luna, no micro
	{
												 //Spec,  Rippl, Mic,   NoAtm, Shadows
		pixelShader = compile ps_3_0 SurfaceTechPS(false, false, false, true, false);
		vertexShader = compile vs_3_0 SurfaceTechVS(false, true);
		AlphaBlendEnable = false;				  //Rippl, NoAtm
	}
}



// ============================================================================
// Render Horizon "Ring" from the space
// ============================================================================

HazeVS RingTechVS(float3 posL : POSITION0)
{
	// Zero output.
	HazeVS outVS = (HazeVS)0;

	posL.xz *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y   = lerp(vTexOff[2], vTexOff[3], posL.y);

	float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);

	if (bOnOff) SkyColor(outVS.insca, normalize(posW)); // HorizonColor(outVS.insca, normalize(posW));
	else outVS.insca = float3(0, 0, 0);

	return outVS;
}

float4 RingTechPS(HazeVS frg) : COLOR
{
	float a = (tex2Dlod(NoiseTexS, float4(frg.texUV,0,0)).r - 0.5f) * ATMNOISE;
	return float4(frg.insca.rgb+a, 1.0f);
}

technique RingTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 RingTechVS();
		pixelShader  = compile ps_3_0 RingTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = One;
		DestBlend = One;
		ZEnable = false;
		ZWriteEnable = false;
	}
}






// ============================================================================
// Render SkyDome and Horizon
// ============================================================================

HazeVS HorizonTechVS(float3 posL : POSITION0)
{
	// Zero output.
	HazeVS outVS = (HazeVS)0;

	outVS.texUV = posL.xy*10.0;

	posL.xz *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y   = lerp(vTexOff[2], vTexOff[3], posL.y);

	float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);

	if (bOnOff) SkyColor(outVS.insca, normalize(posW));
	else outVS.insca = float3(0, 0, 0);

	return outVS;
}


float4 HorizonTechPS(HazeVS frg) : COLOR
{
	float a = (tex2Dlod(NoiseTexS, float4(frg.texUV,0,0)).r - 0.5f) * 0.008;
	return float4(frg.insca.rgb+a, 1.0f);
}

technique HorizonTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 HorizonTechVS();
		pixelShader  = compile ps_3_0 HorizonTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = One;
		DestBlend = One;
		ZEnable = false;
		ZWriteEnable = false;
	}
}



// ============================================================================
// Render Skydome (i.e. Celestial Sphere Background Image Manager) from Space
// ============================================================================

CelSphereVS SpaceTechVS(TILEVERTEX vrt)
{
	// Zero output.
	CelSphereVS outVS = (CelSphereVS)0;
	float3 posW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	outVS.tex0	= vrt.tex0;
	return outVS;
}

float4 SpaceTechPS(CelSphereVS frg) : COLOR
{
	float3 vColor = tex2D(DiffTexS, frg.tex0).rgb * fAlpha;
	return float4(vColor, 1.0);
}

technique SkyDomeTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 SpaceTechVS();
		pixelShader  = compile ps_3_0 SpaceTechPS();

		AlphaBlendEnable = false;
		ZEnable = false;
		ZWriteEnable = false;
	}
}