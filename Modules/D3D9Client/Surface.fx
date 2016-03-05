// =============================================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 - 2016 Jarmo Nikkanen
// =============================================================================================================


// =============================================================================================================
// Shader File for TileManager2 and 3D Terrain implementation
// Contains a light weight implementation 
// =============================================================================================================



// -------------------------------------------------------------------------------------------------------------
// Vertex input layouts from Vertex buffers to vertex shader
// -------------------------------------------------------------------------------------------------------------

struct TILEVERTEX					// (VERTEX_2TEX) Vertex declaration used for surface tiles and cloud layer 
{                         
    float3 posL     : POSITION0;
    float3 normalL  : NORMAL0;
    float2 tex0     : TEXCOORD0;
    float2 tex1     : TEXCOORD1;
};


// -------------------------------------------------------------------------------------------------------------
// Vertex Shader to Pixel Shader datafeeds
// -------------------------------------------------------------------------------------------------------------

struct TileVS
{
    float4 posH     : POSITION0;
    float4 texUV    : TEXCOORD0;  // Texture coordinate
    float3 aux      : TEXCOORD1;  // Night lights
	float3 camW		: TEXCOORD2;
	float3 nrmW		: TEXCOORD3;
	float3 atten    : COLOR0;     // Attennuation
    float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color 
};

struct CloudVS
{
    float4 posH     : POSITION0;
    float2 texUV    : TEXCOORD0;  // Texture coordinate
	float3 atten    : COLOR0;     // Attennuation
    float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color 
};

struct CloudShVS
{
    float4 posH     : POSITION0;
    float2 texUV    : TEXCOORD0;  // Texture coordinate
	float  alpha	: TEXCOORD1;
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




// -------------------------------------------------------------------------------------------------
// Global shader variables
// -------------------------------------------------------------------------------------------------

uniform extern float4x4  mWorld;		    // World matrix
uniform extern float4x4  mViewProj;			// Combined View and Projection matrix
// ------------------------------------------------------------
uniform extern float4    vMSc0;				// Micro Texture A scale factors
uniform extern float4    vMSc1;				// Micro Texture B scale factors
uniform extern float4    vMSc2;				// Micro texture C scale factors
uniform extern float4    vTexOff;			// Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vWater;			// Water material input structure (specular rgb, power) 
uniform extern float3    vSunDir;			// Unit Vector towards the Sun
uniform extern float4    vGeneric;          // Generic Multi-use vector
uniform extern float3    vTangent;			// Unit Vector
uniform extern float3    vBiTangent;		// Unit Vector
uniform extern float3    vMapUVOffset;		// 
// ------------------------------------------------------------
uniform extern float     fDistScale;		// UNUSED: Scale factor
uniform extern float 	 fAlpha;			// Cloud shodow alpha
uniform extern float 	 fNight;			// Nightlights intensity
// ------------------------------------------------------------
uniform extern bool      bSpecular;			// Enable water
uniform extern bool      bCloudSh;			// Enable cloud shadows
uniform extern bool      bLights;			// Enable night-lights
uniform extern bool      bEnvEnable;		// Enable environment maps
uniform extern bool      bMicro;			// Enable micro texture
uniform extern bool      bMicroNormals;		// Enable micro texture
uniform extern int		 iTileLvl;			// Surface tile level being rendered
uniform extern int		 iDebug;			// Debug Mode identifier
uniform extern bool		 bDebug;			// Debug Mode enabled 
// Textures ---------------------------------------------------
uniform extern texture   tDiff;				// Diffuse texture
uniform extern texture   tMask;				// Nightlights / Specular mask texture
uniform extern texture   tNoise;			// 
uniform extern texture	 tOcean;			// Ocean Texture
uniform extern texture	 tEnvMap;	
uniform extern texture	 tMicroA;	
uniform extern texture	 tMicroB;
uniform extern texture	 tMicroC;
uniform extern texture	 tMicroRot;



// -------------------------------------------------------------------------------------------------------------
// Texture Sampler implementations
// -------------------------------------------------------------------------------------------------------------

sampler DiffTexS = sampler_state
{
	Texture = <tDiff>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
    MipMapLODBias = 0;
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
	MinFilter = ANISOTROPIC;
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

sampler MicroAS = sampler_state
{
	Texture = <tMicroA>;
	MinFilter = MICRO_FILTER;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = MICRO_ANISOTROPY;
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
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler MicroRT = sampler_state
{
	Texture = <tMicroRot>;
	MinFilter = POINT;
	MagFilter = POINT;
	MipFilter = NONE;
	AddressU = WRAP;
    AddressV = WRAP;
};

// -------------------------------------------------------------------------------------------------------------
// Atmospheric scattering model 
// -------------------------------------------------------------------------------------------------------------

uniform extern float4	vMPhase;			// Pre-computed factors used in Mie phase function
uniform extern float4	vODCoEff;			// Optical Depth Taylor co-efficients
uniform extern float4	vODCoEffEx;			// Optical Depth Taylor co-efficients
uniform extern float3	vMieInSct;			// Mie scattering factor = Mie/(lambda^m)
uniform extern float3	vRayInSct;			// Rayleigh in-scattering = (ROut*RIn)/(lambda^r)
uniform extern float3	vTotOutSct;			// Total out-scattering = ROut/(lambda^r) + Mie/(lambda^m)
uniform extern float3	vWhiteBalance;		// lambda^x
uniform extern float3	vColorShift;		// lerp([1,1,1], 1.0/lambda^r, Aux3)	
uniform extern float3   vCameraPos;         // Geo-centric camera position 
uniform extern float3   vUnitCameraPos;     // Geo-centric camera position (Unit vector)
uniform extern float	fSunset;			// Sunset color corrector (Bound to sunset slider)
uniform extern float	fScaleHeight;		// Atmosphere scaleheight
uniform extern float	fInvScaleHeight;	// Inverse Scale Height 1.0f/fScaleHeight	
uniform extern float	fInvMieScaleHeight;	// Inverse of Mie scale height
uniform extern float    fRadius;            // PlanetRad
uniform extern float    fCameraAlt;         // Camera Altitude
uniform extern float    fHorizonAlt;        // Horizon (i.e. Skydome, Atmosphere) Altitude/Height
uniform extern float	fAtmRad2;			// Atmosphere upper radius squared (fRadius+fHorizonAlt)^2
uniform extern float	fRPhase;			// Rayleigh phase factor
uniform extern float	fHorizonDst;		// Camera to horizon distance sqrt(dot(vCameraPos,vCameraPos) - fRadius*fRadius)
uniform extern float	fExposure;			// Camera exposure factor (Bound to Exposure slider)
uniform extern float	fAux1;				// Bound to Aux1 slider
uniform extern float	fAux2;				// Bound to Aux2 slider
uniform extern float	fAux4;				// Bound to Aux4 slider
uniform extern float	fInvAux1;			// Inverse of fAux1
uniform extern float	fInvParameter;		// Inverse of optical parameter 1.0/AngleCoEff(0)
uniform extern float	fAmbient;			// Planet specific ambient level pre-multiplied with camera altitude factor
uniform extern float	fGlobalAmb;
uniform extern float	fTime;				// 
uniform extern bool		bInSpace;			// Camera in the space (i.e. fCameraAlt>fHorizonAlt) 
uniform extern bool		bOnOff;				

// Numeric integration points and weights for Gauss–Lobatto integral
//
const  float4 vWeight4 = {0.167, 0.833, 0.833, 0.167};
const  float4 vPoints4 = {0.0f, 0.27639f, 0.72360f, 1.0f};	
const  float3 vWeight3 = {0.33333f, 1.33333f, 0.33333f};			
const  float3 vPoints3 = {0.0f, 0.5f, 1.0f};

const float srfoffset = -0.04;

// -------------------------------------------------------------------------------------------------------------
// Henyey-Greenstein Phase function
// x = (1-g^2)/(4pi), y = 1+g^2, w = -2*g
//
float MPhase(float cw)
{
	return max(1, vMPhase.x * pow(abs(vMPhase.y-vMPhase.w*cw), -1.5f));
}

// -------------------------------------------------------------------------------------------------------------
// Rayleigh Phase function
//
float RPhase(float cw)
{
	return (1.0+cw*cw*fRPhase);
}

// -------------------------------------------------------------------------------------------------------------
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
		float x = saturate(iTileLvl * 0.05);
		return float4(color, 1) * float4(x, 0.0, 1-x, 1.0);
	}
	
	return float4(0,0,0,1);
}


// =============================================================================================================
// Atmospheric scattering implementation. (Will render horizon and sky-color as seen from the atmosphere)
// -------------------------------------------------------------------------------------------------------------
// vIns = Light in scattering 
// vUnitRay = Unit Vector pointing in ray direction. (away from the camera)
// =============================================================================================================    

void SkyColor(out float3 vIns, in float3 vUnitRay)
{    

	float  fCam = fCameraAlt + fRadius;
	float  fPrm = fCam * dot(vUnitRay, vUnitCameraPos);
	float  fRay = sqrt(fAtmRad2 - (fCam*fCam - fPrm*fPrm)) - fPrm;
	float3 vRay = vUnitRay * fRay;
	float  fDRS = -dot(vUnitRay, vSunDir);
	float3 vPos = vCameraPos + vRay*0.5f;
	float3 vNr1 = normalize(vPos);
	float  fAlt = dot(vNr1, vPos)-fRadius;
	float  fDNS = dot(vNr1, vSunDir);
	float  fDNR = dot(vNr1, vUnitRay);
	
	// Setup altitudes for all sample points
	float3 vAlt = float3(fCameraAlt, fAlt, fHorizonAlt);
	
	// Atmospheric densities for sample points	
    float3 vDns = exp2(-vAlt*fInvScaleHeight);	

	// Mean atmospheric density for a viewing ray
	float  fMnD = dot(vDns, vWeight3);

	// Evaluate a Gauss-Lobatto integral (from camera to skydome). Will give optical depth for the ray
    float  fDep = fMnD * (fRay * fInvScaleHeight) * 0.3465735903f;

	float  fASn = AngleCoEff(fDNS);
	float  fANr = AngleCoEff(fDNR);
	
	// Optical Depth correction for sunset
	float  fCmD = vDns[0] * (fANr*fANr) * fASn * (-fDRS+1.0f) * fInvParameter;
	
	// Color of inscattered sunlight
	float3 vSun = exp2(-vTotOutSct * (fAux2*fMnD*fASn + fCmD*fSunset)) * fDep * 0.5f * Shadow(fDNS, 0);

	// Compute in-scattering 
    vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun;
    
    float fNgt = saturate(fDNS*2.924f+0.657f); 

	// Compute ambient light level for the sky
    vIns += (vRayInSct+1.0f) * (fAmbient * fNgt * saturate(0.5f-max(vIns.b, vIns.r)));

    vIns = (1.0f - exp2(vIns * vColorShift)) * vWhiteBalance;

	vIns = pow(abs(vIns), fAux4);  
}






// =============================================================================================================
// Atmospheric scattering implementation. (Renders the horizon ring as seen from the space)
// -------------------------------------------------------------------------------------------------------------
// vIns = Light in scattering 
// fVtxAlt = Vertex altitude
// vPosW = Camera relative vertex position
// vUnitRay = Unit Vector from vertex to a camera
// =============================================================================================================    

void HorizonColor(out float3 vIns, in float3 vUnitRay)
{    
	float  fDCR = -dot(vUnitCameraPos, vUnitRay);
	float  fHrz = (fCameraAlt+fRadius)*fDCR;

	float3 vPos = vCameraPos + vUnitRay * fHrz;
	float3 vNr1 = normalize(vPos);
    float  fDNS = dot(vNr1, vSunDir);
	float  fRad = dot(vNr1, vPos);				// Geo-centric vertex radius
	float  fDRS = -dot(vUnitRay, vSunDir);
	float  fRd2 = fRad*fRad;
	float  fAlt = fRad - fRadius;
	
	if (fRd2>fAtmRad2) {
		vIns = 0;
		return;
	}
	
	// Setup altitudes for all sample points
	float3 vAlt = float3(fHorizonAlt, fAlt, fHorizonAlt);
	float3 vDns = exp2(-vAlt*fInvScaleHeight);	

	// Mean atmospheric density for a viewing ray
	float  fMnD = dot(vDns, vWeight3);
	
	// Compute secant
	float  fSgt = sqrt(fAtmRad2 - fRd2);

	// Evaluate a Gauss-Lobatto integral. Will give optical depth for the ray
    float  fDep = fMnD * fSgt * fInvScaleHeight * 0.6931471806f;

	// Optical depth for incoming sunlight	
	float fCoEff = fAux2 * AngleCoEff(fDNS);
  
    float3 vSun = exp2(-vTotOutSct * fMnD * fCoEff) * fDep * Shadow(fDNS, 0);
	
	// Multiply in-coming light with phase and light scattering factors
    vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun;

    vIns = (1.0f - exp2(vIns * vColorShift)) * vWhiteBalance;

	vIns = pow(abs(vIns), fAux4);  
}




// =============================================================================================================
// Planet Surface Renderer
// =============================================================================================================

#define AUX_DIST		0	// Vertex distance
#define AUX_NIGHT		1	// Night lights intensity
#define AUX_SLOPE		2   // Terrain slope factor 0.0=flat, 1.0=sloped

TileVS SurfaceTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS = (TileVS)0;
	float3 vSunLight;
	
    // Apply a world transformation matrix
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);
	
	float3 vVrt  = vCameraPos + vPosW;					// Geo-centric vertex position
	float3 vPlN  = normalize(vVrt);
	float3 vRay  = normalize(-vPosW);					// Unit viewing ray
	float  fDPS  = dot(vPlN,  vSunDir);					// Dot mean normal, sun direction
	float  fRay  = abs(dot(vPosW, vRay));				// Length of the viewing ray
	float  fNgt	 = fDPS * 4.0f; 
	outVS.camW   = -vPosW;
	outVS.nrmW   = vNrmW;

	// vTexOff, vGeneric only used in cloud shadow rendering
	outVS.texUV.xy  = vTexOff.xy + (vrt.tex0.xy - vTexOff.zw) * vGeneric.xy;
	outVS.texUV.zw  = vrt.tex1.xy;

	outVS.aux[AUX_NIGHT] = -fNgt;
	outVS.aux[AUX_DIST]  =  fRay;

	// If no atmosphere skip the rest
	if (!bOnOff) return outVS;

	// Create UV coords for water
	if (bSpecular) {
		outVS.texUV.zw  = float2(dot(vTangent, vPosW+vMapUVOffset), dot(vBiTangent, vPosW+vMapUVOffset));
		outVS.texUV.zw *= 1e-3f; // Water scale factor
		outVS.texUV.zw += 64.0f; 
		outVS.texUV.z  += fTime*0.008f;
	}

	// Camara altitude dependency multiplier for ambient color of atmosphere
	float fAmb = max(saturate(fNgt+0.9f)*fAmbient, fGlobalAmb) * 0.08f;

	float  fDPR  = dot(vPlN,  vRay);					// Dot mean normal, viewing ray
	float  fDNR	 = dot(vNrmW, vRay);
	float  fDNS  = dot(vNrmW, vSunDir);					// Dot vertex normal, sun direction
	float  fDRS  = dot(vRay,  vSunDir);					// Dot viewing ray, sun direction
	float  fAlt  = dot(vVrt, vPlN) - fRadius;			// Vertex altitude
	
	if (bInSpace) {
		float fDns  = exp2(-fAlt*fInvScaleHeight);
		float fDRay = fDns * AngleCoEff(fDPR);
		vSunLight   = exp2(-vTotOutSct * (fAux2 * fDns * 0.5f * AngleCoEff(fDPS))) * Shadow(fDPS, srfoffset);
		outVS.atten = exp2(-vTotOutSct * fDRay);
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;
	}

	else {

		//float fRay = abs(dot(vPosW, vRay));				// Length of the viewing ray	
	
		// Altitude vector for sample points
	  	float3 vAlt = float3(fAlt, (fAlt+fCameraAlt)*0.5, fCameraAlt);	
		
		// Compute atmospheric density for all sample points
		float3 vDns = exp2(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
	    
		// Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
		float fDRay = dot(vDns, vWeight3) * (fRay * fInvScaleHeight) * 0.3465735903f;
	    
		vSunLight = exp2(-vTotOutSct * (fAux2 * vDns[0] * 0.5f * AngleCoEff(fDPS))) * Shadow(fDPS, srfoffset);
	
		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vTotOutSct * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;
	}

	//float fX = saturate(fDNS);
	//outVS.atten *= max(vSunLight * fX, (vRayInSct+4.0f) * fAmb);
	outVS.atten *= max(vSunLight, (vRayInSct+4.0f) * fAmb);
	
    return outVS;
}	


float4 SurfaceTechPS(TileVS frg) : COLOR
{
	float3 cNgt = 0;
	float3 cSpe = 0;
	float4 cTex = tex2D(DiffTexS, frg.texUV.xy);
	float4 cMsk = tex2D(MaskTexS, frg.texUV.xy);

	float3 nrmW = frg.nrmW;					// Per-pixel surface normal vector
	float3 nvrW = frg.nrmW;					// Per-pixel surface normal vector
	float3 camW = normalize(frg.camW);		// Unit viewing ray
	float3 vVrt = vCameraPos - frg.camW;	// Geo-centric pixel position
	float3 vPlN = normalize(vVrt);			// Planet mean normal	
	float  fRad = dot(vVrt, vPlN);			// Pixel Geo-distance
	
	// Specular Water reflection
	//
	if (bSpecular) {

		// Specular Mask
		float m = (1.0 - cMsk.a) * saturate(0.5f-frg.aux[AUX_NIGHT]*2.0f);

		#if defined(_SURFACERIPPLES)
			float Fct = min(2.0f, 10000.0f / fCameraAlt);
			float3 cNrm = (tex2D(OceaTexS, frg.texUV.zw).xyz - 0.5f) * Fct;
			cNrm.z = cos(cNrm.x * cNrm.y * 1.570796); 
			// Compute world space normal 
			nrmW = (vTangent * cNrm.r) + (vBiTangent * cNrm.g) + (vPlN * cNrm.b);
			nrmW = lerp(nvrW, nrmW, m); 
		#endif

		// Compute specular reflection intensity 
		float s = dot(reflect(-vSunDir, nrmW), camW);
		cSpe = m * pow(saturate(s), 200.0f) * vWater.rgb * 2.0f;

		// Compute Fresnel term
		float f = 1.0-saturate(dot(camW, nrmW));
		float f4 = f*f*f*f;

		float3 cSky = float3(1.2, 1.4, 3.5) * 0.7;
		// Apply fresnel reflection
		cTex.rgb = lerp(cTex.rgb, cSky, m * f4);
	}

	else {

		if (bMicro) {

			float dist = frg.aux[AUX_DIST];

			//float2 UV  = frg.texUV.xy;
			//float2 UVr = frg.texUV.xy * vMSc2.zw + vMSc2.xy;

			float2 UV  = frg.texUV.zw;
			float2 UVr = frg.texUV.zw * vMSc2.zw + vMSc2.xy;

			/*#if defined(_MICROROTATIONS)
				// Noise texture size 128x128
				float fRot = tex2D(MicroRT, frg.texUV.xy*16*0.25f).r;
				if (fRot<0.5f) UVr = float2(-UVr.y, UVr.x);					// Rotate 90deg
				if (fRot<0.35) UVr = -UVr;									// Rotate 180deg
				if (fRot>0.65) UVr = -UVr;									// Rotate 180deg
			#endif*/
		
			#if defined(_DEVELOPPERMODE)
				// Normal in .rg luminance in .b
				float3 cFar = tex2D(MicroCS, UVr).rgb;						// High altitude micro texture C
				float3 cMed = tex2D(MicroBS, UV*vMSc1.zw+vMSc1.xy).rgb;		// Medimum altitude micro texture B
				float3 cLow = tex2D(MicroAS, UV*vMSc0.zw+vMSc0.xy).rgb;		// Low altitude micro texture A
			#else
				// Normal in .ag luminance in .b
				float3 cFar = tex2D(MicroCS, UVr).agb;						// High altitude micro texture C
				float3 cMed = tex2D(MicroBS, UV*vMSc1.zw+vMSc1.xy).agb;		// Medimum altitude micro texture B
				float3 cLow = tex2D(MicroAS, UV*vMSc0.zw+vMSc0.xy).agb;		// Low altitude micro texture A
			#endif

			float step1 = smoothstep(50000, 20000, dist);

			/*
			#if defined(_MICROROTATIONS)
				// Rotate Normals
				if (fRot<0.5f) cFar.xy = float2(cFar.y, 1.0f-cFar.x);		// Rotate 90deg
				if (fRot<0.35) cFar.xy = 1.0f-cFar.xy;						// Rotate 180deg
				if (fRot>0.65) cFar.xy = 1.0f-cFar.xy;						// Rotate 180deg
				// Normal Null range
				cFar.rg = (abs(cFar.rg-0.5)>0.01 ? cFar.rg : 0.5f);
			#endif*/
			
			// OPTIONAL TEXTURE BLEND EQUATIONS -------------------------------
			//float3 cFnl = (cFar+0.5f) * (cMed+0.5f) * (cLow+0.5f);
			//float3 cFnl = (cFar + cMed + cLow) * 0.6666f;
			  float3 cFnl = max(0, min(2, 1.33333f*(cFar+cMed+cLow)-1));
			//float3 cFnl = pow(abs(cFar*cMed*cLow), 0.33333f) * 2.0f;

			// INTERESTING RESULTS
			//float3 hi = max(cFar, max(cMed, cLow));
			//float3 lo = min(cFar, min(cMed, cLow));
			//float3 cFnl = (hi-0.5 > 0.5-lo ? hi : lo) * 2;
			

			// Create normals
			if (bMicroNormals) {
				cFnl = cFnl.bbb;
				#if BLEND==0 
					float2 cMix = (cFar.rg + cMed.rg + cLow.rg) * 0.6666f;				// SOFT BLEND
				#elif BLEND==1 
					float2 cMix  = (cFar.rg+0.5f) * (cMed.rg+0.5f) * (cLow.rg+0.5f);	// MEDIUM BLEND
				#else 
					float2 cMix  = cFar.rg * cMed.rg * cLow.rg * 8.0f;					// HARD BLEND
				#endif

				float3 cNrm  = float3((cMix - 1.0f) * 2.0f, 0) * step1;
				cNrm.z = cos(cNrm.x * cNrm.y * 1.57); 
				// Approximate world space normal
				nrmW = normalize((vTangent * cNrm.x) + (vBiTangent * cNrm.y) + (nvrW * cNrm.z));
			}
		
			// Apply luminance
			cTex.rgb *= lerp(1.0f, cFnl, step1);
		}
	}

	// Is Debug mode enabled
	//
	if (bDebug) return DebugProg(nrmW, cTex.rgb, fRad);
	
	// Do we have an atmosphere ?
	//
	if (!bOnOff) { // No

		float fTrS = saturate(dot(nvrW, vSunDir)*10.0f);	// Shadowing by terrain
		float fPlS = saturate(dot(vPlN, vSunDir)*10.0f);	// Shadowing by planet

		float fDNS = dot(nrmW, vSunDir);
		float fDNR = dot(nrmW, camW);
		float fDRS = dot(camW, vSunDir);

		float fX = saturate(fDNS);						// Lambertian
		float fY = 0.05 + saturate(fDNR)*0.4;			// Lommel-Seeliger compensation
		float fZ = pow(abs(fDRS),10.0f) * 0.3f;			// Shadow compensation
		float fLvl = fX * (fZ+rcp(fX+fY)) * fExposure;	// Bake all together

		// Apply shadows
		fLvl *= (fTrS * fPlS);

		float3 color = cTex.rgb * max(fLvl, 0);			// Apply sunlight

		return float4(pow(abs(color), fAux4), 1.0f);	// Gamma corrention
	}

	else { // Yes, atmosphere is present

		// Night lights
		if (bLights) cNgt = 3.0f * cMsk.rgb * (saturate(frg.aux[AUX_NIGHT]) * fNight);

		// Lambertian shading term
		float fDNS = dot(nrmW, vSunDir);

		// Compose final color, take atmospheric attennuation and inscatter in account
		float3 color = cTex.rgb * frg.atten.rgb * saturate(fDNS) + frg.insca.rgb;

		// Add Specular component and Night lights
		color += cSpe + cTex.rgb*cNgt;

		// Apply color exposure and "white balance"
		color = (1.0f - exp2(-color*fExposure)) * vWhiteBalance;

		// Apply gamma correction
		return float4(pow(abs(color), fAux4), 1.0f);  
	}
}


// =============================================================================================================
// Cloud Layer Renderer
// =============================================================================================================

CloudVS CloudTechVS(TILEVERTEX vrt)
{
    // Zero output.
	CloudVS outVS = (CloudVS)0;
	float3 vSunLight = 1.0;
	
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);
	outVS.texUV  = vTexOff.xy + (vrt.tex0.xy - vTexOff.zw) * vGeneric.xy;
	float3 vVrt  = vCameraPos + vPosW;			// Geo-centric vertex position
	float3 vRay  = normalize(-vPosW);			// Unit viewing ray
	float3 vPlN  = normalize(vVrt);				// Planet mean normal at vertex location
	float  fAlt  = dot(vVrt, vPlN) - fRadius;	// Vertex altitude
	float  fDPS  = dot(vPlN, vSunDir);			// Dot mean normal, sun direction
	float  fDRP  = dot(vPlN, vRay);				// Dot mean normal, sun direction
	float  fDRS  = dot(vRay, vSunDir);			// Dot viewing ray, sun direction
	
	float fX = saturate(fDPS);
	float fY = saturate(fDRP);
	float fLvl = fX * rcp(fX+fY);

	if (bInSpace) {

		float fDns = exp2(-fAlt*fInvScaleHeight);
		
		// An optical depth for a viewing ray
		float fDRay = fDns * AngleCoEff(dot(vPlN, vRay));
	    
		vSunLight = exp2(-vRayInSct * (fAux2 * fDns * AngleCoEff(fDPS)));

		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vTotOutSct * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay * Shadow(fDPS, srfoffset);
		outVS.atten *= vSunLight * fLvl + outVS.insca; 	
	}
	else {

		// Length of the viewing ray
		float fRay = abs(dot(vPosW, vRay));					
	
		// Altitude vector for sample points
	  	float3 vAlt = float3(fAlt, (fAlt+fCameraAlt)*0.5, fCameraAlt);	
		
		// Compute atmospheric density for all sample points
		float3 vDns	= exp2(-vAlt*fInvScaleHeight);
	    
		// Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
		float fDRay = dot(vDns, vWeight3) * (fRay * fInvScaleHeight) * 0.3465735903f;
	    
		vSunLight = exp2(-vRayInSct * (fAux2 * vDns[0] * AngleCoEff(fDPS)));

		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vTotOutSct * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay * Shadow(fDPS, srfoffset);
		outVS.atten *= vSunLight * fLvl + outVS.insca; 	
	}

    return outVS;
}


float4 CloudTechPS(CloudVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.texUV);
	float3 color = cTex.rgb * frg.atten.rgb + frg.insca.rgb;
   	return float4((1.0f - exp2(-color*fExposure))*vWhiteBalance, cTex.a); 
}


technique CloudTech
{
    pass P0
    {
        vertexShader = compile VS_MOD CloudTechVS();
        pixelShader  = compile PS_MOD CloudTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false; 
        ZWriteEnable = false;
    }
}

// =============================================================================================================
// Cloud Shadow Renderer
// =============================================================================================================

CloudShVS ShadowTechVS(TILEVERTEX vrt)
{
    // Zero output.
	CloudShVS outVS = (CloudShVS)0;
    // Apply a world transformation matrix
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);
	outVS.texUV  = vTexOff.xy + (vrt.tex0.xy - vTexOff.zw) * vGeneric.xy;

	float step   = smoothstep(fHorizonDst*0.5f, fCameraAlt, length(vPosW));
	outVS.alpha  = saturate(step*step);

	return outVS;
}	

float4 ShadowTechPS(CloudShVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.texUV);
	return float4(0, 0, 0, (fAlpha)*cTex.a*frg.alpha);   
}


technique CloudShadowTech
{
    pass P0
    {
        vertexShader = compile VS_MOD ShadowTechVS();
        pixelShader  = compile PS_MOD ShadowTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false; 
        ZWriteEnable = false;
    }
}


// This is used for high resolution base tiles ---------------------------------
//
technique TileTech
{
    pass P0
    {
        vertexShader = compile VS_MOD SurfaceTechVS();
        pixelShader  = compile PS_MOD SurfaceTechPS();
        
        AlphaBlendEnable = false;
        ZEnable = true; 
        ZWriteEnable = true;
    }

	pass P1
    {
        vertexShader = compile VS_MOD ShadowTechVS();
        pixelShader  = compile PS_MOD ShadowTechPS();
        
        AlphaBlendEnable = true;
		SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false; 
        ZWriteEnable = false;
    }
}

technique TileTechNoZ
{
    pass P0
    {
        vertexShader = compile VS_MOD SurfaceTechVS();
        pixelShader  = compile PS_MOD SurfaceTechPS();
        
        AlphaBlendEnable = false;
        ZEnable = false; 
        ZWriteEnable = false;
    }
}





// =============================================================================================================
// Render Horizon "Ring" from the space
// =============================================================================================================

HazeVS RingTechVS(float3 posL : POSITION0)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;
	
	posL.xz *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y   = lerp(vTexOff[2], vTexOff[3], posL.y);
	
    float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	
	if (bOnOff) HorizonColor(outVS.insca, normalize(posW));
	else outVS.insca = float3(0, 0, 0);

    return outVS;
}

float4 RingTechPS(HazeVS frg) : COLOR
{
	float a = (tex2Dlod(NoiseTexS, float4(frg.texUV,0,0)).r - 0.5f) * 0.01;
    return float4(frg.insca.rgb+a, 1.0f);
}

technique RingTech
{
    pass P0
    {
        vertexShader = compile VS_MOD RingTechVS();
        pixelShader  = compile PS_MOD RingTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = One;
        DestBlend = One;
        ZEnable = false;
        ZWriteEnable = false;
    }
}






// =============================================================================================================
// Render SkyDome and Horizon
// =============================================================================================================

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
        vertexShader = compile VS_MOD HorizonTechVS();
        pixelShader  = compile PS_MOD HorizonTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = One;
        DestBlend = One;
        ZEnable = false;
        ZWriteEnable = false;
    }
}



// =============================================================================================================
// Render Skydome (i.e. Celestial Sphere Background Image Manager) from the Space
// =============================================================================================================

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
        vertexShader = compile VS_MOD SpaceTechVS();
        pixelShader  = compile PS_MOD SpaceTechPS();

        AlphaBlendEnable = false;
        ZEnable = false;
        ZWriteEnable = false;
    }
}