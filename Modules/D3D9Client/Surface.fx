// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ==============================================================

// -------------------------------------------------------------------------------------------------------------
// Shader File for TileManager2 and 3D Terrain implementation
// -------------------------------------------------------------------------------------------------------------

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
    float4 tex      : TEXCOORD0;  // Texture coordinate
    float3 normalW  : TEXCOORD1; 
    float3 toCamW   : TEXCOORD2;  // Vector from vertex to the camera
    float4 aux      : TEXCOORD3;  // Specular, Unused, Twilight, Night Texture Intensity
    float3 sunlight : TEXCOORD4;  // Sun light color
	float3 atten    : COLOR0;     // Attennuation of incoming fragment color
    float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color 
};



// -------------------------------------------------------------------------------------------------
// Global shader variables
// -------------------------------------------------------------------------------------------------

uniform extern float4x4  mWorld;		    // World matrix
uniform extern float4x4  mViewProj;			// Combined View and Projection matrix
// ------------------------------------------------------------
uniform extern float4    vTexOff;			// Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vWater;			// Water material input structure (specular rgb, power) 
uniform extern float3    vSunDir;			// Sun light direction
uniform extern float4    vAddBkg;           // Planet color through atmosphere
uniform extern float4    vTint;				// Surface Tint ? 
// ------------------------------------------------------------
uniform extern float     fDistScale;		// Scale factor
uniform extern float 	 fAlpha;			// Cloud shodow alpha
uniform extern float 	 fNight;			// Nightlights intensity
// ------------------------------------------------------------
uniform extern bool      bSpecular;			// Enable water
uniform extern bool      bCloudSh;			// Enable cloud shadows
uniform extern bool      bLights;			// Enable night-lights
uniform extern bool      bLegacyAtm;		// Use DX7 atmosphere model

// Textures ---------------------------------------------------
uniform extern texture   tDiff;				// Diffuse texture
uniform extern texture   tMask;				// Nightlights / Mask texture

// Legacy Atmosphere Model -------------------------------------
uniform extern float4	 vFogColor;
uniform extern float     fFogDensity;
uniform extern float     fGlobalAmb;        // Global Ambient Level        
uniform extern float     fSunAppRad;        // Sun apparent size (Radius / Distance)
uniform extern float     fDispersion;       
uniform extern float     fAmbient0;         



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


// -------------------------------------------------------------------------------------------------------------
// Legacy atmospheric haze implementation
// att = attennuation, ins = inscatter, depth = pixel depth [0 to 1]
// -------------------------------------------------------------------------------------------------------------

void LegacyHaze(out float4 att, out float4 ins, in float depth)
{
	float fogFact = 1.0f / exp(max(0,depth) * fFogDensity);
	att = fogFact; 
	ins = float4((1.0f-fogFact) * vFogColor.rgb, 0.0f); 
}


// -------------------------------------------------------------------------------------------------------------
// Legacy sun color on planet surface. Used for planet surface, base tiles and buildings.
// See SurfaceLighting() in D3D9Util.cpp
// -------------------------------------------------------------------------------------------------------------

void LegacySunColor(out float4 diff, out float ambi, in float3 normalW)
{
	float  h = dot(vSunDir, normalW);
	float  ni = (h+0.242)*2.924;
	float3 r0 = 1.0 - float3(0.65, 0.75, 1.0) * fDispersion;
	
	if (fDispersion!=0) { // case 1: planet has atmosphere
		float3 di = (r0 + (1.0-r0) * saturate(h*5.780)) * saturate((h+fSunAppRad)/(2.0*fSunAppRad)); 
		float  am = saturate(max(fAmbient0*saturate(ni)-0.05, fGlobalAmb));
        diff = float4(di*(1.0-am*0.5),1);
        ambi = am;
	} 
	else { // case 2: planet has no atmosphere
        diff = float4(r0*saturate((h+fSunAppRad)/(2.0f*fSunAppRad)), 1);
        ambi = fGlobalAmb;
	}
}




// -------------------------------------------------------------------------------------------------------------
// Atmospheric scattering model 
// -------------------------------------------------------------------------------------------------------------

uniform extern float4	vPhase;				// Pre-computed factors used in phase functions
uniform extern float4	vODCoEff;			// Optical Depth Taylor co-efficients
uniform extern float3	vRayTotal;			// Total rayleigh scattering for surface 
uniform extern float3	vRaySurface;		// Total rayleigh scattering for sun light to surface
uniform extern float3	vRayInSct;			// Total rayleigh scattering for sun light to 
uniform extern float3	vMieTotal;	
uniform extern float3   vCameraPos;         // Geo-centric camera position 
uniform extern float3   vUnitCameraPos;     // Geo-centric camera position (Unit vector)
uniform extern float	fSunIntensity;		// Sun intensity for inscattering
uniform extern float	fSrfIntensity;		// Sun intensity for surface lighting
uniform extern float	fScaleHeight;		// Atmosphere scaleheight
uniform extern float	fInvScaleHeight;		
uniform extern float    fRadius;            // PlanetRad
uniform extern float    fCameraAlt;         // Camera Altitude
uniform extern float	fAtmRad2;			// Atmosphere upper radius squared (rad+scaleheight*5.0)^2
uniform extern float	fBalance;			// Balance between optical depth of viewing ray and sunlight
uniform extern int		iMode;

// Numeric integration points and weights for Gauss–Lobatto integral
//
const  float4 vSample4 = {-1, -0.447, 0.447, 1};		// Smaple points
const  float4 vWeight4 = {0.167, 0.833, 0.833, 0.167};	// Sample weights
static float4 vPoints4 = (vSample4 + 1.0) * 0.5;		// Map to range 0 to 1

const  float3 vSample3 = {-1, 0, 1};					// Smaple points
const  float3 vWeight3 = {0.333, 1.333, 0.333};			// Sample weights
static float3 vPoints3 = (vSample3 + 1.0) * 0.5;		// Map to range 0 to 1


// -------------------------------------------------------------------------------------------------------------
// Henyey-Greenstein Phase function
// x = (1-g^2)/(4pi), y = 1+g^2
//
float MPhase(float cw)
{
	return vPhase.x * pow((vPhase.y-2.0f*cw), -1.5f);
}

// -------------------------------------------------------------------------------------------------------------
// Rayleigh Phase function
//
float RPhase(float cw)
{
	return (1.0+cw*cw*vPhase.z);
}

// -------------------------------------------------------------------------------------------------------------
// Accurate optical depth, h = ray start altitude, c = cosine of the ray direction to inf (1.0 is up)
//

float AngleCoEff(float c)
{
	c = max(c,-0.2);
	float  c2 = c*c;
	float  dt = dot(float4(1.0, c, c2, c2*c), vODCoEff);
	return pow(max(dt, 2e-7), -2.0f);
}
	
float Depth(float h, float c)
{
	return exp(-h*fInvScaleHeight) * AngleCoEff(c);
}


// -------------------------------------------------------------------------------------------------------------
// Accurate optical depth between camera and vertex
// Note: Does not work if the vertex and camara altitudes are near the same
//
float OpticalDepth(float3 vrt)
{
	// Compute geo-centric vertex position
	float3 vp = vrt * fDistScale + vCameraPos;
	
	// Compute vertex altitude and normal
	float  av = max(0, length(vp) - fRadius);
	float3 nv = normalize(vp);
	
	if (av<fCameraAlt) vrt = -vrt; 
	
	vrt = normalize(vrt);
	
	float ov = Depth(av, dot(nv, vrt));
	float oc = Depth(fCameraAlt, dot(vUnitCameraPos, vrt));
	
	return abs(ov - oc);
}
	
	
// -------------------------------------------------------------------------------
// Atmospheric scattering implementation
// -------------------------------------------------------------------------------
    
void AtmoScatterFast(out float3 vOuts, out float3 vIns, out float3 vSun, in float3 vPosW, in float3 vRay)
{    
    float  fRay;
    float3 vAlt;
    
    float3 vp0  = vPosW + vCameraPos;				// Compute geo-centric vertex position
    float  fRad = length(vp0);						// Vextex distance from a geo-center
    float3 vNr0 = vp0/fRad;							// Surface normal at vertex location
    float  fDNS = dot(vNr0, vSunDir);				
    
    // Compute the length of the viewing ray to a camera or a skydome.
    //
    if (fCameraAlt>(fScaleHeight*5.0)) {   // TODO: Should use boolean to create a static branch for better efficiency
		float rdt = -fRad * dot(vRay, vNr0);
		fRay = rdt + sqrt(fAtmRad2 - (fRad*fRad - rdt*rdt));	
    } else {
		fRay = length(vPosW);
	}			
	
	vAlt[0] = fRad - fRadius;						// Sample point 0 (i.e. Vertex) altitude
	vAlt[2] = min(fScaleHeight*5.0, fCameraAlt);	// Sample point 2 (i.e. Camera or atmosphere upper limit altitude)
	vAlt[1] = (vAlt[0]+vAlt[2])*0.5;				// Sample point 1 altitude
	
    float3 vDns	= exp(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
    
    // Evaluate the Gauss-Lobatto integral and map surface to zenith depth to 1.0 via fInvS.H.
    float  fEx0 = dot(vDns, vWeight3) * (fRay * fInvScaleHeight * 0.5f);
    
    // Compute surface texture color attennuation (i.e. extinction term)
    vOuts = exp(-vRayTotal * fEx0);
    
    // Evaluate optical depth between vertex and the Sun. (Accurate)
    float fExSun  = vDns[0] * AngleCoEff(fDNS);				
  
    // Compute sunlight color received by a vertex
    vSun = exp(-vRaySurface * fExSun) * fSrfIntensity;
	
	// Compute rayleigh phase factor
	float fRPha = RPhase(dot(vRay, vSunDir));
	
    vIns = (1.0-vOuts) * (fSunIntensity * fRPha * saturate(fDNS*2.0));	
}
	


// -------------------------------------------------------------------------------
// Atmospheric scattering implementation
// -------------------------------------------------------------------------------
    
void AtmoScatterMedium(out float3 vOuts, out float3 vIns, out float3 vSun, in float3 vPosW, in float3 vRay)
{  
    float  fRay;
    float4 vAlt;
    
    float3 vp0  = vPosW + vCameraPos;				// Compute geo-centric vertex position
    float3 vNr0 = normalize(vp0);					// Surface normal at vertex location
    float  fRad = length(vp0);						// Vextex distance from a geo-center
    float  fDNS = dot(vNr0, vSunDir);				
    
    // Compute the length of the viewing ray to a camera or a skydome.
    //
    if (fCameraAlt>(fScaleHeight*5.0)) {   // TODO: Should use boolean to create a static branch for better efficiency
		float rdt = -fRad * dot(vRay, vNr0);
		fRay = rdt + sqrt(fAtmRad2 - (fRad*fRad - rdt*rdt));	
    } else {
		fRay = length(vPosW);
	}			
	
	
	float3 vp1 = vp0 + vRay * (vPoints4[1]*fRay);	// Compute geo-centric sample 1 position
	float3 vp2 = vp0 + vRay * (vPoints4[2]*fRay);	// Compute geo-centric sample 2 position
	
	vAlt[0] = fRad - fRadius;						// Sample point 0 (i.e. Vertex) altitude
	vAlt[1] = length(vp1) - fRadius;				// Sample point 1 altitude
	vAlt[2] = length(vp2) - fRadius;				// Sample point 2 altitude
	vAlt[3] = min(fScaleHeight*5.0, fCameraAlt);	// Sample point 3 (i.e. Camera or atmosphere upper limit altitude)
    
    float4 vDns	= exp(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
    
    // Evaluate the Gauss-Lobatto integral and map surface to zenith depth to 1.0 via fInvS.H.
    float  fEx0 = dot(vDns, vWeight4) * (fRay * fInvScaleHeight * 0.5f);
  
    // Compute surface texture color attennuation (i.e. extinction term)
    vOuts = exp(-vRayTotal * fEx0);
    
    // Evaluate optical depth between vertex and the Sun. (Accurate)
    float fExSun0  = vDns[0] * AngleCoEff(dot(vNr0, vSunDir));	
    	
    // Compute sunlight color received by a vertex
    vSun = exp(-vRaySurface * fExSun0) * fSrfIntensity;
	
	// Compute rayleigh phase factor
	float fRPha = RPhase(dot(vRay, vSunDir));	
	
	// Compute in-scattering color correction term
	float3 vIn = vRayInSct * exp(-vRayTotal * fExSun0) * (vDns[1] * fRay * fInvScaleHeight);
		
    vIns = ((1.0-vOuts)*vIn) * (fSunIntensity * fRPha * saturate(fDNS*3.0));
    
    vIns = 1.0 - exp(-vIns);
}
	
	
	
// -------------------------------------------------------------------------------
// Atmospheric scattering implementation
// -------------------------------------------------------------------------------
    
void AtmoScatterAccurate(out float3 vOuts, out float3 vIns, out float3 vSun, in float3 vPosW, in float3 vRay)
{  
    float  fRay;
    float4 vAlt;
    float4 vACoEff;
    
    float3 vp0  = vPosW + vCameraPos;				// Compute geo-centric vertex position
    float3 vNr0 = normalize(vp0);					// Surface normal at vertex location
    float  fRad = length(vp0);						// Vextex distance from a geo-center
    			
    // Compute the length of the viewing ray to a camera or a skydome.
    //
    if (fCameraAlt>(fScaleHeight*5.0)) {   // TODO: Should use boolean to create a static branch for better efficiency
		float rdt = -fRad * dot(vRay, vNr0);
		fRay = rdt + sqrt(fAtmRad2 - (fRad*fRad - rdt*rdt));	
    } else {
		fRay = length(vPosW);
	}			
	
	float3 vp1 = vp0 + vRay * (vPoints4[1]*fRay);	// Compute geo-centric sample 1 position
	float3 vp2 = vp0 + vRay * (vPoints4[2]*fRay);	// Compute geo-centric sample 2 position
	float3 vp3 = vp0 + vRay * (vPoints4[3]*fRay);	// Compute geo-centric sample 3 position
	
	float3 vNr1 = normalize(vp1);
	float3 vNr2 = normalize(vp2);
	float3 vNr3 = normalize(vp3);
	
	vAlt[0] = fRad - fRadius;						// Sample point 0 (i.e. Vertex) altitude
	vAlt[1] = length(vp1) - fRadius;				// Sample point 1 altitude
	vAlt[2] = length(vp2) - fRadius;				// Sample point 2 altitude
	vAlt[3] = min(fScaleHeight*5.0, fCameraAlt);	// Sample point 3 (i.e. Camera or atmosphere upper limit altitude)
    
    float4 vDns	= exp(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
    
    // Integral scale factor
    float fIscale = fRay * fInvScaleHeight * 0.5f;
    
    // Evaluate the Gauss-Lobatto integral for vertex extinction
    float  fRayEx0 = dot(vDns, vWeight4) * fIscale;
    
    // Evaluate rest of the ray extinction terms
    float  fRayEx1 = (vDns[1]*0.5f + vDns[2] + vDns[3]*0.5f) * fIscale * (1-vPoints4[1]); 
    float  fRayEx2 = (vDns[2] + vDns[3]) * fIscale * (1-vPoints4[2]); 
    float  fRayEx3 = 0;
    
    float  fDNS = dot(vNr0, vSunDir);
  
    // Compute angle co-efficiency factors 
    vACoEff[0] = AngleCoEff(dot(vNr0, vSunDir));	
    vACoEff[1] = AngleCoEff(dot(vNr1, vSunDir));
    vACoEff[2] = AngleCoEff(dot(vNr2, vSunDir));
    vACoEff[3] = AngleCoEff(dot(vNr3, vSunDir));
    
    // Compute sunlight extinction integrals
    float4 vSunEx = vACoEff * vDns;
    
    float a = fBalance * 2.0;
    float b = 2.0 - a;
    
    // Evaluate inscattering values for each point
    float3 vIs0 = vWeight4[0] * vDns[0] * exp(vRayTotal*(-vSunEx[0]*a-fRayEx0*b));
    float3 vIs1 = vWeight4[1] * vDns[1] * exp(vRayTotal*(-vSunEx[1]*a-fRayEx1*b));
    float3 vIs2 = vWeight4[2] * vDns[2] * exp(vRayTotal*(-vSunEx[2]*a-fRayEx2*b));
    float3 vIs3 = vWeight4[3] * vDns[3] * exp(vRayTotal*(-vSunEx[3]*a-fRayEx3*b));
    
    // Evaluate final inscattering integral
    float3 vInS = (vRayInSct * (vIs0 + vIs1 + vIs2 + vIs3) * fIscale);
    
    // Compute surface texture color attennuation (i.e. extinction term)
    vOuts = exp(-vRayTotal * fRayEx0);
    
    // Compute sunlight color received by a vertex
    vSun  = exp(-vRaySurface * vSunEx[0]) * fSrfIntensity;
	
	// Compute rayleigh phase factor
	float fRPha = RPhase(dot(vRay, vSunDir));	

	//	
    vIns = vInS * (fSunIntensity * fRPha * saturate(fDNS*8.0+0.8));
    
    vIns = 1.0 - exp(-vIns);
}	
	
	
// -------------------------------------------------------------------------------------------------------------
// Shader implementations
// -------------------------------------------------------------------------------------------------------------

TileVS SurfaceTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS  = (TileVS)0;
	
    // Apply a mesh group transformation matrix
    float3 posW   = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 nrmW   = normalize(mul(float4(vrt.normalL, 0.0f), mWorld).xyz);

	// Convert transformed vertex position into a "screen" space using a combined (World, View and Projection) Matrix
	outVS.posH	  = mul(float4(posW, 1.0f), mViewProj);
	
	// A vector from the vertex to the camera
	float3 vRay   = normalize(-posW);
  
    float spec    = pow(saturate(dot(reflect(-vSunDir, nrmW), vRay)), vWater.a);
 
	outVS.tex     = float4(vrt.tex0.x*vTexOff[0] + vTexOff[1], vrt.tex0.y*vTexOff[2] + vTexOff[3], vrt.tex1);
    outVS.toCamW  = vRay;
    outVS.normalW = nrmW;
    outVS.aux     = float4(spec, 0, 0, 0);
    
    if (iMode==0) AtmoScatterFast(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
    if (iMode==1) AtmoScatterMedium(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
    if (iMode==2) AtmoScatterAccurate(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
      
    return outVS;
}	





#define AUX_UNUSED		1	
#define AUX_SPECULAR	0	// Specular light intensity
#define AUX_AMBIENT		2	// Ambient light level in atmosphere
#define AUX_NIGHT		3	// Night lights intensity


float4 SurfaceTechPS(TileVS frg) : COLOR
{
	float3 cNgt = 0;
	float3 cSpe = 0;
	float4 cTex = tex2D(DiffTexS, frg.tex.xy); // + float4(vTint.rgb, 0);
	float4 cMsk = tex2D(MaskTexS, frg.tex.xy);
	
	float dt = dot(vSunDir, frg.normalW);
	float nl = saturate(-(dt+0.242)*2.924-0.2);
	
    float3 cDif  = frg.sunlight.rgb * saturate(dt);				// Sunlight color and intensity
		   cDif += float3(1,1,1) * frg.aux[AUX_AMBIENT];		// Ambient light color and intencity
     
    if (bSpecular) cSpe = ((1-cMsk.a) * frg.aux[AUX_SPECULAR]) * (vWater.rgb*frg.sunlight.rgb);
    if (bLights)   cNgt = cMsk.rgb * (nl * fNight);
    
    float3 color = cDif * cTex.rgb + cSpe + cNgt;
    float3 final = color * max(frg.atten.rgb,nl) + vAddBkg.rgb + frg.insca.rgb;
  
	return float4(final, 1.0);   
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
}



// -------------------------------------------------------------------------------------------------------------
// Shader implementations
// -------------------------------------------------------------------------------------------------------------

TileVS CloudTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS  = (TileVS)0;
	
    // Apply a mesh group transformation matrix
    float3 posW   = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 nrmW   = normalize(mul(float4(vrt.normalL, 0.0f), mWorld).xyz);

	// Convert transformed vertex position into a "screen" space using a combined (World, View and Projection) Matrix
	outVS.posH	  = mul(float4(posW, 1.0f), mViewProj);
	
	// A vector from the vertex to the camera
	float3 tocam  = normalize(-posW);
   
	outVS.tex     = float4(vrt.tex0.x*vTexOff[0] + vTexOff[1], vrt.tex0.y*vTexOff[2] + vTexOff[3], vrt.tex1);
    outVS.toCamW  = tocam;
    outVS.normalW = nrmW;
    //outVS.posW    = vCameraPos + posW * fDistScale;
   
    //LegacySunColor(outVS.sunlight, ambi, nigh, nrmW);
    
	outVS.sunlight = float4(1,1,1,1);
    
    //AtmosphericHaze(outVS.atten, outVS.insca, outVS.posH.z, posW);
    //outVS.insca *= (outVS.sunlight+ambi);
    
    return outVS;
}


float4 CloudTechPS(TileVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.tex.xy);
	
    float3 cDif  = frg.sunlight.rgb * saturate(dot(vSunDir, frg.normalW)*1.5);	// Sunlight color and intensity
    
    //return float4(cDif * cTex.rgb * frg.atten.rgb + vAddBkg.rgb + frg.insca.rgb, 1.0f);
    
    return float4(cDif * cTex.rgb + vAddBkg.rgb, cTex.a);
}


technique CloudTech
{
    pass P0
    {
        vertexShader = compile VS_MOD CloudTechVS();
        pixelShader  = compile PS_MOD CloudTechPS();
        
        AlphaBlendEnable = true;
        ZEnable = false; 
        ZWriteEnable = false;
    }
}
