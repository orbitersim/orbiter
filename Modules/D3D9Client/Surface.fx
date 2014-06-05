// =============================================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
// =============================================================================================================

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
    float2 tex      : TEXCOORD0;  // Texture coordinate
    float4 aux      : TEXCOORD1;  // Specular, Unused, Twilight, Night Texture Intensity
	float3 atten    : COLOR0;     // Combined attennuation and sunlight
    float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color 
};

struct HazeVS
{
    float4 posH    : POSITION0;
    float3 insca   : COLOR0;
    float  alpha   : COLOR1;
};

struct CelSphereVS
{
    float4 posH    : POSITION0;
    float2 tex0    : TEXCOORD0;
    float3 insca   : COLOR0;
};



// -------------------------------------------------------------------------------------------------
// Global shader variables
// -------------------------------------------------------------------------------------------------

uniform extern float4x4  mWorld;		    // World matrix
uniform extern float4x4  mViewProj;			// Combined View and Projection matrix
// ------------------------------------------------------------
uniform extern float4    vTexOff;			// Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vWater;			// Water material input structure (specular rgb, power) 
uniform extern float3    vSunDir;			// Unit Vector towards the Sun
uniform extern float4    vAddBkg;           // Planet color through atmosphere
// ------------------------------------------------------------
uniform extern float     fDistScale;		// Scale factor
uniform extern float 	 fAlpha;			// Cloud shodow alpha
uniform extern float 	 fNight;			// Nightlights intensity
// ------------------------------------------------------------
uniform extern bool      bSpecular;			// Enable water
uniform extern bool      bCloudSh;			// Enable cloud shadows
uniform extern bool      bLights;			// Enable night-lights
// Textures ---------------------------------------------------
uniform extern texture   tDiff;				// Diffuse texture
uniform extern texture   tMask;				// Nightlights / Mask texture

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
// Atmospheric scattering model 
// -------------------------------------------------------------------------------------------------------------

uniform extern float4	vPhase;				// Pre-computed factors used in phase functions
uniform extern float4	vODCoEff;			// Optical Depth Taylor co-efficients
uniform extern float4	vODCoEffEx;			// Optical Depth Taylor co-efficients
uniform extern float3	vRayTotal;			// Total rayleigh scattering for surface 
uniform extern float3	vRaySurface;		// Total rayleigh scattering for sun light to surface
uniform extern float3	vRayInSct;			// Total rayleigh scattering for sun light to 
uniform extern float3	vMieTotal;	
uniform extern float3   vCameraPos;         // Geo-centric camera position 
uniform extern float3   vUnitCameraPos;     // Geo-centric camera position (Unit vector)
uniform extern float	fDepthClamp;		// Maximum optical depth for horizon haze
uniform extern float	fSrfIntensity;		// Sun intensity for surface lighting
uniform extern float	fScaleHeight;		// Atmosphere scaleheight
uniform extern float	fInvScaleHeight;	// Inverse Scale Height 1.0f/fScaleHeight		
uniform extern float    fRadius;            // PlanetRad
uniform extern float    fCameraAlt;         // Camera Altitude
uniform extern float    fHorizonAlt;        // Horizon (i.e. Skydome, Atmosphere) Altitude/Height
uniform extern float	fAtmRad2;			// Atmosphere upper radius squared (fRadius+fHorizonAlt)^2
uniform extern float	fBalance;			// Inscattering Color balance controller
uniform extern float	fHorizonDst;		// Camera to horizon distance sqrt(dot(vCameraPos,vCameraPos) - fRadius*fRadius)
uniform extern float	fExposure;			// Camera exposure factor
uniform extern float	fAux1;				// Light transfer distance in atmosphere
uniform extern float	fAux2;				// Color attennuation during transfer
uniform extern float	fAmbient0;
uniform extern float	fGlobalAmb;
uniform extern int		iMode;
uniform extern bool		bOverSat;			// Avoid over-saturation
uniform extern bool		bInSpace;			// Camera in the space (i.e. fCameraAlt>fHorizonAlt) 
uniform extern bool		bOnOff;				

static float fInvDepthClamp = 1.0/fDepthClamp;

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
// x = (1-g^2)/(4pi), y = 1+g^2, w = -2*g
//
float MPhase(float cw)
{
	return vPhase.x * pow(abs(vPhase.y-vPhase.w*cw), -1.5f);
}

// -------------------------------------------------------------------------------------------------------------
// Rayleigh Phase function
//
float RPhase(float cw)
{
	return (1.0+cw*cw*vPhase.z);
}

// -------------------------------------------------------------------------------------------------------------
// Optical depth angle co-efficiency factor. (Valid for angles 0 to 90deg)
// c = cosine of the ray direction (1.0 is up)
//

float AngleCoEff(float c)
{
	c = saturate(c); float c2 = c*c;
	return rcp(dot(float4(1.0f, c, c2, c2*c), vODCoEff));
}

// -------------------------------------------------------------------------------------------------------------
// Optical depth angle co-efficiency factor. (Valid for angles 80 to 100deg)
// c = cosine of the ray direction (1.0 is up)
//
float AngleCoEffEx(float c)
{
	float c1 = 1.0-c; 
	float c2 = c1*c1;
	float rv = exp2(dot(float4(1.0f, c1, c2, c2*c1), vODCoEffEx));
	return rv;
}


float Shadow(float c)
{
	return saturate(1.0+c*8.0);
}

// Compute shadowing by planet, see if the ray intersects the surface and how much
//
float ShadowEx(float c, float alt)
{
	if (c>0) return 1.0;
	float fVtx = alt + fRadius;
	float fHrz = fVtx * c;
	float fAlt = sqrt(fVtx*fVtx - fHrz*fHrz) - fRadius;

	return smoothstep(-fHorizonAlt, 0, fAlt);
}





// =============================================================================================================
// Atmospheric scattering implementation (Renders a planet surface/clouds from atmosphere and space)
// -------------------------------------------------------------------------------------------------------------
// vAttenuate = Out Scattering (Surface Texture Attennuation)
// vInscatter = In Scattering (Will be added to a pixel color)
// vSunLight = Color of the sun light received by the surface
// vPosW = Camera relative vertex position
// vRay = Unit Vector from vertex to a camera
// vSMN = Surface mean normal at vertex location 
// =============================================================================================================    

void SurfaceScatterFast(out float3 vAttenuate, out float3 vInscatter, out float3 vSunLight, in float3 vPosW, in float3 vRay, in float3 vSMN)
{    
    float  fRay;
    float4 vAlt;
    
    float3 vp0  = vPosW + vCameraPos;				// Compute geo-centric vertex position
    float  fRad = dot(vp0, vSMN);					// Vertex distance from a geo-centre
  
    // Compute the length of the viewing ray to a camera or a skydome.
    //
    if (bInSpace) {   			
		float rdt = -fRad * dot(vRay, vSMN);
		fRay = rdt + sqrt(fAtmRad2 - (fRad*fRad - rdt*rdt));	
    } else {
		fRay = length(vPosW);
	}	
	
	float fDNS = dot(vSMN, vSunDir);				// Dot Normal Sun
    float fDRS = dot(vRay, vSunDir);				// Compute rayleigh phase factor	
  		
	vAlt[0] = fRad - fRadius;						// Sample point 0 (i.e. Vertex) altitude
	vAlt[3] = min(fHorizonAlt, fCameraAlt);			// Sample point 3 (i.e. Camera or Skydome altitude)
	vAlt[1] = lerp(vAlt[0], vAlt[3], vPoints4[1]);	// Sample point 1 altitude
	vAlt[2] = lerp(vAlt[0], vAlt[3], vPoints4[2]);	// Sample point 2 altitude
	
    float4 vDns	= exp2(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
    
	// Mean atmospheric density for a viewing ray
	float fMnD	= dot(vDns, vWeight4) * 0.5f;

    // Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
    float fRay0 = fMnD * (fRay * fInvScaleHeight);
    
	// Angle co-efficiency factor for incoming sunlight
	float fSCo = max(AngleCoEff(fDNS), AngleCoEffEx(fDNS));

	// Limit the optical depth
    fRay0 = fDepthClamp * (1.0f-exp2(-fRay0*fInvDepthClamp));

	// Compute sunlight color received by a vertex
    vSunLight = exp2(-vRaySurface * vDns[0] * fSCo) * fSrfIntensity * Shadow(fDNS);
    
    // Compute surface texture color attennuation (i.e. extinction term)
    vAttenuate = exp2(-vRayTotal * fRay0);
    
	// Color of inscattered sunlight
	float3 vSun = exp2(-vRayTotal * (fMnD * fSCo * fBalance)) * fRay0 * Shadow(fDNS);
    
	// Multiply in-coming light with phase and light scattering factors
    vInscatter = (vRayInSct * vSun) * RPhase(fDRS);
    
    if (bOverSat) vInscatter = 1.0 - exp2(vInscatter*fExposure);
}






// =============================================================================================================
// Atmospheric scattering implementation. (Will render horizon and sky-color as seen from the atmosphere)
// -------------------------------------------------------------------------------------------------------------
// vIns = Light in scattering 
// vUnitRay = Unit Vector pointing in ray direction. (away from the camera)
// =============================================================================================================    

void SkyScatterFast(out float3 vIns, in float3 vUnitRay)
{    
	float4 vAlt;
	
	// Ray length from a camera to skydome
	// 
	float  fCam = fCameraAlt + fRadius;
	float  fPrm = fCam * dot(vUnitRay, vUnitCameraPos);
	float  fRay = sqrt(fAtmRad2 - (fCam*fCam - fPrm*fPrm)) - fPrm;
	float3 vRay = vUnitRay * fRay;
	
	float fDRS = -dot(vUnitRay, vSunDir);
	
	// Setup altitudes for all sample points
	//
	vAlt[0] = fCameraAlt;
	vAlt[3] = fHorizonAlt;
	vAlt[1] = length(vCameraPos + vRay*vPoints4[1]) - fRadius;
	vAlt[2] = length(vCameraPos + vRay*vPoints4[2]) - fRadius;
	
	// Atmospheric densities for sample points	
    float4 vDns = exp2(-vAlt*fInvScaleHeight);				
    
	// Mean atmospheric density for a viewing ray
	float fMnD = dot(vDns, vWeight4) * 0.5f;

	// Evaluate a Gauss-Lobatto integral (from camera to skydome). Will give optical depth for the ray
    float fDRay = fMnD * (fRay * fInvScaleHeight);
    
	// Limit the optical depth
    fDRay = fDepthClamp * (1.0f-exp2(-fDRay*fInvDepthClamp));
    
	// Normal vector in a middle sample point. 
	float3 vNr1 = normalize(vCameraPos + vRay*0.5f);	
	
	// Normal-sun angle
	float  fDNS = dot(vNr1, vSunDir);
	
	// Optical depth for incoming sunlight	    
    float fDSun = fMnD * AngleCoEff(fDNS);
   
	// Color of inscattered sunlight
    float3 vSun = exp2(-vRayTotal * fDSun * fBalance) * fDRay * Shadow(fDNS);
 
	// Compute in-scattering 
    vIns = (vRayInSct*RPhase(fDRS) + vMieTotal*MPhase(fDRS)) * vSun;
    
	// Camara altitude dependency multiplier for ambient color of atmosphere
    float fMult = saturate((fScaleHeight-fCameraAlt)*fInvScaleHeight);

	// Compute ambient light level for atmosphere
    float fNgt = (fDNS+0.242f) * 2.924f; 

    if (bOverSat) vIns = 1.0 - exp2(vIns*fExposure);
    
	float3 vAmbient	= fAmbient0*saturate(fNgt-0.05f) * fMult;
    
    vIns += vAmbient * saturate(0.5f-max(vIns.b, vIns.r))*2.0;
}






// =============================================================================================================
// Atmospheric scattering implementation. (Renders the horizon ring as seen from the space)
// -------------------------------------------------------------------------------------------------------------
// vIns = Light in scattering 
// fVtxAlt = Vertex altitude
// vPosW = Camera relative vertex position
// vUnitRay = Unit Vector from vertex to a camera
// =============================================================================================================    

void HorizonScatterFast(out float3 vIns, in float fVtxAlt, in float3 vPosW, in float3 vUnitRay)
{    
	float3 vNr0 = normalize(vCameraPos + vPosW);
	
	float fDNR = dot(vNr0, vUnitRay);
    float fDns = exp2(-fVtxAlt*fInvScaleHeight);					
	float fRay = fDns * (AngleCoEffEx(-fDNR) + AngleCoEffEx(fDNR));
   
   	// Limit the optical depth
    fRay = fDepthClamp * (1.0f-exp2(-fRay*fInvDepthClamp));
   	
   	float fDNS = dot(vNr0, vSunDir);
   	 
	// Optical depth for incoming sunlight	    
    float fSun = (fDns+0.5f)*0.5f * AngleCoEff(fDNS);
    
    float3 vSun = exp2(-vRayTotal * fSun * fBalance) * fRay * ShadowEx(fDNS, fVtxAlt);
    
    float  fDRS = -dot(vUnitRay, vSunDir);
    
	// Multiply in-coming light with phase and light scattering factors
    vIns = (vRayInSct*RPhase(fDRS) + vMieTotal*MPhase(fDRS)) * vSun;
    
    if (bOverSat) vIns = 1.0 - exp2(vIns*fExposure);
}









// =============================================================================================================
// Planet Surface Renderer
// =============================================================================================================

#define AUX_UNUSED		1	
#define AUX_SPECULAR	0	// Specular light intensity
#define AUX_AMBIENT		2	// Ambient light level in atmosphere
#define AUX_NIGHT		3	// Night lights intensity

TileVS SurfaceTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS = (TileVS)0;
	
    // Apply a world transformation matrix
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;

	// Convert transformed vertex position into a "screen" space using a combined (View and Projection) Matrix
	outVS.posH	= mul(float4(vPosW, 1.0f), mViewProj);
	outVS.tex   = vrt.tex0 * vTexOff.xz + vTexOff.yw;
	
	// A vector from the vertex to the camera
	float3 vRay = normalize(-vPosW);
	float3 vPlN = normalize(vCameraPos + vPosW);
	float  fNgt	= (dot(vSunDir, vPlN)+0.242f) * 2.924f; 
	
    outVS.aux[AUX_SPECULAR] = pow(saturate(dot(reflect(-vSunDir, vNrmW), vRay)), vWater.a);
    outVS.aux[AUX_NIGHT]	= saturate(-fNgt - 0.2f);
    
    float3 vSunLight;

	float fDNS = dot(vSunDir, vNrmW);
    
	// Render Atmospheric effects ?
	if (bOnOff) {

		SurfaceScatterFast(outVS.atten, outVS.insca, vSunLight, vPosW, vRay, vPlN); 

		// Camara altitude dependency multiplier for ambient color of atmosphere
		float fMult = saturate((fScaleHeight-fCameraAlt)*fInvScaleHeight);

		float3 vAmbient	= max(fAmbient0*saturate(fNgt-0.05f)*fMult, fGlobalAmb);

		outVS.atten *= max((vSunLight*fDNS), vAmbient);
	}
    else {
		outVS.atten = saturate(fDNS);
		outVS.insca = 0;
	}	

    return outVS;
}	



float4 SurfaceTechPS(TileVS frg) : COLOR
{
	float3 cNgt = 0;
	float3 cSpe = 0;
	float4 cTex = tex2D(DiffTexS, frg.tex.xy);
	float4 cMsk = tex2D(MaskTexS, frg.tex.xy);
	
    if (bSpecular) cSpe = ((1.0-cMsk.a) * frg.aux[AUX_SPECULAR]) * vWater.rgb * 1.5f;
    if (bLights)   cNgt = cMsk.rgb * (frg.aux[AUX_NIGHT] * fNight);
    
    float3 color = (cTex.rgb + cSpe) * frg.atten.rgb + vAddBkg.rgb + frg.insca.rgb + cNgt;
  
	return float4(color, 1.0);   
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
// Cloud Layer Renderer
// =============================================================================================================

TileVS CloudTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS   = (TileVS)0;
	
    // Apply a mes h group transformation matrix
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;

	// Convert transformed vertex position into a "screen" space using a combined (World, View and Projection) Matrix
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);
	outVS.tex	 = vrt.tex0 * vTexOff.xz + vTexOff.yw;
	
	// A vector from the vertex to the camera
	float3 vRay  = normalize(-vPosW);
	float3 vPlN  = normalize(vCameraPos + vPosW);
	float  fNgt  = (dot(vSunDir, vPlN)+0.242f) * 2.924f; 
   
	float3 vSunLight;
	
	if (bOnOff) SurfaceScatterFast(outVS.atten, outVS.insca, vSunLight, vPosW, vRay, vPlN); 
    else {
		outVS.atten = 1;
		outVS.insca = float3(0, 0, 0.2);
	}	

	float fDot = pow(saturate(dot(vSunDir, vPlN)), 0.5f);

	// Camara altitude dependency multiplier for ambient color of atmosphere
    float fMult = saturate((fScaleHeight-fCameraAlt)*fInvScaleHeight);

	float3 vAmbient	= max(fAmbient0*saturate(fNgt-0.05f)*fMult, fGlobalAmb);
	
	outVS.atten *= max(vSunLight*fDot*2.0f, vAmbient);
		
    return outVS;
}


float4 CloudTechPS(TileVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.tex.xy);
    return float4(cTex.rgb * frg.atten.rgb + frg.insca.rgb, cTex.a);
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
// Render Horizon "Ring" from the space
// =============================================================================================================

HazeVS RingTechVS(float3 posL : POSITION0)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;
	
	float fVtxAlt = posL.y*fAlpha; 
	
	outVS.alpha = saturate(1.0-smoothstep(0.7f, 0.95f, posL.y));
	
	posL.xz *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y   = lerp(vTexOff[2], vTexOff[3], posL.y);
	
    float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	
	if (bOnOff) HorizonScatterFast(outVS.insca, fVtxAlt, posW, normalize(posW));
	else outVS.insca = float3(0.2, 0, 0);
	
    return outVS;
}

float4 RingTechPS(HazeVS frg) : COLOR
{
	float c = max(frg.insca.r, frg.insca.b)*3.0;
    return float4(frg.insca.rgb, 1.0+c*frg.alpha);
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
// Render Low-Altitude horizon
// =============================================================================================================

HazeVS HorizonTechVS(float3 posL : POSITION0)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;
	
	float fVtxAlt = posL.y*fAlpha;	// Vertex Altitude
	
	outVS.alpha = saturate(1.0-smoothstep(0.7f, 0.95f, posL.y));
	
	posL.xz *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y   = lerp(vTexOff[2], vTexOff[3], posL.y);
	
    float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	
	if (bOnOff) SkyScatterFast(outVS.insca, normalize(posW));
	else outVS.insca = float3(0, 0.2, 0);
	
    return outVS;
}

float4 HorizonTechPS(HazeVS frg) : COLOR
{
	float c = max(frg.insca.r, frg.insca.b)*3.0;
    return float4(frg.insca.rgb, c*frg.alpha);
}

technique HorizonTech
{
    pass P0
    {
        vertexShader = compile VS_MOD HorizonTechVS();
        pixelShader  = compile PS_MOD HorizonTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }
}






// =============================================================================================================
// Render Skydome (i.e. Celestial Sphere Background Image Manager) from Atmosphere
// =============================================================================================================

CelSphereVS SkyDomeTechVS(TILEVERTEX vrt)
{
    // Zero output.
	CelSphereVS outVS = (CelSphereVS)0;
	
    float3 posW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	outVS.tex0	= vrt.tex0;
	
	if (bOnOff) SkyScatterFast(outVS.insca, normalize(posW)); 
	else outVS.insca = float3(0, 0, 0.4);
	
    return outVS;
}

float4 SkyDomeTechPS(CelSphereVS frg) : COLOR
{
    float3 vColor = tex2D(DiffTexS, frg.tex0).rgb * fAlpha + frg.insca.rgb;
    return float4(vColor, 1.0);
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
	float3 vColor = tex2D(DiffTexS, frg.tex0).rgb * fAlpha + frg.insca.rgb;
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
    
    pass P1
    {
        vertexShader = compile VS_MOD SkyDomeTechVS();
        pixelShader  = compile PS_MOD SkyDomeTechPS();

        AlphaBlendEnable = false;
        ZEnable = false;
        ZWriteEnable = false;
    }
}