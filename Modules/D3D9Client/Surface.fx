// =============================================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2014 Jarmo Nikkanen
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
    float2 texUV    : TEXCOORD0;  // Texture coordinate
    float2 aux      : TEXCOORD1;  // Specular
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
uniform extern float4    vTexOff;			// Texture offsets used by surface manager (i.e. SubTexRange)
uniform extern float4    vWater;			// Water material input structure (specular rgb, power) 
uniform extern float3    vSunDir;			// Unit Vector towards the Sun
uniform extern float4    vGeneric;          // Generic Multi-use vector
// ------------------------------------------------------------
uniform extern float     fDistScale;		// UNUSED: Scale factor
uniform extern float 	 fAlpha;			// Cloud shodow alpha
uniform extern float 	 fNight;			// Nightlights intensity
// ------------------------------------------------------------
uniform extern bool      bSpecular;			// Enable water
uniform extern bool      bCloudSh;			// Enable cloud shadows
uniform extern bool      bLights;			// Enable night-lights
// Textures ---------------------------------------------------
uniform extern texture   tDiff;				// Diffuse texture
uniform extern texture   tMask;				// Nightlights / Specular mask texture
uniform extern texture   tNoise;			// Pre-computed sunlight



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


// -------------------------------------------------------------------------------------------------------------
// Atmospheric scattering model 
// -------------------------------------------------------------------------------------------------------------

uniform extern float4	vMPhase;			// Pre-computed factors used in Mie phase function
uniform extern float4	vODCoEff;			// Optical Depth Taylor co-efficients
uniform extern float4	vODCoEffEx;			// Optical Depth Taylor co-efficients
uniform extern float3	vMieInSct;			// Mie scattering = M_in/(lambda^m)
uniform extern float3	vRayInSct;			// Rayleigh scattering = R_in/(lambda^r)
uniform extern float3	vOutTotSun;			// Total out-scattering for in-coming sunlight  = R_outsun/(lambda^r) + M_outsun/(lambda^m)
uniform extern float3	vOutTotSrf;			// Total out-scattering for a planet's surface  = R_outsrf/(lambda^r) + M_outsrf/(lambda^m)
uniform extern float3	vWhiteBalance;	
uniform extern float3   vCameraPos;         // Geo-centric camera position 
uniform extern float3   vUnitCameraPos;     // Geo-centric camera position (Unit vector)
uniform extern float	fDepthClamp;		// Maximum optical depth for horizon haze (Bound to Horizon slider)
//uniform extern float	fSrfIntensity;		// Sun intensity for surface lighting (Bound to Sun slider)
uniform extern float	fScaleHeight;		// Atmosphere scaleheight
uniform extern float	fInvScaleHeight;	// Inverse Scale Height 1.0f/fScaleHeight		
uniform extern float    fRadius;            // PlanetRad
uniform extern float    fCameraAlt;         // Camera Altitude
uniform extern float    fHorizonAlt;        // Horizon (i.e. Skydome, Atmosphere) Altitude/Height
uniform extern float	fAtmRad2;			// Atmosphere upper radius squared (fRadius+fHorizonAlt)^2
uniform extern float	fRPhase;			// Rayleigh phase factor
uniform extern float	fHorizonDst;		// Camera to horizon distance sqrt(dot(vCameraPos,vCameraPos) - fRadius*fRadius)
uniform extern float	fExposure;			// Camera exposure factor (Bound to Exposure slider)
uniform extern float	fAux1;				// Unused. Bound to Aux1 slider
uniform extern float	fAux2;				// Unused. Bound to Aux2 slider
uniform extern float	fAmbient;			// Planet specific ambient level pre-multiplied with camera altitude factor
uniform extern float	fGlobalAmb;
uniform extern int		iMode;
uniform extern bool		bOverSat;			// Avoid over-saturation
uniform extern bool		bInSpace;			// Camera in the space (i.e. fCameraAlt>fHorizonAlt) 
uniform extern bool		bOnOff;				

// Numeric integration points and weights for Gauss–Lobatto integral
//
const  float4 vWeight4 = {0.167, 0.833, 0.833, 0.167};
const  float4 vPoints4 = {0.0f, 0.27639f, 0.72360f, 1.0f};	
const  float3 vWeight3 = {0.33333f, 1.33333f, 0.33333f};			
const  float3 vPoints3 = {0.0f, 0.5f, 1.0f};


// -------------------------------------------------------------------------------------------------------------
// Henyey-Greenstein Phase function
// x = (1-g^2)/(4pi), y = 1+g^2, w = -2*g
//
float MPhase(float cw)
{
	return vMPhase.z + vMPhase.x * pow(abs(vMPhase.y-vMPhase.w*cw), -1.5f);
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
	//float  c1 = rcp(max(0.0608, c+0.2)); float c2 = c1*c1;  // 98 deg
	float  c1 = rcp(max(0.0954, c+0.2)); float c2 = c1*c1;  // 96 deg
	float4 v1 = float4(1.0f, c1, c2, c2*c1);
	return dot(v1, vODCoEff) + dot(v1*(c2*c2), vODCoEffEx);
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
	float  fDNS = dot(vNr1, vSunDir);

	// Setup altitudes for all sample points
	float3 vAlt = float3(fCameraAlt, dot(vNr1, vPos)-fRadius, fHorizonAlt);
	
	// Atmospheric densities for sample points	
    float3 vDns = exp2(-vAlt*fInvScaleHeight);				
    
	// Mean atmospheric density for a viewing ray
	float fMnD = dot(vDns, vWeight3);

	// Evaluate a Gauss-Lobatto integral (from camera to skydome). Will give optical depth for the ray
    float fDRay = fMnD * (fRay * fInvScaleHeight) * 0.3465735903f;
    
	// Color of inscattered sunlight
    float3 vSun = exp2(-vOutTotSun * (fMnD * AngleCoEff(fDNS))) * fDRay * Shadow(fDNS) * fExposure;
 
	// Compute in-scattering 
    vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun * fDepthClamp;
    
    float fNgt = saturate(fDNS*2.924f+0.657f); 

	// Compute ambient light level for the sky
    vIns += (vRayInSct+1.0f) * (fAmbient * fNgt * saturate(0.5f-max(vIns.b, vIns.r)));

    vIns = 1.0 - exp2(-vIns);
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
	
	if (fRd2>fAtmRad2) {
		vIns = 0;
		return;
	}
	
	// Setup altitudes for all sample points
	float3 vAlt = float3(fHorizonAlt, fRad - fRadius, fHorizonAlt);
	float3 vDns = exp2(-vAlt*fInvScaleHeight);					
	
	// Mean atmospheric density for a viewing ray
	float  fMnD = dot(vDns, vWeight3);

	// Compute secant
	float  fSgt = sqrt(fAtmRad2 - fRd2) * 2.0;

	// Evaluate a Gauss-Lobatto integral. Will give optical depth for the ray
    float fDRay = fMnD * (fSgt * fInvScaleHeight) * 0.3465735903f;

	// Optical depth for incoming sunlight	    
    float fDSun = fMnD * AngleCoEff(fDNS);
    
    float3 vSun = exp2(-vOutTotSun * fDSun) * fDRay * Shadow(fDNS) * fExposure;
    
	// Multiply in-coming light with phase and light scattering factors
    vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun * fDepthClamp;
    vIns = 1.0 - exp2(-vIns);
}

/*void HorizonColor2(out float3 vIns, in float3 vUnitRay)
{    
	float  fDCR = -dot(vUnitCameraPos, vUnitRay);
	float  fHrz = (fCameraAlt+fRadius)*fDCR;
	float3 vPos = vCameraPos + vUnitRay * fHrz;
	float3 vNr1 = normalize(vPos);
    float  fDNS = dot(vNr1, vSunDir);
	float  fDNR = dot(vNr1, vUnitRay);
	float  fRad = dot(vNr1, vPos);				// Geo-centric vertex radius
	float  fDRS = -dot(vUnitRay, vSunDir);
	float  fRd2 = fRad*fRad;
	float  fDns = exp2((fRadius-fRad)*fInvScaleHeight);
	float fDRay = fDns * (AngleCoEff(fDNR) + AngleCoEff(-fDNR)); 
    float  fMnD = fDRay * rsqrt(fAtmRad2 - fRd2) * fScaleHeight * 1.4427f;  
    float fDSun = fMnD * AngleCoEff(fDNS);
    float3 vSun = exp2(-(vRayTotal+vMieInSct) * fDSun) * fDRay * Shadow(fDNS);
    vIns = (vRayInSct*RPhase(fDRS) + vMieInSct*MPhase(fDRS)) * vSun;
    if (bOverSat) vIns = 1.0 - exp2(vIns*fExposure);
}*/







// =============================================================================================================
// Planet Surface Renderer
// =============================================================================================================

#define AUX_SPECULAR	0	// Specular light intensity
#define AUX_NIGHT		1	// Night lights intensity

TileVS SurfaceTechVS(TILEVERTEX vrt)
{
    // Zero output.
	TileVS outVS = (TileVS)0;
	float3 vSunLight;
	
    // Apply a world transformation matrix
    float3 vPosW = mul(float4(vrt.posL, 1.0f), mWorld).xyz;
    float3 vNrmW = mul(float4(vrt.normalL, 0.0f), mWorld).xyz;
	outVS.posH	 = mul(float4(vPosW, 1.0f), mViewProj);

	outVS.texUV  = vTexOff.xy + (vrt.tex0.xy - vTexOff.zw) * vGeneric.xy;
	
	float3 vVrt  = vCameraPos + vPosW;					// Geo-centric vertex position
	float3 vRay  = normalize(-vPosW);					// Unit viewing ray
	float3 vPlN  = normalize(vVrt);						// Planet mean normal at vertex location
	float  fDPS  = dot(vPlN,  vSunDir);					// Dot mean normal, sun direction
	float  fDNS  = dot(vNrmW, vSunDir);					// Dot vertex normal, sun direction
	float  fDRP  = dot(vPlN, vRay);
	float  fNgt	 = (fDPS+0.242f) * 2.924f; 
	
	outVS.aux[AUX_SPECULAR] = pow(saturate(dot(reflect(-vSunDir, vNrmW), vRay)), vWater.a);

	// Camara altitude dependency multiplier for ambient color of atmosphere
	float fAmb = max(saturate(fNgt-0.05f)*fAmbient, fGlobalAmb) * 0.08f;

	if (!bOnOff) {
		float fX = saturate(fDNS)*2.0;
		float fY = saturate(fDRP);
		float fLvl = fX * rcp(fX+fY) * fExposure;
		outVS.atten = max(fLvl, 4.0f*fAmb);
		outVS.insca = 0.0;
		return outVS;
	}

	float  fAlt = dot(vVrt, vPlN) - fRadius;			// Vertex altitude
	float  fDRS = dot(vRay,  vSunDir);					// Dot viewing ray, sun direction
	
	outVS.aux[AUX_NIGHT] = saturate(-fNgt - 0.2f);
   
	if (bInSpace) {
		float fDns  = exp2(-fAlt*fInvScaleHeight);
		float fDRay = fDns * AngleCoEff(dot(vPlN, vRay));
		vSunLight   = exp2(-vOutTotSun * (fDns * AngleCoEff(fDPS))) * fExposure;
		outVS.atten = exp2(-vOutTotSrf * fDRay);
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay * Shadow(fDPS);
	}

	else {

		float fRay = abs(dot(vPosW, vRay));				// Length of the viewing ray	
	
		// Altitude vector for sample points
	  	float3 vAlt = float3(fAlt, (fAlt+fCameraAlt)*0.5, fCameraAlt);	
		
		// Compute atmospheric density for all sample points
		float3 vDns	= exp2(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
	    
		// Evaluate a Gauss-Lobatto integral to give an optical depth for a viewing ray
		float fDRay = dot(vDns, vWeight3) * (fRay * fInvScaleHeight) * 0.3465735903f;
	    
		vSunLight = exp2(-vOutTotSun * (vDns[0] * AngleCoEff(fDPS))) * fExposure;

		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vOutTotSrf * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay * Shadow(fDPS);
	}

	float fX = saturate(fDNS)*2.0;
	float fY = saturate(fDRP);
	float fLvl = fX * rcp(fX+fY);

	outVS.atten *= max(vSunLight * vWhiteBalance * fLvl, (vRayInSct+4.0f) * fAmb);
	outVS.insca = 1.0f - exp2(-outVS.insca);
	
    return outVS;
}	



float4 SurfaceTechPS(TileVS frg) : COLOR
{
	float3 cNgt = 0;
	float3 cSpe = 0;
	float4 cTex = tex2D(DiffTexS, frg.texUV);
	float4 cMsk = tex2D(MaskTexS, frg.texUV);
	
    if (bSpecular) cSpe = ((1.0-cMsk.a) * frg.aux[AUX_SPECULAR]) * vWater.rgb * fAux1;
    if (bLights)   cNgt = cMsk.rgb * (frg.aux[AUX_NIGHT] * fNight);
    
    float3 color = cTex.rgb * frg.atten.rgb * (1.0+cSpe*2.0) + cSpe + frg.insca.rgb + cNgt;
	return float4(color, 1.0f);   
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

	outVS.alpha  = 1.0f - smoothstep(fCameraAlt, fHorizonDst, length(vPosW));

	return outVS;
}	

float4 ShadowTechPS(CloudShVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.texUV);
	return float4(0, 0, 0, fAlpha*cTex.a*frg.alpha);   
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
	float  fDRS  = dot(vRay, vSunDir);			// Dot viewing ray, sun direction
	float  fFct  = pow(saturate(fDPS), 0.5f);

	if (bInSpace) {

		float fDns = exp2(-fAlt*fInvScaleHeight);
		
		// An optical depth for a viewing ray
		float fDRay = fDns * AngleCoEff(dot(vPlN, vRay));
	    
		vSunLight = exp2(-vRayInSct * (fDns * AngleCoEff(fDPS))) * fExposure;

		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vOutTotSrf * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;

		outVS.atten *= vSunLight * fFct + outVS.insca; 
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
	    
		vSunLight = exp2(-vRayInSct * (vDns[0] * AngleCoEff(fDPS))) * fExposure;

		// Compute surface texture color attennuation (i.e. extinction term)
		outVS.atten = exp2(-vOutTotSrf * fDRay);
	    
		// Multiply in-coming light with phase and light scattering factors
		outVS.insca = ((vRayInSct * RPhase(fDRS)) + (vMieInSct * MPhase(fDRS))) * vSunLight * fDRay;

		outVS.atten *= vSunLight * fFct + outVS.insca; 	
	}

	outVS.insca = 1.0 - exp2(-outVS.insca);
	outVS.insca *= vWhiteBalance; 
	outVS.atten *= vWhiteBalance; 

    return outVS;
}


float4 CloudTechPS(CloudVS frg) : COLOR
{
	float4 cTex = tex2D(DiffTexS, frg.texUV);
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
	else outVS.insca = float3(0.5, 0, 0);
	
	outVS.insca *= vWhiteBalance; 

    return outVS;
}

float4 RingTechPS(HazeVS frg) : COLOR
{
	float a = (tex2Dlod(NoiseTexS, float4(frg.texUV,0,0)).r - 0.5f) * 0.008;
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
	else outVS.insca = float3(0, 0.9, 0.9);
	
	outVS.insca *= vWhiteBalance; 

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