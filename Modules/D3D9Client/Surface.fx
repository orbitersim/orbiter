// =============================================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
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
    float4 tex      : TEXCOORD0;  // Texture coordinate
    float3 normalW  : TEXCOORD1; 
    float3 toCamW   : TEXCOORD2;  // Vector from vertex to the camera
    float4 aux      : TEXCOORD3;  // Specular, Unused, Twilight, Night Texture Intensity
    float3 sunlight : TEXCOORD4;  // Sun light color
	float3 atten    : COLOR0;     // Attennuation of incoming fragment color
    float3 insca    : COLOR1;     // "Inscatter" Added to incoming fragment color 
};

struct HazeVS
{
    float4 posH    : POSITION0;
    float3 insca   : TEXCOORD0;
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
uniform extern float	fHorizonDst;		// Camera to horizon distance
uniform extern int		iMode;
uniform extern bool		bOverSat;			// Avoid over-saturation

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
	c = saturate(c);
	float  c2 = c*c;
	return rcp(dot(float4(1.0f, c, c2, c2*c), vODCoEff));
}
	
// -------------------------------------------------------------------------------
// Atmospheric scattering implementation
// -------------------------------------------------------------------------------
    
void SurfaceScatterFast(out float3 vOuts, out float3 vIns, out float3 vSun, in float3 vPosW, in float3 vRay)
{    
    float  fRay;
    float3 vAlt;
    
    float3 vp0  = vPosW + vCameraPos;				// Compute geo-centric vertex position
    float  fRd2 = dot(vp0,vp0);						// Square radius
    float  fRdR = rsqrt(fRd2);						// Reciprocal Vextex distance from a geo-center
    float  fRad = rcp(fRdR);						// Vertex distance from a geo-centre
    float3 vNr0 = vp0*fRdR;							// Surface normal at vertex location
   	   
    // Compute the length of the viewing ray to a camera or a skydome.
    //
    if (fCameraAlt>(fScaleHeight*5.0)) {   			// TODO: Should use boolean to create a static branch for better efficiency
		float rdt = -fRad * dot(vRay, vNr0);
		fRay = rdt + sqrt(fAtmRad2 - (fRd2 - rdt*rdt));	
    } else {
		fRay = length(vPosW);
	}	
	
	float  fDNS = dot(vNr0, vSunDir);				// Dot Normal Sun
    float fRPha = RPhase(dot(vRay, vSunDir));		// Compute rayleigh phase factor			
	
	vAlt[0] = fRad - fRadius;						// Sample point 0 (i.e. Vertex) altitude
	vAlt[2] = min(fScaleHeight*5.0, fCameraAlt);	// Sample point 2 (i.e. Camera or atmosphere upper limit altitude)
	vAlt[1] = (vAlt[0]+vAlt[2])*0.5;				// Sample point 1 altitude
	
    float3 vDns	= exp(-vAlt*fInvScaleHeight);		// Compute atmospheric density for all sample points
    
    // Evaluate the Gauss-Lobatto integral
    float fExRay0 = dot(vDns, vWeight3) * (fRay * fInvScaleHeight * 0.5f);
    
    // Evaluate optical depth between vertex and the Sun. (Accurate)
    float fExSun0 = vDns[0] * AngleCoEff(fDNS);		
    
    // Compute surface texture color attennuation (i.e. extinction term)
    vOuts = exp(-vRayTotal * fExRay0);
    
    // Compute sunlight color received by a vertex
    vSun = exp(-vRaySurface * fExSun0) * fSrfIntensity;
	
	// Compute in-scattering 
    vIns = (vRayInSct * exp(-vRayTotal * (fExSun0*fBalance*2.0))) * (fSunIntensity * fRPha * fExRay0 * saturate(fDNS*3.0));
    
    if (bOverSat) vIns = 1.0 - exp(-vIns);
}


void SkyScatterFast(out float3 vIns, in float3 vPosW, in float3 vRay)
{    
    float  fRay;
    float3 vAlt;
    
    float3 vp0  = vPosW + vCameraPos;					// Compute geo-centric vertex position
    float  fRd2 = dot(vp0,vp0);							// Square radius
    float  fRdR = rsqrt(fRd2);							// Reciprocal Vextex distance from a geo-center
    float  fRad = rcp(fRdR);							// Vertex distance from a geo-centre
    float3 vNr0 = vp0*fRdR;								// Surface normal at vertex location
    float fRPha = RPhase(dot(vRay, vSunDir));			// Compute rayleigh phase factor			
	
	vAlt[0] = fRad - fRadius;							// Sample point 0 (i.e. Skydome vertex) altitude
	vAlt[2] = fCameraAlt;								// Sample point 2 (i.e. Camera) altitude
	vAlt[1] = (vAlt[0]+vAlt[2])*0.5;					// Sample point 1 altitude
	
    float3 vDns	= exp(-vAlt*fInvScaleHeight);			// Compute atmospheric density for all sample points
    
    // Evaluate the Gauss-Lobatto integral
    float fExRay0 = dot(vDns, vWeight3) * (fHorizonDst * fInvScaleHeight * 0.5f);
    
    // Add the remainder of the ray
    //fExRay0 += vDns[0] * AngleCoEff(dot(vNr0,vRay));		
    
    float fExSun0 = vDns[0] * AngleCoEff(dot(vNr0, vSunDir));
    float fExSun2 = vDns[2] * AngleCoEff(dot(vUnitCameraPos, vSunDir));
    
	// Compute in-scattering 
    vIns = (vRayInSct * exp(-vRayTotal * (fExSun0+fExSun2) * fBalance * 2.0)) * (fSunIntensity * fRPha * fExRay0);
    
    if (bOverSat) vIns = 1.0 - exp(-vIns);
}
	


// =============================================================================================================
// Planet Surface Renderer
// =============================================================================================================

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
    
    if (iMode==0) SurfaceScatterFast(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
    //if (iMode==1) SurfaceScatterMedium(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
    //if (iMode==2) SurfaceScatterExperiment(outVS.atten, outVS.insca, outVS.sunlight, posW, vRay); 
     
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
	
	float dt = saturate(dot(vSunDir, frg.normalW));
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





// =============================================================================================================
// Cloud Layer Renderer
// =============================================================================================================

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
    
	outVS.sunlight = float3(1,1,1);
    
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






// =============================================================================================================
// Render Horizon "Ring" from the space
// =============================================================================================================

HazeVS RingTechVS(float3 posL : POSITION0)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;

	posL.x *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.z *= lerp(vTexOff[0], vTexOff[1], posL.y);
	posL.y  = lerp(vTexOff[2], vTexOff[3], posL.y);
	
    float3 posW = mul(float4(posL, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	
    return outVS;
}

float4 RingTechPS(HazeVS frg) : COLOR
{
    return float4(1,0,0,1);
}

technique RingTech
{
    pass P0
    {
        vertexShader = compile VS_MOD RingTechVS();
        pixelShader  = compile PS_MOD RingTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }
}






// =============================================================================================================
// Render Skydome
// =============================================================================================================

  


HazeVS SkyTechVS(float3 posL : POSITION0)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;
	
	float fVAlt = posL.y*fHorizonDst;
	
    float3 posW = mul(float4(posL*fHorizonDst, 1.0f), mWorld).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), mViewProj);
	
	// A vector from the vertex to the camera
	float3 vRay   = normalize(-posW);
	
	if (iMode==0) SkyScatterFast(outVS.insca, posW, vRay); 
	
    return outVS;
}

float4 SkyTechPS(HazeVS frg) : COLOR
{
    return float4(frg.insca.rgb, 1.0);
}

technique SkyTech
{
    pass P0
    {
        vertexShader = compile VS_MOD SkyTechVS();
        pixelShader  = compile PS_MOD SkyTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }
}

