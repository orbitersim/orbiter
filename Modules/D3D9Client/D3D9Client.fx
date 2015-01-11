// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ==============================================================

// -------------------------------------------------------------------------------------------------------------
// D3D9Client rendering techniques for Orbiter Spaceflight simulator
// -------------------------------------------------------------------------------------------------------------


#define NIGHT_CLOUDS 0.05f          // range(0.0f-0.1f) Cloud ambient level at night
#define CLOUD_INTENSITY 1.8f        // range(0.5f-2.0f)
#define NIGHT_LIGHTS 0.7f           // range(0.2f-1.0f)

struct Mat
{
	float4 diffuse;	  
	float4 ambient;   
	float4 specular;  
    float4 emissive;  
	float  specPower; 
};

struct Mtrl
{
	float4 diffuse;	  
	float4 ambient;   
	float4 specular;  
    float4 emissive;  
	float4 reflect;
	float4 fresnel;	
	float  dislscale;		
	float  dislmag;		  
};

struct Light {
    int      type;             /* Type of light source */
    float4   diffuse;          /* diffuse color of light */
    float4   specular;         /* specular color of light */
    float4   ambient;          /* ambient color of light */
    float3   position;         /* position in world space */
    float3   direction;        /* direction in world space */
    float3   attenuation;      /* Attenuation */
    float4   param;            /* range, falloff, theta, phi */  
};

#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3

// -------------------------------------------------------------------------
//uniform extern Mtrl		 gMtrlInst[4];		// Instance Materials
//uniform extern float 	 gMtrlAlphaInst[4];	// Instance Alpha
uniform extern float4x4  gGrpInst[8];		// Instance Matrices
// -------------------------------------------------------------------------
uniform extern float4x4  gW;			    // World matrix
uniform extern float4x4  gWI;			    // Inverse World matrix
uniform extern float4x4  gVP;			    // Combined View and Projection matrix
uniform extern float4x4  gGrpT;	            // Mesh group transformation matrix
uniform extern float4x4  gGrpTI;	        // Inverse mesh group transformation matrix
uniform extern float4    gAttennuate;       // (Mesh Constant Fog) Attennuation of fragment color
uniform extern float4    gInScatter;        // (Mesh Constant Fog) In scattering light
uniform extern float4    gColor;            // General purpose color parameter
uniform extern float4    gFogColor;         // Distance fog color in "Legacy" implementation
uniform extern float4    gAtmColor;         // Atmospheric Color of the Proxy Gbody.
uniform extern float4    gTexOff;			// Texture offsets used by surface manager
uniform extern float4    gRadius;           // PlanetRad, AtmOuterLimit, CameraRad, CameraAlt
uniform extern float3    gCameraPos;        // Planet relative camera position, Unit vector 
uniform extern float3    gCamOff;			// Custom camera offset
uniform extern Light     gLights[12];     
uniform extern int       gLightCount;      
uniform extern Light     gSun;			    // Sun light input structure
uniform extern Mat       gMat;			    // Material input structure  TODO:  Remove all reference to this. Use gMtrl
uniform extern Mat       gWater;			// Water material input structure
uniform extern Mtrl      gMtrl;			    // Material input structure
uniform extern bool      gModAlpha;		    // Configuration input
uniform extern bool      gFullyLit;			// Always fully lit bypass lighting calculations		
uniform extern bool      gNormalMap;		// Enable Normal Maps
uniform extern bool      gTextured;			// Enable Diffuse Texturing
uniform extern bool      gClamp;			// Texture addressing mode Clamp/Wrap
uniform extern bool      gNight;			// Nighttime/Daytime
uniform extern bool      gUseEmis;			// Enable Emission Maps
uniform extern bool      gUseSpec;			// Enable Specular Maps
uniform extern bool      gDebugHL;			// Enable Debug Highlighting
uniform extern bool      gEnvMapEnable;		// Enable Environment mapping
uniform extern bool      gUseDisl;			// Enable dissolve effect
uniform extern bool      gUseRefl;			// Enable dissolve effect
uniform extern bool		 gInSpace;			// True if a mesh is located in space
uniform extern bool		 gInstanced;		// True if a mesh is using instancing
uniform extern bool		 gLocalLights;		// Local light sources enabled
uniform extern bool		 gGlow;				// Planet glow enabled
uniform extern int       gSpecMode;
uniform extern int       gHazeMode;
uniform extern float     gProxySize;		// Cosine of the angular size of the Proxy Gbody. (one half)
uniform extern float	 gInvProxySize;		// = 1.0 / (1.0f-gProxySize)
uniform extern float     gPointScale;
uniform extern float     gDistScale;
uniform extern float     gFogDensity;
uniform extern float     gTime;			  
uniform extern float     gMix;				// General purpose parameter (multible uses)
uniform extern float 	 gMtrlAlpha;
uniform extern float	 gGlowConst;

// Textures -----------------------------------------------------------------

uniform extern texture   gTex0;			    // Diffuse texture
uniform extern texture   gTex1;			    // Nightlights
uniform extern texture   gTex3;				// Normal Map / Cloud Microtexture
uniform extern texture   gSpecMap;			// Specular Map
uniform extern texture   gEmisMap;	    	// Emission Map
uniform extern texture   gEnvMap;	    	// Environment Map
uniform extern texture   gDislMap;   		// Dissolve Map
uniform extern texture   gReflMap;   		// Reflectivity Map

// Legacy Atmosphere --------------------------------------------------------

uniform extern float     gGlobalAmb;        // Global Ambient Level        
uniform extern float     gSunAppRad;        // Sun apparent size (Radius / Distance)
uniform extern float     gDispersion;       
uniform extern float     gAmbient0;         


// -------------------------------------------------------------------------------------------------------------
// Vertex layouts
// -------------------------------------------------------------------------------------------------------------

struct MESH_VERTEX {                        // D3D9Client Mesh vertex layout
    float3 posL   : POSITION0;
    float3 nrmL   : NORMAL0;
    float3 tanL   : TANGENT0;
    float2 tex0   : TEXCOORD0;
	int2   idx	  : BLENDINDICES;
};

struct NTVERTEX {                           // Orbiter Mesh vertex layout
    float3 posL     : POSITION0;
    float3 nrmL     : NORMAL0;
    float2 tex0     : TEXCOORD0;
};

struct TILEVERTEX {                         // Vertex declaration used for surface tiles and cloud layer
    float3 posL     : POSITION0;
    float3 normalL  : NORMAL0;
    float2 tex0     : TEXCOORD0;
    float2 tex1     : TEXCOORD1;
};

struct HZVERTEX {
    float3 posL     : POSITION0;
    float4 color    : COLOR0;
    float2 tex0     : TEXCOORD0;
};

struct SHADOW_VERTEX {
    float4 posL     : POSITION0;
};
 

// -------------------------------------------------------------------------------------------------------------
// Vertex shader outputs 
// -------------------------------------------------------------------------------------------------------------

struct SimpleVS
{
    float4 posH     : POSITION0;
    half2  tex0     : TEXCOORD0;
    half3  nrmW     : TEXCOORD1;
    half3  toCamW   : TEXCOORD2;
};

struct HazeVS
{
    float4 posH    : POSITION0;
    float4 color   : TEXCOORD0;
    float2 tex0    : TEXCOORD1;
};

struct BShadowVS
{
    float4 posH    : POSITION0;
};

// -------------------------------------------------------------------------------------------------------------
// Texture Sampler implementations
// -------------------------------------------------------------------------------------------------------------

sampler WrapS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler ClampS = sampler_state      // Base tile sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = CLAMP;
    AddressV = CLAMP;
};

sampler SpecS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gSpecMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler EmisS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gEmisMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler ReflS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gReflMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler Tex1S = sampler_state       // Secundary mesh texture sampler (i.e. night texture) 
{
	Texture = <gTex1>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler Nrm0S = sampler_state       // Normal Map Sampler
{
	Texture = <gTex3>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler MFDSamp = sampler_state     // Virtual Cockpit MFD screen sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
    AddressV = CLAMP;
};

sampler Panel0S = sampler_state     // Sampler for mesh based panels, Panel MFDs. Must be compatible with Non-power of two conditional due to MFD screens.
{
	Texture = <gTex0>;
	MinFilter = POINT;
	MagFilter = LINEAR;
	MipFilter = NONE;
	AddressU  = CLAMP;
    AddressV  = CLAMP;
};

sampler SimpleS = sampler_state       // Sampler used for SimpleTech. (Star, VC HUD)
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
    MipMapLODBias = 0;
	AddressU = CLAMP; // Modified for RC29 to fix the line issue in top-right corner
    AddressV = CLAMP; 
};

sampler ExhaustS = sampler_state
{
	Texture = <gTex0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = NONE;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
    AddressV = CLAMP;
};

sampler RingS = sampler_state       // Planetary rings sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
    AddressV = WRAP;
};

sampler EnvMapS = sampler_state
{
	Texture = <gEnvMap>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
    AddressV = CLAMP;
    AddressW = CLAMP;
};

sampler DislMapS = sampler_state
{
	Texture = <gDislMap>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = WRAP;
    AddressV = WRAP;
};



// Planet surface samplers -----------------------------------------------------

sampler Planet0S = sampler_state    // Planet/Cloud diffuse texture sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
    AddressV = CLAMP;
};

sampler Planet1S = sampler_state    // Planet nightlights/specular mask sampler
{
	Texture = <gTex1>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
    AddressV = CLAMP;
};

sampler Planet3S = sampler_state    // Planet/Cloud micro texture sampler
{
	Texture = <gTex3>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
    AddressV = WRAP;
};



// -------------------------------------------------------------------------------------------------------------
// Atmospheric Haze implementation
//
// att = attennuation, ins = inscatter, depth = pixel depth [0 to 1], posW = camera centric world space position of the vertex
// -------------------------------------------------------------------------------------------------------------

void AtmosphericHaze(out half4 att, out half4 ins, in float depth, in float3 posW)
{
    if (gHazeMode==0) {
        att = 1;
        ins = 0;
        return;
    }
    else if (gHazeMode==1) {
        att = gAttennuate;
        ins = gInScatter;
        return;
    }
    else if (gHazeMode==2) {
        float fogFact = 1.0f / exp(max(0,depth) * gFogDensity);
        att = fogFact; 
        ins = half4((1.0f-fogFact) * gFogColor.rgb, 0.0f);
        return;
    }
}


// -------------------------------------------------------------------------------------------------------------
// Legacy sun color on planet surface. Used for planet surface, base tiles and buildings.
// See SurfaceLighting() in D3D9Util.cpp
//
// -------------------------------------------------------------------------------------------------------------

void LegacySunColor(out half4 diff, out float ambi, out float nigh, in float3 normalW)
{
	float   h = dot(-gSun.direction, normalW);
	float   s = saturate((h+gSunAppRad)/(2.0f*gSunAppRad));
	float3 r0 = 1.0 - float3(0.65, 0.75, 1.0) * gDispersion;

	if (gDispersion!=0) { // case 1: planet has atmosphere
		float3 di = (r0 + (1.0-r0) * saturate(h*5.780)) * s; 
		float  ni = (h+0.242)*2.924;	
		float  am = saturate(max(gAmbient0*saturate(ni)-0.05, gGlobalAmb));
	
        diff = float4(di*(1.0-am*0.5),1);
        ambi = am;
        nigh = saturate(-ni-0.2);
	} 
	else { // case 2: planet has no atmosphere
        diff = float4(r0*s, 1);
        ambi = gGlobalAmb;
        nigh = 0;
	}
}



void LocalVertexLight(out float3 diff, out float3 spec, out float3 dir, in float3 nrmW, in float3 posW, in float sp)
{
    float3 diffuse = 0;
    float3 specular = 0;
    float3 direction = 0;

    int i;
    for (i=0;i<gLightCount;i++) 
    {
        float3 relpW = posW - gLights[i].position;
        float3 relpN = normalize(relpW);
		float  dst   = dot(relpW, relpN);

        float att    = rcp(dot(gLights[i].attenuation.xyz, float3(1.0, dst, dst*dst)));
        float spt    = saturate((dot(relpN, gLights[i].direction)-gLights[i].param[Phi]) * gLights[i].param[Theta]);
        
        float d      = saturate(dot(-relpN, nrmW));
        float s      = pow(saturate(dot(reflect(relpN, nrmW), normalize(-posW))), sp);

        if (gMtrl.specular.a<2.0 || d==0) s = 0.0f;
        if (gLights[i].type==1) spt = 1.0f;         // Point light -> set spotlight factor to 1

        float dif = (att*spt);

        diffuse   += gLights[i].diffuse.rgb * (dif * d);
        specular  += gLights[i].specular.rgb * (dif * s);
        direction += relpN * (dif * d);
    }  
   
    diff = 1.5 - exp2(-diffuse.rgb)*1.5;
    spec = 1.5 - exp2(-specular.rgb)*1.5;
    dir  = normalize(direction);
}



// -------------------------------------------------------------------------------------------------------------
// Vertex shader implementations
// -------------------------------------------------------------------------------------------------------------


SimpleVS BasicVS(NTVERTEX vrt)
{
	SimpleVS outVS = (SimpleVS)0;
	float3 posW  = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH   = mul(float4(posW, 1.0f), gVP);
    outVS.nrmW   = mul(float4(vrt.nrmL, 0.0f), gW).xyz;
    outVS.toCamW = -posW;
	outVS.tex0   = vrt.tex0;
    return outVS;
}



// -------------------------------------------------------------------------------------------------------------
// PixelShader Implementations
// -------------------------------------------------------------------------------------------------------------

float4 SimpleTechPS(SimpleVS frg) : COLOR
{
    return tex2D(SimpleS, frg.tex0);
}

float4 PanelTechPS(SimpleVS frg) : COLOR
{
	return tex2D(SimpleS, frg.tex0*gMix);
}

float4 PanelTechBPS(SimpleVS frg) : COLOR
{
    return tex2D(Panel0S, frg.tex0*gMix);
}

float4 ExhaustTechPS(SimpleVS frg) : COLOR
{
    float4 c = tex2D(ExhaustS, frg.tex0);
    return float4(c.rgb, c.a*gMix);
}

float4 SpotTechPS(SimpleVS frg) : COLOR
{
    return (tex2D(SimpleS, frg.tex0) * gColor) * gMix;
}

#include "Particle.fx"
#include "Mesh.fx"
#include "Vessel.fx"
#include "CelestialSphere.fx"
#include "HorizonHaze.fx"
#include "Planet.fx"
#include "BeaconArray.fx"


BShadowVS ArrowTechVS(float3 posL : POSITION0)
{
	// Zero output.
	BShadowVS outVS = (BShadowVS)0;
	float3 posW = mul(float4(posL, 1.0f), gW).xyz; // Apply world transformation matrix
	outVS.posH = mul(float4(posW, 1.0f), gVP); // Apply view projection matrix
	return outVS;
}

float4 ArrowTechPS(BShadowVS frg) : COLOR
{
	return gColor;
}


// This is used for rendering grapple points ------------------------------------------
//
technique ArrowTech
{
    pass P0
    {
	vertexShader = compile VS_MOD ArrowTechVS();
	pixelShader = compile PS_MOD ArrowTechPS();

	AlphaBlendEnable = true;
	BlendOp = Add;
	SrcBlend = SrcAlpha;
	DestBlend = InvSrcAlpha;
	ZWriteEnable = false;
	ZEnable = true;
    } 
}


// This is used for many simple renderings -------------------------------------
//
technique SimpleTech
{
    pass P0
    {
        vertexShader = compile VS_MOD BasicVS();
        pixelShader  = compile PS_MOD SimpleTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }   
}

// This is used for 2DPanel and Glass cockpit ----------------------------------
//
technique PanelTech
{
    pass P0
    {
        vertexShader = compile VS_MOD BasicVS();
        pixelShader  = compile PS_MOD PanelTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
		SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }   
}

technique PanelTechB
{
    pass P0
    {
        vertexShader = compile VS_MOD BasicVS();
        pixelShader  = compile PS_MOD PanelTechBPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }   
}


// Thil will render exhaust textures -------------------------------------------
//
technique ExhaustTech
{
    pass P0
    {
        vertexShader = compile VS_MOD BasicVS();
        pixelShader  = compile PS_MOD ExhaustTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZWriteEnable = false;
        ZEnable = true;
    }   
}

// This is used for rendering beacons ------------------------------------------
//
technique SpotTech
{
    pass P0
    {
        vertexShader = compile VS_MOD BasicVS();
        pixelShader  = compile PS_MOD SpotTechPS();
        
        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZWriteEnable = false;
        ZEnable = true;
    }   
}
