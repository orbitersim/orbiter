// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

// ----------------------------------------------------------------------------
// D3D9Client rendering techniques for Orbiter Spaceflight simulator
// ----------------------------------------------------------------------------


#define NIGHT_CLOUDS 0.05f          // range(0.0f-0.1f) Cloud ambient level at night
#define CLOUD_INTENSITY 1.8f        // range(0.5f-2.0f)
#define NIGHT_LIGHTS 0.7f           // range(0.2f-1.0f)

struct Mat
{
	vec4 diffuse;
	vec4 ambient;
	vec4 specular;
	vec4 emissive;
	float  specPower;
};

struct Mtrl
{
	vec4 diffuse;
	vec4 specular;
	vec3 ambient;
	vec3 emissive;
	vec3 reflect;
	vec3 emission2;
	vec3 fresnel;
	vec2 roughness;
	float  metalness;
	vec4 specialfx;			// x = Heat 
};

struct Sun
{
	vec3 Dir;
	vec3 Color;			// Color and Intensity of received sunlight 
	vec3 Ambient;			// Ambient light level (Base Objects Only, Vessels are using dynamic methods)
	vec3 Transmission;	// Visibility through atmosphere (1.0 = fully visible, 0.0 = obscured)
	vec3 Inscatter;		// Amount of incattered light from haze
};

struct Light
{
	int      type;       	   /* Is is spotlight */
	float    dst2;			   /* Camera-Light Emitter distance squared */
	vec4     diffuse;          /* diffuse color of light */
	vec3     position;         /* position in world space */
	vec3     direction;        /* direction in world space */
	vec3     attenuation;      /* Attenuation */
	vec4     param;            /* range, falloff, theta, phi */
};

// Must match with counterpart in D3D9Effect.h

struct Flow
{
	bool Emis;		// Enable Emission Maps
	bool Spec;		// Enable Specular Maps
	bool Refl;		// Enable Reflection Maps
	bool Transl;	// Enble translucent effect
	bool Transm;	// Enable transmissive effect
	bool Rghn;		// Enable roughness map
	bool Norm;		// Enable normal map
	bool Metl;		// Enable metalness map
	bool Heat;		// Enable heat map
};


// Must match with counterpart D3D9Tune in D3D9Util.h

struct Tune
{
	vec4 Albe;		// Tune Diffese Maps
	vec4 Emis;		// Tune Emission Maps
	vec4 Spec;		// Tune Specular Maps
	vec4 Refl;		// Tune Reflection Maps
	vec4 Transl;		// Tune translucent effect
	vec4 Transm;		// Tune transmissive effect
	vec4 Norm;		// Tune normal map
	vec4 Rghn;		// Tune roughness map
};


#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3


#define SH_SIZE		0
#define SH_INVSIZE	1

layout(binding = 0, row_major, scalar) uniform FxParams	// the uniform extern parameters, packed as the client's structs
{
vec3      kernel[KERNEL_SIZE];

// -------------------------------------------------------------------------
mat4      gW;			    // World matrix
mat4      gLVP;			    // Light view projection
mat4      gVP;			    // Combined View and Projection matrix
mat4      gGrpT;	            // Mesh group transformation matrix
vec4      gAttennuate;       // (Mesh Constant Fog) Attennuation of fragment color
vec4      gInScatter;        // (Mesh Constant Fog) In scattering light
vec4      gColor;            // General purpose color parameter
vec4      gFogColor;         // Distance fog color in "Legacy" implementation
vec4      gAtmColor;         // Earth glow color
vec4      gTexOff;			// Texture offsets used by surface manager
vec4      gRadius;           // PlanetRad, AtmOuterLimit, CameraRad, CameraAlt
vec4      gSHD;				// ShadowMap data
vec3      gCameraPos;        // Planet relative camera position, Unit vector
vec3      gNorth;
vec3      gEast;
Sun		  gSun;				// Sun light direction
Mat       gMat;			    // Material input structure  TODO:  Remove all reference to this. Use gMtrl
Mat       gWater;			// Water material input structure
Mtrl      gMtrl;			    // Material input structure
Tune      gTune;			    // Texture tuning parameters
Light	  gLights[MAX_LIGHTS];
bool	  gLightsEnabled;
bool      gTuneEnabled;
bool      gModAlpha;		    // Configuration input
bool      gFullyLit;			// Always fully lit bypass lighting calculations
bool      gTextured;			// Enable Diffuse Texturing
bool      gFresnel;			// Enable fresnel material
bool      gPBRSw;			// Legacy / PBR Switch
bool      gRghnSw;			// Roughness converter switch
bool      gNight;			// Nighttime/Daytime
bool      gShadowsEnabled;	// Enable shadow maps
bool      gEnvMapEnable;		// Enable Environment mapping
bool	  gInSpace;			// True if a mesh is located in space
bool	  gNoColor;			// No color flag
bool	  gBaseBuilding;
bool	  gOITEnable;
int       gSpecMode;
int       gHazeMode;
float     gProxySize;		// Cosine of the angular size of the Proxy Gbody. (one half)
float	  gInvProxySize;		// = 1.0 / (1.0f-gProxySize)
float     gPointScale;
float     gDistScale;
float     gFogDensity;
float     gTime;
float     gMix;				// General purpose parameter (multible uses)
float 	  gMtrlAlpha;
float	  gGlowConst;
float	  gNightTime;		// 1 for nighttime, 0 for daytime
Flow	  gCfg;
};

// Textures -----------------------------------------------------------------

uniform extern texture   gTex0;			    // Diffuse texture
uniform extern texture   gTex1;			    // Nightlights
uniform extern texture   gTex3;				// Normal Map / Cloud Microtexture
uniform extern texture   gSpecMap;			// Specular Map
uniform extern texture   gRghnMap;			// Roughness Map
uniform extern texture   gEmisMap;	    	// Emission Map
uniform extern texture   gEnvMapA;	    	// Environment Map (Mirror clear)
uniform extern texture   gEnvMapB;	    	// Environment Map (Mipmapped with different levels of blur)
uniform extern texture   gReflMap;   		// Reflectivity Map
uniform extern texture   gMetlMap;   		// Metalness Map
uniform extern texture   gHeatMap;   		// Heat Map
uniform extern texture   gTranslMap;		// Translucence Map
uniform extern texture   gTransmMap;		// Transmittance Map
uniform extern texture   gShadowMap;	    // Shadow Map
uniform extern texture   gIrradianceMap;    // Irradiance Map

// Legacy Atmosphere --------------------------------------------------------

layout(binding = 1, row_major, scalar) uniform FxLegacyAtmo
{
float     gGlobalAmb;        // Global Ambient Level
float     gSunAppRad;        // Sun apparent size (Radius / Distance)
float     gDispersion;
float     gAmbient0;
};


// ----------------------------------------------------------------------------
// Vertex layouts
// ----------------------------------------------------------------------------

struct MESH_VERTEX {                        // D3D9Client Mesh vertex layout
	vec3 posL;     // POSITION0
	vec3 nrmL;     // NORMAL0
	vec3 tanL;     // TANGENT0
	vec3 tex0;     // TEXCOORD0
};

struct NTVERTEX {                           // Orbiter Mesh vertex layout
	vec3 posL;     // POSITION0
	vec3 nrmL;     // NORMAL0
	vec2 tex0;     // TEXCOORD0
};

struct TILEVERTEX {                         // Vertex declaration used for surface tiles and cloud layer
	vec3 posL;     // POSITION0
	vec3 normalL;  // NORMAL0
	vec2 tex0;     // TEXCOORD0
	float  elev;   // TEXCOORD1
};

struct HZVERTEX {
	vec3 posL;     // POSITION0
	vec4 color;    // COLOR0
	vec2 tex0;     // TEXCOORD0
};

struct POSTEX {
	vec3 posL;     // POSITION0
	vec2 tex0;     // TEXCOORD0
};

struct SHADOW_VERTEX {
	vec4 posL;     // POSITION0
};


// ----------------------------------------------------------------------------
// Vertex shader outputs
// ----------------------------------------------------------------------------

struct SimpleVS
{
	vec4 posH;     // POSITION0
	vec2 tex0;     // TEXCOORD0
	vec3 nrmW;     // TEXCOORD1
	vec3 toCamW;   // TEXCOORD2
};

struct HazeVS
{
	vec4 posH;     // POSITION0
	vec4 color;    // TEXCOORD0
	vec2 tex0;     // TEXCOORD1
};

struct BShadowVS
{
	vec4 posH;     // POSITION0
	vec2 dstW;     // TEXCOORD0
	float  alpha;  // TEXCOORD1
};

struct ShadowTexVS
{
	vec4 posH;     // POSITION0
	vec2 dstW;     // TEXCOORD0
	vec3 tex0;     // TEXCOORD1
};

// ----------------------------------------------------------------------------
// Texture Sampler implementations
// ----------------------------------------------------------------------------

sampler2D IrradS = sampler_state      // Irradiance map sampler
{
	Texture = <gIrradianceMap>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D ShadowS = sampler_state      // Shadow map sampler
{
	Texture = <gShadowMap>;
	MinFilter = POINT;
	MagFilter = POINT;
	MipFilter = POINT;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D WrapS = sampler_state       // Primary Mesh texture sampler
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

sampler2D ClampS = sampler_state      // Base tile sampler
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

sampler2D SpecS = sampler_state       // Primary Mesh texture sampler
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

sampler2D EmisS = sampler_state       // Primary Mesh texture sampler
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

sampler2D ReflS = sampler_state       // Primary Mesh texture sampler
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

sampler2D MetlS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gMetlMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler2D HeatS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gHeatMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler2D RghnS = sampler_state       // Primary Mesh texture sampler
{
	Texture = <gRghnMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler2D TranslS = sampler_state       // Translucence texture sampler
{
	Texture = <gTranslMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = WRAP;
	AddressV = WRAP;
};
sampler2D TransmS = sampler_state       // Transmittance texture sampler
{
	Texture = <gTransmMap>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	MipMapLODBias = 0;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler2D Tex1S = sampler_state       // Secundary mesh texture sampler (i.e. night texture)
{
	Texture = <gTex1>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
	AddressV = WRAP;
};

sampler2D Nrm0S = sampler_state       // Normal Map Sampler
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

sampler2D MFDSamp = sampler_state     // Virtual Cockpit MFD screen sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D Panel0S = sampler_state     // Sampler for mesh based panels, Panel MFDs. Must be compatible with Non-power of two conditional due to MFD screens.
{
	Texture = <gTex0>;
	MinFilter = POINT;
	MagFilter = LINEAR;
	MipFilter = NONE;
	AddressU  = CLAMP;
	AddressV  = CLAMP;
};

sampler2D SimpleS = sampler_state       // Sampler used for SimpleTech. (Star, VC HUD)
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

sampler2D ExhaustS = sampler_state
{
	Texture = <gTex0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = NONE;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D RingS = sampler_state       // Planetary rings sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
	AddressV = WRAP;
};

samplerCube EnvMapAS = sampler_state
{
	Texture = <gEnvMapA>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
	AddressV = CLAMP;
	AddressW = CLAMP;
};

samplerCube EnvMapBS = sampler_state
{
	Texture = <gEnvMapB>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = CLAMP;
	AddressV = CLAMP;
	AddressW = CLAMP;
};


// Planet surface samplers ----------------------------------------------------

sampler2D Planet0S = sampler_state    // Planet/Cloud diffuse texture sampler
{
	Texture = <gTex0>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D Planet1S = sampler_state    // Planet nightlights/specular mask sampler
{
	Texture = <gTex1>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D Planet3S = sampler_state    // Planet/Cloud micro texture sampler
{
	Texture = <gTex3>;
	MinFilter = ANISOTROPIC;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = ANISOTROPY_MACRO;
	AddressU = WRAP;
	AddressV = WRAP;
};



// ----------------------------------------------------------------------------
// Atmospheric Haze implementation
//
// att = attennuation, ins = inscatter, depth = pixel depth [0 to 1],
// posW = camera centric world space position of the vertex
// ----------------------------------------------------------------------------

void AtmosphericHaze(out vec4 att, out vec4 ins, in float depth, in vec3 posW)
{
	if (gHazeMode==0) {
		att = vec4(1);
		ins = vec4(0);
		return;
	}
	else if (gHazeMode==1) {
		att = gAttennuate;
		ins = gInScatter;
		return;
	}
	else if (gHazeMode==2) {
		float fogFact = 1.0f / exp(max(0.0f,depth) * gFogDensity);
		att = vec4(fogFact);
		ins = vec4((1.0f-fogFact) * gFogColor.rgb, 0.0f);
		return;
	}
}


// ----------------------------------------------------------------------------
// Legacy sun color on planet surface. Used for planet surface, base tiles and
// buildings.  See SurfaceLighting() in D3D9Util.cpp
// ----------------------------------------------------------------------------

void LegacySunColor(out vec4 diff, out float ambi, out float nigh, in vec3 normalW)
{
	float   h = dot(-gSun.Dir, normalW);
	float   s = clamp((h+gSunAppRad)/(2.0f*gSunAppRad), 0.0, 1.0);
	vec3 r0 = 1.0 - vec3(0.65, 0.75, 1.0) * gDispersion;

	if (gDispersion!=0) { // case 1: planet has atmosphere
		vec3 di = (r0 + (1.0-r0) * clamp(h*5.780, 0.0, 1.0)) * s;
		float  ni = (h+0.242)*2.924;
		float  am = clamp(max(gAmbient0*clamp(ni, 0.0, 1.0)-0.05, gGlobalAmb), 0.0, 1.0);

		diff = vec4(di*(1.0-am*0.5),1);
		ambi = am;
		nigh = clamp(-ni-0.2, 0.0, 1.0);
	}
	else { // case 2: planet has no atmosphere
		diff = vec4(r0*s, 1);
		ambi = gGlobalAmb;
		nigh = 0;
	}
}



// ----------------------------------------------------------------------------
// Vertex shader implementations
// ----------------------------------------------------------------------------


SimpleVS BasicVS(NTVERTEX vrt)
{
	SimpleVS outVS = SimpleVS(vec4(0), vec2(0), vec3(0), vec3(0));
	vec3 posW  = (vec4(vrt.posL, 1.0f) * gW).xyz;
	outVS.posH   = vec4(posW, 1.0f) * gVP;
	outVS.nrmW   = (vec4(vrt.nrmL, 0.0f) * gW).xyz;
	outVS.toCamW = -posW;
	outVS.tex0   = vrt.tex0;
	return outVS;
}

#ifdef VS_BasicVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out SimpleVS oVS;
void main() { oVS = BasicVS(NTVERTEX(iPosL, iNrmL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif



// ----------------------------------------------------------------------------
// PixelShader Implementations
// ----------------------------------------------------------------------------

vec4 SimpleTechPS(SimpleVS frg)
{
	vec4 c = texture(SimpleS, frg.tex0);
	return vec4(c.rgb, c.a * gMix);
}

vec4 PanelTechPS(SimpleVS frg)
{
	vec4 cTex = texture(SimpleS, frg.tex0);
	return vec4(cTex.rgb, cTex.a*gMix);
}

vec4 PanelTechBPS(SimpleVS frg)
{
	vec4 cTex = texture(Panel0S, frg.tex0);
	return vec4(cTex.rgb, cTex.a*gMix);
}

vec4 ExhaustTechPS(SimpleVS frg)
{
	vec4 c = texture(ExhaustS, frg.tex0);
	return vec4(c.rgb, c.a*gMix);
}

vec4 SpotTechPS(SimpleVS frg)
{
	return (texture(SimpleS, frg.tex0) * gColor) * gMix;
}

#if defined(PS_SimpleTechPS) || defined(PS_PanelTechPS) || defined(PS_PanelTechBPS) || defined(PS_ExhaustTechPS) || defined(PS_SpotTechPS)
layout(location = 0) in SimpleVS frg;
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_SimpleTechPS
void main() { oColor = SimpleTechPS(frg PS_ARGS); }
#endif
#ifdef PS_PanelTechPS
void main() { oColor = PanelTechPS(frg PS_ARGS); }
#endif
#ifdef PS_PanelTechBPS
void main() { oColor = PanelTechBPS(frg PS_ARGS); }
#endif
#ifdef PS_ExhaustTechPS
void main() { oColor = ExhaustTechPS(frg PS_ARGS); }
#endif
#ifdef PS_SpotTechPS
void main() { oColor = SpotTechPS(frg PS_ARGS); }
#endif

#include "Particle.fx"
#include "Mesh.fx"
#include "Vessel.fx"
#include "HorizonHaze.fx"
#include "Planet.fx"
#include "BeaconArray.fx"


BShadowVS ArrowTechVS(vec3 posL)	// posL : POSITION0
{
	// Zero output.
	BShadowVS outVS = BShadowVS(vec4(0), vec2(0), 0.0);
	vec3 posW = (vec4(posL, 1.0f) * gW).xyz; // Apply world transformation matrix
	outVS.posH = vec4(posW, 1.0f) * gVP; // Apply view projection matrix
	return outVS;
}

#ifdef VS_ArrowTechVS
layout(location = 0) in vec3 iPosL;
layout(location = 0) out BShadowVS oVS;
void main() { oVS = ArrowTechVS(iPosL VS_ARGS); gl_Position = oVS.posH; }
#endif

vec4 ArrowTechPS(BShadowVS frg)
{
	return gColor;
}

#ifdef PS_ArrowTechPS
layout(location = 0) in BShadowVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = ArrowTechPS(frg PS_ARGS); }
#endif


// This is used for rendering grapple points ----------------------------------
//
technique ArrowTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 ArrowTechVS();
		pixelShader = compile ps_3_0 ArrowTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
		ZEnable = true;
	}
}


// This is used for many simple renderings ------------------------------------
//
technique SimpleTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BasicVS();
		pixelShader  = compile ps_3_0 SimpleTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}

// This is used for 2DPanel and Glass cockpit ---------------------------------
//
technique PanelTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BasicVS();
		pixelShader  = compile ps_3_0 PanelTechPS();

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
		vertexShader = compile vs_3_0 BasicVS();
		pixelShader  = compile ps_3_0 PanelTechBPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}


// Thil will render exhaust textures ------------------------------------------
//
technique ExhaustTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BasicVS();
		pixelShader  = compile ps_3_0 ExhaustTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
		ZEnable = true;
	}
}

// This is used for rendering beacons -----------------------------------------
//
technique SpotTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 BasicVS();
		pixelShader  = compile ps_3_0 SpotTechPS();

		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZWriteEnable = false;
		ZEnable = true;
	}
}
