// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// Metal port of D3D9Client.fx
// ==============================================================

#include <metal_stdlib>
using namespace metal;

#define NIGHT_CLOUDS 0.05f
#define CLOUD_INTENSITY 1.8f
#define NIGHT_LIGHTS 0.7f

#define kernel kernel_

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
	float4 specular;
	float3 ambient;
	float3 emissive;
	float3 reflect;
	float3 emission2;
	float3 fresnel;
	float2 roughness;
	float  metalness;
	float4 specialfx;
};

struct Sun
{
	float3 Dir;
	float3 Color;
	float3 Ambient;
	float3 Transmission;
	float3 Inscatter;
};

struct Light
{
	int      type;
	float    dst2;
	float4   diffuse;
	float3   position;
	float3   direction;
	float3   attenuation;
	float4   param;
};

struct Flow
{
	int Emis;
	int Spec;
	int Refl;
	int Transl;
	int Transm;
	int Rghn;
	int Norm;
	int Metl;
	int Heat;
};

struct Tune
{
	float4 Albe;
	float4 Emis;
	float4 Spec;
	float4 Refl;
	float4 Transl;
	float4 Transm;
	float4 Norm;
	float4 Rghn;
};

#define Range   0
#define Falloff 1
#define Theta   2
#define Phi     3

#define SH_SIZE		0
#define SH_INVSIZE	1

// kernel[] holds up to KERNEL_SIZE (27 or 35) entries, gLights[] up to MAX_LIGHTS (1..20) entries
struct Globals
{
	float3    kernel[35];
	float4x4  gW;
	float4x4  gLVP;
	float4x4  gVP;
	float4x4  gGrpT;
	float4    gAttennuate;
	float4    gInScatter;
	float4    gColor;
	float4    gFogColor;
	float4    gAtmColor;
	float4    gTexOff;
	float4    gRadius;
	float4    gSHD;
	float3    gCameraPos;
	float3    gNorth;
	float3    gEast;
	Sun       gSun;
	Mat       gMat;
	Mat       gWater;
	Mtrl      gMtrl;
	Tune      gTune;
	Light     gLights[20];
	int       gLightsEnabled;
	int       gTuneEnabled;
	int       gModAlpha;
	int       gFullyLit;
	int       gTextured;
	int       gFresnel;
	int       gPBRSw;
	int       gRghnSw;
	int       gNight;
	int       gShadowsEnabled;
	int       gEnvMapEnable;
	int       gInSpace;
	int       gNoColor;
	int       gBaseBuilding;
	int       gOITEnable;
	int       gSpecMode;
	int       gHazeMode;
	float     gProxySize;
	float     gInvProxySize;
	float     gPointScale;
	float     gDistScale;
	float     gFogDensity;
	float     gTime;
	float     gMix;
	float     gMtrlAlpha;
	float     gGlowConst;
	float     gNightTime;
	Flow      gCfg;
	float     gGlobalAmb;
	float     gSunAppRad;
	float     gDispersion;
	float     gAmbient0;
	float     fIntensity;
	float     fDistance;
	float     fThreshold;
	float     fGamma;
	float2    vSB;
	float2    vBB;
	int       bDir;
	int       bBlur;
	int       bBlendIn;
	int       bSample;
	int       PassId;
};

// Textures (effect slots):
// gTex0 0, gTex1 1, gTex3 2, gSpecMap 3, gRghnMap 4, gEmisMap 5, gEnvMapA 6, gEnvMapB 7, gReflMap 8,
// gMetlMap 9, gHeatMap 10, gTranslMap 11, gTransmMap 12, gShadowMap 13, gIrradianceMap 14

// ----------------------------------------------------------------------------
// Texture Sampler implementations
// ----------------------------------------------------------------------------

constexpr sampler IrradS(filter::linear, mip_filter::linear, address::clamp_to_edge);
constexpr sampler ShadowS(filter::nearest, mip_filter::nearest, address::clamp_to_edge);
constexpr sampler WrapS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler ClampS(filter::linear, mip_filter::linear, address::clamp_to_edge, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler SpecS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler EmisS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler ReflS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler MetlS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler HeatS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler RghnS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler TranslS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler TransmS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler Tex1S(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler Nrm0S(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler MFDSamp(filter::linear, mip_filter::linear, address::clamp_to_edge, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler Panel0S(min_filter::nearest, mag_filter::linear, mip_filter::none, address::clamp_to_edge);
constexpr sampler SimpleS(filter::linear, mip_filter::linear, address::clamp_to_edge, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler ExhaustS(filter::linear, mip_filter::none, address::clamp_to_edge);
constexpr sampler RingS(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler EnvMapAS(filter::linear, mip_filter::linear, address::clamp_to_edge);
constexpr sampler EnvMapBS(filter::linear, mip_filter::linear, address::clamp_to_edge);

// Planet surface samplers ----------------------------------------------------

constexpr sampler Planet0S(filter::linear, mip_filter::linear, address::clamp_to_edge, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler Planet1S(filter::linear, mip_filter::linear, address::clamp_to_edge, max_anisotropy(ANISOTROPY_MACRO));
constexpr sampler Planet3S(filter::linear, mip_filter::linear, address::repeat, max_anisotropy(ANISOTROPY_MACRO));


// ----------------------------------------------------------------------------
// Vertex layouts
// ----------------------------------------------------------------------------

struct MESH_VERTEX {
	float3 posL   [[attribute(0)]];
	float3 nrmL   [[attribute(2)]];
	float3 tanL   [[attribute(14)]];
	float3 tex0   [[attribute(4)]];
};

struct NTVERTEX {
	float3 posL     [[attribute(0)]];
	float3 nrmL     [[attribute(2)]];
	float2 tex0     [[attribute(4)]];
};

struct TILEVERTEX {
	float3 posL     [[attribute(0)]];
	float3 normalL  [[attribute(2)]];
	float2 tex0     [[attribute(4)]];
	float  elev     [[attribute(5)]];
};

struct HZVERTEX {
	float3 posL     [[attribute(0)]];
	float4 color    [[attribute(12)]];
	float2 tex0     [[attribute(4)]];
};

struct POSTEX {
	float3 posL     [[attribute(0)]];
	float2 tex0     [[attribute(4)]];
};

struct SHADOW_VERTEX {
	float4 posL     [[attribute(0)]];
};

struct POSONLY {
	float3 posL     [[attribute(0)]];
};


// ----------------------------------------------------------------------------
// Vertex shader outputs
// ----------------------------------------------------------------------------

struct SimpleVS
{
	float4 posH     [[position]];
	float2 tex0;
	float3 nrmW;
	float3 toCamW;
};

struct HazeVS
{
	float4 posH    [[position]];
	float4 color;
	float2 tex0;
};

struct BShadowVS
{
	float4 posH    [[position]];
	float2 dstW;
	float  alpha;
};

struct ShadowTexVS
{
	float4 posH    [[position]];
	float2 dstW;
	float3 tex0;
};


// ----------------------------------------------------------------------------
// Atmospheric Haze implementation
// ----------------------------------------------------------------------------

void AtmosphericHaze(constant Globals &g, thread float4 &att, thread float4 &ins, float depth, float3 posW)
{
	if (g.gHazeMode==0) {
		att = 1.0f;
		ins = 0.0f;
		return;
	}
	else if (g.gHazeMode==1) {
		att = g.gAttennuate;
		ins = g.gInScatter;
		return;
	}
	else if (g.gHazeMode==2) {
		float fogFact = 1.0f / exp(max(0.0f, depth) * g.gFogDensity);
		att = fogFact;
		ins = float4((1.0f-fogFact) * g.gFogColor.rgb, 0.0f);
		return;
	}
}


// ----------------------------------------------------------------------------
// Legacy sun color on planet surface.
// ----------------------------------------------------------------------------

void LegacySunColor(constant Globals &g, thread float4 &diff, thread float &ambi, thread float &nigh, float3 normalW)
{
	float   h = dot(-g.gSun.Dir, normalW);
	float   s = saturate((h+g.gSunAppRad)/(2.0f*g.gSunAppRad));
	float3 r0 = 1.0f - float3(0.65f, 0.75f, 1.0f) * g.gDispersion;

	if (g.gDispersion!=0) {
		float3 di = (r0 + (1.0f-r0) * saturate(h*5.780f)) * s;
		float  ni = (h+0.242f)*2.924f;
		float  am = saturate(max(g.gAmbient0*saturate(ni)-0.05f, g.gGlobalAmb));

		diff = float4(di*(1.0f-am*0.5f),1.0f);
		ambi = am;
		nigh = saturate(-ni-0.2f);
	}
	else {
		diff = float4(r0*s, 1.0f);
		ambi = g.gGlobalAmb;
		nigh = 0.0f;
	}
}


// ----------------------------------------------------------------------------
// Vertex shader implementations
// ----------------------------------------------------------------------------

vertex SimpleVS BasicVS(NTVERTEX vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	SimpleVS outVS = {};
	float3 posW  = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH   = g.gVP * float4(posW, 1.0f);
	outVS.nrmW   = (g.gW * float4(vrt.nrmL, 0.0f)).xyz;
	outVS.toCamW = -posW;
	outVS.tex0   = vrt.tex0;
	return outVS;
}


// ----------------------------------------------------------------------------
// PixelShader Implementations
// ----------------------------------------------------------------------------

fragment float4 SimpleTechPS(SimpleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 c = gTex0.sample(SimpleS, frg.tex0);
	return float4(c.rgb, c.a * g.gMix);
}

fragment float4 PanelTechPS(SimpleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 cTex = gTex0.sample(SimpleS, frg.tex0);
	return float4(cTex.rgb, cTex.a*g.gMix);
}

fragment float4 PanelTechBPS(SimpleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 cTex = gTex0.sample(Panel0S, frg.tex0);
	return float4(cTex.rgb, cTex.a*g.gMix);
}

fragment float4 ExhaustTechPS(SimpleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 c = gTex0.sample(ExhaustS, frg.tex0);
	return float4(c.rgb, c.a*g.gMix);
}

fragment float4 SpotTechPS(SimpleVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	return (gTex0.sample(SimpleS, frg.tex0) * g.gColor) * g.gMix;
}

#include "Particle.metal"
#include "Mesh.metal"
#include "Vessel.metal"
#include "HorizonHaze.metal"
#include "Planet.metal"
#include "BeaconArray.metal"


vertex BShadowVS ArrowTechVS(POSONLY vrt [[stage_in]], constant Globals &g [[buffer(0)]])
{
	BShadowVS outVS = {};
	float3 posW = (g.gW * float4(vrt.posL, 1.0f)).xyz;
	outVS.posH = g.gVP * float4(posW, 1.0f);
	return outVS;
}

fragment float4 ArrowTechPS(BShadowVS frg [[stage_in]], constant Globals &g [[buffer(0)]])
{
	return g.gColor;
}


// @technique ArrowTech
// @pass P0 vs=ArrowTechVS ps=ArrowTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false ZEnable=true

// @technique SimpleTech
// @pass P0 vs=BasicVS ps=SimpleTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false

// @technique PanelTech
// @pass P0 vs=BasicVS ps=PanelTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false

// @technique PanelTechB
// @pass P0 vs=BasicVS ps=PanelTechBPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false

// @technique ExhaustTech
// @pass P0 vs=BasicVS ps=ExhaustTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false ZEnable=true

// @technique SpotTech
// @pass P0 vs=BasicVS ps=SpotTechPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZWriteEnable=false ZEnable=true
