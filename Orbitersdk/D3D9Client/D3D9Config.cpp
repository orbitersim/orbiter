// ==============================================================
// Configuration Manager
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2007 Martin Schweiger
//				 2011 Jarmo Nikkanen (D3D9Client modification) 
// ==============================================================

#include "D3D9Config.h"
#include "orbitersdk.h"

static char *cfgfile = "D3D9Client.cfg";

class D3D9Config *Config;				// configuration manager

// ==============================================================

D3D9Config::D3D9Config()
{
	Reset();
	ReadParams();
}

D3D9Config::~D3D9Config ()
{
	WriteParams ();
	delete []DebugFont;
	delete []SolCfg;
}

void D3D9Config::Reset ()
{
	PlanetPreloadMode   = 0;
	PlanetLoadFrequency = 20;
	Anisotrophy			= 4;
	SceneAntialias		= 4;
	DebugLvl			= 1;
	VCNearPlane		    = 0.1; 
	MaxLights			= 12;
	NearClipPlane		= 0;
	NVPerfHUD			= 0;
	PreLBaseVis			= 0;
	GDIRTSWrn			= 0;
	DebugFontSize		= 18;
	UseNormalMap		= 1;
	ManagedTiles		= 0;
	SketchpadMode		= 0;
	SketchpadFont		= 1;
	RwyLightAnimate		= 1;
	RwyLightAngle		= 120.0;
	RwyBrightness		= 1.0;
	Convergence			= 0.2;
	Separation			= 65.0;
	SunAngle			= 10.0;
	BumpAmp				= 1.0;
	PlanetGlow			= 0.7;
	SunBrightness		= 1.2;
	EnvMapSize		    = 256;
	EnvMapMode			= 0;
	EnvMapFaces			= 1;
	EnableGlass			= 1;
	EnableMeshDbg		= 1;
	ShadowMapMode		= 1;
	ShadowMapSize		= 1024;
	FrameRate			= 200.0;
	EnableLimiter		= 0;
	CustomCamMode		= 1;
	TileMipmaps			= 0;
	LODBias				= 0;
	MeshRes				= 0;
	TileDebug			= 0;

	DisableDriverManagement = 0;
	DisableVisualHelperReadout = 0;

	SolCfg				= new char[64];   strcpy_s(SolCfg,64,"Sol");
	DebugFont		    = new char[64];   strcpy_s(DebugFont,64,"Fixed");
}

bool D3D9Config::ReadParams ()
{
	int i;
	double d;

	FILEHANDLE hFile = oapiOpenFile(cfgfile, FILE_IN_ZEROONFAIL, ROOT);
	if (!hFile) return false;

	if (oapiReadItem_float (hFile, "FrameRate", d))				FrameRate = max(0.0, min(300.0, d));
	if (oapiReadItem_int   (hFile, "EnableLimiter", i))			EnableLimiter = max(0, min(1, i));
	if (oapiReadItem_int   (hFile, "CustomCamMode", i))			CustomCamMode = max(0, min(1, i));
	if (oapiReadItem_int   (hFile, "PlanetPreloadMode", i))		PlanetPreloadMode = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "PlanetTexLoadFreq", i))		PlanetLoadFrequency = max (1, min (1000, i));
	if (oapiReadItem_int   (hFile, "Anisotrophy", i))			Anisotrophy = max (1, min (16, i));
	if (oapiReadItem_int   (hFile, "SceneAntialias", i))		SceneAntialias = i;
	if (oapiReadItem_int   (hFile, "SketchpadMode",i))			SketchpadMode = i;
	if (oapiReadItem_int   (hFile, "SketchpadFont",i))			SketchpadFont =  max (0, min (3, i));
	if (oapiReadItem_int   (hFile, "PreLoadBaseVisuals", i))	PreLBaseVis =  max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "EnableNormalMapping", i))	UseNormalMap =  max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "NearClipPlaneMode", i))		NearClipPlane =  max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "RwyLightAnimate", i))		RwyLightAnimate =  max (0, min (1, i));
	if (oapiReadItem_float (hFile, "RwyLightAngle", d))			RwyLightAngle = max(10.0, min(180.0,d));
	if (oapiReadItem_float (hFile, "RwyBrightness", d))			RwyBrightness = max(0.3,  min(3.0,d));
	if (oapiReadItem_float (hFile, "NightLightsAngle", d))		SunAngle = max(0.1,  min(20.0,d));
	if (oapiReadItem_float (hFile, "BumpMapAmplitude", d))		BumpAmp = max(0.1,  min(10.0,d));
	if (oapiReadItem_float (hFile, "PlanetGlow", d))			PlanetGlow = max(0.01,  min(2.0,d));
	if (oapiReadItem_float (hFile, "SunBrightness", d))			SunBrightness = max(0.7,  min(4.0,d));
	if (oapiReadItem_int   (hFile, "EnvMapSize", i))			EnvMapSize = max(64, min(512, i));
	if (oapiReadItem_int   (hFile, "EnvMapMode", i))			EnvMapMode = max(0, min(2, i));
	if (oapiReadItem_int   (hFile, "EnvMapFaces", i))			EnvMapFaces = max(1, min(3, i));
	if (oapiReadItem_int   (hFile, "ShadowMapMode", i))			ShadowMapMode = max(0, min(2, i));
	if (oapiReadItem_int   (hFile, "ShadowMapSize", i))			ShadowMapSize = max(512, min(4096, i));
	if (oapiReadItem_int   (hFile, "EnableGlass", i))			EnableGlass = max(0, min(1, i));
	if (oapiReadItem_int   (hFile, "EnableMeshDbg", i))			EnableMeshDbg = max(0, min(1, i));
	if (oapiReadItem_int   (hFile, "TileMipmaps", i))			TileMipmaps = max(0, min(1, i));
	if (oapiReadItem_int   (hFile, "TileDebug", i))				TileDebug = max(0, min(1, i));
	if (oapiReadItem_float (hFile, "StereoSeparation", d))		Separation = max(10.0,  min(100.0,d));
	if (oapiReadItem_float (hFile, "StereoConvergence", d))		Convergence = max(0.05,  min(1.0,d));
	if (oapiReadItem_int   (hFile, "DebugLvl", i))					DebugLvl = i;
	if (oapiReadItem_float (hFile, "VCNearPlane", d))				VCNearPlane = max (-1.0, min (1.0, d));
	if (oapiReadItem_int   (hFile, "LightSourcesInUse", i))			MaxLights = i;
	if (oapiReadItem_int   (hFile, "DisableDrvMgm", i))				DisableDriverManagement = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "NVPerfHUD", i))					NVPerfHUD = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "DebugLineFontSize", i))			DebugFontSize = i;	
	if (oapiReadItem_int   (hFile, "GDIRTSDebug", i))				GDIRTSWrn = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "ManagedTiles",i))				ManagedTiles = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "DisableVisualHelperReadout",i))	DisableVisualHelperReadout = max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "LODBias",i))					LODBias = max (-3, min (3, i));
	if (oapiReadItem_int   (hFile, "MeshRes",i))					MeshRes = max (0, min (2, i));
	
	oapiReadItem_string (hFile, "SolCfg", SolCfg);	
	oapiReadItem_string (hFile, "DebugLineFont", DebugFont);		

	oapiCloseFile (hFile, FILE_IN_ZEROONFAIL);

	return true;
}


void D3D9Config::WriteParams ()
{
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, ROOT);

	oapiWriteItem_float (hFile, "FrameRate", FrameRate);
	oapiWriteItem_int   (hFile, "EnableLimiter", EnableLimiter);
	oapiWriteItem_int   (hFile, "CustomCamMode", CustomCamMode);
	oapiWriteItem_int   (hFile, "PlanetPreloadMode", PlanetPreloadMode);
	oapiWriteItem_int   (hFile, "PlanetTexLoadFreq", PlanetLoadFrequency);
	oapiWriteItem_int   (hFile, "Anisotrophy", Anisotrophy);
	oapiWriteItem_int   (hFile, "SceneAntialias", SceneAntialias);
	oapiWriteItem_int   (hFile, "SketchpadMode", SketchpadMode);
	oapiWriteItem_int   (hFile, "SketchpadFont", SketchpadFont);
	oapiWriteItem_int   (hFile, "PreLoadBaseVisuals", PreLBaseVis);
	oapiWriteItem_int   (hFile, "EnableNormalMapping", UseNormalMap);
	oapiWriteItem_int   (hFile, "NearClipPlaneMode", NearClipPlane);
	oapiWriteItem_int   (hFile, "RwyLightAnimate", RwyLightAnimate);

	oapiWriteItem_float (hFile, "RwyLightAngle", RwyLightAngle);
	oapiWriteItem_float (hFile, "RwyBrightness", RwyBrightness);
	oapiWriteItem_float (hFile, "NightLightsAngle", SunAngle);
	oapiWriteItem_float (hFile, "BumpMapAmplitude", BumpAmp);	
	oapiWriteItem_float (hFile, "PlanetGlow", PlanetGlow);	
	oapiWriteItem_float (hFile, "SunBrightness", SunBrightness);
	oapiWriteItem_int   (hFile, "EnvMapSize", EnvMapSize);
	oapiWriteItem_int   (hFile, "EnvMapMode", EnvMapMode);
	oapiWriteItem_int   (hFile, "EnvMapFaces", EnvMapFaces);
	oapiWriteItem_int   (hFile, "ShadowMapMode", ShadowMapMode);
	oapiWriteItem_int   (hFile, "ShadowMapSize", ShadowMapSize);
	oapiWriteItem_int   (hFile, "EnableGlass", EnableGlass);
	oapiWriteItem_int   (hFile, "EnableMeshDbg", EnableMeshDbg);
	oapiWriteItem_int   (hFile, "TileMipmaps", TileMipmaps);
	oapiWriteItem_int   (hFile, "TileDebug", TileDebug);
	oapiWriteItem_float (hFile, "StereoSeparation", Separation);
	oapiWriteItem_float (hFile, "StereoConvergence", Convergence);
	oapiWriteItem_int   (hFile, "DebugLvl", DebugLvl);
	oapiWriteItem_float (hFile, "VCNearPlane", VCNearPlane);
	oapiWriteItem_int   (hFile, "LightSourcesInUse", MaxLights);
	oapiWriteItem_int   (hFile, "DisableDrvMgm", DisableDriverManagement);
	oapiWriteItem_int   (hFile, "NVPerfHUD", NVPerfHUD);
	oapiWriteItem_int   (hFile, "DebugLineFontSize", DebugFontSize);	
	oapiWriteItem_int   (hFile, "GDIRTSDebug", GDIRTSWrn);
	oapiWriteItem_int   (hFile, "ManagedTiles", ManagedTiles);
	oapiWriteItem_int   (hFile, "DisableVisualHelperReadout", DisableVisualHelperReadout);
	oapiWriteItem_int   (hFile, "LODBias", LODBias);
	oapiWriteItem_int   (hFile, "MeshRes", MeshRes);
	
	oapiWriteItem_string (hFile, "SolCfg", SolCfg);
	oapiWriteItem_string (hFile, "DebugLineFont", DebugFont);		
	
	oapiCloseFile (hFile, FILE_OUT);
}