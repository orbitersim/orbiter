// ==============================================================
// Configuration Manager
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
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
	delete []Shaders;
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
	MemoryLogic			= 0;
	NearClipPlane		= 0;
	NVPerfHUD			= 0;
	RejectRTDC          = 0;
	PreLBaseVis			= 0;
	GDIRTSWrn			= 0;
	DebugFontSize		= 18;
	DepthBuffer			= 24;
	UseNormalMap		= 1;
	LoadInSystemMem     = 0;
	ManagedTiles		= 0;
	SketchpadMode		= 0;
	SketchpadFont		= 1;
	RwyLightAnimate		= 1;
	RwyLightAngle		= 120.0;
	RwyBrightness		= 1.0;
	RwyHazeScale		= 3.0;
	Convergence			= 0.2;
	Separation			= 65.0;
	SunAngle			= 10.0;
	BumpAmp				= 1.0;
	EnvMapSize		    = 256;
	EnableEnvMaps		= 0;

	DisableDriverManagement = 0;
	DisableVisualHelperReadout = 0;

	SolCfg				= new char[64];   strcpy_s(SolCfg,64,"Sol");
	Shaders				= new char[64];   strcpy_s(Shaders,64,"Default");
	DebugFont		    = new char[64];   strcpy_s(DebugFont,64,"Fixed");
}

bool D3D9Config::ReadParams ()
{
	int i;
	double d;

	FILEHANDLE hFile = oapiOpenFile(cfgfile, FILE_IN, ROOT);
	if (!hFile) return false;
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
	if (oapiReadItem_int   (hFile, "EnvMapSize", i))			EnvMapSize = max(64, min(512, i));
	if (oapiReadItem_int   (hFile, "EnableEnvMaps", i))			EnableEnvMaps = max(0, min(1, i));

	if (oapiReadItem_float (hFile, "StereoSeparation", d))		Separation = max(10.0,  min(100.0,d));
	if (oapiReadItem_float (hFile, "StereoConvergence", d))		Convergence = max(0.05,  min(1.0,d));
	//if (oapiReadItem_float (hFile, "RwyHazeScale", d))			RwyHazeScale  = max(1.0,  min(30.0,d));

	oapiReadItem_string (hFile, "Shaders", Shaders);

	if (oapiReadItem_int   (hFile, "DebugLvl", i))				DebugLvl = i;
	if (oapiReadItem_float (hFile, "VCNearPlane", d))			VCNearPlane = max (-1.0, min (1.0, d));
	if (oapiReadItem_int   (hFile, "LightSourcesInUse", i))		MaxLights = i;
	if (oapiReadItem_int   (hFile, "MemAllocLogic", i))			MemoryLogic = i;
	if (oapiReadItem_int   (hFile, "DisableDrvMgm", i))			DisableDriverManagement = i;
	if (oapiReadItem_int   (hFile, "NVPerfHUD", i))				NVPerfHUD = i;
	if (oapiReadItem_int   (hFile, "RejectRTDC", i))			RejectRTDC = i;
	if (oapiReadItem_int   (hFile, "DebugLineFontSize", i))	    DebugFontSize = i;	
	if (oapiReadItem_int   (hFile, "GDIRTSDebug", i))			GDIRTSWrn = i;
	if (oapiReadItem_int   (hFile, "LoadTexturesInSystemMem",i)) LoadInSystemMem =  max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "ManagedTiles",i))			ManagedTiles =  max (0, min (1, i));
	if (oapiReadItem_int   (hFile, "DisableVisualHelperReadout",i))	DisableVisualHelperReadout = max (0, min (1, i));
	
	oapiReadItem_string (hFile, "SolCfg", SolCfg);	
	oapiReadItem_string (hFile, "DebugLineFont", DebugFont);		

	oapiCloseFile (hFile, FILE_IN);

	return true;
}


void D3D9Config::WriteParams ()
{
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, ROOT);
	
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
	oapiWriteItem_int   (hFile, "EnvMapSize", EnvMapSize);
	oapiWriteItem_int   (hFile, "EnableEnvMaps", EnableEnvMaps);

	//oapiWriteItem_float (hFile, "RwyHazeScale", RwyHazeScale);

	oapiWriteItem_float (hFile, "StereoSeparation", Separation);
	oapiWriteItem_float (hFile, "StereoConvergence", Convergence);

	oapiWriteItem_string (hFile, "Shaders", Shaders);

	oapiWriteItem_int   (hFile, "DebugLvl", DebugLvl);
	oapiWriteItem_float (hFile, "VCNearPlane", VCNearPlane);
	oapiWriteItem_int   (hFile, "LightSourcesInUse", MaxLights);
	oapiWriteItem_int   (hFile, "MemAllocLogic", MemoryLogic);
	oapiWriteItem_int   (hFile, "DisableDrvMgm", DisableDriverManagement);
	oapiWriteItem_int   (hFile, "NVPerfHUD", NVPerfHUD);
	oapiWriteItem_int   (hFile, "RejectRTDC", RejectRTDC);
	oapiWriteItem_int   (hFile, "DebugLineFontSize", DebugFontSize);	
	oapiWriteItem_int   (hFile, "GDIRTSDebug", GDIRTSWrn);
	oapiWriteItem_int   (hFile, "LoadTexturesInSystemMem", LoadInSystemMem);
	oapiWriteItem_int   (hFile, "ManagedTiles", ManagedTiles);
	oapiWriteItem_int   (hFile, "DisableVisualHelperReadout", DisableVisualHelperReadout);
	
	oapiWriteItem_string (hFile, "SolCfg", SolCfg);
	oapiWriteItem_string (hFile, "DebugLineFont", DebugFont);		
	
	oapiCloseFile (hFile, FILE_OUT);
}