// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CONFIG_H
#define __D3D9CONFIG_H

extern class D3D9Config *Config;

class D3D9Config {
public:
	D3D9Config ();
	~D3D9Config ();

	void Reset();
	bool ReadParams();
	void WriteParams();

	int PlanetPreloadMode;   // 0=load on demand, 1=preload
	int PlanetLoadFrequency; // load frequency for on-demand textures [Hz]
	int Anisotrophy;    
	int SceneAntialias;
	int DisableDriverManagement;
	int DisableVisualHelperReadout;
	int MemoryLogic;
	int NearClipPlane;
	int RejectRTDC;
	int PreLBaseVis;
	int DebugFontSize;
	int DepthBuffer;
	int GDIRTSWrn;
	int UseNormalMap;
	int LoadInSystemMem;
	int SketchpadMode;
	int SketchpadFont;
	int RwyLightAnimate;
	double RwyLightAngle;
	double RwyBrightness;
	double RwyHazeScale;
	double VCNearPlane;
	double Convergence;
	double Separation;
	int DebugLvl;
	int MaxLights; 
	int NVPerfHUD;
	int ManagedTiles;
	char *InSurface;
	char *DebugFont;
	char *Shaders;
	char *SolCfg;
	
private:

};

#endif // !__D3D9CONFIG_H
