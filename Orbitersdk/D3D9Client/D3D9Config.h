// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2007 Martin Schweiger
//				 2012 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CONFIG_H
#define __D3D9CONFIG_H

extern class D3D9Config *Config;

/**
 * \brief Configuration Manager
 *
 * This class provides access to config-parameters that were read from the
 * config file.
 */
class D3D9Config {
public:
	D3D9Config ();
	~D3D9Config ();

	void Reset();
	bool ReadParams();
	void WriteParams();

	int PlanetPreloadMode;			///< Planet preload mode setting (0=load on demand, 1=preload)
	int PlanetLoadFrequency;		///< Load frequency for on-demand textures [Hz] (1...1000)
	int Anisotrophy;    
	int SceneAntialias;
	int DisableDriverManagement;	///< Disable the D3D9 driver management [sets the D3DCREATE_DISABLE_DRIVER_MANAGEMENT behavior flag]  (0=default, 1:disabled)
	int DisableVisualHelperReadout;	///< Disable the hooking of the visual helper windows, to allow acces to config parameter that Orbiter core doesn't provide (0=normal mode, 1=disable any hooking)
	int MemoryLogic;
	int NearClipPlane;				///< Near clip plane mode (0,1)
	int RejectRTDC;
	int PreLBaseVis;				///< Preload base visuals (0=load on demand, 1=preload)
	int DebugFontSize;
	int DepthBuffer;
	int GDIRTSWrn;
	int UseNormalMap;				///< Enable normal mapping (0,1)
	int LoadInSystemMem;
	int SketchpadMode;
	int SketchpadFont;				///< Sketchpad Font (0=Crisp, 1=Default, 2=Cleartype, 3=Proof Quality)
	int RwyLightAnimate;			///< Runway light animate (0,1)
	double RwyLightAngle;			///< Runway light angle [deg] (10...180)
	double RwyBrightness;			///< Runway light brightness (0.3...3.0)
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
