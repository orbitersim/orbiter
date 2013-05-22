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
	int PlanetLoadFrequency;		///< Load frequency for on-demand textures \[Hz\] (1...1000)
	int Anisotrophy;				///< Anisotropic filtering setting \[factor\] (1...16)
	int SceneAntialias;				///< Antialiasing setting \[factor\] (0...)
	int DisableDriverManagement;	///< Disable the D3D9 driver management \[sets the D3DCREATE_DISABLE_DRIVER_MANAGEMENT behavior flag\]  (0=default, 1:disabled)
	int DisableVisualHelperReadout;	///< Disable the hooking of the visual helper windows, to allow acces to config parameter that Orbiter core doesn't provide (0=normal mode, 1=disable any hooking)
	int MemoryLogic;				///< Memory allocation logic used at clbkCreateSurface (0=MakePlainSurface, 1:MakeRenderingTexture)
	int NearClipPlane;				///< Near clip plane mode (0,1)
	int RejectRTDC;					///< Whether to reject using GDI access to render target if the suface type is D3D9S_RTGTTEX
	int PreLBaseVis;				///< Preload base visuals (0=load on demand, 1=preload)
	int DebugFontSize;				///< Debug font height \[pixel\] (default=18px)
	int GDIRTSWrn;					///< Whether to identify RT-GDI conflict by flashing the surface (0,1)
	int UseNormalMap;				///< Enable normal mapping (0,1)
	int LoadInSystemMem;			///< Whether to load textures into system-memory pool or into default pool (0=default pool, 1=system-memory pool)
	int SketchpadMode;				///< Sketckpad mode to use (0=auto select DirectX/GDI, 1=use GDI only)
	int SketchpadFont;				///< Sketchpad Font (0=Crisp, 1=Default, 2=Cleartype, 3=Proof Quality)
	int RwyLightAnimate;			///< Runway light animate (0,1)
	double RwyLightAngle;			///< Runway light angle \[deg\] (10...180)
	double RwyBrightness;			///< Runway light brightness (0.3...3.0)
	double VCNearPlane;				///< Virtual cockpit near clip-plane distance \[m\] (-1.0...1.0, default=0.1)
	double Convergence;				///< StereoScopic 3D convergence distance \[m\] (0.05...1.0, default=0.2)
	double Separation;				///< StereoScopic 3D depth of field separation \[m\] (10.0...100.0, default=65)
	double SunAngle;				///< Sun-angle above horizon when night-lights set it \[deg\] (0.1...20.0, default=10)
	double BumpAmp;					///< Bump map amplification setting (0.1...10.0, default=1)
	double PlanetGlow;				///< Intensity of planet glow effect (0.01...1.0, default=0.3)
	double FrameRate;				///< Framerate limiter
	int DebugLvl;					///< Level of debug output 'verbosity'. Higher values create more detailed output (0...4, default=1)
	int MaxLights;					///< Maximum number of light sources in use (0...12, default=12)
	int NVPerfHUD;					///< ??? (0,1)
	int ManagedTiles;				///< Whether to read DDS surfaces managed or un-managed (0,1 default=0)
	int EnvMapSize;					///< Environment map size (64...512)
	int EnvMapMode;					///< Environment map mode (0=disabled, 1=planet only, 2=fullscene)
	int EnvMapFaces;				///< Number of environment map faces render per frame (1..6, default=1)
	int EnableGlass;				///< Enable improved glass shading (Fresnel reflection)
	int EnableMeshDbg;				///< Enable mesh debugger
	int ShadowMapMode;				///< Shadow Mapping Mode
	int ShadowMapSize;				///< Shadow Map size
	char *DebugFont;				///< Font face for debug lines (default="Fixed")
	char *Shaders;					///< Shaders set to use. Naming a sub-directory of "Modules/" (default="Default")
	char *SolCfg;					///< Solar system to use (default="Sol")

private:

};

#endif // !__D3D9CONFIG_H
