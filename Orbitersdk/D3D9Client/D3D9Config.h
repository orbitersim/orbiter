// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2006-2016 Martin Schweiger
//				 2012-2016 Jarmo Nikkanen
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
	int NearClipPlane;				///< Near clip plane mode (0,1)
	int PreLBaseVis;				///< Preload base visuals (0=load on demand, 1=preload)
	int DebugFontSize;				///< Debug font height \[pixel\] (default=18px)
	int GDIRTSWrn;					///< Whether to identify RT-GDI conflict by flashing the surface (0,1)
	int UseNormalMap;				///< Enable normal mapping (0,1)
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
	double PlanetGlow;				///< Intensity of planet glow effect (0.01...2.0, default=0.7)
	double FrameRate;				///< Framerate limiter
	int EnableLimiter;				///< Enable framerate limiter
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
	int CustomCamMode;				///< Custom Camera Mode
	int TileMipmaps;				///< Enable surface tile mipmaps
	int LODBias;					///< 3D Terrain resolution bias
	int MeshRes;					///< Tile patch mesh resolution
	int TileDebug;					///< Enable tile debugger
	int TextureMips;				///< Texture mipmap autogen policy
	int MicroMode;			
	int MicroFilter;
	char *DebugFont;				///< Font face for debug lines (default="Fixed")
	char *SolCfg;					///< Solar system to use (default="Sol")

private:

};

#endif // !__D3D9CONFIG_H
