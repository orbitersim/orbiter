// ==============================================================
// OapiExtension.h
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2018 Peter Schneider (Kuddel)
// ==============================================================

#ifndef __OAPIEXTENSION_H
#define __OAPIEXTENSION_H

#include <Windows.h>
#include <string>
#include "D3D9Util.h"

class D3D9Config;

/// \defgroup cfgprm Configuration parameter identifiers
/// Used by OapiExtension::GetConfigParam()
/// @{


/// Bit flag for "body force vectors display mode" elements.
/// For a description of the available bit flags, see \ref bfvflag
/// \par Parameter type:
///   DWORD
#define CFGPRM_SHOWBODYFORCEVECTORSFLAG  0x1000

/// Length factor for body force vectors display (0-1)
/// \par Parameter type:
///   float
#define CFGPRM_BODYFORCEVECTORSSCALE  0x1001

/// Opacity of body force vectors (0-1)
/// \par Parameter type:
///   float
#define CFGPRM_BODYFORCEVECTORSOPACITY  0x1002

/// Bit flag for "coordinate axes display mode" elements.
/// For a description of the available bit flags, see \ref scaflag
/// \par Parameter type:
///   DWORD
#define CFGPRM_SHOWCOORDINATEAXESFLAG  0x1003

/// Length factor for coordinate axes display (0-1)
/// \par Parameter type:
///   float
#define CFGPRM_COORDINATEAXESSCALE  0x1004

/// Opacity of coordinate axes (0-1)
/// \par Parameter type:
///   float
#define CFGPRM_COORDINATEAXESOPACITY  0x1005


/// Load tiles in separate thread?
/// \par Parameter type:
///   bool
#define CFGPRM_TILELOADTHREAD  0x1006

/// @}


/// \defgroup bfvflag Bit flags for body force vector display mode elements
/// @{
#define BFV_ENABLE    0x0001 ///< Enable body force vectors display mode (master flag)
#define BFV_SCALE_LOG 0x0002 ///< Flag indicating logarithmic (1) or linear (0) scale is used for displaying vector lengths
#define BFV_WEIGHT    0x0004 ///< Enable weight force vector display
#define BFV_THRUST    0x0008 ///< Enable thrust force vector display
#define BFV_LIFT      0x0010 ///< Enable lift force vector display
#define BFV_DRAG      0x0020 ///< Enable drag force vector display
#define BFV_TOTAL     0x0040 ///< Enable total force vector display
#define BFV_TORQUE    0x0080 ///< Enable torque force vector display

/// @}


/// \defgroup scaflag Bit flags for coordinate axes vector display mode elements
/// @{
#define SCA_ENABLE   0x0001 ///< Enable coordinate axes display mode (master flag)
#define SCA_NEGATIVE 0x0002 ///< Enable display of negative axes
#define SCA_VESSEL   0x0004 ///< Enable vessel coordinate axes
#define SCA_CELBODY  0x0008 ///< Enable celestial body coordinate axes
#define SCA_SURFBASE 0x0010 ///< Enable surface base coordinate axes

/// @}


/**
 * \brief Storage structure to keep hooking information.
 *
 * This struct basically keeps track of whether a hook is installed or not and
 * stores the original handles for later recovery.
 */
typedef struct {
	int     cid;              ///< ControlID (IDC_xxx)
	DWORD   hookFlag;         ///< Bit flag for the hookMap
	WNDPROC lpWrapWndFunc;    ///< Wrapped WindowProc
	WNDPROC lpOrigWndFunc;    ///< Original WindowProc
	HWND    hWnd;             ///< Window (e.g. CheckBox) handle
	HWND    hWndScaleGauge;   ///< Scale gauge handle
	HWND    hWndOpacityGauge; ///< Opacity gauge handle
} HOOKINFO, *LPHOOKINFO;


/**
 * \brief Configuration parameter provider for non-API parameters
 *
 * This class provides access to config-parameters that are not (yet) available
 * through the 'official' API of Orbiter. Currently the switches and values of
 * the 'Visual helpers' dialog.
 */
class OapiExtension
{
public:

	/**
	 * \brief Initializes the OapiExtension.
	 *
	 * This function should be called early on, because it defines whether this
	 * class will install any hooking functions depending on the \ref
	 * D3D9Config::DisableVisualHelperReadout value.
	 * \param Config A reference to the configuration class, to get the \ref
	 *   D3D9Config::DisableVisualHelperReadout value.
	 */
	static void GlobalInit(const D3D9Config &Config);

	/**
	 * \brief Handles the read-out of values from an opened 'visual helpers'
	 * dialog.
	 *
	 * This function handles the attachment of delegate functions to the popup
	 * windows. It has to be called whenever popup widows appears/disappears
	 * \param hPopupWnd The list returned by the \ref
	 *   oapi::GraphicsClient::GetPopupList method, containing the handles of
	 *   popup windows that are to be rendered.
	 * \param count Number of entries in the list (return value of \ref
	 *   oapi::GraphicsClient::GetPopupList.
	 */
	static void HandlePopupWindows (const HWND *hPopupWnd, DWORD count);

	/**
	 * \brief Same functionality than 'official' GetConfigParam, but for
	 * non-provided config parameters
	 *
	 * This function can be used to access various configuration parameters
	 * defined in the OapiExtension core (e.g. body force vector display mode).
	 * \param paramtype Parameter identifier (see \ref cfgprm)
	 * \return Pointer to parameter
	 * \note The pointer must be cast into the appropriate variable type.
	 *   The variable types can be found in the parameter type list (\ref
	 *   cfgprm).
	 * \par Example:
	 * \code
	 * float scale = *(float*)GetConfigParam(CFGPRM_COORDINATEAXESSCALE);
	 * \endcode
	 */
	static const void *GetConfigParam (DWORD paramtype);

	/**
	 * \brief Returns whether we run Orbiter 2010 (and derivatives)
	 *
	 * \return Whether we run Orbiter 2010
	 */
	static const bool RunsOrbiter2010 () { return isOrbiter2010; }

	/**
	 * \brief Returns whether OrbiterSound 4.0 is up and running
	 *
	 * \return Whether OrbiterSound 4.0 is active
	 */
	static const bool RunsOrbiterSound40() { return orbiterSound40; }

	/**
	 * \brief Returns whether Orbiter runs under WINE
	 *
	 * \return Whether Orbiter runs under WINE
	 */
	static const bool RunsUnderWINE() { return runsUnderWINE; }

	/**
	* \brief Returns whether the current Scenario uses Spacecraft.dll
	*
	* \return Whether the current Scenario uses Spacecraft.dll
	*/
	static const bool RunsSpacecraftDll() { return runsSpacecraftDll; }

	/**
	 * \brief Returns the current path to Config folder
	 *
	 * \return Path to Config folder
	 */
	static const char *GetConfigDir() { return configDir.c_str(); }

	/**
	 * \brief Returns the current path to Mesh folder
	 *
	 * \return Path to Mesh folder
	 */
	static const char *GetMeshDir() { return meshDir.c_str(); }

	/**
	 * \brief Returns the current path to Texture folder
	 *
	 * \return Path to Texture folder
	 */
	static const char *GetTextureDir() { return textureDir.c_str(); }

	/**
	 * \brief Returns the current path to High Texture folder
	 *
	 * \return Path to High Texture folder
	 */
	static const char *GetHightexDir() { return hightexDir.c_str(); }

	/**
	 * \brief Returns the current path to Scenario folder
	 *
	 * \return Path to Scenario folder
	 */
	static const char *GetScenarioDir() { return scenarioDir.c_str(); }

	/**
	 * \brief Returns the value of a Startup Scenario (-s CLI option)
	 *
	 * \return Path to Startup Scenario ("" if not given via CLI)
	 */
	static const char *GetStartupScenario() { return startupScenario.c_str(); }

	// NOT REALLY PUBLIC! (Only to be used by FileParser class!)
	static void SetSpacecraftDllUsed(bool value = true) { runsSpacecraftDll = value; }

private:
	OapiExtension(void); // avoid default constructor creation & instantiation
	~OapiExtension(void);

	// Planet rendering parameters
	static bool tileLoadThread; ///< Whether to load planet tiles inseparate thread [true|false]
	// Body forces
	static DWORD showBodyForceVectorsFlags;
	static float bodyForceScale;   // [0.25..4.0]
	static float bodyForceOpacity; // [0...1]
	// Coordinate axes
	static DWORD showCoordinateAxesFlags;
	static float coordinateAxesScale;   // [0.25...4.0]
	static float coordinateAxesOpacity; // [0...1]
	// OrbiterSound 4.0 helper
	static bool isOrbiter2010;          ///< Whether we run Orbiter  2010 (and derivatives)
	static bool orbiterSound40;
	static std::string configDir;       ///< Value of Orbiters ConfigDir parameter
	static std::string meshDir;         ///< Value of Orbiters MeshDir parameter
	static std::string textureDir;      ///< Value of Orbiters TextureDir parameter
	static std::string hightexDir;      ///< Value of Orbiters HightexDir parameter
	static std::string scenarioDir;     ///< Value of Orbiters ScenarioDir config parameter
	static std::string startupScenario; ///< Scenario-Path if Orbiters was started with "-s {Scenario}" command line parameter
	// WINE detection
	static bool runsUnderWINE;          ///< Whether Orbiter runs under WINE
	// Spacecraft.dll detection
	static bool runsSpacecraftDll;      ///< Whether the current Scenario uses Spacecraft.dll
	static void LogD3D9Modules(void);   ///< Logs loaded D3D9 DLLs and their versions

	// Hooking
	static DWORD    hookMap;     // Flags indicating 'already delegated' widgets
	static HOOKINFO hookInfos[]; // Table of information of wrapped methods and items

	static bool configParameterRead;      ///< Indication that Orbiter_NG.cfg has been read
	static bool GetConfigParameter(void); ///< Tries to read parameter from Orbiter_NG.cfg
	static std::string ScanCommandLine(void); ///< Tries to read a Startup Scenario given by "-s" command line parameter

	static bool AllHooksAttached(void) {return hookMap == 0x7FFF;}
	static const LPHOOKINFO GetHookInfo(DWORD cid);
	static const LPHOOKINFO GetHookInfo(HWND hwnd);
	static const void RemoveHook(LPHOOKINFO lpHookInfo);

	static const bool IsOurDialog(HWND hwnd);
	static BOOL CALLBACK EnumChildProc(HWND hwnd, LPARAM lParam);
	static LRESULT CALLBACK CheckBoxWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static LRESULT CALLBACK GaugeWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

#endif // !__OAPIEXTENSION_H
