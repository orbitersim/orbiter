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

/// Load tiles in separate thread?
/// \par Parameter type:
///   bool
#define CFGPRM_TILELOADTHREAD  0x1006

/// Elevation interpolation mode 0-cubic 1-linear
/// \par Parameter type:
///   DWORD
#define CFGPRM_ELEVATIONINTERPOLATION 0x1007

/// @}


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
	static DWORD elevationMode;
	static bool tileLoadThread; ///< Whether to load planet tiles in separate thread [true|false]
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

	static bool configParameterRead;      ///< Indication that Orbiter_NG.cfg has been read
	static bool GetConfigParameter(void); ///< Tries to read parameter from Orbiter_NG.cfg
	static std::string ScanCommandLine(void); ///< Tries to read a Startup Scenario given by "-s" command line parameter
};

#endif // !__OAPIEXTENSION_H
