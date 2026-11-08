// ==============================================================
// OapiExtension.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2018 Peter Schneider (Kuddel)
// ==============================================================

#include <algorithm>
#include <filesystem>
#include "D3D9Util.h"
#include "OapiExtension.h"
#include "D3D9Config.h"
#include "OrbiterAPI.h"


// ===========================================================================
// Class statics initialization

DWORD OapiExtension::elevationMode = 0;
// Orbiters default directories
std::string OapiExtension::configDir(".\\Config\\");
std::string OapiExtension::meshDir(".\\Meshes\\");
std::string OapiExtension::textureDir(".\\Textures\\");
std::string OapiExtension::hightexDir(".\\Textures2\\");
std::string OapiExtension::scenarioDir(".\\Scenarios\\");

std::string OapiExtension::startupScenario = OapiExtension::ScanCommandLine();

bool OapiExtension::configParameterRead = OapiExtension::GetConfigParameter();

// 2010       100606
// 2010-P1    100830
// 2010-P2    110822
// 2010-P2.1  110824
bool OapiExtension::isOrbiter2010 = (oapiGetOrbiterVersion() <= 110824 && oapiGetOrbiterVersion() >= 100606);

bool OapiExtension::orbiterSound40 = false;
bool OapiExtension::tileLoadThread = true;
bool OapiExtension::runsUnderWINE = false;
bool OapiExtension::runsSpacecraftDll = false;


// ===========================================================================
// Construction
//
OapiExtension::OapiExtension(void) {
}

// ===========================================================================
// Destruction
//
OapiExtension::~OapiExtension(void)
{
}


/*
------------------------------------------------------------------------------
	PUBLIC INTERFACE METHODS
------------------------------------------------------------------------------
*/

// ===========================================================================
// Initialization
//
void OapiExtension::GlobalInit(const D3D9Config &Config)
{
}

// ===========================================================================
// Same functionality than 'official' GetConfigParam, but for non-provided
// config parameters
//
const void *OapiExtension::GetConfigParam (DWORD paramtype)
{
	switch (paramtype) {
		case CFGPRM_ELEVATIONINTERPOLATION	: return (void*)&elevationMode;
		case CFGPRM_TILELOADTHREAD          : return (void*)&tileLoadThread;
		default                             : return NULL;
	}
}

/*
------------------------------------------------------------------------------
	PRIVATE METHODS
------------------------------------------------------------------------------
*/

// ===========================================================================
// Logs loaded D3D9 DLLs and their versions to Orbiter.log
//
void OapiExtension::LogD3D9Modules(void)
{
	IDirect3D9 *d3d = Direct3DCreate9(D3D_SDK_VERSION);
	if (!d3d) return;
	D3DADAPTER_IDENTIFIER9 ai;
	if (d3d->GetAdapterIdentifier(0, 0, &ai) == S_OK) oapiWriteLogV("D3D9: Metal device %s", ai.Description);
	d3d->Release();
}



// ===========================================================================
// Tries to get the initial settings from Orbiter_NG.cfg file
//
bool OapiExtension::GetConfigParameter(void)
{
	char *pLine;
	bool orbiterSoundModuleEnabled = false;

	FILEHANDLE f = oapiOpenFile("Orbiter_NG.cfg", FILE_IN_ZEROONFAIL, ROOT);
	if (f) {
		char  string[MAX_PATH];
		DWORD flags;
		float scale, opacity;

		// General check for OrbiterSound module enabled
		while (oapiReadScenario_nextline(f, pLine)) {
			if (NULL != strstr(pLine, "OrbiterSound")) {
				orbiterSoundModuleEnabled = true;
				break;
			}
		}

		if (oapiReadItem_string(f, (char*)"ElevationMode", string)) {
			if (1 == sscanf_s(string, "%u", &flags)) {
				elevationMode = flags;
			}
		}

		// Get planet rendering parameters
		oapiReadItem_bool(f, (char*)"TileLoadThread", tileLoadThread);

		// Get directory config
		if (oapiReadItem_string(f, (char*)"ConfigDir", string)) {
			configDir = string;
		}
		if (oapiReadItem_string(f, (char*)"MeshDir", string)) {
			meshDir = string;
		}
		if (oapiReadItem_string(f, (char*)"TextureDir", string)) {
			textureDir = string;
		}
		if (oapiReadItem_string(f, (char*)"HightexDir", string)) {
			hightexDir = string;
		}
		if (oapiReadItem_string(f, (char*)"ScenarioDir", string)) {
			scenarioDir = string;
		}

		oapiCloseFile(f, FILE_IN_ZEROONFAIL);

		// Log directory config
		auto logPath = [](const char *name, const std::string &path) {
			std::string p = path;
			std::replace(p.begin(), p.end(), '\\', '/');
			std::error_code ec;
			std::filesystem::path full = std::filesystem::absolute(p, ec).lexically_normal();
			auto result = std::filesystem::is_directory(full, ec) ? "" : " [[DIR NOT FOUND!]]";
			oapiWriteLogV("%-11s: %s%s", name, full.string().c_str(), result);
		};
		oapiWriteLog((char*)"---------------------------------------------------------------");
		logPath("BaseDir"    , ".\\");
		logPath("ConfigDir"  , configDir);
		logPath("MeshDir"    , meshDir);
		logPath("TextureDir" , textureDir);
		logPath("HightexDir" , hightexDir);
		logPath("ScenarioDir", scenarioDir);
		oapiWriteLog((char*)"---------------------------------------------------------------");
		LogD3D9Modules();
		oapiWriteLog((char*)"---------------------------------------------------------------");

	}

	// Check for the OrbiterSound version
	if (orbiterSoundModuleEnabled)  {
		orbiterSound40 = false;

		f = oapiOpenFile("Sound\\version.txt", FILE_IN_ZEROONFAIL, ROOT);
		while (f && oapiReadScenario_nextline(f, pLine)) {
			if (NULL != strstr(pLine, "OrbiterSound 4.0 (3D)")) {
				orbiterSound40 = true;
				break;
			}
		}
		oapiCloseFile(f, FILE_IN_ZEROONFAIL);
	}


	return true;
}

// ===========================================================================
// Try to read a startup scenario given by "-s" command line parameter
//
std::string OapiExtension::ScanCommandLine (void)
{
	std::string commandLine(GetCommandLine());

	// Is there a "-s <scenario_name>" option at all?
	size_t pos = rfind_ci(commandLine, "-s");
	if (pos != std::string::npos)
	{
		std::string scenarioName = commandLine.substr(pos+2, std::string::npos);
		trim(scenarioName);

		// Remove (optional) quotes
		std::replace(scenarioName.begin(), scenarioName.end(), '"', ' ');

		// Build the path (like ".\\Scenarios\\(Current State).scn"
		//startupScenario = GetScenarioDir() + trim(scenarioName) + ".scn";
		return GetScenarioDir() + trim(scenarioName) + ".scn";
	}
	return "";
}

// --- eof ---
