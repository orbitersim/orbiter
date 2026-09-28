// ==============================================================
// OapiExtension.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2018 Peter Schneider (Kuddel)
// ==============================================================

#include <algorithm>
#include "D3D9Util.h"
#include "OapiExtension.h"
#include "D3D9Config.h"
#include "OrbiterAPI.h"
// psapi.h left out: the loaded libraries come from dl_iterate_phdr
#include <link.h>
#include <sys/stat.h>
#include <climits>
#include <cstdlib>


// ===========================================================================
// Class statics initialization

DWORD OapiExtension::elevationMode = 0;
// Orbiters default directories
std::string OapiExtension::configDir("./Config/");
std::string OapiExtension::meshDir("./Meshes/");
std::string OapiExtension::textureDir("./Textures/");
std::string OapiExtension::hightexDir("./Textures2/");
std::string OapiExtension::scenarioDir("./Scenarios/");

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
// (the Vulkan loader, the ICD driver and glslang libraries here; the version is the soname's)
//
void OapiExtension::LogD3D9Modules(void)
{
	std::vector<std::string> mods;

	// Get a list of all the modules in this process.
	dl_iterate_phdr([](struct dl_phdr_info *info, size_t, void *data) -> int {
		if (info->dlpi_name && info->dlpi_name[0]) ((std::vector<std::string> *)data)->push_back(info->dlpi_name);
		return 0;
	}, &mods);
	{
		unsigned int n = 0;
		for (auto &path : mods)
		{
			const char *szModName = path.c_str();
			{
				std::string name = path.substr(path.find_last_of('/') + 1); toUpper(name);
				// Module of interest?
				if (0 == name.compare(0, 9, "LIBVULKAN") || name.find("VK_") != std::string::npos || 0 == name.compare(0, 10, "LIBGLSLANG") ||
					0 == name.compare(0, 13, "LIBNVIDIA-GLC") || 0 == name.compare(0, 13, "LIBVULKAN_LVP"))
				{
					// the full path to the module's file is dlpi_name
					{

						/*DWORD crc = 0;
						FILE *hFile = 0;
						if (fopen_s(&hFile, szModName, "rb") == 0) {
							while (true) {
								int data = fgetc(hFile);
								if (data == EOF) break;
								crc = crc ^ ((data&0xFF) << 8);
								for (int j = 0; j < 8; j++) {
									if (crc & 0x8000) crc = (crc << 1) ^ 0x1021;
									else crc = (crc << 1);
									crc &= 0xFFFF;
								}
							}
							crc &= 0xFFFF;
							fclose(hFile);
						}*/

						char versionString[128] = "";
						char real[PATH_MAX];
						if (realpath(szModName, real)) { // version resource → the ".so.X.Y.Z" of the file the soname points at
							const char *v = strstr(real, ".so.");
							if (v) snprintf(versionString, std::size(versionString), " [v %s]", v + 4);
						}

						// Print the module name.
						auto prefix = (n++ ? "         " : "D3D9 DLLs");
						oapiWriteLogV("%s  : %s%s",	prefix, szModName, versionString);
					}
				}
			}
		}
	}

	// CloseHandle left out: no process handle was opened
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
			if (1 == sscanf(string, "%u", &flags)) {
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
			char buff[PATH_MAX];
			struct stat st;
			bool found = realpath(p.c_str(), buff) != NULL; // GetFullPathName
			if (!found) snprintf(buff, sizeof(buff), "%s", p.c_str());
			auto result = (!found || stat(buff, &st) != 0 || !S_ISDIR(st.st_mode) ? " [[DIR NOT FOUND!]]" : "");
			oapiWriteLogV("%-11s: %s%s", name, buff, result);
		};
		oapiWriteLog((char*)"---------------------------------------------------------------");
		logPath("BaseDir"    , "./");
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

		f = oapiOpenFile("Sound/version.txt", FILE_IN_ZEROONFAIL, ROOT);
		while (f && oapiReadScenario_nextline(f, pLine)) {
			if (NULL != strstr(pLine, "OrbiterSound 4.0 (3D)")) {
				orbiterSound40 = true;
				break;
			}
		}
		oapiCloseFile(f, FILE_IN_ZEROONFAIL);
	}

	// Check for WINE environment
	// (left out: a native Linux build never runs under WINE, runsUnderWINE stays false)

	return true;
}

// ===========================================================================
// Try to read a startup scenario given by "-s" command line parameter
//
std::string OapiExtension::ScanCommandLine (void)
{
	std::string commandLine; // GetCommandLine: the arguments from /proc/self/cmdline, space separated
	FILE *cl = fopen("/proc/self/cmdline", "rb");
	if (cl) {
		int ch;
		while ((ch = fgetc(cl)) != EOF) commandLine += (ch ? (char)ch : ' ');
		fclose(cl);
	}

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
