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
#include <psapi.h>


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
	HMODULE hMods[1024];
	HANDLE hProcess;
	DWORD cbNeeded;

	// Get a handle to the process.
	hProcess = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());
	if (NULL == hProcess) {
		return;
	}

	// Get a list of all the modules in this process.
	if (EnumProcessModules(hProcess, hMods, sizeof(hMods), &cbNeeded))
	{
		for (unsigned int i = 0, n = 0; i < (cbNeeded / sizeof(HMODULE)); ++i)
		{
			TCHAR szModName[MAX_PATH];

			if (GetModuleBaseName(hProcess, hMods[i], szModName, ARRAYSIZE(szModName)))
			{
				std::string name = std::string(szModName); toUpper(name);
				// Module of interest?
				if (name == "D3D9.DLL" || 0 == name.compare(0, 6, "D3DX9_"))
				{
					// Get the full path to the module's file.
					if (GetModuleFileNameEx(hProcess, hMods[i], szModName, ARRAYSIZE(szModName)))
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

						TCHAR versionString[128] = "";
						LPDWORD pDummy = 0;
						DWORD versionInfoSize = GetFileVersionInfoSize(szModName, pDummy);
						if (versionInfoSize) {
							DWORD dummy = 0;
							char *data = new char[versionInfoSize]();
							if (GetFileVersionInfo(szModName, dummy, versionInfoSize, data))
							{
								UINT size = 0;
								VS_FIXEDFILEINFO *verInfo;

								if (VerQueryValue(data, "\\", (LPVOID*)&verInfo, &size) && size)
								{
									sprintf_s(versionString, ARRAYSIZE(versionString),
										" [v %d.%d.%d.%d]",
										HIWORD( verInfo->dwProductVersionMS ),
										LOWORD( verInfo->dwProductVersionMS ),
										HIWORD( verInfo->dwProductVersionLS ),
										LOWORD( verInfo->dwProductVersionLS )
									);
								}
							}
							delete[] data;
						}

						// Print the module name.
						auto prefix = (n++ ? "         " : "D3D9 DLLs");
						oapiWriteLogV("%s  : %s%s",	prefix, szModName, versionString);
					}
				}
			}
		}
	}

	// Release the handle to the process.
	CloseHandle( hProcess );
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

		if (oapiReadItem_string(f, "ElevationMode", string)) {
			if (1 == sscanf_s(string, "%lu", &flags)) {
				elevationMode = flags;
			}
		}

		// Get planet rendering parameters
		oapiReadItem_bool(f, "TileLoadThread", tileLoadThread);

		// Get directory config
		if (oapiReadItem_string(f, "ConfigDir", string)) {
			configDir = string;
		}
		if (oapiReadItem_string(f, "MeshDir", string)) {
			meshDir = string;
		}
		if (oapiReadItem_string(f, "TextureDir", string)) {
			textureDir = string;
		}
		if (oapiReadItem_string(f, "HightexDir", string)) {
			hightexDir = string;
		}
		if (oapiReadItem_string(f, "ScenarioDir", string)) {
			scenarioDir = string;
		}

		oapiCloseFile(f, FILE_IN_ZEROONFAIL);

		// Log directory config
		auto logPath = [](const char *name, const std::string &path) {
			TCHAR buff[MAX_PATH];
			if (GetFullPathName(path.c_str(), MAX_PATH, buff, NULL)) {
				DWORD ftyp = GetFileAttributes(buff);
				auto result = (ftyp == INVALID_FILE_ATTRIBUTES || !(ftyp & FILE_ATTRIBUTE_DIRECTORY) ? " [[DIR NOT FOUND!]]" : "");
				oapiWriteLogV("%-11s: %s%s", name, buff, result);
			}
		};
		oapiWriteLog("---------------------------------------------------------------");
		logPath("BaseDir"    , ".\\");
		logPath("ConfigDir"  , configDir);
		logPath("MeshDir"    , meshDir);
		logPath("TextureDir" , textureDir);
		logPath("HightexDir" , hightexDir);
		logPath("ScenarioDir", scenarioDir);
		oapiWriteLog("---------------------------------------------------------------");
		LogD3D9Modules();
		oapiWriteLog("---------------------------------------------------------------");

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

	// Check for WINE environment
	HMODULE hntdll = GetModuleHandle("ntdll.dll");
	if (NULL != hntdll)
	{
		// static const char * (CDECL *pwine_get_version)(void);
		void *pWineGetVersion = (void *)GetProcAddress(hntdll, "wine_get_version");
		if (NULL != pWineGetVersion) {
			runsUnderWINE = true;
		} // else { Not running WINE }
	} // else { Not running on NT ?! }

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
		std::string scenarioName = trim( commandLine.substr(pos+2, std::string::npos) );

		// Remove (optional) quotes
		std::replace(scenarioName.begin(), scenarioName.end(), '"', ' ');

		// Build the path (like ".\\Scenarios\\(Current State).scn"
		//startupScenario = GetScenarioDir() + trim(scenarioName) + ".scn";
		return GetScenarioDir() + trim(scenarioName) + ".scn";
	}
	return "";
}

// --- eof ---
