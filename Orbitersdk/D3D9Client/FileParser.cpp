// ==============================================================
// FileParser.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
//				 2012 - 2016 Émile "Bibi Uncle" Grégoire
//				 2012 - 2016 Peter Schneider (Kuddel)
// ==============================================================

#include <sstream>

#include "FileParser.h"
#include "D3D9Config.h"
#include "D3D9Client.h"
#include "Log.h"
#include "OapiExtension.h"

extern oapi::D3D9Client *g_client;

// ===========================================================================================
//
FileParser::FileParser (const std::string &scenario) :
	system(),
	context(),
  mjd(oapiGetSimMJD())
{
	_TRACE;

	LogAlw("==== Scanning Configuration Files ====");

	ParseScenario(scenario);

	if (HasMissingObjects()) {
		LogAlw("--- Some bases are still missing ---");
		DWORD npl = oapiGetGbodyCount();
		for (DWORD p=0;p<npl;p++) {
			OBJHANDLE hPl = oapiGetGbodyByIndex(p);
			if (hPl && oapiGetObjectType(hPl) == OBJTP_PLANET) {
				DWORD nb = oapiGetBaseCount(hPl);
				for (DWORD b=0;b<nb;b++) {
					OBJHANDLE hBs = oapiGetBaseByIndex(hPl, b);
					if (!DoesExist(hBs)) {
						if (ScanBases(hPl, "", hBs, false)==false) {
							//char name[256];
							//oapiGetObjectName(hBs, name, 256);
							//LogAlw("[Difficult to find base 0x%X (%s)]",hBs,name);
							//ScanBases(hPl, "", hBs, true);
						}
					}
				}
			}
		}
	}
}

// ===========================================================================================
//
FileParser::~FileParser ()
{
	for (auto it = entries.begin(); it != entries.end(); ++it) {
		delete[] it->second->file;
		delete it->second;
	}
	entries.clear();
}

// ===========================================================================================
//
const char *FileParser::GetConfigFile (OBJHANDLE hObj)
{
	ObjEntry *e = GetEntry(hObj);
	return e ? e->file : NULL;
}

// ===========================================================================================
//
VECTOR3 FileParser::GetAlbedo (OBJHANDLE hObj)
{
	ObjEntry *e = GetEntry(hObj);
	return e ? e->Albedo : _V(1,1,1);
}

// ===========================================================================================
//
bool FileParser::DoesExist (OBJHANDLE hObj) {
	return GetEntry(hObj) != NULL;
}

// ===========================================================================================
//
FileParser::ObjEntry *FileParser::GetEntry (OBJHANDLE hObj, bool create/* = false*/)
{
	auto it = entries.find(hObj);
	if (it != entries.end()) {
		return it->second;
	}
	if (create) {
		return entries[hObj] = new ObjEntry();
	}
	return NULL;
}

// ===========================================================================================
//
bool FileParser::HasMissingObjects ()
{
	DWORD npl = oapiGetGbodyCount();
	for (DWORD p=0;p<npl;p++) {
		OBJHANDLE hPl = oapiGetGbodyByIndex(p);
		if (hPl && oapiGetObjectType(hPl) == OBJTP_PLANET) {
			if (!DoesExist(hPl)) return true;
			else {
				DWORD nbs = oapiGetBaseCount(hPl);
				for (DWORD b=0;b<nbs;b++) {
					OBJHANDLE hBs = oapiGetBaseByIndex(hPl, b);
					if (!DoesExist(hBs)) return true;
				}
			}
		}
	}
	return false;
}

// ===========================================================================================
//
void FileParser::LogContent ()
{
	char buf[256];

	DWORD npl = oapiGetGbodyCount();

	for (DWORD p=0;p<npl;p++) {
		OBJHANDLE hPl = oapiGetGbodyByIndex(p);
		if (hPl && oapiGetObjectType(hPl) == OBJTP_PLANET) {
			if (!DoesExist(hPl)) {
				oapiGetObjectName(hPl, buf, 256);
				LogErr("Planet %s = '%s' not cataloged", _PTR(hPl), buf);
			}
			else {
				DWORD nbs = oapiGetBaseCount(hPl);
				for (DWORD b=0;b<nbs;b++) {
					OBJHANDLE hBs = oapiGetBaseByIndex(hPl, b);
					if (!DoesExist(hBs)) {
						oapiGetObjectName(hBs, buf, 256);
						LogErr("Base Object %s = '%s' not cataloged", _PTR(hBs), buf);
					}
				}
			}
		}
	}
}

// ===========================================================================================
//
bool FileParser::ParseScenario (const std::string &name)
{
	std::ifstream fs(name);
	if (fs.fail())
	{
		// try to get it from "-s <scenario_name>" option
		// by setting D3D9Client instances value we chage 'name' reference accordingly!
		g_client->SetScenarioName(OapiExtension::GetStartupScenario());
		// try again
		fs.open(name);
		if (fs.fail()) {
			LogErr("Could not open a scenario '%s'", name.c_str());
			return false;
		}
	}

	bool result = false;
	std::string line, // One file line
	            dummy;
	std::istringstream iss;

	// skip until BEGIN_ENVIRONMENT
	while (std::getline(fs, line) && !startsWith( trim(line), "BEGIN_ENVIRONMENT")) {}

	while (std::getline(fs, line))
	{
		line = trim(line);

		// skip empty lines & comment lines
		if (!line.length() || line[0] == ';') {
			continue;
		}

		// re-set string stream
		iss.str(line);
		iss.clear();

		// SYSTEM <string>
		if (startsWith(line, "System")) {
			system = splitAssignment(line, ' ').second;
			LogAlw("Scenario System=%s", system.c_str());
		}
		// CONTEXT <string>
		else if (startsWith(line, "CONTEXT")) {
			context = splitAssignment(line, ' ').second;
			LogAlw("Scenario Context=%s", context.c_str());
		}
		// Date MJD <float>
		else if (startsWith(line, "Date MJD")) {
			iss >> dummy >> dummy >> mjd;
			LogAlw("Scenario MJD=%.12f", mjd);
		}
		// Date JD <float>
		else if (startsWith(line, "Date JD")) {
			iss >> dummy >> dummy >> mjd;
			if (!iss.fail()) { mjd -= 2400000.5; }
			LogAlw("Scenario MJD=%.12f", mjd);
		}
		// Date JE <float>
		else if (startsWith(line, "Date JE")) {
			LogErr("UnImplemented parameter 'Date JE' in scenario '%s',", name.c_str());
		}
		else if (startsWith(line, "END_ENVIRONMENT")) {
			if (!system.empty()) {
				result = ParseSystem(system); // Most likely 'result = true;' ;)
				break;
			}
			else {
				LogErr("Failed to find a system from scenario '%s'", name.c_str());
			}
			break;
		}
	}

	// Spacecraft.dll detection
	//
	bool spacecraftDllUsed = false;

	// skip until BEGIN_SHIPS
	while (std::getline(fs, line) && !startsWith(trim(line), "BEGIN_SHIPS")) {}

	while (std::getline(fs, line))
	{
		if ( contains(line, ":spacecraft\\spacecraft") ) {
			spacecraftDllUsed = true;
			break;
		}
	}
	OapiExtension::SetSpacecraftDllUsed(spacecraftDllUsed);


	if (!result) {
		LogErr("Failed to parse a scenario '%s'", name.c_str());
	}
	return result;
}

// ===========================================================================================
//
bool FileParser::ParseSystem (const std::string &_name)
{
	std::string name;
	name = OapiExtension::GetConfigDir() + _name + ".cfg";

	std::ifstream fs(name);

	if (fs.fail()) {
		LogErr("Could not open a solar system file '%s'", name.c_str());
		return false;
	}

	std::string line; // One file line

	while (std::getline(fs, line))
	{
		line = trim(line);
		// skip empty lines, comments and the "Name = Sol" lines
		if (!line.length() || line[0] == ';' || startsWith(line, "Name")) {
			continue;
		}

		// <PlanetX> = <string>
		auto ass = splitAssignment(line);

		if (!ass.second.empty()) {
			ParsePlanet(ass.second);
		}
	}
	return true;
}

// ===========================================================================================
//
bool FileParser::ParsePlanet (const std::string &_name)
{
	__TRY {
		bool bHasPath = false;

		std::string path, name, def, dummy;

		name = _name + ".cfg";                       // e.g. "Earth.cfg"
		path = OapiExtension::GetConfigDir() + name; // e.g. ".\Config\Earth.cfg"
		def  = _name + "\\Base";                     // e.g. "Earth\Base"

		std::ifstream fs(path);
		if (fs.fail()) {
			LogErr("Could not open a planet configuration file '%s'", name.c_str());
			return false;
		}

		OBJHANDLE hPlanet = NULL;
		std::string line; // One file line

		while (std::getline(fs, line))
		{
			line = trim(line);
			// skip empty lines and comments
			if (!line.length() || line[0] == ';') {
				continue;
			}

			// Name = <string>
			if (startsWith(line, "Name"))
			{
				auto ass = splitAssignment(line);
				hPlanet = oapiGetObjectByName(const_cast<char*>(ass.second.c_str()));
				if (hPlanet)
				{
					if (DoesExist(hPlanet)) {
						continue; // Already exists, ignore this file
					}
					ObjEntry *e = GetEntry(hPlanet, true);
					e->file = new char[name.length() + 1];
					strcpy_s(e->file, name.length() + 1, &name[0]);
				}
				else {
					LogErr("Planet Not Found '%s'", &line[5]);
				}
			}

			if (hPlanet == NULL) continue;

			// AlbedoRGB = <float> <float> <float>
			if (startsWith(line, "AlbedoRGB"))
			{
				std::istringstream iss(line);
				ObjEntry *e = GetEntry(hPlanet, true);
				iss >> dummy >> dummy
					>> e->Albedo.x
					>> e->Albedo.y
					>> e->Albedo.z;
			}

			if (startsWith(line, "BEGIN_SURFBASE"))
			{
				std::string dir, buf0, buf1;

				bool bCtx = true;  // CONTEXT fits
				bool bPer = true;  // PERIOD fits
				bool bPth = false; // PATH set

				double mjd0 = DBL_MIN;// was: 0.0;
				double mjd1 = DBL_MAX;// was: 1e6;

				while (std::getline(fs, line))
				{
					line = trim(line);

					if (startsWith(line, "END_SURFBASE")) break;

					// DIR <folder> [PERIOD <mjd0> <mjd1>] [CONTEXT <string>]
					if (startsWith(line, "DIR")) {
						std::istringstream iss(line);
						iss >> dummy >> dir;
						bPth = true;
						// handle optional PERIOD or CONTEXT
						do {
							iss >> dummy;
							if (dummy == "PERIOD") {
								iss >> buf0 >> buf1;
								if (buf0 != "-") { mjd0 = atof(buf0.c_str()); }
								if (buf1 != "-") { mjd1 = atof(buf1.c_str()); }
								if (mjd0 > mjd || mjd1 < mjd) { bPer = false; }
							}
							else if (dummy == "CONTEXT") {
								iss >> buf0;
								bCtx = (buf0 == context);//if (buf0 != context) { bCtx = false; }
							}
						} while (!iss.fail());
					}

					if (bPth && bCtx && bPer) {
						bHasPath = true;
						ScanBases(hPlanet, dir);
					}
				}
			}
		}

		// Is the default path already searched ?
		if (!bHasPath && hPlanet) {
			ScanBases(hPlanet, def);
		}
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("FileParser::ParsePlanet(%s)", _name.c_str());
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	return true;
}

// ===========================================================================================
//
OBJHANDLE FileParser::ParseBase (OBJHANDLE hPlanet, const char *name, OBJHANDLE hBase)
{
	std::ifstream fs(name);

	if (fs.fail()) {
		LogErr("Could not open a base configuration file '%s'", name);
		return NULL;
	}

	std::string line; // One file line
	char cbuf[512];   // big enough (for objectName)?
	bool bBase = false;

	while (std::getline(fs, line))
	{
		line = trim(line);
		// skip empty lines and comments
		if (!line.length() || line[0] == ';') {
			continue;
		}

		// BASE (start of block)
		if (startsWith(line, "BASE")) {
			bBase = true;
		}
		// Name = <string>
		else if (startsWith(line, "Name"))
		{
			if (!bBase) break;

			// auto ass = splitAssignment(line);
			std::string baseName = splitAssignment(line).second;

			if (hBase)
			{
				oapiGetObjectName(hBase, cbuf, sizeof(cbuf));
				if (baseName == cbuf && !DoesExist(hBase))
				{
					ObjEntry *e = GetEntry(hBase, true);
					e->file = new char[lstrlen(name) + 1];
					strcpy_s(e->file, strlen(name) + 1, name);

					LogAlw("Base Added: %s, %s", _PTR(hBase), name);
					return hBase;
				}
			}
			else
			{
				OBJHANDLE hBs = oapiGetBaseByName(hPlanet, const_cast<char*>(baseName.c_str()) );
				if (hBs && !DoesExist(hBs))
				{
					ObjEntry *e = GetEntry(hBs, true);
					e->file = new char[lstrlen(name) + 1];
					strcpy_s(e->file, strlen(name) + 1, name);

					LogAlw("Base Added: %s, %s", _PTR(hBs), name);
					return hBs;
				}
			}
			break; // we've got what we came for (and was already known)
		} // end-if (startsWith(line, "Name"))
	}

	return NULL;
}


bool FileParser::ScanBases (OBJHANDLE hPlanet, const std::string &dir, OBJHANDLE hBase, bool bDeep)
{
	_TRACE;

	std::string name;
	name = OapiExtension::GetConfigDir() + dir;

	if (hPlanet==NULL) {
		LogErr("hPlanet is NULL in FileParser::ScanBases(%s)", name.c_str());
		return false;
	}

	std::string     strFilePath;             // Filepath
	std::string     strPattern;              // Pattern
	std::string     strExtension;            // Extension
	HANDLE          hFile;                   // Handle to file
	WIN32_FIND_DATA FileInformation;         // File information

	strPattern.erase();
	strPattern = name;

	if (dir.empty()) strPattern += "*.*";
	else             strPattern += "\\*.*";

	hFile = FindFirstFile(strPattern.c_str(), &FileInformation);

	if (hFile != INVALID_HANDLE_VALUE)
	{
		do
		{
			if (FileInformation.cFileName[0] != '.')
			{
				if (FileInformation.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
				{
					strFilePath.erase();
					strFilePath = dir;
					strFilePath += FileInformation.cFileName;
					// Search the sub-directory only in a special cases
					if (bDeep) ScanBases(hPlanet, strFilePath.c_str(), hBase, bDeep);
				}
				else
				{
					strFilePath.erase();
					strFilePath = name;
					if (!dir.empty()) strFilePath += "\\";
					strFilePath += FileInformation.cFileName;

					// Check extension
					strExtension = FileInformation.cFileName;
					strExtension = strExtension.substr(strExtension.rfind(".") + 1);

					if (!strcmp(strExtension.c_str(), "cfg"))
					{
						OBJHANDLE hB = ParseBase(hPlanet, strFilePath.c_str(), hBase);
						if (hBase==hB && hBase!=NULL) return true;
					}
				}
			}
		} while (FindNextFileA(hFile, &FileInformation) == TRUE);

		FindClose(hFile);
	}
	return false;
}
