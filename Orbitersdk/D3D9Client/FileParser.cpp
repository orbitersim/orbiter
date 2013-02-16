// ==============================================================
// FileParser.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2012 Jarmo Nikkanen
//				 2012 Émile "Bibi Uncle" Grégoire
//				 2012 Peter Schneider (Kuddel)
// ==============================================================

#include "FileParser.h"
#include "D3D9Config.h"
#include "D3D9Client.h"
#include "Log.h"
#include "OapiExtension.h"

#include <vector>
#include <iostream>

#include <functional> 
#include <algorithm>

using namespace std;


// ===========================================================================================
//
FileParser::FileParser(const char *scenario)
{
	_TRACE;
	bContext = false;
	eidx = 0;
	mjd = 51981.0;
	strcpy(context," ");
	entry = new ObjEntry[4096];

	for (int i=0;i<4096;i++) {
		entry[i].hObj = NULL;
		entry[i].file = NULL;
		entry[i].Albedo = _V(1,1,1);
	}

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
					if (!DoesExists(hBs)) {
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
FileParser::~FileParser()
{
	for (DWORD i=0;i<eidx;i++) if (entry[i].file) delete[] entry[i].file;
	delete[] entry;
}

// ===========================================================================================
//
const char *FileParser::GetConfigFile(OBJHANDLE hObj)
{
	for (DWORD i=0;i<eidx;i++) if (entry[i].hObj==hObj) return entry[i].file;
	return NULL;
}

// ===========================================================================================
//
VECTOR3 FileParser::GetAlbedo(OBJHANDLE hObj)
{
	for (DWORD i=0;i<eidx;i++) if (entry[i].hObj==hObj) return entry[i].Albedo;
	return _V(1,1,1);
}

bool FileParser::DoesExists(OBJHANDLE hObj)
{
	for (DWORD i=0;i<eidx;i++) if (entry[i].hObj==hObj) return true;
	return false;
}

// ===========================================================================================
//
bool FileParser::HasMissingObjects()
{
	DWORD npl = oapiGetGbodyCount();
	for (DWORD p=0;p<npl;p++) {
		OBJHANDLE hPl = oapiGetGbodyByIndex(p);
		if (hPl && oapiGetObjectType(hPl) == OBJTP_PLANET) {
			if (!DoesExists(hPl)) return true;
			else {
				DWORD nbs = oapiGetBaseCount(hPl);
				for (DWORD b=0;b<nbs;b++) {
					OBJHANDLE hBs = oapiGetBaseByIndex(hPl, b);
					if (!DoesExists(hBs)) return true;
				}
			}
		}
	}
	return false;
}

// ===========================================================================================
//
void FileParser::LogContent()
{
	char buf[256];

	DWORD npl = oapiGetGbodyCount();

	for (DWORD p=0;p<npl;p++) {
		OBJHANDLE hPl = oapiGetGbodyByIndex(p);
		if (hPl && oapiGetObjectType(hPl) == OBJTP_PLANET) {
			if (!DoesExists(hPl)) {
				oapiGetObjectName(hPl, buf, 256);
				LogErr("Planet 0x%X = '%s' not cataloged", hPl, buf);
			}
			else {
				DWORD nbs = oapiGetBaseCount(hPl);
				for (DWORD b=0;b<nbs;b++) {
					OBJHANDLE hBs = oapiGetBaseByIndex(hPl, b);
					if (!DoesExists(hBs)) {
						oapiGetObjectName(hBs, buf, 256);
						LogErr("Base Object 0x%X = '%s' not cataloged", hBs, buf);
					}
				}
			}
		}
	}
}



// ===========================================================================================
//
bool FileParser::ParseScenario(const char *name)
{
	FILE* file = NULL;
	char cbuf[256];

	fopen_s(&file, name, "r");

	if (file==NULL) {
		LogErr("Could not open a scenario '%s'", name);
		return false;
	}

	bool bSystem = false;

	while(fgets2(cbuf, 256, file)>=0) {
		if(!strncmp(cbuf, "BEGIN_ENVIRONMENT", 17)) {
			while(fgets2(cbuf, 256, file)>=0) {

				if(!_strnicmp(cbuf, "System", 6))	{
					strcpy_s(system, 64, cbuf+7);
					bSystem = true;
					LogAlw("Scenario System=%s",system);
				}

				if(!_strnicmp(cbuf, "CONTEXT", 7))	{
					strcpy_s(context, 64, cbuf+8);
					bContext = true;
					LogAlw("Scenario Context=%s",context);
				}

				if(!_strnicmp(cbuf, "Date MJD", 8))	{
					sscanf(cbuf, "Date MJD %lf", &mjd);
					LogAlw("Scenario MJD=%.12f",mjd);
				}

				if(!_strnicmp(cbuf, "Date JD", 7))	{
					sscanf(cbuf, "Date JD %lf", &mjd);
					mjd-=2400000.5;
					LogAlw("Scenario MJD=%.12f",mjd);
				}

				if(!_strnicmp(cbuf, "Date JE", 7))	{
					LogErr("UnImplemented parameter 'Date JE' in scenario '%s',",name);
				}

				if(!strncmp(cbuf, "END_ENVIRONMENT",15)) {
					fclose(file);
					if (bSystem) {
						ParseSystem(system);
						return true;
					}
					LogErr("Failed to found a system from scenario '%s'",name);
					return false;
				}
			}
		}
	}

	fclose(file);
	LogErr("Failed to parse a scenario '%s'",name);			
	return false;
}

// ===========================================================================================
//
bool FileParser::ParseSystem(const char *_name)
{
	FILE* file = NULL;
	char cbuf[256];
	char name[256];
	sprintf_s(name, 256,"%s%s.cfg", OapiExtension::GetConfigDir(), _name);

	fopen_s(&file, name, "r");

	if(file==NULL) {
		LogErr("Could not open a solar system file '%s'", name);
		return false;
	}

	while (true) {
		int rv = fgets2(cbuf, 256, file);
		if (rv<0) break; if (rv!=2) continue;
		if (!_strnicmp(cbuf, "Name", 4)) continue;
		ParsePlanet(strchr(cbuf,'=')+1);
	}

	fclose(file);
	return true;
}

// ===========================================================================================
//
bool FileParser::ParsePlanet(const char *_name)
{
	__TRY {

		FILE* file = NULL;
		char cbuf[256];
		char name[256];
		char path[256];
		char def[256];
		bool bDefault = false;
		bool bHasPath = false;

		sprintf_s(path, 256, "%s%s.cfg", OapiExtension::GetConfigDir(), _name);
		sprintf_s(name, 256, "%s.cfg",_name);
		sprintf_s(def,  256, "%s\\Base",_name);

		fopen_s(&file, path, "r");

		if (file==NULL) {
			LogErr("Could not open a planet configuration file '%s'", name);
			return false;
		}

		OBJHANDLE hPlanet = NULL;
		DWORD pidx = 0;

		while (true) {

			int rv = fgets2(cbuf, 256, file);
			if (rv<0) break; if (rv==0) continue;

			if(!_strnicmp(cbuf, "Name=", 5))	{
				hPlanet = oapiGetObjectByName(cbuf+5);
				if (hPlanet) {
					if (!DoesExists(hPlanet)) {
						pidx = eidx;
						entry[eidx].hObj = hPlanet;
						entry[eidx].file = new char[strlen(name)+1];
						strcpy(entry[eidx].file, name);
						LogAlw("Planet Added: 0x%X, %s",hPlanet,name);
						eidx++;
					}
					else {
						hPlanet=NULL; // Already exists, ignore this file
						break;
					}
				}
				else LogErr("Planet Not Found '%s'", cbuf+5);
			}

			if (hPlanet==NULL) continue;

			if(!_strnicmp(cbuf, "AlbedoRGB=", 10)) {
				VECTOR3 vec;
				sscanf(cbuf+10, "%lf %lf %lf", &vec.x, &vec.y, &vec.z);
				entry[pidx].Albedo = vec;
			}

			if(!strncmp(cbuf, "BEGIN_SURFBASE", 14)) {
				
				char dir[64], buf0[64], buf1[64];
			
				while (true) {

					bool bCtx = true;
					bool bPer = true;
					bool bPth = false;

					double mjd0 = 0.0;
					double mjd1 = 1e6;

					int rv = fgets2(cbuf, 256, file);
					if (rv<0) break; if (rv==0) continue;

					if(!strncmp(cbuf, "END_SURFBASE", 12))	break;

					char *d = strstr(cbuf, "DIR");
					if (d) {
						bPth = true;
						sscanf_s(d, "DIR %s", dir, 64);
					}

					d = strstr(cbuf, "CONTEXT");
					if (d) {
						sscanf_s(d, "CONTEXT %s", buf0, 64);
						if (strcmp(buf0,context)) bCtx = false;
					}
				
					d = strstr(cbuf, "PERIOD");
					if (d) {
						sscanf_s(d, "PERIOD %s %s", buf0, 64, buf1, 64);
						if (buf0[0]!='-') sscanf(buf0,"%lf",&mjd0);
						if (buf1[0]!='-') sscanf(buf1,"%lf",&mjd1);
						if (mjd0>mjd || mjd1<mjd) bPer = false;
					}	

					if (bPth && bCtx && bPer) {
						bHasPath = true;
						ScanBases(hPlanet, dir);
					}
				}	
			}
		}

		//Is the default path already searched ?
		if (!bHasPath && hPlanet) ScanBases(hPlanet, def);

		fclose(file);
	}
	__EXCEPT(ExcHandler(GetExceptionInformation()))
	{
		LogErr("FileParser::ParsePlanet(%s)",_name);
		FatalAppExitA(0,"Critical error has occured. See Orbiter.log for details");
	}

	return true;
}

// ===========================================================================================
//
OBJHANDLE FileParser::ParseBase(OBJHANDLE hPlanet, const char *name, OBJHANDLE hBase)
{
	FILE* file = NULL;
	char cbuf[256];
	char base[256];
	bool bBase = false;

	fopen_s(&file, name, "r");

	if (file==NULL) {
		LogErr("Could not open a base configuration file '%s'", name);
		return NULL;
	}

	while (true) {

		int rv = fgets2(cbuf, 256, file);
		if (rv<0) break; if (rv==0) continue;

		if (strncmp(cbuf, "BASE", 4)==0) bBase = true;

		if (!_strnicmp(cbuf, "Name=", 5)) {
			
			if (!bBase) break;

			if (hBase) {
				oapiGetObjectName(hBase, base, 256);
				if (strcmp(cbuf+5, base)==0) {
					if (!DoesExists(hBase)) {
						entry[eidx].hObj = hBase;
						entry[eidx].file = new char[strlen(name)+1];
						strcpy(entry[eidx].file, name);
						eidx++;
						LogAlw("Base Added: 0x%X, %s",hBase,name);
						fclose(file);
						return hBase;
					}
				}
			}
			else {
				OBJHANDLE hBs = oapiGetBaseByName(hPlanet, cbuf+5);
				if (hBs) {
					if (!DoesExists(hBs)) {
						entry[eidx].hObj = hBs;
						entry[eidx].file = new char[strlen(name)+1];
						strcpy(entry[eidx].file, name);
						eidx++;
						LogAlw("Base Added: 0x%X, %s",hBs,name);
						fclose(file);
						return hBs;
					}
				}
			}
			break;
		}
	}
	fclose(file);
	return NULL;
}


bool FileParser::ScanBases(OBJHANDLE hPlanet, const char *_name, OBJHANDLE hBase, bool bDeep)
{
	_TRACE;
	char name[256];

	sprintf_s(name, 256, "%s%s", OapiExtension::GetConfigDir(), _name);

	if (hPlanet==NULL) {
		LogErr("hPlanet is NULL in FileParser::ScanBases(%s)", name);
		return false;
	}

	std::string     strFilePath;             // Filepath
	std::string     strPattern;              // Pattern
	std::string     strExtension;            // Extension
	HANDLE          hFile;                   // Handle to file
	WIN32_FIND_DATA FileInformation;         // File information

	strPattern.erase();
	strPattern = name;

	if (_name[0]!='\0')	strPattern += "\\*.*";
	else				strPattern += "*.*";

	hFile = FindFirstFile(strPattern.c_str(), &FileInformation);

	if(hFile != INVALID_HANDLE_VALUE)
	{
		do
		{
			if(FileInformation.cFileName[0] != '.')
			{
				if (FileInformation.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
				{
					strFilePath.erase();
					strFilePath = _name;
					strFilePath += FileInformation.cFileName;
					// Search the sub-directory only in a special cases
					if (bDeep) ScanBases(hPlanet, strFilePath.c_str(), hBase, bDeep);
				}
				else
				{
					strFilePath.erase();
					strFilePath = name;
					if (_name[0]!='\0') strFilePath += "\\";
					strFilePath += FileInformation.cFileName;

					// Check extension
					strExtension = FileInformation.cFileName;
					strExtension = strExtension.substr(strExtension.rfind(".") + 1);

					if(!strcmp(strExtension.c_str(), "cfg"))
					{
						OBJHANDLE hB = ParseBase(hPlanet, strFilePath.c_str(), hBase);
						if (hBase==hB && hBase!=NULL) return true;
					}
				}
			}
		} while(FindNextFileA(hFile, &FileInformation) == TRUE);

		FindClose(hFile);
	}
	return false;
}