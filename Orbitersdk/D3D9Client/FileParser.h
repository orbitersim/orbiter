// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ==============================================================

#ifndef __FILEPARSER_H
#define __FILEPARSER_H

#include "OrbiterAPI.h"
#include "D3D9Util.h"

typedef struct {
	OBJHANDLE hObj;
	VECTOR3 Albedo;
	char *file;
} ObjEntry;

/**
 * \brief Configuration file parser for D3D9Client
 *
 */

class FileParser
{

public:

	/**
	 * \brief Create a BeaconArray object for rendering multiple beacons at the same time
	 * \param pArray Pointer into a BeaconArrayEntry list
	 * \param nArray Number of entries in the array
	 */
	FileParser(const char *scenario);
	~FileParser();
	
	const char * GetConfigFile(OBJHANDLE hObj);
	VECTOR3 GetAlbedo(OBJHANDLE hObj);
	void LogContent();
	bool HasMissingObjects();
	bool DoesExists(OBJHANDLE hObj);

private:

	bool ParseScenario(const char *file);
	bool ParseSystem(const char *file);
	bool ParsePlanet(const char *file);
	OBJHANDLE ParseBase(OBJHANDLE hPlanet, const char *file, OBJHANDLE hBase=NULL);
	bool ScanBases(OBJHANDLE hPlanet, const char *dir, OBJHANDLE hBase=NULL, bool bDeep=false);

	bool bContext;
	double mjd;
	DWORD eidx;
	char system[64];
	char context[64];
	ObjEntry *entry;
};

#endif // !__FILEPARSER_H