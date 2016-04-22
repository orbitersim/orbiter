// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#ifndef __FILEPARSER_H
#define __FILEPARSER_H

#include <map>
#include "OrbiterAPI.h"
#include "D3D9Util.h"


/**
 * \brief Configuration file parser for D3D9Client
 *
 */
class FileParser
{
public:

	explicit FileParser(const std::string & scenario);
	        ~FileParser ();

	bool        DoesExist (OBJHANDLE hObj);
	const char *GetConfigFile (OBJHANDLE hObj);
	VECTOR3     GetAlbedo (OBJHANDLE hObj);
	void        LogContent ();
	bool        HasMissingObjects ();

private:
	typedef struct ObjEntry_t {
		VECTOR3 Albedo; ///< Albedo setting (only valid for plantes)
		char   *file;   ///< Configuration file path
		ObjEntry_t() : file(NULL), Albedo(_V(1, 1, 1)) {}
	} ObjEntry;

	bool      ParseScenario (const std::string &file);
	bool      ParseSystem (const std::string &_name);
	bool      ParsePlanet (const std::string &_name);
	OBJHANDLE ParseBase (OBJHANDLE hPlanet, const char *file, OBJHANDLE hBase=NULL);
	bool      ScanBases (OBJHANDLE hPlanet, const std::string &dir, OBJHANDLE hBase=NULL, bool bDeep=false);
	ObjEntry *GetEntry (OBJHANDLE hObj, bool create = false);

	double      mjd;                        ///< Scenario MJD
	std::string system;                     ///< Name of the planetary system (e.g. "Sol")
	std::string context;                    ///< Optional Scenario context
	std::map<OBJHANDLE, ObjEntry*> entries; ///< OBJHANDLE to ObjEntry mapping
};

#endif // !__FILEPARSER_H