// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2012 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation 
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, 
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software 
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================

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