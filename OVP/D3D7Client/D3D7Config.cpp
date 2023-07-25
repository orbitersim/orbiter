// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// D3D7Config.cpp
// Management of configuration parameters for the D3D7 client.
// ==============================================================

#include "D3D7Config.h"
#include "orbitersdk.h"
#include <algorithm>
using std::min;
using std::max;

static const char *cfgfile = "D3D7Client.cfg";

// ==============================================================
// default values

int D3D7Config::def_PlanetPreloadMode = 0;     // don't preload hires tiles
int D3D7Config::def_PlanetLoadFrequency = 20;  // On-demand texture load frequency [Hz]
int D3D7Config::def_PlanetMipmapMode = 2;      // interpolated mipmaps
int D3D7Config::def_PlanetAnisoMode = 1;       // no anisotropic filtering
int D3D7Config::def_PlanetTileLoadFlags = 0x0003; // load from cache and archive
double D3D7Config::def_PlanetMipmapBias = 0.0; // Mipmap LOD bias (no bias)

// ==============================================================

D3D7Config::D3D7Config ()
{
	Reset ();
	ReadParams ();
}

D3D7Config::~D3D7Config ()
{
	WriteParams ();
}

void D3D7Config::Reset ()
{
	PlanetPreloadMode   = def_PlanetPreloadMode;
	PlanetLoadFrequency = def_PlanetLoadFrequency;
	PlanetMipmapMode    = def_PlanetMipmapMode;
	PlanetAnisoMode     = def_PlanetAnisoMode;
	PlanetTileLoadFlags = def_PlanetTileLoadFlags;
	PlanetMipmapBias    = def_PlanetMipmapBias;
}

bool D3D7Config::ReadParams ()
{
	int i;
	double d;

	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_IN, ROOT);
	if (!hFile) return false;
	if (oapiReadItem_int (hFile, (char*)"PlanetPreloadMode", i))
		PlanetPreloadMode = max (0, min (1, i));
	if (oapiReadItem_int (hFile, (char*)"PlanetTexLoadFreq", i))
		PlanetLoadFrequency = max (1, min (1000, i));
	if (oapiReadItem_int (hFile, (char*)"PlanetMipmapMode", i))
		PlanetMipmapMode = max (0, min (2, i));
	if (oapiReadItem_int (hFile, (char*)"PlanetAnisoMode", i))
		PlanetAnisoMode = max (1, min (16, i));
	if (oapiReadItem_int (hFile, (char*)"PlanetTileLoadFlags", i))
		PlanetTileLoadFlags = max(1, min (3, i));
	if (oapiReadItem_float (hFile, (char*)"PlanetMipmapBias", d))
		PlanetMipmapBias = max (-1.0, min (1.0, d));
	oapiCloseFile (hFile, FILE_IN);
	return true;
}

void D3D7Config::WriteParams ()
{
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, ROOT);
	oapiWriteItem_int (hFile, (char*)"PlanetPreloadMode", PlanetPreloadMode);
	oapiWriteItem_int (hFile, (char*)"PlanetTexLoadFreq", PlanetLoadFrequency);
	oapiWriteItem_int (hFile, (char*)"PlanetAnisoMode", PlanetAnisoMode);
	oapiWriteItem_int (hFile, (char*)"PlanetMipmapMode", PlanetMipmapMode);
	oapiWriteItem_int (hFile, (char*)"PlanetTileLoadFlags", PlanetTileLoadFlags);
	oapiWriteItem_float (hFile, (char*)"PlanetMipmapBias", PlanetMipmapBias);
	oapiCloseFile (hFile, FILE_OUT);
}
