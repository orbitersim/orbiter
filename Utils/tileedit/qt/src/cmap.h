#ifndef CMAP_H
#define CMAP_H

#ifndef __linux__
#include <windows.h>
#else // __linux__
#include "OrbiterPlatform.h" // windows.h left out: DWORD
#endif // __linux__

enum CmapName {
	CMAP_GREY,
	CMAP_JET,
	CMAP_TOPO1,
	CMAP_TOPO2
};

typedef DWORD Cmap[256];

const Cmap &cmap(CmapName name);

#endif // !CMAP_H
