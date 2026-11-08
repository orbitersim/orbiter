#ifndef CMAP_H
#define CMAP_H

#include <cstdint>

enum CmapName {
	CMAP_GREY,
	CMAP_JET,
	CMAP_TOPO1,
	CMAP_TOPO2
};

typedef uint32_t Cmap[256];

const Cmap &cmap(CmapName name);

#endif // !CMAP_H
