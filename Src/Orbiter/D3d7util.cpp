// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT
#include "D3d7util.h"
#include "Log.h"

// =======================================================================

VERTEX_XYZ *GetVertexXYZ (DWORD n)
{
	static DWORD nv = 1;
	static VERTEX_XYZ *v = new VERTEX_XYZ[1];
	if (n > nv) {
		delete []v;
		v = new VERTEX_XYZ[nv = n]; TRACENEW
	}
	return v;
}

VERTEX_XYZC *GetVertexXYZC (DWORD n)
{
	static DWORD nv = 1;
	static VERTEX_XYZC *v = new VERTEX_XYZC[1];
	if (n > nv) {
		delete []v;
		v = new VERTEX_XYZC[nv = n]; TRACENEW
	}
	return v;
}
