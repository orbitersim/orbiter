// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CATALOG_H
#define __D3D9CATALOG_H

#include <windows.h>
#include <d3d9.h> 
#include <d3dx9.h>

class D3D9Catalog {
public:
	D3D9Catalog(const char *name);
	~D3D9Catalog();

	void  Add(DWORD data);
	void  Clear();
	DWORD Seek(DWORD data);
	DWORD Get(DWORD id);
	DWORD CountEntries();
	bool  Remove(DWORD data);
private:
	DWORD *data;
	DWORD nmax, count;
	CRITICAL_SECTION Crits;
	char  name[16];
};

#endif // !__D3D9EXTRA_H