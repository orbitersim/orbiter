// ===========================================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2014 Jarmo Nikkanen
// ===========================================================================================

#include "D3D9Catalog.h"
#include "D3D9Util.h"
#include "AABBUtil.h"
#include "string.h"
#include "Log.h"

D3D9Catalog::D3D9Catalog(const char *n)
{
	count = 0;
	nmax  = 0x2000;
	data  = new DWORD[nmax];
	strcpy_s(name,16,n);
	for (DWORD i=0;i<nmax;i++) data[i]=0;
}

D3D9Catalog::~D3D9Catalog()
{
	delete []data;
}

void D3D9Catalog::Clear()
{
	for (DWORD i=0;i<nmax;i++) {
		if (data[i]!=0) LogErr("Catalog contains undeleted data 0x%X (%s)",data[i],name);
		data[i]=0;
	}
}

void D3D9Catalog::Add(DWORD d)
{
	if (d==0) {
		LogErr("Trying to catalog a NULL pointer.(%s)",name);
		return;
	}
	for (DWORD i=0;i<count;i++) if (data[i]==d) {
		LogErr("Data 0x%X already catalogged (%s)",d,name);
		return;
	}
	if (count==nmax) {
		nmax = count*2;
		DWORD *n = new DWORD[nmax];
		memset2(n, 0, nmax*sizeof(DWORD));
		memcpy2(n, data, count*sizeof(DWORD));
		delete []data;
		data=n;
	}
	data[count]=d;
	count++;
}

DWORD D3D9Catalog::CountEntries()
{
	return count;
}

DWORD D3D9Catalog::Seek(DWORD d)
{
	for (DWORD i=0;i<count;i++) if (data[i]==d) return i;
	return 0xFFFFFFFF;
}

bool D3D9Catalog::Remove(DWORD d)
{
	for (DWORD i=0;i<count;i++) {
		if (data[i]==d) {
			for (DWORD k=i;k<(count-1);k++) data[k]=data[k+1];
			count--;
			data[count] = 0;
			return true;
		}
	}	
	LogErr("Entry 0x%X wasn't in the catalog (%s)",d,name);
	return false;
}

DWORD D3D9Catalog::Get(DWORD index)
{
	if (index<count) return data[index];
	return 0;
}
