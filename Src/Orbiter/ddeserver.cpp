// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DDE server class
// Dynamic data exchange (DDE) orbiter server implementation
// =======================================================================

#include "ddeserver.h"
#include "Psys.h"

extern char DBG_MSG[256];
extern PlanetarySystem *g_psys;
extern TimeData td;

DDEServer::DDEServer (HWND hserver)
{
	hServer = hserver;
	nclient = 0;
	aApp   = GlobalAddAtom ("orbiter");
	aTopic = GlobalAddAtom ("data");

	nitem = 1;
	aItem = new ATOM[nitem];
	aItem[0] = GlobalAddAtom ("get_vesselcount");
}

DDEServer::~DDEServer ()
{
	int i;

	if (nclient) delete []client;
	GlobalDeleteAtom (aApp);
	GlobalDeleteAtom (aTopic);

	for (i = 0; i < nitem; i++)
		GlobalDeleteAtom (aItem[i]);
	delete []aItem;
}

void DDEServer::MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_DDE_INITIATE:
		DDEInit (hWnd, (HWND)wParam, (ATOM)LOWORD(lParam), (ATOM)HIWORD(lParam));
		break;
	case WM_DDE_REQUEST:
		DDERequest (hWnd, (HWND)wParam, LOWORD(lParam), (ATOM)HIWORD(lParam));
		break;
	case WM_DDE_ACK:
		break;
	}
}

bool DDEServer::DDEInit (HWND hServer, HWND hClient, ATOM app, ATOM topic)
{
	char cbuf[256];

	if (aApp) {
		GlobalGetAtomName (app, cbuf, 256);
		if (_stricmp (cbuf, "orbiter")) return false;
	}
	if (aTopic) {
		GlobalGetAtomName (topic, cbuf, 256);
		if (_stricmp (cbuf, "data")) return false;
	}
	if (!AddClient (hClient)) return false;

	SendMessage (hClient, WM_DDE_ACK, (WPARAM)hServer, MAKELPARAM (aApp, aTopic));
	return true;
}

bool DDEServer::DDERequest (HWND hServer, HWND hClient, WORD fmt, ATOM item)
{
	int i;
	char cbuf[256];

	// 1. check for static request strings
	for (i = 0; i < nitem; i++)
		if (item == aItem[i]) break;
	switch (i) {
	case 0:
		get_vesselcount(hClient, item);
		return true;
	}

	// 2. check for dynamic request strings
	GlobalGetAtomName (item, cbuf, 256);
	if (!strncmp (cbuf, "get_vesselid", 12)) {
		int idx;
		sscanf(cbuf+13, "%d", &idx);
		idx--; // switch from 1-based to 0-based
		get_vesselid (hClient, item, idx);
		return true;
	} else if (!strncmp (cbuf, "get_objid", 9)) {
		if (get_objid (hClient, item, cbuf+10))
			return true;
	} else if (!strncmp (cbuf, "get_name", 8)) {
		int id;
		sscanf(cbuf+9, "%d", &id);
		if (get_name (hClient, item, id))
			return true;
	} else if (!strncmp (cbuf, "get_size", 8)) {
		int id;
		sscanf (cbuf+9, "%d", &id);
		if (get_size (hClient, item, id))
			return true;
	} else if (!strncmp (cbuf, "get_state", 9)) {
		if (get_state (hClient, item, cbuf+10))
			return true;
	}

	DDEACK ddeack = {-1, 0, 0, 0};
	SendMessage (hClient, WM_DDE_ACK, (WPARAM)hServer, PackDDElParam(WM_DDE_ACK, *(UINT*)&ddeack, item));
	return false;
}

bool DDEServer::AddClient (HWND hclient)
{
	int i;

	// check if already present
	for (i = 0; i < nclient; i++)
		if (client[i].hwnd == hclient) return false;

	ClientSpec *tmp = new ClientSpec[nclient+1];
	if (nclient) {
		memcpy (tmp, client, nclient*sizeof(ClientSpec));
		delete []client;
	}
	client = tmp;
	client[nclient].hwnd = hclient;
	nclient++;

	return true;
}

DDEDATA *DDEServer::AllocDatablock (DWORD bufsize)
{
	DDEDATA *ddedata = (DDEDATA*)GlobalAlloc (0, sizeof(DDEDATA)+bufsize);
	memset (ddedata, 0, sizeof(DDEDATA)+bufsize);
	ddedata->fResponse = TRUE;
	ddedata->fRelease = TRUE;
	ddedata->cfFormat = CF_TEXT;
	return ddedata;
}

bool DDEServer::isVessel (Vessel *v)
{
	// check if v is a valid vessel pointer
	if (!g_psys) return false;
	DWORD i, nvessel = g_psys->nVessel();
	for (i = 0; i < nvessel; i++)
		if (g_psys->GetVessel(i) == v)
			return true;
	return false;
}

bool DDEServer::isBody (Body *b)
{
	// check if v is a valid vessel pointer
	if (!g_psys) return false;
	int i, nobj = g_psys->nObj();
	for (i = 0; i < nobj; i++)
		if (g_psys->GetObj(i) == b)
			return true;
	return false;
}

void DDEServer::get_vesselcount (HWND hClient, ATOM item)
{
	DWORD nvessel = (g_psys ? g_psys->nVessel() : 0);
	DDEDATA *ddedata = AllocDatablock (256);
	char *cbuf = (char*)ddedata->Value;
	sprintf (cbuf, "%d", nvessel);
	PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
}

void DDEServer::get_vesselid (HWND hClient, ATOM item, DWORD idx)
{
	Vessel *id;
	DWORD nvessel = (g_psys ? g_psys->nVessel() : 0);
	if (idx >= 0 && idx < nvessel) {
		id = g_psys->GetVessel(idx);
	} else {
		id = 0;
	}
	DDEDATA *ddedata = AllocDatablock (32);
	char *cbuf = (char*)ddedata->Value;
	sprintf (cbuf, "%d", id);
	PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
}

bool DDEServer::get_objid (HWND hClient, ATOM item, char *fmtstr)
{
	if (!g_psys) return false;
	
	Body *id = 0;
	int idx;
	DWORD n;
	// check for index
	n = (DWORD)sscanf (fmtstr, "%d", &idx);
	if (n) {
		DWORD nobj = g_psys->nObj();
		if (n < nobj)
			id = g_psys->GetObj(n);
	} else { // check for name
		char name[256];
		n = (DWORD)sscanf (fmtstr, "%s", name);
		if (n)
			id = g_psys->GetObj (name, true);
	}
	if (id) {
		DDEDATA *ddedata = AllocDatablock(32);
		char *cbuf = (char*)ddedata->Value;
		sprintf (cbuf, "%d", id);
		PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
		return true;
	} else {
		return false;
	}
}

bool DDEServer::get_name (HWND hClient, ATOM item, LONG_PTR id)
{
	Body *body = (Body*)id;
	// For security, make sure that the pointer is valid
	if (!isBody(body)) return false;

	char *name = body->Name();
	DDEDATA *ddedata = AllocDatablock(strlen(name)+1);
	char *cbuf = (char*)ddedata->Value;
	strcpy (cbuf, name);
	PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
	return true;
}

bool DDEServer::get_size (HWND hClient, ATOM item, LONG_PTR id)
{
	Body *body = (Body*)id;
	// For security, make sure that the pointer is valid
	if (!isBody(body)) return false;

	double size = body->Size();
	DDEDATA *ddedata = AllocDatablock(64);
	char *cbuf = (char*)ddedata->Value;
	sprintf (cbuf, "%0.8e", size);
	PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
	return true;
}

bool DDEServer::get_state (HWND hClient, ATOM item, char *fmtstr)
{
	LONG_PTR n,id;
	bool mapequ = false;
	char *c;
	c = strtok (fmtstr,",");
	n = sscanf(c, "%d", &id);
	if (!n) return false;

	// Currently, this function is only defined for vessels
	Vessel *v = (Vessel*)id;
	if (!isVessel(v)) return false;

	Body *ref = 0;
	c = strtok(NULL,",");
	if (c && _stricmp(c,"global")) {
		ref = g_psys->GetGravObj(c,true);
		if (!ref) return false;
	}

	c = strtok(NULL,",");
	if (c && !_stricmp(c,"equ"))
		mapequ = true;

	strcpy(DBG_MSG, mapequ ? "equ":"no equ");

	DDEDATA *ddedata = AllocDatablock(1024);
	char *cbuf = (char*)ddedata->Value;
	double t = td.SimT0;
	const Vector &pos = v->GPos();
	if (ref) {
		if (mapequ) {
			double lng, lat, rad;
			ref->GlobalToEquatorial(pos, lng, lat, rad);
			sprintf (cbuf, "%g\n%0.8e\n%0.8e\n%0.8e\n", t, lng, lat, rad);
		} else {
			const Vector &pref = ref->GPos();
			sprintf (cbuf, "%g\n%0.8e\n%0.8e\n%0.8e\n", t, pos.x-pref.x, pos.y-pref.y, pos.z-pref.z);
		}
	} else {
		sprintf (cbuf, "%g\n%0.8e\n%0.8e\n%0.8e\n", t, pos.x, pos.y, pos.z);
	}
	PostMessage (hClient, WM_DDE_DATA, (WPARAM)hServer, PackDDElParam(WM_DDE_DATA,(UINT_PTR)ddedata,item));
	return true;
}
