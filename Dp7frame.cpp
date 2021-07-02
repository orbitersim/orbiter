// ====================================================================================
// File: Dp7frame.cpp
// Desc: Class to manage the DirectPlay environment objects
// ====================================================================================

#include "Dp7frame.h"
#include "D3d7util.h"
#include "Log.h"

CDPFramework7::CDPFramework7 ()
{
	m_hDPMessageEvent = NULL;
	m_pDP = NULL;
	localDPID = 0;
	server = false;
	dpfirst = NULL;
	dplast = NULL;
}

CDPFramework7::~CDPFramework7 ()
{
	Destroy ();
}

HRESULT CDPFramework7::Create (HINSTANCE hInst)
{
	HRESULT hr;

	m_hDPMessageEvent = CreateEvent (NULL, FALSE, FALSE, NULL);

	if (FAILED (hr = CoInitialize (NULL))) {
		LOGOUT_DDERR (hr);
		return hr;
	}

	// create an IDirectPlay object
	if (FAILED (hr = CoCreateInstance (CLSID_DirectPlay, NULL, CLSCTX_ALL, 
		IID_IDirectPlay4A, (VOID**)&m_pDP))) {
		LOGOUT_DDERR (hr);
		return hr;
	}

	LOGOUT("Created DirectPlay object");
	return S_OK;
}

VOID CDPFramework7::Destroy ()
{
	if (m_pDP) {
		SAFE_RELEASE (m_pDP);
	}
	CoUninitialize ();
	CloseHandle (m_hDPMessageEvent);
}

HRESULT CDPFramework7::Restart ()
{
	HRESULT hr;
	SAFE_RELEASE (m_pDP);
    if (FAILED (hr = CoCreateInstance (CLSID_DirectPlay, NULL, CLSCTX_ALL, 
		IID_IDirectPlay4A, (VOID**)&m_pDP))) {
		LOGOUT_ERR("CoCreateInstance failed");
        return hr;
	}
	return S_OK;
}

HRESULT CDPFramework7::CreateLocalPlayer (DPNAME *dpname, bool isServer)
{
	HRESULT hr;
	if (FAILED (hr = m_pDP->CreatePlayer (&localDPID, dpname, m_hDPMessageEvent, NULL, 0, 0))) {
		LOGOUT_DPERR (hr);
		return hr;
	}
	server = isServer;
	return S_OK;
}

HRESULT CDPFramework7::EnumPlayers (LPDPENUMPLAYERSCALLBACK2 lpEnumPlayersCallback2)
{
	return m_pDP->EnumPlayers (NULL, lpEnumPlayersCallback2, (LPVOID)this, DPENUMPLAYERS_ALL);
}

HRESULT CDPFramework7::GetSessionDesc (LPDPSESSIONDESC2 *pdesc)
{
	static DWORD size = sizeof (DPSESSIONDESC2);
	static BYTE *data = new BYTE[size];
	HRESULT hr;

	hr = m_pDP->GetSessionDesc ((LPVOID)data, &size);
	if (hr == DPERR_BUFFERTOOSMALL) {
		delete []data;
		data = new BYTE[size]; TRACENEW
		hr = m_pDP->GetSessionDesc ((LPVOID)data, &size);
	}
	*pdesc = (LPDPSESSIONDESC2)data;
	return hr;
}

BOOL FAR PASCAL RefreshPListClbk (DPID id, DWORD ptype, LPCDPNAME name, DWORD flags, LPVOID context)
{
	CDPFramework7 *dplay = (CDPFramework7*)context;
	dplay->AddPlayer (id, name);
	return TRUE;
}

void CDPFramework7::RefreshPlayerList ()
{
	ClearPlayerList();
	EnumPlayers (RefreshPListClbk);
}

void CDPFramework7::AddPlayer (DPID dpid, const DPNAME *dpname)
{
	DPlayer *player = new DPlayer; TRACENEW
	memset (player->name, 0, 256);
	memset (player->call, 0, 256);
	player->id = dpid;
	strncpy (player->name, dpname->lpszLongNameA, 255);
	strncpy (player->call, dpname->lpszShortNameA, 255);
	player->prev = dplast;
	player->next = NULL;
	if (dplast) dplast->next = player;
	dplast = player;
	if (!dpfirst) dpfirst = player;
}

void CDPFramework7::DelPlayer (DPID dpid)
{
	DPlayer *player = dpfirst;
	while (player && player->id != dpid) player = player->next;
	if (player) { // found it
		if (player->prev) player->prev->next = player->next;
		if (player->next) player->next->prev = player->prev;
		if (player == dpfirst) dpfirst = player->next;
		if (player == dplast)  dplast  = player->prev;
		delete player;
	}
}

void CDPFramework7::ClearPlayerList ()
{
	DPlayer *player = dpfirst;
	while (player) {
		DPlayer *tmp = player;
		player = player->next;
		delete tmp;
	}
	dpfirst = NULL;
	dplast = NULL;
}

void CDPFramework7::SetSysMsgHandler (void (*ProcessSysMsg)(DPMSG_GENERIC*, DWORD, DPID, DPID, LPVOID), LPVOID context)
{
	SysMsgHandler = ProcessSysMsg;
	SysMsgContext = context;
}

void CDPFramework7::SetAppMsgHandler (void (*ProcessAppMsg)(LPVOID, DWORD, DPID, DPID, LPVOID), LPVOID context)
{
	AppMsgHandler = ProcessAppMsg;
	AppMsgContext = context;
}

HRESULT CDPFramework7::Receive ()
{
	HRESULT hr;
	DPID idFrom, idTo;
	static DWORD size = 64;
	static char *data = new char[size];
	static DpMsg *msg = (DpMsg*)data;

	while (true) {
		idFrom = 0;
		idTo   = 0;
		hr = m_pDP->Receive (&idFrom, &idTo, DPRECEIVE_ALL, (LPVOID)data, &size);
		if (hr == DPERR_BUFFERTOOSMALL) {
			delete []data;
			data = new char[size]; TRACENEW
			msg = (DpMsg*)data;
		} else if (SUCCEEDED (hr)) {
			if (idFrom == DPID_SYSMSG) {
				if (size >= sizeof (DPMSG_GENERIC)) {
					SysMsgHandler ((DPMSG_GENERIC*)data, size, idFrom, idTo, SysMsgContext);
				}
			} else {
				if (msg->id == MPMSG_CONFIRM) { // filter handshakes
					if (server) nClientsConfirmed++;
					else        bServerConfirmed = true;
				} else
					AppMsgHandler (data, size, idFrom, idTo, AppMsgContext);
			}
		} else break;
	}
	return hr;
}

HRESULT CDPFramework7::Broadcast (LPVOID data, DWORD size)
{
	return m_pDP->SendEx (localDPID, DPID_ALLPLAYERS, 0/*DPSEND_ASYNC | DPSEND_GUARANTEED*/,
						  data, size, 0, 0, NULL, NULL);
}

void CDPFramework7::ResetHandshake()
{
	if (server) nClientsConfirmed = 0;
	else        bServerConfirmed = false;
}

void CDPFramework7::Confirm()
{
	if (server) return;
	HRESULT hr;
	static DpMsg msg = {MPMSG_CONFIRM, {0}};
	if (FAILED (hr = m_pDP->SendEx (localDPID, DPID_ALLPLAYERS, 0, &msg, sizeof(DWORD), 0, 0, NULL, NULL)))
		LOGOUT_DPERR(hr);
}

HRESULT CDPFramework7::WaitForAll ()
{
	if (!server) return DPERR_INVALIDPLAYER;
	DWORD msg = MPMSG_CONFIRM;
	LPDPSESSIONDESC2 pdesc;
	GetSessionDesc (&pdesc);
	DWORD nClients = pdesc->dwCurrentPlayers-1;
	while (nClientsConfirmed < nClients)
		Receive();
	// now we have received confirmations from all clients
	// so we broadcast an acknowledgement
	Broadcast (&msg, sizeof (DWORD));
	return S_OK;
}

HRESULT CDPFramework7::WaitForServer ()
{
	if (server) return DPERR_INVALIDPLAYER;
	while (!bServerConfirmed) Receive();
	return S_OK;
}

void CDPFramework7::BroadcastPlayerList ()
{
	if (!server) return;
#ifdef UNDEF
	m_pDP->SendEx (localDPID, DPID_ALLPLAYERS, DPSEND_ASYNC | DPSEND_GUARANTEED,
		(void*)dpfirst, 
#endif
}
