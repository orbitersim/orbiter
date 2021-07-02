// ====================================================================================
// File: Dp7frame.h
// Desc: Class to manage the DirectPlay environment objects
// ====================================================================================

#ifndef DP7FRAME_H
#define DP7FRAME_H
#define STRICT 1
#include <dplay.h>

#define MPMSG_LAUNCH 1
#define MPMSG_CONFIRM 2
#define MPMSG_STARTSIM 3

struct DPSessionInfo {
	GUID guidSession;
	char szSession[256];
	DPSessionInfo *pDPSINext;
};

struct DPlayer {
	DPlayer *prev, *next;
	DPID id;        // player identifier
	char name[256]; // (long) player name
	char call[256]; // player callsign
};

#pragma pack(push,1)
struct DpMsg {
	DWORD id;
	BYTE data[1];
};
#pragma pack(pop)

class CDPFramework7
{
public:

	HANDLE m_hDPMessageEvent;
	LPDIRECTPLAY4 m_pDP;    // IDirectPlay4 object

public:
	CDPFramework7 ();
	~CDPFramework7 ();

	inline LPDIRECTPLAY4 GetDP() const { return m_pDP; }
	// return DirectPlay object

	inline HANDLE GetMsgEvent() const { return m_hDPMessageEvent; }

	inline DPID GetLocalDPID() const { return localDPID; }
	// local player identifier

	inline bool isServer() const { return server; }
	// returns true if we are session server

	HRESULT Create (HINSTANCE hInst);
	// Initialise the DirectPlay objects

	VOID Destroy ();
	// Destroy devices and DPlay object

	HRESULT Restart ();
	// re-creates the DirectPlay object

	inline DPlayer *FirstPlayer() { return dpfirst; }
	// returns the first player in the list

	HRESULT CreateLocalPlayer (DPNAME *dpname, bool isServer = false);
	// create a new local player for the current session

	HRESULT EnumPlayers (LPDPENUMPLAYERSCALLBACK2 lpEnumPlayersCallback2);
	// enumerate all players in current session

	HRESULT GetSessionDesc (LPDPSESSIONDESC2 *pdesc);
	// returns a pointer to current session description

	void RefreshPlayerList();
	// update the player list by enumerating all players in current session

	void AddPlayer (DPID dpid, const DPNAME *dpname);
	void DelPlayer (DPID dpid);
	void ClearPlayerList ();
	// add or remove a player from the player list

	void SetSysMsgHandler (void (*ProcessSysMsg)(DPMSG_GENERIC*, DWORD, DPID, DPID, LPVOID), LPVOID context);
	void SetAppMsgHandler (void (*ProcessAppMsg)(LPVOID, DWORD, DPID, DPID, LPVOID), LPVOID context);

	HRESULT Receive ();
	// read multiplayer messages from the message queue and pass them to the
	// provided functions for system and application messages

	HRESULT Broadcast (LPVOID data, DWORD size);

	void ResetHandshake();
	// This clears the confirmation flags for all clients on the server,
	// and the server acknowledgement flag on the clients

	HRESULT WaitForAll ();
	// This function should only be invoked by the server.
	// It waits until all clients have sent a confirmation message after
	// the last "ResetHandshake"

	HRESULT WaitForServer ();
	// This function should only be invoked by clients.
	// It waits until the server has sent a confirmation message

	void Confirm();
	// This sends a confirmation to the server to allow synchronisation with
	// "WaitForAll". Should only be invoked by clients

	void BroadcastPlayerList ();

private:
	DPID localDPID;  // id of local player
	bool server;     // true if we are game server
	DPlayer *dpfirst, *dplast;
	DWORD nClientsConfirmed; // confirmation count for synchronisation
	bool  bServerConfirmed;  // server acknowledgement flag
	void (*SysMsgHandler)(DPMSG_GENERIC*, DWORD, DPID, DPID, LPVOID);
	void (*AppMsgHandler)(LPVOID, DWORD, DPID, DPID, LPVOID);
	LPVOID SysMsgContext, AppMsgContext;
};

#endif // !DP7FRAME_H
