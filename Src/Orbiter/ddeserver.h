// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DDE server class
// Dynamic data exchange (DDE) orbiter server implementation
// =======================================================================

#ifndef __DDESERVER_H
#define __DDESERVER_H

#include "Orbiter.h"

class DDEServer {
public:
	// Creates the global server instance
	DDEServer (HWND hserver);

	// Destroys the server instance
	~DDEServer ();

	// Connection request from hClient to hServer for given application and topic
	// Returns true if request is granted.
	bool DDEInit (HWND hServer, HWND hClient, ATOM aApp, ATOM aTopic);

	// Data request from hClient to hServer for given clipboard format and data item
	// Returns true if data can be sent.
	bool DDERequest (HWND hServer, HWND hClient, WORD fmt, ATOM item);

	// Message handler for DDE-related messages to the orbiter window
	void MsgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

	// Adds a new client (identified by its window handle) to the list
	// Returns false if client is already in the list
	bool AddClient (HWND hclient);

protected:
	void get_vesselcount (HWND hClient, ATOM item);
	void get_vesselid (HWND hClient, ATOM item, DWORD idx);
	bool get_objid (HWND hClient, ATOM item, char *fmtstr);
	bool get_name (HWND hClient, ATOM item, LONG_PTR id);
	bool get_size (HWND hClient, ATOM item, LONG_PTR id);
	bool get_state (HWND hClient, ATOM item, char *fmtstr);

	// Send a vessel list to hClient
	void VesselList (HWND hClient);

	DDEDATA *AllocDatablock (DWORD bufsize);

	bool isVessel (Vessel *v);
	bool isBody (Body *b);

private:
	HWND hServer;  // server (orbiter) window handle
	struct ClientSpec {
		HWND hwnd; // client window handle
	} *client;
	int nclient;   // number of clients in the list

	ATOM aApp;
	ATOM aTopic;
	ATOM *aItem;   // list of data items recognised by the server
	int nitem;     // number of data items
};

#endif // !__DDESERVER