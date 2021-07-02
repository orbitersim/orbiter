#ifndef __NETWORK_H
#define __NETWORK_H

#include <winsock2.h>
#include "Config.h"

#define WM_CONN_EVENT WM_USER

#define CON_REQ_MJD 0x0001 // request current MJD from remote host

class OrbiterConnect {
public:
	static int Startup ();
	static int Cleanup ();
	static OrbiterConnect *Init (const NetParam &prm);
	
	OrbiterConnect (int type, unsigned short port = 5001);
	// define a socket connection
	// type can be SOCK_STREAM (for TCP) or SOCK_DGRAM (for UDP)

	virtual void SetAsyncMsgWindow (HWND hWnd);
	// set the window to receive network event notifications

	int Send (const char *buf, int len);
	// send a data block via sock

	int Recv (char *buf, int len);
	// receive a data block via sock

	int SendRequest (DWORD req);
	// send a request ID to the socket

	int RecvRequest (DWORD &req);
	// receive a request ID from the socket

	enum role {SERVER, CLIENT};
	virtual role Role() const = 0;
	bool isServer() const { return Role() == SERVER; }
	bool isClient() const { return Role() == CLIENT; }

protected:
	int sock_type;
	unsigned short sock_port;
	SOCKET sock;
};

#endif // !__NETWORK_H