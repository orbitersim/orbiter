#include "Network.h"
#include "Server.h"
#include "Client.h"
#include "Orbiter.h"

extern Orbiter *g_pOrbiter;

int OrbiterConnect::Startup ()
{
	WSADATA wsaData;
	return WSAStartup (0x202, &wsaData);
}

int OrbiterConnect::Cleanup ()
{
	return WSACleanup ();
}

OrbiterConnect *OrbiterConnect::Init (const NetParam &prm)
{
	switch (prm.role) {
	case NetParam::SERVER:
		return new OrbiterServer (SOCK_STREAM, prm.port);
	case NetParam::CLIENT:
		return new OrbiterClient (SOCK_STREAM, prm.port, prm.server_name);
	default:
		return NULL;
	}
}

OrbiterConnect::OrbiterConnect (int type, unsigned short port)
{
	sock_type = type;
	sock_port = port;
	sock = 0;
}

void OrbiterConnect::SetAsyncMsgWindow (HWND hWnd)
{
	WSAAsyncSelect (sock, g_pOrbiter->Get_hWnd(), WM_USER, FD_READ);
	// make the socket non-blocking (receives events via window messages)
}

int OrbiterConnect::Send (const char *buf, int len)
{
	if (sock) return send (sock, buf, len, 0);
	else return 0;
}

int OrbiterConnect::Recv (char *buf, int len)
{
	if (sock) return recv (sock, buf, len, 0);
	else return 0;
}

int OrbiterConnect::SendRequest (DWORD req)
{
	DWORD lreq = req;
	if (sock) return send (sock, (char*)&lreq, sizeof(DWORD), 0);
	else return 0;
}

int OrbiterConnect::RecvRequest (DWORD &req)
{
	if (sock) return recv (sock, (char*)&req, sizeof(DWORD), 0);
	else return 0;
}
