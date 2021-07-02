#include "Server.h"
#include "Orbiter.h"

extern char DBG_MSG[256];
extern Orbiter *g_pOrbiter;

OrbiterServer::OrbiterServer (int type, unsigned short port): OrbiterConnect (type, port)
{
	local.sin_family = AF_INET;
	local.sin_addr.s_addr = INADDR_ANY;
	local.sin_port = htons (port);
	listen_socket = socket (AF_INET, sock_type, 0); // TCP socket

	bind (listen_socket, (struct sockaddr*)&local, sizeof(local));
	if (sock_type != SOCK_DGRAM)
		listen (listen_socket, 5);
	sprintf (DBG_MSG, "listening to %s, port %d", inet_ntoa (local.sin_addr), htons (local.sin_port));
}

OrbiterServer::~OrbiterServer ()
{}

void OrbiterServer::SetAsyncMsgWindow (HWND hWnd)
{
	OrbiterConnect::SetAsyncMsgWindow (hWnd);
	WSAAsyncSelect (listen_socket, g_pOrbiter->Get_hWnd(), WM_USER, FD_ACCEPT|FD_READ);
	// make the socket non-blocking (receives events via window messages)
}

void OrbiterServer::Accept ()
{
	int fromlen = sizeof(from);
	if (sock_type != SOCK_DGRAM) {
		sock = accept (listen_socket, (struct sockaddr*)&from, &fromlen);
		sprintf (DBG_MSG, "Accepted connection from %s, port %d",
			inet_ntoa (from.sin_addr), htons (from.sin_port));
	} else
		sock = listen_socket;
}
