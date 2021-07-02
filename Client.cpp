#include "Client.h"
#include "Orbiter.h"
#include "Log.h"

extern Orbiter *g_pOrbiter;
extern char DBG_MSG[256];

OrbiterClient::OrbiterClient (int type, unsigned short port, const char *server_name): OrbiterConnect (type, port)
{
	struct hostent *hp; 

	if (isalpha (server_name[0])) { // server address is a name
		hp = gethostbyname (server_name);
	} else {                   // convert nnn.nnn address to a usable one
		unsigned int addr = inet_addr (server_name);
		hp = gethostbyaddr ((char*)&addr, 4, AF_INET);
	}
	if (hp == NULL) {
		LOGOUT2P ("ERROR: Client could not resolve address [%s]: Error %d",
			server_name, WSAGetLastError());
		return;
	}

	// Copy the resolved information into the sockaddr_in structure 
	memset (&server, 0, sizeof(server));
	memcpy (&(server.sin_addr), hp->h_addr, hp->h_length);
	server.sin_family = hp->h_addrtype;
	server.sin_port = htons (port);
	
	sock = socket (AF_INET, sock_type, 0); // Open a socket
	sprintf (DBG_MSG, "Client connecting to: %s", hp->h_name);
}

OrbiterClient::~OrbiterClient ()
{
	closesocket (sock);
}

void OrbiterClient::Connect ()
{
	if (sock) {
		if (connect (sock, (struct sockaddr*)&server, sizeof(server)) == SOCKET_ERROR)
			LOGOUT1P ("ERROR: Connection to server vailed: Error %d", WSAGetLastError());
	}
}