#ifndef __SERVER_H
#define __SERVER_H

#include "Network.h"

class OrbiterServer: public OrbiterConnect {
public:
	OrbiterServer (int type, unsigned short port);
	~OrbiterServer ();
	role Role() const { return SERVER; }
	void SetAsyncMsgWindow (HWND hWnd);
	void Accept ();

private:
	sockaddr_in local, from;
	SOCKET listen_socket;
};

#endif // !__SERVER_H