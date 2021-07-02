#ifndef __CLIENT_H
#define __CLIENT_H

#include "Network.h"

class OrbiterClient: public OrbiterConnect {
public:
	OrbiterClient (int type, unsigned short port, const char *server_name);
	~OrbiterClient ();
	role Role() const { return CLIENT; }
	void Connect ();

private:
	struct sockaddr_in server;
};

#endif // !__CLIENT_H