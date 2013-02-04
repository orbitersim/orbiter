
#ifndef __OAPI_VERSION
#define __OAPI_VERSION

#include <OrbiterAPI.h>		// <= or wherever the version will be defined in
#include <GraphicsAPI.h>	// RP_REQUIRETEXPOW2 comes from here

#if !defined(OAPI_VERSION)
   #if defined(RP_REQUIRETEXPOW2)
     #define OAPI_BETA
   #endif
#else
#if (OAPI_VERSION > 100830)
	#define OAPI_BETA
#endif
#endif // !OAPI_VERSION

#endif // __OAPI_VERSION
