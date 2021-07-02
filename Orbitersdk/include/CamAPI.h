// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
//                  Copyright (C) 2006-2008 Martin Schweiger
//                           All rights reserved
// CamAPI.h
// ORBITER Application Programming Interface (OAPI)
// External camera control interface
// ======================================================================

#ifndef __CAMAPI_H
#define __CAMAPI_H

#include "Orbitersdk.h"

#define CAMDATA_X              0x01
#define CAMDATA_Y              0x02
#define CAMDATA_Z              0x04
#define CAMDATA_POS            0x07
#define CAMDATA_YAW            0x08
#define CAMDATA_PITCH          0x10
#define CAMDATA_ROLL           0x20
#define CAMDATA_DIR            0x38

#define CAMMODE_VC             0x01
#define CAMMODE_PANELCOCKPIT   0x02
#define CAMMODE_GENERICCOCKPIT 0x04
#define CAMMODE_COCKPIT        0x07
#define CAMMODE_TRACK          0x08
#define CAMMODE_GROUND         0x10
#define CAMMODE_EXTERNAL       0x18

// ======================================================================
// class ExternalCameraControl
// interface to externally provided camera position data

class OAPIFUNC ExternalCameraControl {
public:
	ExternalCameraControl (DWORD dmode, DWORD cmode);

	void SetDataMode (DWORD mode);
	DWORD GetDataMode () const;

	void SetCameraMode (DWORD mode);
	DWORD GetCameraMode () const;

	struct CamData { // position data
		double x, y, z;
		double yaw, pitch, roll;
	};

	struct VCMode { // defines the VC behaviour
		bool trackrotation;   // apply rotation data
		bool trackposition;   // apply translation data
		bool freezeonmouse;   // freeze camera after mouse move
		double rotationrange; // default rotation range [rad]
		double positionrange; // default translation range [m]
		double freeze_t;      // freeze time [s]
	};

	struct TrackMode { // defines external track view behaviour
		bool trackrotation;   // allow camera orbiting target
		bool trackzoom;       // allow camera advancing on/retreating from target
		enum {BYROTATION,BYPOSITION} rotationdata; // use rotation/position data for rotation
		double deadzone;      // tracking deadzone (0-1)
		double speed;         // movement speed
	};

	inline const VCMode *GetVCMode () const { return &vcmode; }
	inline const TrackMode *GetTrackMode () const { return &trkmode; }

	virtual bool clbkPoll (CamData *data) = 0;
	// callback function for providing absolute position data
	// must be provided by implementations

protected:
	DWORD datamode;
	DWORD cameramode;
	VCMode vcmode;
	TrackMode trkmode;
};

// ======================================================================
// External control registration

OAPIFUNC bool oapiRegisterExternalCameraControl (ExternalCameraControl *ecc);
OAPIFUNC bool oapiUnregisterExternalCameraControl ();

#endif // !__CAMAPI_H