// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DirectInput user interface class
// =======================================================================

#ifndef __INPUT_H
#define __INPUT_H

#include "Di7frame.h"

struct DIJOYSTATE2 {};

class DInput {
	friend class Orbiter;

public:
	DInput (Orbiter *pOrbiter);
	~DInput ();

	uint32_t Create ();
	void Destroy ();

	void SetRenderWindow();

	bool CreateKbdDevice();
	bool CreateJoyDevice ();
	void DestroyDevices ();

	inline CDIFramework7 *GetDIFrame() const { return diframe; }
	inline const LPDIRECTINPUTDEVICE8 GetKbdDevice() const { return diframe->GetKbdDevice(); }
	inline const LPDIRECTINPUTDEVICE8 GetJoyDevice() const { return diframe->GetJoyDevice(); }

	void OptionChanged(uint32_t cat, uint32_t item);

	bool PollJoystick (DIJOYSTATE2 *js);

	struct JoyProp {
		bool bThrottle;  // joystick has throttle control
		bool bRudder;    // joystick has rudder control
		int ThrottleOfs; // throttle data offset
	};

protected:
	uint32_t SetJoystickProperties ();

private:
	Orbiter *orbiter;
	CDIFramework7 *diframe;
	JoyProp joyprop;
};

#endif // !__INPUT_H