// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DirectInput user interface class
// =======================================================================

#ifndef __INPUT_H
#define __INPUT_H

#include "Di7frame.h"

#include <list>
#include <SDL3/SDL_events.h>

class DInput {
	friend class Orbiter;

public:
	DInput (Orbiter *pOrbiter);
	~DInput ();

	HRESULT Create (HINSTANCE hInst);
	void Destroy ();

	void SetRenderWindow(HWND hWnd);

	bool CreateKbdDevice();
	bool CreateJoyDevice ();
	void DestroyDevices ();

	inline CDIFramework7 *GetDIFrame() const { return diframe; }
	inline const LPDIRECTINPUTDEVICE8 GetKbdDevice() const { return diframe->GetKbdDevice(); }
	inline const LPDIRECTINPUTDEVICE8 GetJoyDevice() const { return diframe->GetJoyDevice(); }

	void OptionChanged(DWORD cat, DWORD item);

	bool PollJoystick (DIJOYSTATE2 *js);

	struct JoyProp {
		bool bThrottle;  // joystick has throttle control
		bool bRudder;    // joystick has rudder control
		int ThrottleOfs; // throttle data offset
	};

	bool ConsumeEvent(const SDL_Event &event);

protected:
	HRESULT SetJoystickProperties ();

private:
	Orbiter *orbiter;
	CDIFramework7 *diframe;
	JoyProp joyprop;
	HWND m_hWnd;
	std::list<SDL_KeyboardEvent> m_bufferedEvents;
};

#endif // !__INPUT_H