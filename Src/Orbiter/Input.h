// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// DirectInput user interface class
// =======================================================================

#ifndef __INPUT_H
#define __INPUT_H

#include "Di7frame.h"

#ifdef __linux__
class Orbiter; // g++: a friend declaration doesn't introduce the name

#endif // __linux__
class DInput {
	friend class Orbiter;

public:
	DInput (Orbiter *pOrbiter);
	~DInput ();

#ifndef __linux__
	HRESULT Create (HINSTANCE hInst);
#else // __linux__
	int Create (void *hInst);
#endif // __linux__
	void Destroy ();

#ifndef __linux__
	void SetRenderWindow(HWND hWnd);
#else // __linux__
	void SetRenderWindow(QWindow *hWnd);
#endif // __linux__

	bool CreateKbdDevice();
	bool CreateJoyDevice ();
	void DestroyDevices ();

	inline CDIFramework7 *GetDIFrame() const { return diframe; }
#ifndef __linux__
	inline const LPDIRECTINPUTDEVICE8 GetKbdDevice() const { return diframe->GetKbdDevice(); }
	inline const LPDIRECTINPUTDEVICE8 GetJoyDevice() const { return diframe->GetJoyDevice(); }
#else // __linux__
	inline KeyboardDevice *GetKbdDevice() const { return diframe->GetKbdDevice(); }
	inline JoystickDevice *GetJoyDevice() const { return diframe->GetJoyDevice(); }
#endif // __linux__

	void OptionChanged(DWORD cat, DWORD item);

#ifndef __linux__
	bool PollJoystick (DIJOYSTATE2 *js);
#else // __linux__
	bool PollJoystick (JoyState *js);
#endif // __linux__

	struct JoyProp {
		bool bThrottle;  // joystick has throttle control
		bool bRudder;    // joystick has rudder control
		int ThrottleOfs; // throttle data offset
	};

protected:
#ifndef __linux__
	HRESULT SetJoystickProperties ();
#else // __linux__
	int SetJoystickProperties ();
#endif // __linux__

private:
	Orbiter *orbiter;
	CDIFramework7 *diframe;
	JoyProp joyprop;
#ifndef __linux__
	HWND m_hWnd;
#else // __linux__
	QWindow *m_hWnd;
#endif // __linux__
};

#endif // !__INPUT_H
