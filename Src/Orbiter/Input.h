// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// User input: keyboard state and events, and joystick
// =======================================================================

#ifndef __INPUT_H
#define __INPUT_H

#include <windows.h>
#include <string>
#include <vector>

class Orbiter;

struct JoyState {
	LONG lX, lY, lZ;
	LONG lRx, lRy, lRz;
	LONG rglSlider[2];
	DWORD rgdwPOV[4];
	BYTE rgbButtons[128];
};

struct KeyEvent {
	DWORD key;
	bool down;
};

class InputManager {
	friend class Orbiter;

public:
	InputManager (Orbiter *pOrbiter);
	~InputManager ();

	HRESULT Create ();
	void Destroy ();

	void SetRenderWindow (HWND hWnd);

	bool CreateKbdDevice ();
	bool CreateJoyDevice ();
	void DestroyDevices ();

	bool HasKeyboard () const { return m_bKbd; }

	void GetKeyState (char kstate[256]) const;
	void TakeKeyEvents (std::vector<KeyEvent> &events);

	std::vector<std::string> Joysticks () const;
	DWORD NumJoysticks () const { return (DWORD)Joysticks().size(); }

	void OptionChanged (DWORD cat, DWORD item);

	bool PollJoystick (JoyState *js);
	LONG ThrottleValue (const JoyState &js) const;

	struct JoyProp {
		bool bThrottle;
		bool bRudder;
		int ThrottleAxis;
	};

protected:
	HRESULT SetJoystickProperties ();

private:
	Orbiter *orbiter;
	JoyProp joyprop;
	HWND m_hWnd;
	bool m_bKbd;
	bool m_bJoy;
};

#endif // !__INPUT_H
