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

struct JoyState {
	Sint16 xAx;
	Sint16 yAx;
	Sint16 zRot;
	Sint16 throttle;
	Uint8 hat;
	bool btn3;
};

class DInput {
	friend class Orbiter;

public:
	DInput (Orbiter *pOrbiter);
	~DInput ();

	void SetRenderWindow(HWND hWnd);

	bool CreateJoyDevice ();
	void DestroyDevices ();

	inline SDL_Joystick* GetJoyDevice() const { return joystick; }

	void OptionChanged(DWORD cat, DWORD item);

	bool PollJoystick (JoyState *js) const;

	struct JoyProp {
		bool bThrottle;  // joystick has throttle control
		bool bRudder;    // joystick has rudder control
		int ThrottleOfs; // throttle axis ID
		int xAxId;       // X-axis ID
		int yAxId;       // Y-axis ID
		int zRotId;      // Z rotation axis ID
	};

	bool ConsumeEvent(const SDL_Event &event);

protected:
	void SetJoystickProperties ();

private:
	Orbiter *orbiter;
	SDL_Joystick* joystick;
	JoyProp joyprop;
	HWND m_hWnd;
	std::list<SDL_KeyboardEvent> m_bufferedEvents;

	Sint16 PollAxisWithDeadzone(int axis, Sint16 deadzone) const;
};

#endif // !__INPUT_H