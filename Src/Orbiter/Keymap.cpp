// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Keymap.h"
#include "Config.h"
#include <fstream>

#define NKEY 95

using namespace std;

struct {
	BYTE id;
	char *name;
} keyname[NKEY] = {
	{OAPI_KEY_ESCAPE,     "ESC"},         // 0x01: Escape
	{OAPI_KEY_1,          "1"},           // 0x02: 1
	{OAPI_KEY_2,          "2"},           // 0x03: 2
	{OAPI_KEY_3,          "3"},           // 0x04:
	{OAPI_KEY_4,          "4"},           // 0x05
	{OAPI_KEY_5,          "5"},           // 0x06
	{OAPI_KEY_6,          "6"},           // 0x07
	{OAPI_KEY_7,          "7"},           // 0x08
	{OAPI_KEY_8,          "8"},           // 0x09
	{OAPI_KEY_9,          "9"},           // 0x0A
	{OAPI_KEY_0,          "0"},           // 0x0B
	{OAPI_KEY_MINUS,      "MINUS"},       // 0x0C
	{OAPI_KEY_EQUALS,     "EQUALS"},      // 0x0D
	{OAPI_KEY_BACK,       "BACK"},        // 0x0E
	{OAPI_KEY_TAB,        "TAB"},         // 0x0F
	{OAPI_KEY_Q,          "Q"},           // 0x10
	{OAPI_KEY_W,          "W"},           // 0x11
	{OAPI_KEY_E,          "E"},           // 0x12
	{OAPI_KEY_R,          "R"},           // 0x13
	{OAPI_KEY_T,          "T"},           // 0x14
	{OAPI_KEY_Y,          "Y"},           // 0x15
	{OAPI_KEY_U,          "U"},           // 0x16
	{OAPI_KEY_I,          "I"},           // 0x17
	{OAPI_KEY_O,          "O"},           // 0x18
	{OAPI_KEY_P,          "P"},           // 0x19
	{OAPI_KEY_LBRACKET,   "LBRACKET"},    // 0x1A
	{OAPI_KEY_RBRACKET,   "RBRACKET"},    // 0x1B
	{OAPI_KEY_RETURN,     "RETURN"},      // 0x1C
	{OAPI_KEY_A,          "A"},           // 0x1E
	{OAPI_KEY_S,          "S"},           // 0x1F
	{OAPI_KEY_D,          "D"},           // 0x20
	{OAPI_KEY_F,          "F"},           // 0x21
	{OAPI_KEY_G,          "G"},           // 0x22
	{OAPI_KEY_H,          "H"},           // 0x23
	{OAPI_KEY_J,          "J"},           // 0x24
	{OAPI_KEY_K,          "K"},           // 0x25
	{OAPI_KEY_L,          "L"},           // 0x26
	{OAPI_KEY_SEMICOLON,  "SEMICOLON"},   // 0x27
	{OAPI_KEY_APOSTROPHE, "APOSTROPHE"},  // 0x28
	{OAPI_KEY_GRAVE,      "GRAVE"},       // 0x29
	{OAPI_KEY_BACKSLASH,  "BACKSLASH"},   // 0x2B
	{OAPI_KEY_Z,          "Z"},           // 0x2C
	{OAPI_KEY_X,          "X"},           // 0x2D
	{OAPI_KEY_C,          "C"},           // 0x2E
	{OAPI_KEY_V,          "V"},           // 0x2F
	{OAPI_KEY_B,          "B"},           // 0x30
	{OAPI_KEY_N,          "N"},           // 0x31
	{OAPI_KEY_M,          "M"},           // 0x32
	{OAPI_KEY_COMMA,      "COMMA"},       // 0x33
	{OAPI_KEY_PERIOD,     "PERIOD"},      // 0x34
	{OAPI_KEY_SLASH,      "SLASH"},       // 0x35
	{OAPI_KEY_MULTIPLY,   "MULTIPLY"},    // 0x37
	{OAPI_KEY_SPACE,      "SPACE"},       // 0x39
	{OAPI_KEY_CAPITAL,    "CAPITAL"},     // 0x3A
	{OAPI_KEY_F1,         "F1"},          // 0x3B
	{OAPI_KEY_F2,         "F2"},          // 0x3C
	{OAPI_KEY_F3,         "F3"},          // 0x3D
	{OAPI_KEY_F4,         "F4"},          // 0x3E
	{OAPI_KEY_F5,         "F5"},          // 0x3F
	{OAPI_KEY_F6,         "F6"},          // 0x40
	{OAPI_KEY_F7,         "F7"},          // 0x41
	{OAPI_KEY_F8,         "F8"},          // 0x42
	{OAPI_KEY_F9,         "F9"},          // 0x43
	{OAPI_KEY_F10,        "F10"},         // 0x44
	{OAPI_KEY_NUMLOCK,    "NUMLOCK"},     // 0x45
	{OAPI_KEY_SCROLL,     "SCROLL"},      // 0x46
	{OAPI_KEY_NUMPAD7,    "NUMPAD7"},     // 0x47
	{OAPI_KEY_NUMPAD8,    "NUMPAD8"},     // 0x48
	{OAPI_KEY_NUMPAD9,    "NUMPAD9"},     // 0x49
	{OAPI_KEY_SUBTRACT,   "SUBTRACT"},    // 0x4A
	{OAPI_KEY_NUMPAD4,    "NUMPAD4"},     // 0x4B
	{OAPI_KEY_NUMPAD5,    "NUMPAD5"},     // 0x4C
	{OAPI_KEY_NUMPAD6,    "NUMPAD6"},     // 0x4D
	{OAPI_KEY_ADD,        "ADD"},         // 0x4E
	{OAPI_KEY_NUMPAD1,    "NUMPAD1"},     // 0x4F
	{OAPI_KEY_NUMPAD2,    "NUMPAD2"},     // 0x50
	{OAPI_KEY_NUMPAD3,    "NUMPAD3"},     // 0x51
	{OAPI_KEY_NUMPAD0,    "NUMPAD0"},     // 0x52
	{OAPI_KEY_DECIMAL,    "DECIMAL"},     // 0x53
	{OAPI_KEY_OEM_102,    "OEM_102"},     // 0x56
	{OAPI_KEY_F11,        "F11"},         // 0x57
	{OAPI_KEY_F12,        "F12"},         // 0x58
	{OAPI_KEY_NUMPADENTER,"NUMPADENTER"}, // 0x9C
	{OAPI_KEY_DIVIDE,     "DIVIDE"},      // 0xB5
	{OAPI_KEY_SYSRQ,      "SYSRQ"},       // 0xB7
	{OAPI_KEY_HOME,       "HOME"},        // 0xC7
	{OAPI_KEY_UP,         "UP"},          // 0xC8
	{OAPI_KEY_PRIOR,      "PGUP"},        // 0xC9
	{OAPI_KEY_LEFT,       "LEFT"},        // 0xCB
	{OAPI_KEY_RIGHT,      "RIGHT"},       // 0xCD
	{OAPI_KEY_END,        "END"},         // 0xCF
	{OAPI_KEY_DOWN,       "DOWN"},        // 0xD0
	{OAPI_KEY_NEXT,       "PGDOWN"},      // 0xD1
	{OAPI_KEY_INSERT,     "INSERT"},      // 0xD2
	{OAPI_KEY_DELETE,     "DELETE"}       // 0xD3
};

struct {
	WORD defkey;
	const char *itemstr;
} lkeyspec[LKEY_COUNT] = {
	{OAPI_KEY_LEFT     | KMOD_ALT,                 "CockpitCamRotateLeft"},
	{OAPI_KEY_RIGHT    | KMOD_ALT,                 "CockpitCamRotateRight"},
	{OAPI_KEY_UP       | KMOD_ALT,                 "CockpitCamRotateUp"},
	{OAPI_KEY_DOWN     | KMOD_ALT,                 "CockpitCamRotateDown"},
	{OAPI_KEY_DOWN     | KMOD_CTRL | KMOD_ALT,     "CockpitCamDontLean"},
	{OAPI_KEY_UP       | KMOD_CTRL | KMOD_ALT,     "CockpitCamLeanForward"},
	{OAPI_KEY_LEFT     | KMOD_CTRL | KMOD_ALT,     "CockpitCamLeanLeft"},
	{OAPI_KEY_RIGHT    | KMOD_CTRL | KMOD_ALT,     "CockpitCamLeanRight"},
	{OAPI_KEY_HOME,                                "CockpitResetCam"},
	{OAPI_KEY_LEFT,                                "PanelShiftLeft"},
	{OAPI_KEY_RIGHT,                               "PanelShiftRight"},
	{OAPI_KEY_UP,                                  "PanelShiftUp"},
	{OAPI_KEY_DOWN,                                "PanelShiftDown"},
	{OAPI_KEY_LEFT     | KMOD_CTRL,                "PanelSwitchLeft"},
	{OAPI_KEY_RIGHT    | KMOD_CTRL,                "PanelSwitchRight"},
	{OAPI_KEY_UP       | KMOD_CTRL,                "PanelSwitchUp"},
	{OAPI_KEY_DOWN     | KMOD_CTRL,                "PanelSwitchDown"},
	{OAPI_KEY_LEFT     | KMOD_CTRL,                "TrackCamRotateLeft"},
	{OAPI_KEY_RIGHT    | KMOD_CTRL,                "TrackCamRotateRight"},
	{OAPI_KEY_UP       | KMOD_CTRL,                "TrackCamRotateUp"},
	{OAPI_KEY_DOWN     | KMOD_CTRL,                "TrackCamRotateDown"},
	{OAPI_KEY_NEXT,                                "TrackCamAdvance"},
	{OAPI_KEY_PRIOR,                               "TrackCamRetreat"},
	{OAPI_KEY_LEFT,                                "GroundCamTiltLeft"},
	{OAPI_KEY_RIGHT,                               "GroundCamTiltRight"},
	{OAPI_KEY_UP,                                  "GroundCamTiltUp"},
	{OAPI_KEY_DOWN,                                "GroundCamTiltDown"},
	{OAPI_KEY_ADD      | KMOD_CTRL,                "IncMainThrust"},
	{OAPI_KEY_SUBTRACT | KMOD_CTRL,                "DecMainThrust"},
	{OAPI_KEY_MULTIPLY,                            "KillMainRetroThrust"},
	{OAPI_KEY_ADD,                                 "OverrideFullMainThrust"},
	{OAPI_KEY_SUBTRACT,                            "OverrideFullRetroThrust"},
	{OAPI_KEY_NUMPAD0,                             "IncHoverThrust"},
	{OAPI_KEY_DECIMAL,                             "DecHoverThrust"},
	{OAPI_KEY_DIVIDE   | KMOD_CTRL,                "RCSEnable"},
	{OAPI_KEY_DIVIDE,                              "RCSMode"},
	{OAPI_KEY_NUMPAD2,                             "RCSPitchUp"},
	{OAPI_KEY_NUMPAD8,                             "RCSPitchDown"},
	{OAPI_KEY_NUMPAD1,                             "RCSYawLeft"},
	{OAPI_KEY_NUMPAD3,                             "RCSYawRight"},
	{OAPI_KEY_NUMPAD4,                             "RCSBankLeft"},
	{OAPI_KEY_NUMPAD6,                             "RCSBankRight"},
	{OAPI_KEY_NUMPAD2,                             "RCSUp"},
	{OAPI_KEY_NUMPAD8,                             "RCSDown"},
	{OAPI_KEY_NUMPAD1,                             "RCSLeft"},
	{OAPI_KEY_NUMPAD3,                             "RCSRight"},
	{OAPI_KEY_NUMPAD6,                             "RCSForward"},
	{OAPI_KEY_NUMPAD9,                             "RCSBack"},
	{OAPI_KEY_NUMPAD2 | KMOD_CTRL,                 "LPRCSPitchUp"},
	{OAPI_KEY_NUMPAD8 | KMOD_CTRL,                 "LPRCSPitchDown"},
	{OAPI_KEY_NUMPAD1 | KMOD_CTRL,                 "LPRCSYawLeft"},
	{OAPI_KEY_NUMPAD3 | KMOD_CTRL,                 "LPRCSYawRight"},
	{OAPI_KEY_NUMPAD4 | KMOD_CTRL,                 "LPRCSBankLeft"},
	{OAPI_KEY_NUMPAD6 | KMOD_CTRL,                 "LPRCSBankRight"},
	{OAPI_KEY_NUMPAD2 | KMOD_CTRL,                 "LPRCSUp"},
	{OAPI_KEY_NUMPAD8 | KMOD_CTRL,                 "LPRCSDown"},
	{OAPI_KEY_NUMPAD1 | KMOD_CTRL,                 "LPRCSLeft"},
	{OAPI_KEY_NUMPAD3 | KMOD_CTRL,                 "LPRCSRight"},
	{OAPI_KEY_NUMPAD6 | KMOD_CTRL,                 "LPRCSForward"},
	{OAPI_KEY_NUMPAD9 | KMOD_CTRL,                 "LPRCSBack"},
	{OAPI_KEY_A,                                   "NMHoldAltitude"},
	{OAPI_KEY_L,                                   "NMHLevel"},
	{OAPI_KEY_LBRACKET,                            "NMPrograde"},
	{OAPI_KEY_RBRACKET,                            "NMRetrograde"},
	{OAPI_KEY_SEMICOLON,                           "NMNormal"},
	{OAPI_KEY_APOSTROPHE,                          "NMAntinormal"},
	{OAPI_KEY_NUMPAD5,                             "NMKillrot"},
	{OAPI_KEY_D       | KMOD_CTRL,                 "Undock"},
	{OAPI_KEY_DELETE,                              "IncElevatorTrim"},
	{OAPI_KEY_INSERT,                              "DecElevatorTrim"},
	{OAPI_KEY_COMMA,                               "WheelbrakeLeft"},
	{OAPI_KEY_PERIOD,                              "WheelbrakeRight"},
	{OAPI_KEY_H       | KMOD_CTRL,                 "HUD"},
	{OAPI_KEY_H,                                   "HUDMode"},
	{OAPI_KEY_R       | KMOD_CTRL,                 "HUDReference"},
	{OAPI_KEY_R       | KMOD_CTRL | KMOD_ALT,      "HUDTarget"},
	{OAPI_KEY_H       | KMOD_ALT,                  "HUDColour"},
	{OAPI_KEY_T,                                   "IncSimSpeed"},
	{OAPI_KEY_R,                                   "DecSimSpeed"},
	{OAPI_KEY_X,                                   "IncFOV"},
	{OAPI_KEY_Z,                                   "DecFOV"},
	{OAPI_KEY_X       | KMOD_CTRL,                 "StepIncFOV"},
	{OAPI_KEY_Z       | KMOD_CTRL,                 "StepDecFOV"},
	{OAPI_KEY_F4,                                  "MainMenu"},
	{OAPI_KEY_F1      | KMOD_ALT,                  "DlgHelp"},
	{OAPI_KEY_F1      | KMOD_CTRL,                 "DlgCamera"},
	{OAPI_KEY_F2      | KMOD_CTRL,                 "DlgSimspeed"},
	{OAPI_KEY_F4      | KMOD_CTRL,                 "DlgCustomCmd"},
	{OAPI_KEY_F9      | KMOD_CTRL,                 "DlgVisualHelpers"},
	{OAPI_KEY_F5      | KMOD_CTRL,                 "DlgRecorder"},
	{OAPI_KEY_I       | KMOD_CTRL,                 "DlgInfo"},
	{OAPI_KEY_M       | KMOD_CTRL,                 "DlgMap"},
	{OAPI_KEY_F1,                                  "ToggleCamInternal"},
	{OAPI_KEY_F2,                                  "ToggleTrackMode"},
	{OAPI_KEY_F8,                                  "TogglePanelMode"},
	{OAPI_KEY_F9,                                  "TogglePlanetarium"},
	{OAPI_KEY_C       | KMOD_CTRL,                 "ToggleRecPlay"},
	{OAPI_KEY_P       | KMOD_CTRL,                 "Pause"},
	{OAPI_KEY_S       | KMOD_CTRL,                 "Quicksave"},
	{OAPI_KEY_Q       | KMOD_CTRL,                 "Quit"},
	{OAPI_KEY_F3,                                  "DlgSelectVessel"},
	{OAPI_KEY_F3      | KMOD_CTRL,                 "SelectPrevVessel"},
	{OAPI_KEY_SYSRQ   | KMOD_CTRL,                 "DlgCapture"}
};

Keymap::Keymap ()
{
	SetDefault ();
}

void Keymap::SetDefault ()
{
	for (WORD i = 0; i < LKEY_COUNT; i++)
		func[i] = lkeyspec[i].defkey;
}

void Keymap::Write (char *fname)
{
	char cbuf[256];
	ofstream ofs (fname);
	for (int i = 0; i < LKEY_COUNT; i++)
		ofs << lkeyspec[i].itemstr << " = " << PrintStr (cbuf, func[i]) << endl;
}

bool Keymap::Read (char *fname)
{
	char cbuf[256];
	ifstream ifs (fname, ios::in);
	if (!ifs) return false;
	for (int i = 0; i < LKEY_COUNT; i++) {
		//func[i] = 0;
		if (GetItemString (ifs, lkeyspec[i].itemstr, cbuf)) {
			if (!ScanStr (cbuf, func[i])) func[i] = 0;
		}
	}
	return true;
}

bool Keymap::IsLogicalKey (char *kstate, int lfunc) const
{
	// check key
	if (!KEYDOWN (kstate, func[lfunc] & 0x00FF)) return false;
	// check modifiers, if any
	return IsMatchingModifier (kstate, func[lfunc]);
}

bool Keymap::IsLogicalKey (DWORD &key, char *kstate, int lfunc, bool clearkey) const
{
	// check key
	if ((func[lfunc] & 0x00FFu) != key) return false;
	// check modifiers, if any
	if (IsMatchingModifier (kstate, func[lfunc])) {
		if (clearkey) key = 0;
		return true;
	} else {
		return false;
	}
}

bool Keymap::IsMatchingModifier (char *kstate, WORD key) const
{
	WORD kmod;
	if (kmod = (key & KMOD_CTRL)) {
		if (kmod == KMOD_CTRL) {
			if (!KEYMOD_CONTROL(kstate))  return false;
		} else if (kmod == KMOD_LCTRL) {
			if (!KEYMOD_LCONTROL(kstate)) return false;
		} else {
			if (!KEYMOD_RCONTROL(kstate)) return false;
		}
	} else {
		if (KEYMOD_CONTROL(kstate))       return false;
	}

	if (kmod = (key & KMOD_SHIFT)) {
		if (kmod == KMOD_SHIFT) {
			if (!KEYMOD_SHIFT(kstate))    return false;
		} else if (kmod == KMOD_LSHIFT) {
			if (!KEYMOD_LSHIFT(kstate))   return false;
		} else {
			if (!KEYMOD_RSHIFT(kstate))   return false;
		}
	} else {
		if (KEYMOD_SHIFT(kstate))         return false;
	}

	if (kmod = (key & KMOD_ALT)) {
		if (kmod == KMOD_ALT) {
			if (!KEYMOD_ALT(kstate))      return false;
		} else if (kmod == KMOD_LALT) {
			if (!KEYMOD_LALT(kstate))     return false;
		} else {
			if (!KEYMOD_RALT(kstate))     return false;
		}
	} else {
		if (KEYMOD_ALT(kstate))           return false;
	}

	return true;
}

bool Keymap::ScanStr (char *cbuf, WORD &key) const
{
	int i;
	char *tok = strtok (cbuf, " ");
	if (!tok) return false;
	for (i = 0; i < NKEY; i++)
		if (!_stricmp (tok, keyname[i].name)) break;
	if (i == NKEY) return false; // key not found
	key = keyname[i].id;
	for (;;) {
		tok = strtok (NULL, " ");
		if (!tok) break;
		if      (!_stricmp (tok, "LSHIFT")) key |= KMOD_LSHIFT;
		else if (!_stricmp (tok, "RSHIFT")) key |= KMOD_RSHIFT;
		else if (!_stricmp (tok, "SHIFT"))  key |= KMOD_SHIFT;
		else if (!_stricmp (tok, "LCTRL"))  key |= KMOD_LCTRL;
		else if (!_stricmp (tok, "RCTRL"))  key |= KMOD_RCTRL;
		else if (!_stricmp (tok, "CTRL"))   key |= KMOD_CTRL;
		else if (!_stricmp (tok, "LALT"))   key |= KMOD_LALT;
		else if (!_stricmp (tok, "RALT"))   key |= KMOD_RALT;
		else if (!_stricmp (tok, "ALT"))    key |= KMOD_ALT;
	}
	return true;
}

char *Keymap::PrintStr (char *cbuf, WORD &key) const
{
	WORD i, kmod, kbase = key & 0x00ff;

	for (i = 0; i < NKEY; i++)
		if (keyname[i].id == kbase) break;
	if (i == NKEY) {
		cbuf[0] = '\0'; 
	} else {
		strcpy (cbuf, keyname[i].name);
		if (kmod = (key & KMOD_SHIFT)) {
			switch (kmod) {
			case KMOD_SHIFT:  strcat (cbuf, " SHIFT");  break;
			case KMOD_LSHIFT: strcat (cbuf, " LSHIFT"); break;
			case KMOD_RSHIFT: strcat (cbuf, " RSHIFT"); break;
			}
		}
		if (kmod = (key & KMOD_CTRL)) {
			switch (kmod) {
			case KMOD_CTRL:  strcat (cbuf, " CTRL");  break;
			case KMOD_LCTRL: strcat (cbuf, " LCTRL"); break;
			case KMOD_RCTRL: strcat (cbuf, " RCTRL"); break;
			}
		}
		if (kmod = (key & KMOD_ALT)) {
			switch (kmod) {
			case KMOD_ALT:  strcat (cbuf, " ALT");  break;
			case KMOD_LALT: strcat (cbuf, " LALT"); break;
			case KMOD_RALT: strcat (cbuf, " RALT"); break;
			}
		}
	}
	return cbuf;
}

