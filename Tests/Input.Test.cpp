// not upstream: unit tests for Src/Orbiter/Di7frame (keyboard device) and Memstat
#include <catch2/catch_test_macros.hpp>
#include <linux/input.h>
#include "Di7frame.h"
#include "Memstat.h"
#include "OrbiterAPI.h"

// log stubs: Di7frame.cpp logs through Log.cpp, which needs the whole core
void LogOut (const char *msg, ...) {}
void LogOut_Error (const char *func, const char *file, int line, const char *msg, ...) {}
void LogOut_DIErr (int err, const char *func, const char *file, int line) {}

TEST_CASE("evdev key codes map to DIK scan codes", "[input]")
{
	REQUIRE(KeyboardDevice::DIKCode (KEY_ESC) == OAPI_KEY_ESCAPE);
	REQUIRE(KeyboardDevice::DIKCode (KEY_A) == OAPI_KEY_A);
	REQUIRE(KeyboardDevice::DIKCode (KEY_F12) == OAPI_KEY_F12);
	REQUIRE(KeyboardDevice::DIKCode (KEY_102ND) == OAPI_KEY_OEM_102);
	REQUIRE(KeyboardDevice::DIKCode (KEY_RIGHTCTRL) == OAPI_KEY_RCONTROL);
	REQUIRE(KeyboardDevice::DIKCode (KEY_KPENTER) == OAPI_KEY_NUMPADENTER);
	REQUIRE(KeyboardDevice::DIKCode (KEY_KPSLASH) == OAPI_KEY_DIVIDE);
	REQUIRE(KeyboardDevice::DIKCode (KEY_RIGHTALT) == OAPI_KEY_RALT);
	REQUIRE(KeyboardDevice::DIKCode (KEY_UP) == OAPI_KEY_UP);
	REQUIRE(KeyboardDevice::DIKCode (KEY_PAGEDOWN) == OAPI_KEY_NEXT);
	REQUIRE(KeyboardDevice::DIKCode (KEY_DELETE) == OAPI_KEY_DELETE);
	REQUIRE(KeyboardDevice::DIKCode (KEY_PAUSE) == OAPI_KEY_PAUSE);
	REQUIRE(KeyboardDevice::DIKCode (KEY_SYSRQ) == OAPI_KEY_SYSRQ);
	REQUIRE(KeyboardDevice::DIKCode (0) == 0);
}

TEST_CASE("Keyboard device state and buffered transitions", "[input]")
{
	KeyboardDevice kbd (3);
	char st[256];
	REQUIRE(kbd.GetDeviceState (256, st) == DIERR_NOTACQUIRED);
	kbd.Acquire ();
	kbd.KeyEvent (KEY_LEFTSHIFT, true);
	kbd.KeyEvent (KEY_A, true);
	kbd.KeyEvent (KEY_A, true);   // auto-repeat: no second transition
	REQUIRE(kbd.GetDeviceState (256, st) == DI_OK);
	REQUIRE(KEYDOWN (st, OAPI_KEY_A));
	REQUIRE(KEYMOD_SHIFT (st));
	REQUIRE(!KEYDOWN (st, OAPI_KEY_B));
	kbd.KeyEvent (KEY_A, false);
	kbd.KeyEvent (KEY_B, true);   // fourth transition overflows the 3-entry buffer
	KeyData dod[10];
	DWORD n = 10;
	REQUIRE(kbd.GetDeviceData (sizeof(KeyData), dod, &n, 0) == DI_OK);
	REQUIRE(n == 3);
	REQUIRE(dod[1].dwOfs == OAPI_KEY_A);
	REQUIRE(dod[1].dwData == 0x80);
	REQUIRE(dod[2].dwData == 0);
	REQUIRE(dod[2].dwSequence == dod[1].dwSequence + 1);
	kbd.Unacquire ();             // focus lost releases everything
	kbd.Acquire ();
	REQUIRE(kbd.GetDeviceState (256, st) == DI_OK);
	REQUIRE(!KEYDOWN (st, OAPI_KEY_B));
}

TEST_CASE("Working set from /proc", "[memstat]")
{
	MemStat ms;
	REQUIRE(ms.HeapUsage () > 1024*1024);
}
