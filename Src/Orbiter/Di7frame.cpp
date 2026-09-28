// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ====================================================================================
// File: Di7frame.cpp
// Desc: Class to manage the DirectInput environment objects
// ====================================================================================

#include "Di7frame.h"
#ifndef __linux__
#include "D3d7util.h"
#endif // !__linux__
#include "Log.h"
#ifdef __linux__
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <chrono>
#include <algorithm>
#include <linux/input.h>

#define TEST_BIT(bit, array) ((array[(bit)/8] >> ((bit)%8)) & 1)

// evdev key codes that differ from the set-1 scan codes DirectInput uses (codes 1-88 are identical)
static const struct { int ev; DWORD dik; } evdik[] = {
	{KEY_KPENTER, 0x9C}, {KEY_RIGHTCTRL, 0x9D}, {KEY_KPSLASH, 0xB5}, {KEY_SYSRQ, 0xB7},
	{KEY_RIGHTALT, 0xB8}, {KEY_PAUSE, 0xC5}, {KEY_HOME, 0xC7}, {KEY_UP, 0xC8}, {KEY_PAGEUP, 0xC9},
	{KEY_LEFT, 0xCB}, {KEY_RIGHT, 0xCD}, {KEY_END, 0xCF}, {KEY_DOWN, 0xD0}, {KEY_PAGEDOWN, 0xD1},
	{KEY_INSERT, 0xD2}, {KEY_DELETE, 0xD3}, {KEY_LEFTMETA, 0xDB}, {KEY_RIGHTMETA, 0xDC},
	{KEY_COMPOSE, 0xDD}, {KEY_KPEQUAL, 0x8D}, {KEY_F13, 0x64}, {KEY_F14, 0x65}, {KEY_F15, 0x66},
	{KEY_KPCOMMA, 0xB3}, {KEY_MUTE, 0xA0}, {KEY_VOLUMEDOWN, 0xAE}, {KEY_VOLUMEUP, 0xB0}
};

static DWORD MsTime ()
{
	return (DWORD)std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now().time_since_epoch()).count();
}

// ====================================================================================
// KeyboardDevice

KeyboardDevice::KeyboardDevice (DWORD _bufsize)
{
	acquired = false;
	memset (kstate, 0, 256);
	bufsize = _bufsize;
	seq = 0;
}

DWORD KeyboardDevice::DIKCode (int evcode)
{
	if (evcode > 0 && evcode <= KEY_F12) return (DWORD)evcode;
	for (auto &m : evdik) if (m.ev == evcode) return m.dik;
	return 0;
}

void KeyboardDevice::KeyEvent (int evcode, bool down)
{
	if (!acquired) return;
	DWORD dik = DIKCode (evcode);
	if (!dik || dik > 255) return;
	if (((kstate[dik] & 0x80) != 0) == down) return; // auto-repeat: DirectInput reports transitions only
	kstate[dik] = (down ? (char)0x80 : 0);
	if (buf.size() >= bufsize) return;                // buffer overflow drops events, like DIPROP_BUFFERSIZE
	buf.push_back ({dik, down ? 0x80u : 0u, MsTime(), seq++});
}

int KeyboardDevice::Acquire ()
{
	acquired = true;
	return DI_OK;
}

void KeyboardDevice::Unacquire ()
{
	acquired = false;
	memset (kstate, 0, 256);
	buf.clear();
}

int KeyboardDevice::GetDeviceState (DWORD size, void *state)
{
	if (!acquired) return DIERR_NOTACQUIRED;
	memcpy (state, kstate, std::min (size, (DWORD)256));
	return DI_OK;
}

int KeyboardDevice::GetDeviceData (DWORD size, KeyData *dod, DWORD *n, DWORD flags)
{
	if (!acquired) { *n = 0; return DIERR_NOTACQUIRED; }
	DWORD k = std::min (*n, (DWORD)buf.size());
	for (DWORD i = 0; i < k; i++) dod[i] = buf[i];
	buf.erase (buf.begin(), buf.begin()+k);
	*n = k;
	return DI_OK;
}

// ====================================================================================
// JoystickDevice

static const int axcode[JoystickDevice::NAXIS] = { ABS_X, ABS_Y, ABS_Z, ABS_RX, ABS_RY, ABS_RZ, ABS_THROTTLE, ABS_RUDDER };

JoystickDevice::JoystickDevice (const char *path)
{
	acquired = false;
	memset (hat, 0, sizeof(hat));
	memset (btn, 0, sizeof(btn));
	fd = open (path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
	unsigned char absbit[ABS_MAX/8+1] = {0}, keybit[KEY_MAX/8+1] = {0};
	if (fd >= 0) {
		ioctl (fd, EVIOCGBIT(EV_ABS, sizeof(absbit)), absbit);
		ioctl (fd, EVIOCGBIT(EV_KEY, sizeof(keybit)), keybit);
	}
	for (int a = 0; a < NAXIS; a++) {
		AxisPrm &p = axis[a];
		p.code = axcode[a];
		p.present = (fd >= 0 && TEST_BIT(p.code, absbit));
		p.amin = -32767, p.amax = 32767, p.raw = 0;
		if (p.present) {
			struct input_absinfo ai;
			if (!ioctl (fd, EVIOCGABS(p.code), &ai)) p.amin = ai.minimum, p.amax = ai.maximum, p.raw = ai.value;
		}
		p.lMin = 0, p.lMax = 65535, p.dz = 0, p.sat = 10000; // DirectInput defaults
	}
	for (int c = BTN_MISC; c < KEY_MAX && btncode.size() < 128; c++) // buttons in code order, as SDL does
		if (fd >= 0 && TEST_BIT(c, keybit)) btncode.push_back (c);
}

JoystickDevice::~JoystickDevice ()
{
	if (fd >= 0) close (fd);
}

int JoystickDevice::Acquire ()
{
	if (fd < 0) return DIERR_INPUTLOST;
	acquired = true; // DISCL_EXCLUSIVE left out: an EVIOCGRAB would take the stick away from every other program
	return DI_OK;
}

void JoystickDevice::Unacquire ()
{
	acquired = false;
}

int JoystickDevice::ReadAll ()
{
	struct input_event ev[64];
	ssize_t n;
	while ((n = read (fd, ev, sizeof(ev))) > 0 || (n < 0 && errno == EINTR)) {
		if (n < 0) continue;
		for (ssize_t i = 0; i < n/(ssize_t)sizeof(ev[0]); i++) {
			const input_event &e = ev[i];
			if (e.type == EV_ABS) {
				for (int a = 0; a < NAXIS; a++) if (axis[a].code == e.code) axis[a].raw = e.value;
				if (e.code >= ABS_HAT0X && e.code <= ABS_HAT3Y) hat[(e.code-ABS_HAT0X)/2][(e.code-ABS_HAT0X)%2] = e.value;
			} else if (e.type == EV_KEY) {
				for (size_t b = 0; b < btncode.size(); b++) if (btncode[b] == e.code) btn[b] = (e.value ? 0x80 : 0);
			} else if (e.type == EV_SYN && e.code == SYN_DROPPED) { // kernel buffer overran: re-read the full state
				for (int a = 0; a < NAXIS; a++) if (axis[a].present) {
					struct input_absinfo ai;
					if (!ioctl (fd, EVIOCGABS(axis[a].code), &ai)) axis[a].raw = ai.value;
				}
			}
		}
	}
	return (n < 0 && errno != EAGAIN ? errno : 0);
}

int JoystickDevice::Poll ()
{
	if (fd < 0) return DIERR_INPUTLOST;
	if (!acquired) return DIERR_NOTACQUIRED;
	return (ReadAll () ? DIERR_INPUTLOST : DI_OK); // ENODEV: unplugged
}

LONG JoystickDevice::Scale (int a) const
{
	const AxisPrm &p = axis[a];
	if (!p.present) return (p.lMin + p.lMax)/2;
	double c = 0.5*(p.amin + p.amax), h = 0.5*(p.amax - p.amin);
	double t = (h > 0 ? (p.raw - c)/h : 0.0), at = fabs(t);
	double dz = p.dz*1e-4, sat = std::max (p.sat*1e-4, dz+1e-6);
	at = (at <= dz ? 0.0 : at >= sat ? 1.0 : (at-dz)/(sat-dz));
	t = (t < 0 ? -at : at);
	return (LONG)floor (p.lMin + 0.5*(t+1.0)*(p.lMax - p.lMin) + 0.5);
}

int JoystickDevice::GetDeviceState (DWORD size, JoyState *js)
{
	if (fd < 0) return DIERR_INPUTLOST;
	if (!acquired) return DIERR_NOTACQUIRED;
	memset (js, 0, sizeof(JoyState));
	js->lX = Scale(AX_X);   js->lY = Scale(AX_Y);   js->lZ = Scale(AX_Z);
	js->lRx = Scale(AX_RX); js->lRy = Scale(AX_RY); js->lRz = Scale(AX_RZ);
	js->rglSlider[0] = Scale(AX_SLIDER0); js->rglSlider[1] = Scale(AX_SLIDER1);
	for (int i = 0; i < 4; i++) {
		int x = hat[i][0], y = hat[i][1];
		if (!x && !y) js->rgdwPOV[i] = 0xFFFFFFFF;
		else js->rgdwPOV[i] = (DWORD)(((int)lround (atan2 ((double)x, (double)-y)*18000.0/M_PI) + 36000) % 36000);
	}
	memcpy (js->rgbButtons, btn, sizeof(btn));
	return DI_OK;
}

bool JoystickDevice::SetAxisRange (Axis a, LONG lMin, LONG lMax)
{
	if (!axis[a].present) return false;
	axis[a].lMin = lMin, axis[a].lMax = lMax;
	return true;
}

bool JoystickDevice::SetAxisDeadzone (Axis a, DWORD dz)
{
	if (!axis[a].present || dz > 10000) return false;
	axis[a].dz = dz;
	return true;
}

bool JoystickDevice::SetAxisSaturation (Axis a, DWORD sat)
{
	if (!axis[a].present || sat > 10000) return false;
	axis[a].sat = sat;
	return true;
}
#endif // __linux__

//-----------------------------------------------------------------------------
// Name: CDIFramework7()
// Desc: Constructor
//-----------------------------------------------------------------------------
CDIFramework7::CDIFramework7 ()
{
#ifndef __linux__
	m_pDI             = NULL;
#endif // !__linux__
	m_pdidKbdDevice   = NULL;
#ifndef __linux__
	m_pdidMouseDevice = NULL;
#endif // !__linux__
	m_pdidJoyDevice   = NULL;
#ifdef __linux__
	jList.descJoy     = NULL;
#endif // __linux__
	jList.nJoy        = 0;
}

//-----------------------------------------------------------------------------
// Name: ~CDIFramework7()
// Desc: Destructor
//-----------------------------------------------------------------------------
CDIFramework7::~CDIFramework7 ()
{
	Destroy ();
}

//-----------------------------------------------------------------------------
// Name: Create()
// Desc: Initialises the DirectInput objects
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT CDIFramework7::Create (HINSTANCE hInst)
#else // __linux__
int CDIFramework7::Create (void *hInst)
#endif // __linux__
{
#ifndef __linux__
	HRESULT hr;
#else // __linux__
	// DirectInput8Create left out: evdev has no input system object
#endif // __linux__

#ifndef __linux__
	// Create the main DirectInput object
	if (FAILED (hr = DirectInput8Create (hInst, DIRECTINPUT_VERSION,
		IID_IDirectInput8, (LPVOID*)&m_pDI, NULL))) {
		LOGOUT("ERROR: DI: DirectInputCreate failed");
		LOGOUT_DIERR(hr);
		return hr;
	}

	// Check to see whether a joystick is present. If one is, the enumeration
	// callback will save the joystick's GUID, so we can create it later
	ZeroMemory (&m_guidJoystick, sizeof (GUID));
	
	m_pDI->EnumDevices (DI8DEVCLASS_GAMECTRL, EnumJoysticksCallback,
		(LPVOID)&jList, DIEDFL_ATTACHEDONLY);

	// pick first joystick by default
	if (jList.nJoy)
		memcpy (&m_guidJoystick, &jList.descJoy[0].guidInstance, sizeof (GUID));
#else // __linux__
	// Check to see whether a joystick is present: enumerate /dev/input/event* nodes
	// that report an x axis and joystick or gamepad buttons (DI8DEVCLASS_GAMECTRL)
	if (DIR *d = opendir ("/dev/input")) {
		std::vector<std::string> nodes;
		while (struct dirent *e = readdir (d))
			if (!strncmp (e->d_name, "event", 5)) nodes.push_back (e->d_name);
		closedir (d);
		std::sort (nodes.begin(), nodes.end(), [](const std::string &a, const std::string &b) {
			return atoi (a.c_str()+5) < atoi (b.c_str()+5); });
		for (auto &name : nodes) {
			JoyDeviceInstance inst;
			snprintf (inst.path, sizeof(inst.path), "/dev/input/%s", name.c_str());
			int fd = open (inst.path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
			if (fd < 0) continue;
			unsigned char evbit[EV_MAX/8+1] = {0}, absbit[ABS_MAX/8+1] = {0}, keybit[KEY_MAX/8+1] = {0};
			ioctl (fd, EVIOCGBIT(0, sizeof(evbit)), evbit);
			ioctl (fd, EVIOCGBIT(EV_ABS, sizeof(absbit)), absbit);
			ioctl (fd, EVIOCGBIT(EV_KEY, sizeof(keybit)), keybit);
			bool isjoy = TEST_BIT(EV_ABS, evbit) && TEST_BIT(ABS_X, absbit) &&
				(TEST_BIT(BTN_JOYSTICK, keybit) || TEST_BIT(BTN_GAMEPAD, keybit) || TEST_BIT(BTN_TRIGGER_HAPPY1, keybit));
			if (isjoy) {
				if (ioctl (fd, EVIOCGNAME(sizeof(inst.tszProductName)), inst.tszProductName) < 0)
					strcpy (inst.tszProductName, "Joystick");
				strcpy (inst.tszInstanceName, inst.tszProductName);
				EnumJoysticksCallback (&inst, (void*)&jList);
			}
			close (fd);
		}
	}

	// pick first joystick by default (m_guidJoystick left out: CreateJoyDevice takes the list index)
#endif // __linux__

	LOGOUT("Found %d joystick(s)", jList.nJoy);
#ifndef __linux__
	return S_OK;
#else // __linux__
	return DI_OK;
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: Destroy()
// Desc: Deletes devices and DI object
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID CDIFramework7::Destroy ()
#else // __linux__
void CDIFramework7::Destroy ()
#endif // __linux__
{
	DestroyDevices();
#ifndef __linux__
	SAFE_RELEASE (m_pDI);
#else // __linux__
	if (jList.nJoy) {
		delete []jList.descJoy;
		jList.descJoy = NULL;
		jList.nJoy = 0;
	}
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: EnumJoysticksCallback()
// Desc: Called once for each enumerated joystick. If we find one, create a device
//       interface on it so that we can play with it
//-----------------------------------------------------------------------------
#ifndef __linux__
BOOL CALLBACK CDIFramework7::EnumJoysticksCallback (LPCDIDEVICEINSTANCE pInst, VOID* pvContext)
#else // __linux__
bool CDIFramework7::EnumJoysticksCallback (const JoyDeviceInstance *pInst, void* pvContext)
#endif // __linux__
{
	// Check here whether the enumerated device is appropriate
	struct JLIST *jlist = (struct JLIST*)pvContext;
#ifndef __linux__
	DIDEVICEINSTANCE *tmp = new DIDEVICEINSTANCE[jlist->nJoy+1]; TRACENEW
#else // __linux__
	JoyDeviceInstance *tmp = new JoyDeviceInstance[jlist->nJoy+1]; TRACENEW
#endif // __linux__
	if (jlist->nJoy) {
#ifndef __linux__
		memcpy (tmp, jlist->descJoy, jlist->nJoy*sizeof(DIDEVICEINSTANCE));
#else // __linux__
		memcpy (tmp, jlist->descJoy, jlist->nJoy*sizeof(JoyDeviceInstance));
#endif // __linux__
		delete []jlist->descJoy;
	}
	jlist->descJoy = tmp;
#ifndef __linux__
	memcpy (jlist->descJoy + jlist->nJoy++, pInst, sizeof(DIDEVICEINSTANCE));
	return DIENUM_CONTINUE;
#else // __linux__
	memcpy (jlist->descJoy + jlist->nJoy++, pInst, sizeof(JoyDeviceInstance));
	return true; // DIENUM_CONTINUE
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: GetJoysticks()
// Desc: Returns the list of enumerated joysticks
//-----------------------------------------------------------------------------
#ifndef __linux__
VOID CDIFramework7::GetJoysticks (DIDEVICEINSTANCE **dev, DWORD *pdwCount)
#else // __linux__
void CDIFramework7::GetJoysticks (JoyDeviceInstance **dev, DWORD *pdwCount)
#endif // __linux__
{
	*dev = jList.descJoy;
	*pdwCount = jList.nJoy;
}

//-----------------------------------------------------------------------------
#ifndef __linux__
// Name: CreateDevice()
// Desc: Creates a DirectInput device
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateDevice (HWND hWnd, LPDIRECTINPUT8 pDI,
	LPDIRECTINPUTDEVICE8 pDIDevice, GUID guidDevice, const DIDATAFORMAT *pdidDataFormat,
	DWORD dwFlags)
{
	// Obtain an interface to the input device
	if (FAILED (pDI->CreateDevice (guidDevice, &pDIDevice, NULL))) {
		LOGOUT("ERROR: DI: CreateDeviceEx failed");
		return E_FAIL;
	}

	// Set the device data format. A data format specifies which controls on a
	// device you are interested in and indicates how they should be reported.
	if (FAILED (pDIDevice->SetDataFormat (pdidDataFormat))) {
		LOGOUT("ERROR: DI: SetDataFormat failed");
		return E_FAIL;
	}

	// Set the cooperative level to let DirectInput know how this device should
	// interact with the system and with other DirectInput applications
	if (FAILED (pDIDevice->SetCooperativeLevel (hWnd, dwFlags))) {
		LOGOUT("ERROR: DI: SetCooperativeLevel failed");
		return E_FAIL;
	}

	if (guidDevice == GUID_SysKeyboard)
		m_pdidKbdDevice = pDIDevice;
	else if (guidDevice == GUID_SysMouse)
		m_pdidMouseDevice = pDIDevice;
	else
		m_pdidJoyDevice = pDIDevice;

	return S_OK;
}

//-----------------------------------------------------------------------------
#endif // !__linux__
// Name: CreateKbdDevice()
// Desc: Creates a DirectInput device for the keyboard
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT CDIFramework7::CreateKbdDevice (HWND hWnd)
#else // __linux__
int CDIFramework7::CreateKbdDevice (QWindow *hWnd)
#endif // __linux__
{
#ifndef __linux__
	HRESULT hr;
	if (FAILED (hr = CreateDevice (hWnd, m_pDI, m_pdidKbdDevice, GUID_SysKeyboard,
		&c_dfDIKeyboard, DISCL_NONEXCLUSIVE | DISCL_FOREGROUND))) return hr;
#else // __linux__
	// cooperative level (DISCL_NONEXCLUSIVE | DISCL_FOREGROUND): the render window only
	// feeds key events while it has focus, and releases all keys when focus is lost
#endif // __linux__

	// set buffer size for storage of buffered key events
#ifndef __linux__
	DIPROPDWORD dipdw;
	dipdw.diph.dwSize = sizeof(DIPROPDWORD);
	dipdw.diph.dwHeaderSize = sizeof(DIPROPHEADER);
	dipdw.diph.dwObj = 0;
	dipdw.diph.dwHow = DIPH_DEVICE;
	dipdw.dwData = 10;
	return m_pdidKbdDevice->SetProperty (DIPROP_BUFFERSIZE, &dipdw.diph);
}

//-----------------------------------------------------------------------------
// Name: CreateMouseDevice()
// Desc: Creates a DirectInput device for the mouse
//-----------------------------------------------------------------------------
HRESULT CDIFramework7::CreateMouseDevice (HWND hWnd)
{
	HRESULT hr;
	if (FAILED (hr = CreateDevice (hWnd, m_pDI, m_pdidMouseDevice, GUID_SysMouse,
		&c_dfDIMouse, DISCL_NONEXCLUSIVE | DISCL_FOREGROUND))) return hr;

	// switch mouse to absolute mode
	DIPROPDWORD diprw;
	diprw.diph.dwSize       = sizeof (DIPROPDWORD);
	diprw.diph.dwHeaderSize = sizeof (DIPROPHEADER);
	diprw.diph.dwObj        = 0;
	diprw.diph.dwHow        = DIPH_DEVICE;
	diprw.dwData            = DIPROPAXISMODE_ABS;
	return m_pdidMouseDevice->SetProperty (DIPROP_AXISMODE, &diprw.diph);
#else // __linux__
	m_pdidKbdDevice = new KeyboardDevice (10);
	return DI_OK;
#endif // __linux__
}

//-----------------------------------------------------------------------------
// Name: CreateJoyDevice()
// Desc: Creates a DirectInput device for a joystick
//-----------------------------------------------------------------------------
#ifndef __linux__
HRESULT CDIFramework7::CreateJoyDevice (HWND hWnd, DWORD idx)
#else // __linux__
int CDIFramework7::CreateJoyDevice (QWindow *hWnd, DWORD idx)
#endif // __linux__
{
#ifndef __linux__
	if (idx >= jList.nJoy) return E_FAIL;
	return CreateDevice (hWnd, m_pDI, m_pdidJoyDevice, jList.descJoy[idx].guidInstance,
		&c_dfDIJoystick2, DISCL_EXCLUSIVE | DISCL_FOREGROUND);
#else // __linux__
	if (idx >= jList.nJoy) return DIERR_INPUTLOST;
	JoystickDevice *dev = new JoystickDevice (jList.descJoy[idx].path);
	if (!dev->Valid()) {
		LOGOUT_DIERR(errno);
		delete dev;
		return DIERR_INPUTLOST;
	}
	m_pdidJoyDevice = dev;
	return DI_OK;
#endif // __linux__
}

void CDIFramework7::DestroyJoyDevice()
{
	if (m_pdidJoyDevice) {
		m_pdidJoyDevice->Unacquire();
#ifndef __linux__
		m_pdidJoyDevice->Release();
#else // __linux__
		delete m_pdidJoyDevice;
#endif // __linux__
		m_pdidJoyDevice = NULL;
	}
}

//-----------------------------------------------------------------------------
// Name: DestroyDevices()
// Desc: Releases the DirectInput devices
//-----------------------------------------------------------------------------
void CDIFramework7::DestroyDevices()
{
	if (m_pdidKbdDevice) {
		m_pdidKbdDevice->Unacquire ();
#ifndef __linux__
		m_pdidKbdDevice->Release ();
#else // __linux__
		delete m_pdidKbdDevice;
#endif // __linux__
		m_pdidKbdDevice = NULL;
	}
#ifndef __linux__
	if (m_pdidMouseDevice) {
		m_pdidMouseDevice->Unacquire ();
		m_pdidMouseDevice->Release ();
		m_pdidMouseDevice = NULL;
	}
#else // __linux__
	// mouse device left out (see Di7frame.h)
#endif // __linux__
	DestroyJoyDevice();
}
#ifndef __linux__

#endif // !__linux__
