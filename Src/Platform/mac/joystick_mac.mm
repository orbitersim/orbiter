#import <Foundation/Foundation.h>
#import <IOKit/hid/IOHIDManager.h>
#include "OrbiterJoystick.h"
#include <mutex>
#include <algorithm>

namespace {

struct Element {
	IOHIDElementRef ref;
	int kind;
	int index;
	CFIndex min, max;
};

enum { EL_AXIS, EL_HAT, EL_BUTTON };

struct Device {
	IOHIDDeviceRef ref;
	std::string name;
	std::vector<Element> elements;
	oplat::JoystickRaw state;
};

IOHIDManagerRef g_manager = nullptr;
std::vector<Device> g_devices;
int g_open = -1;
std::mutex g_lock;

std::string DeviceName(IOHIDDeviceRef d)
{
	CFTypeRef p = IOHIDDeviceGetProperty(d, CFSTR(kIOHIDProductKey));
	if (p && CFGetTypeID(p) == CFStringGetTypeID()) {
		char buf[256];
		if (CFStringGetCString((CFStringRef)p, buf, sizeof(buf), kCFStringEncodingUTF8)) return buf;
	}
	return "Game controller";
}

void CollectElements(Device &dev)
{
	CFArrayRef els = IOHIDDeviceCopyMatchingElements(dev.ref, nullptr, kIOHIDOptionsTypeNone);
	if (!els) return;
	int nbutton = 0, nhat = 0, nslider = 0;
	bool axisUsed[8] = {};
	for (CFIndex i = 0; i < CFArrayGetCount(els); i++) {
		IOHIDElementRef e = (IOHIDElementRef)CFArrayGetValueAtIndex(els, i);
		IOHIDElementType t = IOHIDElementGetType(e);
		if (t != kIOHIDElementTypeInput_Misc && t != kIOHIDElementTypeInput_Button && t != kIOHIDElementTypeInput_Axis) continue;
		uint32_t page = IOHIDElementGetUsagePage(e), usage = IOHIDElementGetUsage(e);
		Element el{e, -1, -1, IOHIDElementGetLogicalMin(e), IOHIDElementGetLogicalMax(e)};
		if (page == kHIDPage_Button) {
			if (nbutton < 128) { el.kind = EL_BUTTON; el.index = nbutton++; }
		} else if (page == kHIDPage_GenericDesktop) {
			switch (usage) {
			case kHIDUsage_GD_X: el.kind = EL_AXIS; el.index = oplat::JOY_X; break;
			case kHIDUsage_GD_Y: el.kind = EL_AXIS; el.index = oplat::JOY_Y; break;
			case kHIDUsage_GD_Z: el.kind = EL_AXIS; el.index = oplat::JOY_Z; break;
			case kHIDUsage_GD_Rx: el.kind = EL_AXIS; el.index = oplat::JOY_RX; break;
			case kHIDUsage_GD_Ry: el.kind = EL_AXIS; el.index = oplat::JOY_RY; break;
			case kHIDUsage_GD_Rz: el.kind = EL_AXIS; el.index = oplat::JOY_RZ; break;
			case kHIDUsage_GD_Slider: case kHIDUsage_GD_Dial: case kHIDUsage_GD_Wheel:
				if (nslider < 2) { el.kind = EL_AXIS; el.index = oplat::JOY_SLIDER0 + nslider++; }
				break;
			case kHIDUsage_GD_Hatswitch:
				if (nhat < 4) { el.kind = EL_HAT; el.index = nhat++; }
				break;
			}
		} else if (page == kHIDPage_Simulation) {
			switch (usage) {
			case kHIDUsage_Sim_Throttle: case kHIDUsage_Sim_Accelerator:
				if (nslider < 2) { el.kind = EL_AXIS; el.index = oplat::JOY_SLIDER0 + nslider++; }
				break;
			case kHIDUsage_Sim_Rudder:
				if (!axisUsed[oplat::JOY_RZ]) { el.kind = EL_AXIS; el.index = oplat::JOY_RZ; }
				break;
			}
		}
		if (el.kind == EL_AXIS) {
			if (axisUsed[el.index]) continue;
			axisUsed[el.index] = true;
			dev.state.hasAxis[el.index] = true;
		}
		if (el.kind >= 0) dev.elements.push_back(el);
	}
	dev.state.nButtons = nbutton;
	dev.state.nPOV = nhat;
	CFRelease(els);
}

void DeviceAdded(void *, IOReturn, void *, IOHIDDeviceRef d)
{
	std::lock_guard<std::mutex> lk(g_lock);
	for (auto &dev : g_devices)
		if (dev.ref == d) return;
	Device dev{d, DeviceName(d), {}, {}};
	CollectElements(dev);
	g_devices.push_back(dev);
}

void DeviceRemoved(void *, IOReturn, void *, IOHIDDeviceRef d)
{
	std::lock_guard<std::mutex> lk(g_lock);
	for (size_t i = 0; i < g_devices.size(); i++) {
		if (g_devices[i].ref == d) {
			if (g_open == (int)i) g_open = -1;
			else if (g_open > (int)i) g_open--;
			g_devices.erase(g_devices.begin() + i);
			return;
		}
	}
}

CFDictionaryRef Match(uint32_t page, uint32_t usage)
{
	NSDictionary *d = @{ @kIOHIDDeviceUsagePageKey: @(page), @kIOHIDDeviceUsageKey: @(usage) };
	return (__bridge_retained CFDictionaryRef)d;
}

void EnsureManager()
{
	if (g_manager) return;
	g_manager = IOHIDManagerCreate(kCFAllocatorDefault, kIOHIDOptionsTypeNone);
	CFDictionaryRef m[] = {
		Match(kHIDPage_GenericDesktop, kHIDUsage_GD_Joystick),
		Match(kHIDPage_GenericDesktop, kHIDUsage_GD_GamePad),
		Match(kHIDPage_GenericDesktop, kHIDUsage_GD_MultiAxisController),
	};
	CFArrayRef arr = CFArrayCreate(kCFAllocatorDefault, (const void **)m, 3, &kCFTypeArrayCallBacks);
	IOHIDManagerSetDeviceMatchingMultiple(g_manager, arr);
	CFRelease(arr);
	for (auto d : m) CFRelease(d);
	IOHIDManagerRegisterDeviceMatchingCallback(g_manager, DeviceAdded, nullptr);
	IOHIDManagerRegisterDeviceRemovalCallback(g_manager, DeviceRemoved, nullptr);
	IOHIDManagerScheduleWithRunLoop(g_manager, CFRunLoopGetMain(), kCFRunLoopDefaultMode);
	IOHIDManagerOpen(g_manager, kIOHIDOptionsTypeNone);
	CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.05, false);
}

}

namespace oplat {

std::vector<std::string> EnumerateJoysticks()
{
	EnsureManager();
	std::lock_guard<std::mutex> lk(g_lock);
	std::vector<std::string> names;
	for (auto &d : g_devices) names.push_back(d.name);
	return names;
}

bool OpenJoystick(int index)
{
	EnsureManager();
	std::lock_guard<std::mutex> lk(g_lock);
	if (index < 0 || index >= (int)g_devices.size()) {
		g_open = -1;
		return false;
	}
	g_open = index;
	return true;
}

void CloseJoystick()
{
	std::lock_guard<std::mutex> lk(g_lock);
	g_open = -1;
}

bool PollJoystick(JoystickRaw &out)
{
	std::lock_guard<std::mutex> lk(g_lock);
	if (g_open < 0 || g_open >= (int)g_devices.size()) return false;
	Device &dev = g_devices[g_open];
	JoystickRaw &s = dev.state;
	for (int i = 0; i < 4; i++) s.pov[i] = -1;
	for (auto &el : dev.elements) {
		IOHIDValueRef v = nullptr;
		if (IOHIDDeviceGetValue(dev.ref, el.ref, &v) != kIOReturnSuccess || !v) continue;
		CFIndex iv = IOHIDValueGetIntegerValue(v);
		switch (el.kind) {
		case EL_AXIS: {
			double range = (double)(el.max - el.min);
			s.axis[el.index] = range > 0 ? (float)(2.0 * (iv - el.min) / range - 1.0) : 0.0f;
			break;
		}
		case EL_HAT: {
			CFIndex n = el.max - el.min + 1;
			CFIndex pos = iv - el.min;
			if (pos >= 0 && pos < n && (n == 8 || n == 4)) s.pov[el.index] = (int)(pos * 36000 / n);
			break;
		}
		case EL_BUTTON:
			s.buttons[el.index] = iv ? 0x80 : 0;
			break;
		}
	}
	out = s;
	return true;
}

}
