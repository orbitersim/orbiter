// not upstream: SetCursorPos for the camera's rotation mode on Wayland (pointer lock + relative motion)

#include "WlPointer.h"
#include <QGuiApplication>
#include <QWindow>
#include <cmath>
#include <cstring>
#include <wayland-client.h>
#include "pointer-constraints-unstable-v1-client-protocol.h"
#include "relative-pointer-unstable-v1-client-protocol.h"

namespace {

wl_display *display = nullptr;
wl_event_queue *queue = nullptr;               // own queue, dispatched here rather than by Qt
zwp_pointer_constraints_v1 *constraints = nullptr;
zwp_relative_pointer_manager_v1 *relmgr = nullptr;
wl_pointer *pointer = nullptr;                 // own pointer object on Qt's seat: its enter events name the surface
wl_surface *surface = nullptr;                 // the surface under the pointer
zwp_locked_pointer_v1 *locked = nullptr;
zwp_relative_pointer_v1 *relptr = nullptr;
double accx = 0.0, accy = 0.0;                 // motion not taken yet, in surface coordinates
qreal dpr = 1.0;
bool tried = false;

void Global (void*, wl_registry *reg, uint32_t name, const char *iface, uint32_t)
{
	if (!strcmp (iface, zwp_pointer_constraints_v1_interface.name))
		constraints = (zwp_pointer_constraints_v1*)wl_registry_bind (reg, name, &zwp_pointer_constraints_v1_interface, 1);
	else if (!strcmp (iface, zwp_relative_pointer_manager_v1_interface.name))
		relmgr = (zwp_relative_pointer_manager_v1*)wl_registry_bind (reg, name, &zwp_relative_pointer_manager_v1_interface, 1);
}
void GlobalRemove (void*, wl_registry*, uint32_t) {}
const wl_registry_listener registryListener = { Global, GlobalRemove };

void Enter (void*, wl_pointer*, uint32_t, wl_surface *s, wl_fixed_t, wl_fixed_t) { surface = s; }
void Leave (void*, wl_pointer*, uint32_t, wl_surface*) { surface = nullptr; } // a destroyed surface comes as NULL
void Motion (void*, wl_pointer*, uint32_t, wl_fixed_t, wl_fixed_t) {}
void Button (void*, wl_pointer*, uint32_t, uint32_t, uint32_t, uint32_t) {}
void Axis (void*, wl_pointer*, uint32_t, uint32_t, wl_fixed_t) {}
void Frame (void*, wl_pointer*) {}
void AxisSource (void*, wl_pointer*, uint32_t) {}
void AxisStop (void*, wl_pointer*, uint32_t, uint32_t) {}
void AxisDiscrete (void*, wl_pointer*, uint32_t, int32_t) {}
void AxisValue120 (void*, wl_pointer*, uint32_t, int32_t) {}
void AxisRelDir (void*, wl_pointer*, uint32_t, uint32_t) {}
const wl_pointer_listener pointerListener = { Enter, Leave, Motion, Button, Axis, Frame, AxisSource, AxisStop, AxisDiscrete, AxisValue120, AxisRelDir };

void RelMotion (void*, zwp_relative_pointer_v1*, uint32_t, uint32_t, wl_fixed_t dx, wl_fixed_t dy, wl_fixed_t, wl_fixed_t)
{
	accx += wl_fixed_to_double (dx); // accelerated, as the cursor moves
	accy += wl_fixed_to_double (dy);
}
const zwp_relative_pointer_v1_listener relListener = { RelMotion };

void Locked (void*, zwp_locked_pointer_v1*) {}
void Unlocked (void*, zwp_locked_pointer_v1*) {}
const zwp_locked_pointer_v1_listener lockListener = { Locked, Unlocked };

bool Init ()
{
	if (tried) return (constraints && relmgr);
	tried = true;
	if (!QGuiApplication::platformName().startsWith ("wayland")) return false;
	auto *app = qGuiApp->nativeInterface<QNativeInterface::QWaylandApplication>();
	if (!app || !app->display() || !app->seat()) return false;
	display = app->display();
	queue = wl_display_create_queue (display);
	wl_display *dpy = (wl_display*)wl_proxy_create_wrapper (display);
	wl_proxy_set_queue ((wl_proxy*)dpy, queue);
	wl_registry *reg = wl_display_get_registry (dpy);
	wl_proxy_wrapper_destroy (dpy);
	wl_registry_add_listener (reg, &registryListener, nullptr);
	wl_display_roundtrip_queue (display, queue);
	wl_registry_destroy (reg);
	return (constraints && relmgr);
}

} // namespace

void WlPointerAttach ()
{
	if (pointer || !Init ()) return;
	auto *app = qGuiApp->nativeInterface<QNativeInterface::QWaylandApplication>();
	wl_seat *seat = (wl_seat*)wl_proxy_create_wrapper (app->seat());
	wl_proxy_set_queue ((wl_proxy*)seat, queue);
	pointer = wl_seat_get_pointer (seat);
	wl_proxy_wrapper_destroy (seat);
	wl_pointer_add_listener (pointer, &pointerListener, nullptr);
	wl_display_flush (display);
}

void WlPointerDetach ()
{
	WlPointerUnlock ();
	if (pointer) {
		if (wl_pointer_get_version (pointer) >= WL_POINTER_RELEASE_SINCE_VERSION) wl_pointer_release (pointer);
		else wl_pointer_destroy (pointer);
		pointer = nullptr;
		wl_display_flush (display);
	}
	surface = nullptr;
}

void WlPointerDispatch ()
{
	if (queue) wl_display_dispatch_queue_pending (display, queue);
}

bool WlPointerLock (QWindow *hWnd)
{
	if (!hWnd || !pointer) return false;
	WlPointerDispatch ();
	if (!surface) return false;
	WlPointerUnlock ();
	dpr = hWnd->devicePixelRatio ();
	accx = accy = 0.0;
	locked = zwp_pointer_constraints_v1_lock_pointer (constraints, surface, pointer, nullptr, ZWP_POINTER_CONSTRAINTS_V1_LIFETIME_ONESHOT);
	zwp_locked_pointer_v1_add_listener (locked, &lockListener, nullptr);
	relptr = zwp_relative_pointer_manager_v1_get_relative_pointer (relmgr, pointer);
	zwp_relative_pointer_v1_add_listener (relptr, &relListener, nullptr);
	wl_display_flush (display);
	return true;
}

void WlPointerUnlock ()
{
	if (relptr) { zwp_relative_pointer_v1_destroy (relptr); relptr = nullptr; }
	if (locked) { zwp_locked_pointer_v1_destroy (locked); locked = nullptr; }
	if (display) wl_display_flush (display);
}

bool WlPointerLocked ()
{
	return (locked != nullptr);
}

void WlPointerMotion (int &dx, int &dy)
{
	dx = dy = 0;
	if (!locked) return;
	WlPointerDispatch ();
	dx = (int)std::trunc (accx * dpr);
	dy = (int)std::trunc (accy * dpr);
	accx -= dx / dpr; // the fraction carries over
	accy -= dy / dpr;
}
