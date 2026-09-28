// not upstream: KDE Plasma's global shortcuts take keys Orbiter uses (Alt+F1, Ctrl+F1-F4, Ctrl+F9), which Windows leaves
// to the program; the render window inhibits the desktop's shortcuts and passes on the ones Orbiter doesn't use

#include "WlShortcuts.h"
#include <QGuiApplication>
#include <QKeyEvent>
#include <QDBusArgument>
#include <QDBusConnection>
#include <QDBusConnectionInterface>
#include <QDBusMessage>
#include <QDBusMetaType>
#include <QDBusObjectPath>
#include <cstdlib>
#include <cstring>
#include <map>
#include <unistd.h>
#include <wayland-client.h>
#include "keyboard-shortcuts-inhibit-unstable-v1-client-protocol.h"

namespace {

const char *service = "org.kde.kglobalaccel";

wl_display *display = nullptr;
wl_event_queue *queue = nullptr;                 // own queue, dispatched here rather than by Qt
zwp_keyboard_shortcuts_inhibit_manager_v1 *manager = nullptr;
wl_keyboard *keyboard = nullptr;                 // own keyboard object on Qt's seat: its enter events name the surface
wl_surface *focus = nullptr;                     // the surface with the keyboard focus
wl_surface *inhibited = nullptr;
zwp_keyboard_shortcuts_inhibitor_v1 *inhibitor = nullptr;
bool active = false;                             // the compositor applies the inhibitor
bool tried = false;
int modOnly = 0;                                 // modifier key pressed alone: a modifier-only shortcut on its release

struct Action { QString path, name; };           // kglobalaccel component object and shortcut; empty path: none
std::map<int, Action> actions;                   // key combination (Qt key | modifiers) -> shortcut

void Global (void*, wl_registry *reg, uint32_t name, const char *iface, uint32_t)
{
	if (!strcmp (iface, zwp_keyboard_shortcuts_inhibit_manager_v1_interface.name))
		manager = (zwp_keyboard_shortcuts_inhibit_manager_v1*)wl_registry_bind (reg, name, &zwp_keyboard_shortcuts_inhibit_manager_v1_interface, 1);
}
void GlobalRemove (void*, wl_registry*, uint32_t) {}
const wl_registry_listener registryListener = { Global, GlobalRemove };

void Keymap (void*, wl_keyboard*, uint32_t, int32_t fd, uint32_t) { close (fd); }
void Enter (void*, wl_keyboard*, uint32_t, wl_surface *s, wl_array*) { focus = s; }
void Leave (void*, wl_keyboard*, uint32_t, wl_surface*) { focus = nullptr; }
void Key (void*, wl_keyboard*, uint32_t, uint32_t, uint32_t, uint32_t) {}
void Modifiers (void*, wl_keyboard*, uint32_t, uint32_t, uint32_t, uint32_t, uint32_t) {}
void RepeatInfo (void*, wl_keyboard*, int32_t, int32_t) {}
const wl_keyboard_listener keyboardListener = { Keymap, Enter, Leave, Key, Modifiers, RepeatInfo };

void Active (void*, zwp_keyboard_shortcuts_inhibitor_v1*) { active = true; }
void Inactive (void*, zwp_keyboard_shortcuts_inhibitor_v1*) { active = false; }
const zwp_keyboard_shortcuts_inhibitor_v1_listener inhibitorListener = { Active, Inactive };

bool Init ()
{
	if (tried) return (manager != nullptr);
	tried = true;
	if (!QGuiApplication::platformName().startsWith ("wayland")) return false;
	const char *desktop = getenv ("XDG_CURRENT_DESKTOP");
	if (!desktop || !strstr (desktop, "KDE")) return false; // the shortcuts go back through KDE's kglobalaccel
	QDBusConnectionInterface *bus = QDBusConnection::sessionBus().interface();
	if (!bus || !bus->isServiceRegistered (service)) return false;
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
	return (manager != nullptr);
}

// a key reached the render window, so the keyboard focus is on its surface
void Inhibit ()
{
	wl_display_dispatch_queue_pending (display, queue);
	if (!focus || focus == inhibited) return;
	if (inhibitor) zwp_keyboard_shortcuts_inhibitor_v1_destroy (inhibitor);
	auto *app = qGuiApp->nativeInterface<QNativeInterface::QWaylandApplication>();
	inhibitor = zwp_keyboard_shortcuts_inhibit_manager_v1_inhibit_shortcuts (manager, focus, app->seat());
	zwp_keyboard_shortcuts_inhibitor_v1_add_listener (inhibitor, &inhibitorListener, nullptr);
	inhibited = focus;
	active = false;
	wl_display_flush (display);
}

void Invoke (const Action &a)
{
	if (a.path.isEmpty()) return;
	QDBusMessage m = QDBusMessage::createMethodCall (service, a.path, "org.kde.kglobalaccel.Component", "invokeShortcut");
	m << a.name;
	QDBusConnection::sessionBus().send (m);
}

QDBusMessage Call (const QString &path, const char *iface, const char *method)
{
	QDBusMessage m = QDBusMessage::createMethodCall (service, path, iface, method);
	return QDBusConnection::sessionBus().call (m, QDBus::Block, 1000);
}

// the shortcuts of kglobalaccel's active components, read at the start of a session
void Load ()
{
	actions.clear();
	QDBusMessage r = Call ("/kglobalaccel", "org.kde.KGlobalAccel", "allComponents");
	if (r.type() != QDBusMessage::ReplyMessage || r.arguments().isEmpty()) return;
	for (const QDBusObjectPath &p : qdbus_cast<QList<QDBusObjectPath>> (r.arguments().at(0))) {
		QDBusMessage a = Call (p.path(), "org.kde.kglobalaccel.Component", "isActive");
		if (a.type() != QDBusMessage::ReplyMessage || a.arguments().isEmpty() || !a.arguments().at(0).toBool()) continue;
		QDBusMessage s = Call (p.path(), "org.kde.kglobalaccel.Component", "allShortcutInfos");
		if (s.type() != QDBusMessage::ReplyMessage || s.arguments().isEmpty()) continue;
		const QDBusArgument arg = s.arguments().at(0).value<QDBusArgument>();
		arg.beginArray();
		while (!arg.atEnd()) {                   // name, friendly name, component and context names, keys, default keys
			QString str[6];
			QList<int> keys, defkeys;
			arg.beginStructure();
			for (QString &x : str) arg >> x;
			arg >> keys >> defkeys;
			arg.endStructure();
			for (int k : keys) if (k) actions.emplace (k, Action{ p.path(), str[0] });
		}
		arg.endArray();
	}
}

// the desktop's shortcut for a key combination; false if there is none
bool PassOn (int combo)
{
	auto it = actions.find (combo);
	if (it == actions.end()) return false;
	Invoke (it->second);
	return true;
}

bool IsModifierKey (int key)
{
	return (key == Qt::Key_Shift || key == Qt::Key_Control || key == Qt::Key_Meta || key == Qt::Key_Alt || key == Qt::Key_AltGr);
}

} // namespace

void WlShortcutsAttach ()
{
	if (keyboard || !Init ()) return;
	auto *app = qGuiApp->nativeInterface<QNativeInterface::QWaylandApplication>();
	wl_seat *seat = (wl_seat*)wl_proxy_create_wrapper (app->seat());
	wl_proxy_set_queue ((wl_proxy*)seat, queue);
	keyboard = wl_seat_get_keyboard (seat);
	wl_proxy_wrapper_destroy (seat);
	wl_keyboard_add_listener (keyboard, &keyboardListener, nullptr);
	wl_display_flush (display);
	Load ();
}

void WlShortcutsDetach ()
{
	if (inhibitor) { zwp_keyboard_shortcuts_inhibitor_v1_destroy (inhibitor); inhibitor = nullptr; }
	if (keyboard) {
		if (wl_keyboard_get_version (keyboard) >= WL_KEYBOARD_RELEASE_SINCE_VERSION) wl_keyboard_release (keyboard);
		else wl_keyboard_destroy (keyboard);
		keyboard = nullptr;
	}
	if (display) wl_display_flush (display);
	focus = inhibited = nullptr;
	active = false;
	modOnly = 0;
	actions.clear();                             // the desktop's shortcuts may change until the next session
}

bool WlShortcutsKey (QKeyEvent *e, bool orbiterKey)
{
	if (!keyboard) return false;
	Inhibit ();
	if (!active) return false;                   // not yet or no longer applied: the desktop gets its shortcuts itself

	const Qt::KeyboardModifiers modmask = Qt::ShiftModifier | Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier;
	int key = e->key();
	if (key == Qt::Key_Super_L || key == Qt::Key_Super_R) key = Qt::Key_Meta;
	int mods = (int)(e->modifiers() & modmask);
	bool press = (e->type() == QEvent::KeyPress);

	if (IsModifierKey (key)) {                   // kglobalaccel: a modifier pressed and released alone
		int own = (key == Qt::Key_Shift ? Qt::ShiftModifier : key == Qt::Key_Control ? Qt::ControlModifier :
		           key == Qt::Key_Meta ? Qt::MetaModifier : Qt::AltModifier);
		if (e->isAutoRepeat()) return false;
		if (press) modOnly = ((mods & ~own) ? 0 : key);
		else {
			if (modOnly == key) PassOn (key);
			modOnly = 0;
		}
		return false;                            // Orbiter sees the modifier keys as before
	}

	if (!press) return false;
	modOnly = 0;
	if (orbiterKey && !(mods & Qt::MetaModifier)) return false; // the keymap has no Meta (Windows key) modifier
	if (key == Qt::Key_Backtab) key = Qt::Key_Tab; // Shift+Tab is stored as Tab with Shift
	auto it = actions.find (key | mods);
	if (it == actions.end()) return false;
	if (!e->isAutoRepeat() || !(mods & (Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier)))
		Invoke (it->second);                     // a held combination once, held media keys repeat
	return true;                                 // the desktop's key: Orbiter doesn't get it, as without the inhibitor
}

void WlShortcutsReset ()
{
	modOnly = 0;
}
