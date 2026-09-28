// not upstream: KDE Plasma's global shortcuts take keys Orbiter uses (Alt+F1, Ctrl+F1-F4, Ctrl+F9), which Windows leaves
// to the program; the render window inhibits the desktop's shortcuts and passes on the ones Orbiter doesn't use

#ifndef __WLSHORTCUTS_H
#define __WLSHORTCUTS_H

class QKeyEvent;

void WlShortcutsAttach ();                           // before the render window shows; Plasma on Wayland only
void WlShortcutsDetach ();
bool WlShortcutsKey (QKeyEvent *e, bool orbiterKey); // render window key press/release; orbiterKey: the keymap uses it;
                                                     // true: the desktop's shortcut, passed on and not for Orbiter
void WlShortcutsReset ();                            // mouse button or focus change: no modifier-only shortcut

#endif // !__WLSHORTCUTS_H
