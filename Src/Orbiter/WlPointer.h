// not upstream: SetCursorPos for the camera's rotation mode on Wayland (pointer lock + relative motion)

#ifndef __WLPOINTER_H
#define __WLPOINTER_H

class QWindow;

void WlPointerAttach ();                 // before the render window shows: its enter event names the window's surface
void WlPointerDetach ();
void WlPointerDispatch ();               // the attached pointer's events; once per frame
bool WlPointerLock (QWindow *hWnd);      // lock the pointer where it is; false when not on Wayland or not supported
void WlPointerUnlock ();
bool WlPointerLocked ();
void WlPointerMotion (int &dx, int &dy); // motion since the last call, in the window's device pixels

#endif // !__WLPOINTER_H
