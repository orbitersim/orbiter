// not upstream: Dear ImGui platform backend for a Qt window (stands in for imgui_impl_win32)

#include "imgui_impl_qt.h"
#include <QClipboard>
#include <QCursor>
#include <QElapsedTimer>
#include <QFocusEvent>
#include <QGuiApplication>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QWindow>
#include <cfloat>

struct ImGui_ImplQt_Data {
	QWindow *Window = nullptr;
	QElapsedTimer Timer;
	qint64 Time = 0;
	ImGuiMouseCursor LastMouseCursor = ImGuiMouseCursor_COUNT;
	QByteArray ClipboardText;
};

static ImGui_ImplQt_Data *ImGui_ImplQt_GetBackendData ()
{
	return ImGui::GetCurrentContext() ? (ImGui_ImplQt_Data*)ImGui::GetIO().BackendPlatformUserData : nullptr;
}

static const char *ImGui_ImplQt_GetClipboardText (ImGuiContext*)
{
	ImGui_ImplQt_Data *bd = ImGui_ImplQt_GetBackendData();
	bd->ClipboardText = QGuiApplication::clipboard()->text().toUtf8();
	return bd->ClipboardText.constData();
}

static void ImGui_ImplQt_SetClipboardText (ImGuiContext*, const char *text)
{
	QGuiApplication::clipboard()->setText (QString::fromUtf8 (text));
}

bool ImGui_ImplQt_Init (QWindow *window)
{
	ImGuiIO &io = ImGui::GetIO();
	IMGUI_CHECKVERSION();
	IM_ASSERT(io.BackendPlatformUserData == nullptr && "Already initialized a platform backend!");

	ImGui_ImplQt_Data *bd = IM_NEW(ImGui_ImplQt_Data)();
	io.BackendPlatformUserData = (void*)bd;
	io.BackendPlatformName = "imgui_impl_qt";
	io.BackendFlags |= ImGuiBackendFlags_HasMouseCursors;
	io.BackendFlags |= ImGuiBackendFlags_HasSetMousePos;

	bd->Window = window;
	bd->Timer.start();
	bd->Time = 0;

	ImGuiPlatformIO &pio = ImGui::GetPlatformIO();
	pio.Platform_GetClipboardTextFn = ImGui_ImplQt_GetClipboardText;
	pio.Platform_SetClipboardTextFn = ImGui_ImplQt_SetClipboardText;

	ImGuiViewport *vp = ImGui::GetMainViewport();
	vp->PlatformHandle = vp->PlatformHandleRaw = (void*)window;
	return true;
}

void ImGui_ImplQt_Shutdown ()
{
	ImGui_ImplQt_Data *bd = ImGui_ImplQt_GetBackendData();
	IM_ASSERT(bd != nullptr && "No platform backend to shutdown, or already shutdown?");
	ImGuiIO &io = ImGui::GetIO();
	ImGuiPlatformIO &pio = ImGui::GetPlatformIO();
	pio.Platform_GetClipboardTextFn = nullptr;
	pio.Platform_SetClipboardTextFn = nullptr;
	io.BackendPlatformName = nullptr;
	io.BackendPlatformUserData = nullptr;
	io.BackendFlags &= ~(ImGuiBackendFlags_HasMouseCursors | ImGuiBackendFlags_HasSetMousePos);
	IM_DELETE(bd);
}

static void ImGui_ImplQt_UpdateMouseCursor (ImGui_ImplQt_Data *bd)
{
	ImGuiIO &io = ImGui::GetIO();
	if (io.ConfigFlags & ImGuiConfigFlags_NoMouseCursorChange) return;
	ImGuiMouseCursor cur = ImGui::GetMouseCursor();
	if (io.MouseDrawCursor) cur = ImGuiMouseCursor_None;
	if (cur == bd->LastMouseCursor) return;
	bd->LastMouseCursor = cur;
	Qt::CursorShape shape;
	switch (cur) {
	case ImGuiMouseCursor_None:       shape = Qt::BlankCursor; break;
	case ImGuiMouseCursor_TextInput:  shape = Qt::IBeamCursor; break;
	case ImGuiMouseCursor_ResizeAll:  shape = Qt::SizeAllCursor; break;
	case ImGuiMouseCursor_ResizeNS:   shape = Qt::SizeVerCursor; break;
	case ImGuiMouseCursor_ResizeEW:   shape = Qt::SizeHorCursor; break;
	case ImGuiMouseCursor_ResizeNESW: shape = Qt::SizeBDiagCursor; break;
	case ImGuiMouseCursor_ResizeNWSE: shape = Qt::SizeFDiagCursor; break;
	case ImGuiMouseCursor_Hand:       shape = Qt::PointingHandCursor; break;
	case ImGuiMouseCursor_Wait:       shape = Qt::WaitCursor; break;
	case ImGuiMouseCursor_Progress:   shape = Qt::BusyCursor; break;
	case ImGuiMouseCursor_NotAllowed: shape = Qt::ForbiddenCursor; break;
	default:                          shape = Qt::ArrowCursor; break;
	}
	bd->Window->setCursor (QCursor (shape));
}

void ImGui_ImplQt_NewFrame ()
{
	ImGui_ImplQt_Data *bd = ImGui_ImplQt_GetBackendData();
	IM_ASSERT(bd != nullptr && "Context or backend not initialized? Did you call ImGui_ImplQt_Init()?");
	ImGuiIO &io = ImGui::GetIO();

	// display size in logical pixels; the renderer scales by the framebuffer ratio
	io.DisplaySize = ImVec2 ((float)bd->Window->width(), (float)bd->Window->height());
	float dpr = (float)bd->Window->devicePixelRatio();
	io.DisplayFramebufferScale = ImVec2 (dpr, dpr);

	qint64 now = bd->Timer.nsecsElapsed();
	io.DeltaTime = (now > bd->Time ? (float)((now - bd->Time) * 1e-9) : 1.0f/60.0f);
	bd->Time = now;

	if (io.WantSetMousePos && bd->Window->isActive())
		QCursor::setPos (bd->Window->mapToGlobal (QPoint ((int)io.MousePos.x, (int)io.MousePos.y)));

	ImGui_ImplQt_UpdateMouseCursor (bd);
}

static ImGuiKey ImGui_ImplQt_KeyToImGuiKey (int key, bool keypad)
{
	if (keypad) {
		switch (key) {
		case Qt::Key_0: return ImGuiKey_Keypad0;
		case Qt::Key_1: return ImGuiKey_Keypad1;
		case Qt::Key_2: return ImGuiKey_Keypad2;
		case Qt::Key_3: return ImGuiKey_Keypad3;
		case Qt::Key_4: return ImGuiKey_Keypad4;
		case Qt::Key_5: return ImGuiKey_Keypad5;
		case Qt::Key_6: return ImGuiKey_Keypad6;
		case Qt::Key_7: return ImGuiKey_Keypad7;
		case Qt::Key_8: return ImGuiKey_Keypad8;
		case Qt::Key_9: return ImGuiKey_Keypad9;
		case Qt::Key_Period: return ImGuiKey_KeypadDecimal;
		case Qt::Key_Slash: return ImGuiKey_KeypadDivide;
		case Qt::Key_Asterisk: return ImGuiKey_KeypadMultiply;
		case Qt::Key_Minus: return ImGuiKey_KeypadSubtract;
		case Qt::Key_Plus: return ImGuiKey_KeypadAdd;
		case Qt::Key_Enter: return ImGuiKey_KeypadEnter;
		case Qt::Key_Equal: return ImGuiKey_KeypadEqual;
		default: break;
		}
	}
	if (key >= Qt::Key_A && key <= Qt::Key_Z) return (ImGuiKey)(ImGuiKey_A + (key - Qt::Key_A));
	if (key >= Qt::Key_0 && key <= Qt::Key_9) return (ImGuiKey)(ImGuiKey_0 + (key - Qt::Key_0));
	if (key >= Qt::Key_F1 && key <= Qt::Key_F24) return (ImGuiKey)(ImGuiKey_F1 + (key - Qt::Key_F1));
	switch (key) {
	case Qt::Key_Tab: case Qt::Key_Backtab: return ImGuiKey_Tab;
	case Qt::Key_Left: return ImGuiKey_LeftArrow;
	case Qt::Key_Right: return ImGuiKey_RightArrow;
	case Qt::Key_Up: return ImGuiKey_UpArrow;
	case Qt::Key_Down: return ImGuiKey_DownArrow;
	case Qt::Key_PageUp: return ImGuiKey_PageUp;
	case Qt::Key_PageDown: return ImGuiKey_PageDown;
	case Qt::Key_Home: return ImGuiKey_Home;
	case Qt::Key_End: return ImGuiKey_End;
	case Qt::Key_Insert: return ImGuiKey_Insert;
	case Qt::Key_Delete: return ImGuiKey_Delete;
	case Qt::Key_Backspace: return ImGuiKey_Backspace;
	case Qt::Key_Space: return ImGuiKey_Space;
	case Qt::Key_Return: return ImGuiKey_Enter;
	case Qt::Key_Enter: return ImGuiKey_KeypadEnter;
	case Qt::Key_Escape: return ImGuiKey_Escape;
	case Qt::Key_Apostrophe: return ImGuiKey_Apostrophe;
	case Qt::Key_Comma: return ImGuiKey_Comma;
	case Qt::Key_Minus: return ImGuiKey_Minus;
	case Qt::Key_Period: return ImGuiKey_Period;
	case Qt::Key_Slash: return ImGuiKey_Slash;
	case Qt::Key_Semicolon: return ImGuiKey_Semicolon;
	case Qt::Key_Equal: return ImGuiKey_Equal;
	case Qt::Key_BracketLeft: return ImGuiKey_LeftBracket;
	case Qt::Key_Backslash: return ImGuiKey_Backslash;
	case Qt::Key_BracketRight: return ImGuiKey_RightBracket;
	case Qt::Key_QuoteLeft: return ImGuiKey_GraveAccent;
	case Qt::Key_CapsLock: return ImGuiKey_CapsLock;
	case Qt::Key_ScrollLock: return ImGuiKey_ScrollLock;
	case Qt::Key_NumLock: return ImGuiKey_NumLock;
	case Qt::Key_Print: return ImGuiKey_PrintScreen;
	case Qt::Key_Pause: return ImGuiKey_Pause;
	case Qt::Key_Shift: return ImGuiKey_LeftShift;
	case Qt::Key_Control: return ImGuiKey_LeftCtrl;
	case Qt::Key_Alt: return ImGuiKey_LeftAlt;
	case Qt::Key_AltGr: return ImGuiKey_RightAlt;
	case Qt::Key_Meta: return ImGuiKey_LeftSuper;
	case Qt::Key_Menu: return ImGuiKey_Menu;
	default: return ImGuiKey_None;
	}
}

static void ImGui_ImplQt_UpdateKeyModifiers (Qt::KeyboardModifiers mod)
{
	ImGuiIO &io = ImGui::GetIO();
	io.AddKeyEvent (ImGuiMod_Ctrl, (mod & Qt::ControlModifier) != 0);
	io.AddKeyEvent (ImGuiMod_Shift, (mod & Qt::ShiftModifier) != 0);
	io.AddKeyEvent (ImGuiMod_Alt, (mod & Qt::AltModifier) != 0);
	io.AddKeyEvent (ImGuiMod_Super, (mod & Qt::MetaModifier) != 0);
}

static int ImGui_ImplQt_MouseButton (Qt::MouseButton b)
{
	switch (b) {
	case Qt::LeftButton:   return 0;
	case Qt::RightButton:  return 1;
	case Qt::MiddleButton: return 2;
	case Qt::BackButton:   return 3;
	case Qt::ForwardButton: return 4;
	default:               return -1;
	}
}

bool ImGui_ImplQt_EventHandler (QWindow *window, QEvent *event)
{
	if (!ImGui::GetCurrentContext()) return false;
	ImGui_ImplQt_Data *bd = ImGui_ImplQt_GetBackendData();
	if (!bd || window != bd->Window) return false;
	ImGuiIO &io = ImGui::GetIO();

	switch (event->type()) {
	case QEvent::MouseMove: {
		QMouseEvent *me = static_cast<QMouseEvent*> (event);
		io.AddMouseSourceEvent (ImGuiMouseSource_Mouse);
		io.AddMousePosEvent ((float)me->position().x(), (float)me->position().y());
		} return false;
	case QEvent::Leave:
		io.AddMousePosEvent (-FLT_MAX, -FLT_MAX);
		return false;
	case QEvent::MouseButtonPress:
	case QEvent::MouseButtonDblClick:
	case QEvent::MouseButtonRelease: {
		QMouseEvent *me = static_cast<QMouseEvent*> (event);
		int b = ImGui_ImplQt_MouseButton (me->button());
		if (b >= 0) {
			ImGui_ImplQt_UpdateKeyModifiers (me->modifiers());
			io.AddMouseSourceEvent (ImGuiMouseSource_Mouse);
			io.AddMousePosEvent ((float)me->position().x(), (float)me->position().y());
			io.AddMouseButtonEvent (b, event->type() != QEvent::MouseButtonRelease);
		}
		} return false;
	case QEvent::Wheel: {
		QWheelEvent *we = static_cast<QWheelEvent*> (event);
		QPoint d = we->angleDelta(); // 120 per notch, as WHEEL_DELTA
		io.AddMouseWheelEvent (-(float)d.x() / 120.0f, (float)d.y() / 120.0f);
		} return false;
	case QEvent::KeyPress:
	case QEvent::KeyRelease: {
		QKeyEvent *ke = static_cast<QKeyEvent*> (event);
		bool down = (event->type() == QEvent::KeyPress);
		ImGui_ImplQt_UpdateKeyModifiers (ke->modifiers());
		ImGuiKey key = ImGui_ImplQt_KeyToImGuiKey (ke->key(), (ke->modifiers() & Qt::KeypadModifier) != 0);
		if (key != ImGuiKey_None) {
			io.AddKeyEvent (key, down);
			io.SetKeyEventNativeData (key, ke->key(), (int)ke->nativeScanCode());
		}
		if (down && !ke->text().isEmpty()) { // WM_CHAR
			QByteArray utf8 = ke->text().toUtf8();
			if ((unsigned char)utf8[0] >= 0x20 && utf8[0] != 0x7f)
				io.AddInputCharactersUTF8 (utf8.constData());
		}
		} return false;
	case QEvent::FocusIn:
		io.AddFocusEvent (true);
		return false;
	case QEvent::FocusOut:
		io.AddFocusEvent (false);
		return false;
	default:
		return false;
	}
}
