// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define EXPORT_IMGUI_CONTEXT
#define OAPI_IMPLEMENTATION
#define IMGUI_DEFINE_MATH_OPERATORS
#include <stdio.h>
#include "OrbiterAPI.h"
#include "DlgMgr.h"
#include "Resource.h"
#include "Orbiter.h"
#include "Log.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "imgui_impl_win32.h"
#include "imgui_impl_sdl3.h"
#include "IconsFontAwesome6.h"
#include <chrono>
#include <algorithm>
#include <vector>
#include <string>

using namespace oapi;

extern char DBG_MSG[256];
extern Orbiter *g_pOrbiter;

static int x_sizeframe = GetSystemMetrics (SM_CXSIZEFRAME);
static int y_sizeframe = GetSystemMetrics (SM_CYSIZEFRAME);
static int x_fixedframe = GetSystemMetrics (SM_CXFIXEDFRAME);
static int y_fixedframe = GetSystemMetrics (SM_CYFIXEDFRAME);

static bool doflip = true;
static void RenderNotifications();

// dialog thread messages
#define TM_OPENDIALOG WM_USER
#define DLG_CAPTIONBUTTON (DLG_CAPTIONCLOSE|DLG_CAPTIONHELP)

struct THREADDATA {
	HINSTANCE hInst;
	HWND hParent;
	int id;
	DWORD flag;
	DLGPROC procDlg;
	void *context;
} g_tdata;

HWND g_hDlg;       // dialog handle passed from dlg to main thread
HANDLE hCreateDlg; // used for synchronising dialog creation
static DIALOGENTRY *de_create = 0;

// ==================================================================
// class DialogManager

DialogManager::DialogManager (Orbiter *orbiter, HWND hAppWnd) : ImCtxBase(orbiter, ImGui::CreateContext())
{
	pOrbiter = orbiter;
	gc = orbiter->GetGraphicsClient();
	hWnd = hAppWnd;
	nEntry = 0;
	nList = nListBuf = 0;
	firstEntry = NULL;
	lastEntry = NULL;
	InitImGui();
}

DialogManager::~DialogManager ()
{
	Clear();
	ShutdownImGui();
}


void DialogManager::Init (HWND hAppWnd)
{
	Clear();
	hWnd        = hAppWnd;
}


void DialogManager::Clear ()
{
	DIALOGENTRY *tmp;
	while (firstEntry) {
		tmp = firstEntry;
		firstEntry = firstEntry->next;
		delete tmp->dlg;
		delete tmp;
	}
	lastEntry = NULL;
	if (nListBuf) {
		delete []DlgList;
		DlgList = NULL;
		nListBuf = 0;
	}
	nList = 0;

	nEntry = 0;
}

// =======================================================================

HWND DialogManager::OpenDialogEx (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context)
{
	if ((flag & DLG_ALLOWMULTI) == 0)
		if (IsEntry (hInst, id)) return NULL; // already open, and multiple instances not allowed
	return AddEntry (hInst, id, hParent, pDlg, flag, context);
}

void DialogManager::OpenDialogAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context)
{
	if ((flag & DLG_ALLOWMULTI) == 0)
		if (IsEntry (hInst, id)) return; // already open, and multiple instances not allowed
	AddEntryAsync (hInst, id, hParent, pDlg, flag, context);
}

bool DialogManager::CloseDialog (HWND hDlg)
{
	if (DelEntry (hDlg, 0, 0)) {
		//DestroyWindow (hDlg);
		return true;
	}
	return false;
}

void *DialogManager::GetDialogContext (HWND hDlg)
{
	DialogWin *dlg = (DialogWin*)GetWindowLongPtr (hDlg, DWLP_USER);
	return (dlg ? dlg->GetContext() : 0);
}

DIALOGENTRY *DialogManager::AddWindow (HINSTANCE hInst, HWND hWnd, HWND hParent, DWORD flag)
{
	DIALOGENTRY *tmp = new DIALOGENTRY; TRACENEW
	de_create = tmp;
	tmp->dlg = new DialogWin (hInst, hWnd, hParent, flag);

	tmp->prev = lastEntry;
	tmp->next = NULL;

	if (lastEntry)
		lastEntry->next = tmp;
	else
		firstEntry = tmp;

	lastEntry = tmp;
	nEntry++;

	de_create = 0;
	AddList (tmp->dlg->GetHwnd());

	return tmp;
}

HWND DialogManager::AddEntry (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context)
{
	return AddEntry (new DialogWin (hInst, hParent, id, pDlg, flag, context));
}

HWND DialogManager::AddEntry (DialogWin *dlg)
{
	DIALOGENTRY *tmp = new DIALOGENTRY; TRACENEW
	de_create = tmp;
	tmp->dlg = dlg;
	tmp->prev = lastEntry;
	tmp->next = NULL;
	if (lastEntry)
		lastEntry->next = tmp;
	else
		firstEntry = tmp;
	lastEntry = tmp;
	nEntry++;
	de_create = 0;
	HWND hWnd = tmp->dlg->OpenWindow(); // is this the best place to create the window?
	AddList (hWnd);
	return hWnd;
}

void DialogManager::AddEntryAsync (HINSTANCE hInst, int id, HWND hParent, DLGPROC pDlg, DWORD flag, void *context)
{
#ifdef USEDLGTHREAD
	g_tdata.hInst = hInst;
	g_tdata.hParent = hParent;
	g_tdata.id = id;
	g_tdata.flag = flag;
	g_tdata.procDlg = pDlg;
	g_tdata.context = context;
	PostThreadMessage (thid, TM_OPENDIALOG, (WPARAM)&g_tdata, 0);
#else
	AddEntry (hInst, id, hParent, pDlg, flag, context);
#endif
}

bool DialogManager::DelEntry (HWND hDlg, HINSTANCE hInst, int id)
{
	DIALOGENTRY *tmp;
	for (tmp = firstEntry; tmp; tmp = tmp->next) {
		DialogWin *dlg = tmp->dlg;
		if (hDlg && hDlg != dlg->GetHwnd()) continue;
		if (hInst && hInst != dlg->GetHinst()) continue;
		if (id && id != dlg->GetResId()) continue;
		delete dlg;
		DelList (hDlg);
		if (tmp == firstEntry) firstEntry = tmp->next;
		if (tmp == lastEntry)  lastEntry  = tmp->prev;
		if (tmp->prev) tmp->prev->next = tmp->next;
		if (tmp->next) tmp->next->prev = tmp->prev;
		delete tmp;
		nEntry--;
		return true;
	}
	return false;
}

void DialogManager::AddList (HWND hWnd)
{
	if (nList == nListBuf) { // grow buffer
		HWND *tmp = new HWND[nListBuf += 16];
		if (nList) {
			memcpy (tmp, DlgList, nList*sizeof(HWND));
			delete []DlgList;
		}
		DlgList = tmp;
	}
	DlgList[nList++] = hWnd;
}

void DialogManager::DelList (HWND hWnd)
{
	DWORD i;
	for (i = 0; i < nList; i++) {
		if (DlgList[i] == hWnd) break;
	}
	if (i < nList) {
		for (; i < nList-1; i++)
			DlgList[i] = DlgList[i+1];
		DlgList[i] = NULL;
		nList--;
	}
}

HWND DialogManager::GetNextEntry (HWND hWnd) const
{
	if (!hWnd) {
		if (firstEntry) {
			searchEntry = firstEntry;
			return firstEntry->dlg->GetHwnd();
		} else {
			searchEntry = NULL;
			return NULL;
		}
	} else {
		if (!searchEntry || searchEntry->dlg->GetHwnd() != hWnd) {
			for (searchEntry = firstEntry; searchEntry && searchEntry->dlg->GetHwnd() != hWnd; searchEntry = searchEntry->next);
		}
		if (searchEntry) searchEntry = searchEntry->next;
		return (searchEntry ? searchEntry->dlg->GetHwnd() : NULL);
	}
}

HWND DialogManager::IsEntry (HINSTANCE hInst, int id)
{
	DIALOGENTRY *tmp = firstEntry;
	while (tmp) {
		DialogWin *dlg = tmp->dlg;
		if (dlg->GetHinst() == hInst && dlg->GetResId() == id)
			return tmp->dlg->GetHwnd();
		tmp = tmp->next;
	}
	return 0;
}

bool DialogManager::AddTitleButton (DWORD msg, HBITMAP hBmp, DWORD flag)
{
	return DialogWin::Create_AddTitleButton (msg, hBmp, flag);
}

DWORD DialogManager::GetTitleButtonState (HWND hDlg, DWORD msg)
{
	DIALOGENTRY *tmp;
	for (tmp = firstEntry; tmp; tmp = tmp->next)
		if (hDlg == tmp->dlg->GetHwnd())
			return tmp->dlg->GetTitleButtonState (msg);
	return 0;
}

bool DialogManager::SetTitleButtonState (HWND hDlg, DWORD msg, DWORD state)
{
	state = (state ? 1:0);
	if (DialogWin::Create_SetTitleButtonState (msg, state)) return true;

	DIALOGENTRY *de;
	for (de = firstEntry; de; de = de->next)
		if (hDlg == de->dlg->GetHwnd())
			return de->dlg->SetTitleButtonState (msg, state);
	return false;
}

void DialogManager::UpdateDialogs ()
{
	for (DIALOGENTRY *tmp = firstEntry; tmp; tmp = tmp->next) {
		if (tmp->dlg->UpdateContinuously())
			tmp->dlg->Update();
	}
}

void DialogManager::BroadcastMessage (DWORD msg, void *data)
{
	for (DIALOGENTRY *tmp = firstEntry; tmp; tmp = tmp->next)
		tmp->dlg->Message (msg, data);
}

//-----------------------------------------------------------------------------
// Name: OrbiterDefDialogProc()
// Desc: Default message handler for orbiter dialog boxes
//-----------------------------------------------------------------------------
INT_PTR OrbiterDefDialogProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_SETCURSOR:
		// implements "focus follows mouse" behaviour
		if (!g_pOrbiter->StickyFocus() && g_pOrbiter->Cfg()->CfgUIPrm.MouseFocusMode == 2 && GetFocus() != hDlg &&
			!IsChild (hDlg, GetFocus()) && GetParent (hDlg) == g_pOrbiter->GetRenderWnd()->Win32Handle()) {
				SetFocus (hDlg);
				return FALSE;
		}
		break;
	case WM_NCLBUTTONDBLCLK: {
		// implements window minimisation on title bar double-click
		DialogWin *dlg = (DialogWin*)GetWindowLongPtr (hDlg, DWLP_USER);
		if (dlg) dlg->ToggleShrink();
		} break;

	// *** Create a timer to force frame updates during dialog box moves ***
	case WM_ENTERSIZEMOVE:
		SetTimer (hDlg, 0xff, 1, NULL);
		return 0;
	case WM_EXITSIZEMOVE:
		KillTimer (hDlg, 0xff);
		return 0;
	case WM_TIMER:
		if (wParam == 0xff) g_pOrbiter->SingleFrame();
		return 0;

	// *** Provide custom buttons in the window title bar ***
//	case WM_SETTEXT:
//	case WM_ACTIVATE:
//	case WM_NCPAINT:
//		PostMessage (hDlg, WM_USER+2, 0, 0);
//		break;
//	case WM_USER+2: {
//		DialogWin *dlg = (DialogWin*)GetWindowLongPtr (hDlg, DWLP_USER);
//		if (dlg) dlg->PaintTitleButtons();
//		} return 0;
//	case WM_NCLBUTTONDOWN:
//		if (wParam == HTCAPTION) {
//			DialogWin *dlg = (DialogWin*)GetWindowLongPtr (hDlg, DWLP_USER);
//			if (dlg) dlg->CheckTitleButtons (MAKEPOINTS (lParam));
//		}
//		return 0;
	//case WM_CTLCOLORDLG: {
	//	HDC hDC = (HDC)wParam;
	//	SetBkColor (hDC, 0x808080);
	//	} return (INT_PTR)GetStockObject (GRAY_BRUSH);
	}
	return FALSE;
}

// ====================================================================
// Tread management for dialog thread
// ====================================================================

void DialogManager::StartDialogThread ()
{
	DWORD WINAPI DlgThreadProc (void *data);
	hThread = CreateThread (NULL, 2048, DlgThreadProc, this, 0, &thid);
	hCreateDlg = CreateEvent (NULL, FALSE, FALSE, NULL);
}

void DialogManager::DestroyDialogThread ()
{
	PostThreadMessage (thid, WM_QUIT, 0, 0);
	CloseHandle (hThread);
	CloseHandle (hCreateDlg);
}

DWORD WINAPI DlgThreadProc (void *data)
{
	DialogManager *dlgmgr = (DialogManager*)data;
	MSG msg;
	void DoDialog (THREADDATA *tdata);

	while (GetMessage (&msg, NULL, 0, 0)) {
		switch (msg.message) {
		case TM_OPENDIALOG: {
			THREADDATA *tdata = (THREADDATA*)msg.wParam;
			dlgmgr->AddEntry (tdata->hInst, tdata->id, tdata->hParent, tdata->procDlg, tdata->flag, tdata->context);
			} break;
		default:
			TranslateMessage (&msg);
			DispatchMessage (&msg);
			break;
		}
	}

	return 0;
}

// ====================================================================
// End tread management
// ====================================================================
// 
// ====================================================================
// ImGui
// ====================================================================

DLLEXPORT ImGuiContext* GImGui = NULL;

const ImWchar* GetGlyphRangesOrbiter()
{
	static const ImWchar ranges[] =
	{
		0x0020, 0x00FF, // Basic Latin + Latin Supplement
		0x00A0, 0x02D9, // Polish characters 
		0x0393, 0x03C2, // Greek characters
		0x221A, 0x221A, // √
		0x222B, 0x222B, // ∫
		0x2260, 0x2264, // ≠ ≤ ≥
		0x02DD, 0x02DD, // ˝
		0,
	};
	return &ranges[0];
}

void DialogManager::InitImGui()
{
	if(!gc) return;

    WithImCtx _ = PushLocal();
	ImGuiIO& io = ImGui::GetIO();
	// Viewports don't play nice when in full screen mode
	if(!pOrbiter->IsFullscreen())
		io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	//io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls
	//io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;

    ImGui_ImplSDL3_InitForOther(g_pOrbiter->GetRenderWnd()->Inner());
	gc->clbkImGuiInit();
}

void DialogManager::ShutdownImGui()
{
	if(!gc) return;

    WithImCtx _ = PushLocal();
	gc->clbkImGuiShutdown();
    ImGui_ImplSDL3_Shutdown();
}

static bool EventIsKeyboard(Uint32 type) {
    return type >= SDL_EVENT_KEY_UP && type < SDL_EVENT_MOUSE_MOTION;
}

static bool EventIsMouse(Uint32 type) {
    return type >= SDL_EVENT_MOUSE_MOTION &&
           type < SDL_EVENT_JOYSTICK_AXIS_MOTION;
}

bool DialogManager::ConsumeEvent(const SDL_Event &event, bool &wantsOut) {
    bool consumed = false;
    WithImCtx _ = PushLocal();
    ImGui_ImplSDL3_ProcessEvent(&event);
    ImGuiIO &io = ImGui::GetIO();
    if ((io.WantCaptureMouse && EventIsMouse(event.type)) ||
        (io.WantCaptureKeyboard && EventIsKeyboard(event.type))) {
        consumed = true;
    }
    // close requested are ignored TBD, until this uses Win32

    return consumed;
}

bool DialogManager::BeginFrame() {
    WithImCtx _ = PushLocal();
    gc->clbkImGuiNewFrame();
    ImGui_ImplWin32_NewFrame();
    ImGui::NewFrame();

    //ImGui::ShowDemoWindow();

    // Render notifications
    ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 5.f); // Round borders
    //	ImGui::PushStyleColor(ImGuiCol_WindowBg, ImVec4(43.f / 255.f, 43.f / 255.f, 43.f / 255.f, 240.f / 255.f)); // Background color
    RenderNotifications(); // <-- Here we render all notifications
    ImGui::PopStyleVar(1); // Don't forget to Pop()
    //	ImGui::PopStyleColor(1);

    // We can't use a range-based loop here because Show() may unregister the current dialog
    for (auto it = DlgImGuiList.begin(); it != DlgImGuiList.end();)
    {
        auto current = it++;
        if ((*current)->IsActive()) {
            (*current)->Display();
        }
    }
    return true;
}

void DialogManager::EndFrame() {
    WithImCtx _ = PushLocal();
    ImGui::EndFrame();
}

void DialogManager::ImGuiNewFrame()
{
	if(!gc) return;
    WithImCtx _ = PushLocal();
    if (BeginFrame()) {
        EndFrame();
    }
}

ImFont *DialogManager::GetFont(ImGuiFont f)
{
	switch(f) {
	case ImGuiFont::MONO: return MonoFont();
	case ImGuiFont::DEFAULT: return DefaultFont();
	default: return DefaultFont();
	}
}

ImGuiDialog::~ImGuiDialog(){
	// Make sure this dialog is no longer referenced in the DialogManager
	oapiCloseDialog(this);
}

bool ImGuiDialog::HandleHelpButton() {
	if(!helpfile.empty()) {
		HELPCONTEXT hc;
		hc.helpfile = const_cast<char *>(helpfile.c_str());
		hc.topic = helptopic.empty() ? NULL : const_cast<char *>(helptopic.c_str());
		hc.toc = (char*)"html/orbiter.chm::/orbiter.hhc";
		hc.index = (char*)"html/orbiter.chm::/orbiter.hhk";

		if(ImGui::MenuButton(ICON_FA_CIRCLE_QUESTION, "Help")) {
			g_pOrbiter->OpenHelp(&hc);
		}
		return true;
	}
	return false;
}

void ImGuiDialog::Display() {
    ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);

	if(ImGui::Begin(name.c_str(), &active)) {
		HandleHelpButton();
		OnDraw();
	}
	ImGui::End();
	if (!active) OnClose();
}

/* 
Notification handling, borrowed heavily from https://github.com/patrickcjk/imgui-notify
Added:
- permanent discardable notifications
- copy text to clipboard (permanent notifications)
- deduplication
- fall animation
- limit notification box to the main viewport

MIT License

Copyright (c) 2021 Patrick

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

using namespace std::chrono_literals;
using duration_t = std::chrono::duration<double>;
using time_point_t = std::chrono::time_point<std::chrono::steady_clock, duration_t>;

const float NOTIFY_PADDING_X = 20.f;            // Bottom-left X padding
const float NOTIFY_PADDING_Y = 20.f;            // Bottom-left Y padding
const float NOTIFY_PADDING_MESSAGE_Y = 10.f;    // Padding Y between each message
const uint32_t NOTIFICATION_FLAGS = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoNav | ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoFocusOnAppearing;
const duration_t FADE_TIME = 0.5s;
static const ImVec4 notifcolors[4] = {
	{ 0, 255, 0, 255 },     // Success
	{ 255, 255, 0, 255 },   // Warning
	{ 255, 0, 0, 255 },     // Error
	{ 0, 157, 255, 255 }    // Info
};

static const char *notificons[4] = {
	ICON_FA_CIRCLE_CHECK,         // Success
	ICON_FA_TRIANGLE_EXCLAMATION, // Warning
	ICON_FA_CIRCLE_EXCLAMATION,	  // Error
	ICON_FA_CIRCLE_INFO			  // Info
};

static duration_t notifduration[4] = {
	3.0s,  // Success
	10.0s, // Warning
	86400.0s, // Error - basically permanent and needs to be acknowledged
	7.0s   // Info
};


struct Notification
{
	uint32_t occurrences;
	std::string title;
	std::string content;
	size_t hash;
	ImVec4 color;
	bool expired = false;
	bool permanent = false;
	const char *icon;
	duration_t duration;
	time_point_t creation;
	float height;
	float speed;

	Notification(uint32_t type, const char *title_, const char *content_, size_t hash_):title(title_),content(content_),hash(hash_)
	{
		if(type >= OAPINOTIF_INFO) type = OAPINOTIF_INFO;
		color = notifcolors[type];
		icon = notificons[type];
		duration = notifduration[type];
		creation = std::chrono::steady_clock::now();
		occurrences = 1;
		expired = false;
		permanent = type == OAPINOTIF_ERROR;
		speed = 1.0f;
		height = -1.0;
	}
	void ReTrigger() {
		occurrences++;
		creation = std::chrono::steady_clock::now() - FADE_TIME;
		duration = 86400.0s;
	}
	void UpdateState(time_point_t now, float h, duration_t dt) {
		duration_t elapsed = now - creation;
		// Update alpha/expiration base on elapsed time
		if(elapsed > FADE_TIME + duration + FADE_TIME) { // expired
			color.w = 0.0f;
			expired = true;
		} else if(elapsed < FADE_TIME) { // appearing
			color.w = elapsed / FADE_TIME;
		} else if(elapsed > FADE_TIME + duration) { // disappearing
			color.w = 1.0f - (elapsed - FADE_TIME - duration) / FADE_TIME;
		} else { // steady
			color.w = 1.0; 
		}

		if(h < height) {
			height -= speed;
			speed += std::chrono::duration_cast<std::chrono::seconds>(dt).count() * 0.001;
			if(height <= h) {
				height = h;
				speed = 0.0f;
			}
		} else {
			height = h;
			speed = 0.0f;
		}
	}
};

static std::vector<Notification> notifications;

static void RenderNotifications()
{
	ImVec2 vp_size = ImGui::GetMainViewport()->Size;
	vp_size.x += ImGui::GetMainViewport()->Pos.x;
	vp_size.y += ImGui::GetMainViewport()->Pos.y;
	float height = 0.0f;

	int i = 0;
	static time_point_t last = std::chrono::steady_clock::now();
	auto now = std::chrono::steady_clock::now();
	duration_t dt = now - last;
	for (auto &notif: notifications)
	{
		notif.UpdateState(now, height, dt);

		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, notif.color.w);
		ImGui::SetNextWindowPos(ImVec2(vp_size.x - NOTIFY_PADDING_X, vp_size.y - NOTIFY_PADDING_Y - notif.height), ImGuiCond_Always, ImVec2(1.0f, 1.0f));

		// Generate new unique name for this toast
		char window_name[32];
		sprintf(window_name, "##NOTIF%d", i);
		i++;
		ImGuiWindowFlags wf = NOTIFICATION_FLAGS;
		if(notif.permanent) {
			wf &= ~ImGuiWindowFlags_NoInputs;
		}

		// Prevent the notification from spawning outside the main window
		ImGui::SetNextWindowViewport(ImGui::GetMainViewport()->ID);
		ImGui::Begin(window_name, NULL, wf);

		ImGui::PushTextWrapPos(vp_size.x / 2.0f);

		ImGui::PushStyleColor(ImGuiCol_Text, notif.color);
		ImGui::TextUnformatted(notif.icon);
		ImGui::PopStyleColor();

		ImGui::SameLine();
		if(notif.occurrences > 1) {
			ImGui::Text("%s (x%d)", notif.title.c_str(), notif.occurrences);
		} else {
			ImGui::TextUnformatted(notif.title.c_str());
		}

		if(notif.permanent)
		{
			ImGui::SameLine();
			if (ImGui::SmallButton(ICON_FA_XMARK))
			{
				// Change duration so that the notification will expire right now;
				notif.duration = now - notif.creation - FADE_TIME;
			}
		}

		// In case ANYTHING was rendered in the top, we want to add a small padding so the text (or icon) looks centered vertically
		if (!notif.content.empty())
		{
			ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 5.0f); // Must be a better way to do this!!!!
			ImGui::Separator();
			ImGui::TextUnformatted(notif.content.c_str()); // Render content text

			// Allow copying error messages to the clipboard
			if(notif.permanent) {
				char popup_name[32];
				sprintf(popup_name, "##POPUP%d", i);

				if (ImGui::BeginPopupContextItem(popup_name))
				{
					if (ImGui::MenuItem("Copy")) {
						ImGui::SetClipboardText(notif.content.c_str());
					}
					ImGui::EndPopup();
				}
			}
		}

		ImGui::PopTextWrapPos();

		ImGui::PopStyleVar();
		// Save height for next notification
		height += ImGui::GetWindowHeight() + NOTIFY_PADDING_MESSAGE_Y;

		// End
		ImGui::End();
	}
    // Remove expired notifications
    notifications.erase(std::remove_if(notifications.begin(), notifications.end(),
										[=](auto &notif){return notif.expired;}),
						notifications.end());

}


// OAPI implementation
DLLEXPORT void oapiAddNotification(int type, const char *title, const char *content) {
	const size_t hash = std::hash<std::string>{}(title) ^ std::hash<std::string>{}(content);
	for (auto &notif: notifications) {
		if(notif.hash == hash && notif.title == title && notif.content == content) {
			notif.ReTrigger();
			return;
		}
	}
	notifications.emplace_back(type, title, content, hash);
}

#include "imgui_internal.h"

// ImGui utils
namespace ImGui {
	// Resettable slider from https://github.com/ocornut/imgui/issues/1751
	DLLEXPORT bool SliderFloatReset(const char* label, float* v, float v_min, float v_max, float v_default, const char* display_format)
	{
		bool ret = ImGui::SliderFloat(label, v, v_min, v_max, display_format);
		if (ImGui::BeginPopupContextItem(label))
		{
			char buf[64];
			sprintf(buf, "Reset to %f", v_default);
			if (ImGui::MenuItem(buf))
				*v = v_default;
			ImGui::MenuItem("Close");
			ImGui::EndPopup();
		}
		return ret;
	}

    DLLEXPORT bool SliderScalarReset(const char* label, ImGuiDataType dtype, const size_t t_size, void* v, const void *v_min, const void *v_max, const void *v_default, const char* display_format)
	{
	    bool ret = ImGui::SliderScalar(label, dtype, v, v_min, v_max, display_format);
	    if (ImGui::BeginPopupContextItem(label))
	    {
	        char buf[64];
	        char fmt[64];
	        snprintf(fmt, 64, "Reset to %s", display_format);
	        if (dtype == ImGuiDataType_S32 || dtype == ImGuiDataType_U32)
	            snprintf(buf, 64, fmt, *(const ImU32*)v_default);
	        if (dtype == ImGuiDataType_S64 || dtype == ImGuiDataType_U64)
	            snprintf(buf, 64, fmt, *(const ImU64*)v_default);
	        if (dtype == ImGuiDataType_Float)
	            snprintf(buf, 64, fmt, *(const float*)v_default);
	        if (dtype == ImGuiDataType_Double)
	            snprintf(buf, 64, fmt, *(const double*)v_default);
	        if (dtype == ImGuiDataType_S8)
	            snprintf(buf, 64, fmt, *(const ImS8*)v_default);
	        if (dtype == ImGuiDataType_U8)
	            snprintf(buf, 64, fmt, *(const ImU8*)v_default);
	        if (dtype == ImGuiDataType_S16)
	            snprintf(buf, 64, fmt, *(const ImS16*)v_default);
	        if (dtype == ImGuiDataType_U16)
	            snprintf(buf, 64, fmt, *(const ImU16*)v_default);
	        if (ImGui::MenuItem(buf))
	            memcpy(v, v_default, t_size);
	        ImGui::MenuItem("Close");
	        ImGui::EndPopup();
	    }
	    return ret;
	}

	DLLEXPORT void PushFont(ImGuiFont f)
	{
		ImGui::PushFont(g_pOrbiter->DlgMgr()->GetFont(f));
	}

	// From imgui_demo.cpp, with added sameline argument
	DLLEXPORT void HelpMarker(const char* desc, bool sameline)
	{
		if(sameline)
			ImGui::SameLine();
		ImGui::TextDisabled(ICON_FA_CIRCLE_QUESTION);
		if (ImGui::BeginItemTooltip())
		{
			ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
			ImGui::TextUnformatted(desc);
			ImGui::PopTextWrapPos();
			ImGui::EndTooltip();
		}
	}

	// Code from thedmd: https://github.com/ocornut/imgui/issues/1496
	static ImVector<ImRect> s_GroupPanelLabelStack;
	DLLEXPORT void BeginGroupPanel(const char* name, const ImVec2& size)
	{
		ImGui::BeginGroup();
		auto cursorPos = ImGui::GetCursorScreenPos();
		auto itemSpacing = ImGui::GetStyle().ItemSpacing;
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0.0f, 0.0f));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.0f, 0.0f));
		auto frameHeight = ImGui::GetFrameHeight();
    // workaround for incorrect capture of columns/table width by placing
    // zero-sized dummy element in the same group, this ensure
    // max X cursor position is updated correctly
    ImGui::SameLine(0.0f, 0.0f);
    ImGui::Dummy(ImVec2(0.0f, 0.0f));
		ImGui::BeginGroup();
		ImVec2 effectiveSize = size;
		if (size.x < 0.0f)
			effectiveSize.x = ImGui::GetContentRegionAvail().x;
		else
			effectiveSize.x = size.x;
		ImGui::Dummy(ImVec2(effectiveSize.x, 0.0f));
		ImGui::Dummy(ImVec2(frameHeight * 0.5f, 0.0f));
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::BeginGroup();
		ImGui::Dummy(ImVec2(frameHeight * 0.5f, 0.0f));
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::TextUnformatted(name);
		auto labelMin = ImGui::GetItemRectMin();
		auto labelMax = ImGui::GetItemRectMax();
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::Dummy(ImVec2(0.0, frameHeight + itemSpacing.y));
		ImGui::BeginGroup();
		//ImGui::GetWindowDrawList()->AddRect(labelMin, labelMax, IM_COL32(255, 0, 255, 255));
		ImGui::PopStyleVar(2);
		ImGui::GetCurrentWindow()->ContentRegionRect.Max.x -= frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->WorkRect.Max.x          -= frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->InnerRect.Max.x         -= frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->Size.x                   -= frameHeight;
		auto itemWidth = ImGui::CalcItemWidth();
		ImGui::PushItemWidth(ImMax(0.0f, itemWidth - frameHeight));
		s_GroupPanelLabelStack.push_back(ImRect(labelMin, labelMax));
	}
	DLLEXPORT void EndGroupPanel()
	{
		ImGui::PopItemWidth();
		auto itemSpacing = ImGui::GetStyle().ItemSpacing;
		ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0.0f, 0.0f));
		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0.0f, 0.0f));
		auto frameHeight = ImGui::GetFrameHeight();
		ImGui::EndGroup();
		//ImGui::GetWindowDrawList()->AddRectFilled(ImGui::GetItemRectMin(), ImGui::GetItemRectMax(), IM_COL32(0, 255, 0, 64), 4.0f);
		ImGui::EndGroup();
		ImGui::SameLine(0.0f, 0.0f);
		ImGui::Dummy(ImVec2(frameHeight * 0.5f, 0.0f));
		ImGui::Dummy(ImVec2(0.0, frameHeight - frameHeight * 0.5f - itemSpacing.y));
		ImGui::EndGroup();
		auto itemMin = ImGui::GetItemRectMin();
		auto itemMax = ImGui::GetItemRectMax();
		//ImGui::GetWindowDrawList()->AddRectFilled(itemMin, itemMax, IM_COL32(255, 0, 0, 64), 4.0f);
		auto labelRect = s_GroupPanelLabelStack.back();
		s_GroupPanelLabelStack.pop_back();
		ImVec2 halfFrame = ImVec2(frameHeight * 0.25f, frameHeight) * 0.5f;
		ImRect frameRect = ImRect(itemMin + halfFrame, itemMax - ImVec2(halfFrame.x, 0.0f));
		labelRect.Min.x -= itemSpacing.x;
		labelRect.Max.x += itemSpacing.x;
		for (int i = 0; i < 4; ++i)
		{
			switch (i)
			{
				// left half-plane
				case 0: ImGui::PushClipRect(ImVec2(-FLT_MAX, -FLT_MAX), ImVec2(labelRect.Min.x, FLT_MAX), true); break;
				// right half-plane
				case 1: ImGui::PushClipRect(ImVec2(labelRect.Max.x, -FLT_MAX), ImVec2(FLT_MAX, FLT_MAX), true); break;
				// top
				case 2: ImGui::PushClipRect(ImVec2(labelRect.Min.x, -FLT_MAX), ImVec2(labelRect.Max.x, labelRect.Min.y), true); break;
				// bottom
				case 3: ImGui::PushClipRect(ImVec2(labelRect.Min.x, labelRect.Max.y), ImVec2(labelRect.Max.x, FLT_MAX), true); break;
			}
			ImGui::GetWindowDrawList()->AddRect(
				frameRect.Min, frameRect.Max,
				ImColor(ImGui::GetStyleColorVec4(ImGuiCol_Border)),
				halfFrame.x);
			ImGui::PopClipRect();
		}
		ImGui::PopStyleVar(2);
		ImGui::GetCurrentWindow()->ContentRegionRect.Max.x += frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->WorkRect.Max.x          += frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->InnerRect.Max.x         += frameHeight * 0.5f;
		ImGui::GetCurrentWindow()->Size.x                  += frameHeight;
		ImGui::Dummy(ImVec2(0.0f, 0.0f));
		ImGui::EndGroup();
		//ImGui::Dummy(ImVec2(0.0f, 0.0f));
	}

	DLLEXPORT bool MenuButton(const char *label, const char *tooltip, float xoffset)
	{
		ImGuiContext& g = *GImGui;
		const ImGuiWindow* window = ImGui::GetCurrentWindow();
		const ImRect titleBarRect = window->TitleBarRect();
		const ImGuiStyle& style = g.Style;
        ImGuiLastItemData last_item_backup = g.LastItemData;
		ImGui::PushClipRect( titleBarRect.Min, titleBarRect.Max, false );
		float width = (strlen(label)+1) * g.FontSize;
		float offset = titleBarRect.Max.x - width - titleBarRect.Min.x - g.Style.FramePadding.x;
		offset -= xoffset;
		ImGui::SetCursorPos( ImVec2( offset, 0.0f ) );
        ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0,0,0,0));
		bool ret = ImGui::Button( label );
		ImGui::PopStyleColor();
		if(tooltip && ImGui::IsItemHovered())
			ImGui::SetTooltip(tooltip);
		ImGui::PopClipRect();
        g.LastItemData = last_item_backup;
		return ret;
	}
}
