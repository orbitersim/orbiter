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
#include "imgui_internal.h"
#include "imgui_extras.h"
#include "imgui_impl_win32.h"
#include "implot.h"
#include "IconsFontAwesome6.h"
#include <chrono>
#include <algorithm>
#include <vector>
#include <string>
#include <unordered_map>

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

DialogManager::DialogManager (Orbiter *orbiter, HWND hAppWnd)
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
			!IsChild (hDlg, GetFocus()) && GetParent (hDlg) == g_pOrbiter->GetRenderWnd()) {
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
DLLEXPORT ImPlotContext* GImPlot = NULL;
static void ApplyGlowTheme(const ImVec4& accent)
{
    ImGuiStyle& style = ImGui::GetStyle();
    ImVec4* colors = style.Colors;

    // Base backgrounds
    const ImVec4 bgDark   = ImVec4(0.02f, 0.03f, 0.05f, 1.0f);
    const ImVec4 bgMed    = ImVec4(0.07f, 0.08f, 0.12f, 1.0f);

    // Glow layers
    ImVec4 glowSoft   = ImVec4(accent.x, accent.y, accent.z, 0.20f);
    ImVec4 glowMedium = ImVec4(accent.x, accent.y, accent.z, 0.35f);
    ImVec4 glowStrong = ImVec4(accent.x * 1.2f, accent.y * 1.2f, accent.z * 1.2f, 1.0f);
    ImVec4 title = ImVec4(accent.x * 0.2f, accent.y * 0.2f, accent.z * 0.2f, 1.0f);
    ImVec4 titleActive = ImVec4(accent.x * 0.35f, accent.y * 0.35f, accent.z * 0.35f, 1.0f);

    // Neon text: bright foreground + soft glow shadow
    colors[ImGuiCol_Text]         = ImVec4(1, 1, 1, 1);
    colors[ImGuiCol_TextDisabled] = ImVec4(0.4f, 0.4f, 0.44f, 1);

    // Window backgrounds
    colors[ImGuiCol_WindowBg] = bgDark;
    colors[ImGuiCol_ChildBg]  = bgDark;
    colors[ImGuiCol_PopupBg]  = bgMed;

    // Glow-ish borders
    colors[ImGuiCol_Border]       = glowMedium;
    colors[ImGuiCol_BorderShadow] = ImVec4(0,0,0,0);

    // Frames (inputs, checkboxes, sliders)
    colors[ImGuiCol_FrameBg]        = bgMed;
    colors[ImGuiCol_FrameBgHovered] = glowSoft;
    colors[ImGuiCol_FrameBgActive]  = glowMedium;

    // Buttons
    colors[ImGuiCol_Button]        = glowSoft;
    colors[ImGuiCol_ButtonHovered] = glowMedium;
    colors[ImGuiCol_ButtonActive]  = glowStrong;

    // Headers (collapsing header, tree node)
    colors[ImGuiCol_Header]        = glowSoft;
    colors[ImGuiCol_HeaderHovered] = glowMedium;
    colors[ImGuiCol_HeaderActive]  = glowStrong;

    // Checkmark & sliders use bright neon
    colors[ImGuiCol_CheckMark]       = glowStrong;
    colors[ImGuiCol_SliderGrab]      = glowMedium;
    colors[ImGuiCol_SliderGrabActive]= glowStrong;

    // Scrollbar
    colors[ImGuiCol_ScrollbarBg]          = bgDark;
    colors[ImGuiCol_ScrollbarGrab]        = glowSoft;
    colors[ImGuiCol_ScrollbarGrabHovered] = glowMedium;
    colors[ImGuiCol_ScrollbarGrabActive]  = glowStrong;

    // Separators: bright neon lines
    colors[ImGuiCol_Separator]        = glowMedium;
    colors[ImGuiCol_SeparatorHovered] = glowStrong;
    colors[ImGuiCol_SeparatorActive]  = glowStrong;

    // Title bar
    colors[ImGuiCol_TitleBg]          = title;
    colors[ImGuiCol_TitleBgActive]    = titleActive;
    colors[ImGuiCol_TitleBgCollapsed] = bgDark;

    // Resize grip
    colors[ImGuiCol_ResizeGrip]        = glowSoft;
    colors[ImGuiCol_ResizeGripHovered] = glowMedium;
    colors[ImGuiCol_ResizeGripActive]  = glowStrong;

    // Tabs
    colors[ImGuiCol_Tab]                = glowSoft;
    colors[ImGuiCol_TabHovered]         = glowMedium;
    colors[ImGuiCol_TabActive]          = glowMedium;
    colors[ImGuiCol_TabUnfocused]       = ImVec4(glowSoft.x, glowSoft.y, glowSoft.z, 0.5f);
    colors[ImGuiCol_TabUnfocusedActive] = glowMedium;

    style.FrameRounding = 6;
    style.GrabRounding  = 4;
    style.WindowRounding = 4;

    style.FrameBorderSize = 1.0f;
    style.ChildBorderSize = 1.0f;
    style.WindowBorderSize = 1.0f;
}

static void ImGuiSetStyle()
{
    ImVec4 green = ImVec4(0.0f, 1.0f, 0.0f, 1.0f);
    ApplyGlowTheme(green);

    ImGuiStyle& style = ImGui::GetStyle();
    style.WindowMenuButtonPosition = ImGuiDir_Right;

    style.Colors[ImGuiCol_Separator] = style.Colors[ImGuiCol_Button];
}

void DialogManager::InitImGui()
{
	if(!gc) return;

	IMGUI_CHECKVERSION();
	ImGui::CreateContext();
	ImPlot::CreateContext();
	ImGuiIO& io = ImGui::GetIO();
	// Viewports don't play nice when in full screen mode
	if(!pOrbiter->IsFullscreen())
		io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;     // Enable Keyboard Controls
	//io.ConfigFlags |= ImGuiConfigFlags_NavEnableGamepad;      // Enable Gamepad Controls
	//io.ConfigFlags |= ImGuiConfigFlags_DockingEnable;

	ImGuiSetStyle();

	ImFontConfig config;

	static const ImWchar icons_ranges[] = { ICON_MIN_FA, ICON_MAX_FA, 0 };
	ImFontConfig icons_config;
	icons_config.MergeMode = true;
	icons_config.PixelSnapH = true;
	icons_config.FontDataOwnedByAtlas = false;
	
	const CFG_FONTPRM &prm = g_pOrbiter->Cfg()->CfgFontPrm;

	defaultFont = io.Fonts->AddFontFromFileTTF(prm.ImGui_FontFile, prm.ImGui_FontSize);
	io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize, &icons_config, icons_ranges);
	consoleFont = io.Fonts->AddFontFromFileTTF("Cousine-Regular.ttf", prm.ImGui_FontSize);
	monoFont = io.Fonts->AddFontFromFileTTF("Lekton-Bold.ttf", prm.ImGui_FontSize);
	manuscriptFont = io.Fonts->AddFontFromFileTTF("architext.regular.ttf", prm.ImGui_FontSize);

	ImGui_ImplWin32_Init(hWnd);
	gc->clbkImGuiInit();
}

void DialogManager::ShutdownImGui()
{
	if(!gc) return;

	gc->clbkImGuiShutdown();
	ImGui_ImplWin32_Shutdown();
	ImPlot::DestroyContext();
	ImGui::DestroyContext();
}

void DialogManager::ImGuiNewFrame()
{
	if(!gc) return;

	gc->clbkImGuiNewFrame();
	ImGui_ImplWin32_NewFrame();
	ImGui::NewFrame();

	RenderNotifications();

	// We can't use a range-based loop here because Display() may unregister the current dialog
	for (auto it = DlgImGuiList.begin(); it != DlgImGuiList.end();)
	{
		auto current = it++;
		if ((*current)->IsActive()) {
			(*current)->Display();
		}
	}

	ImGui::EndFrame();
}

ImFont *DialogManager::GetFont(ImGuiFont f)
{
	switch(f) {
	case ImGuiFont::MONO: return monoFont;
	case ImGuiFont::CONSOLE: return consoleFont;
	case ImGuiFont::DEFAULT: return defaultFont;
	case ImGuiFont::MANUSCRIPT: return manuscriptFont;
	default: return defaultFont;
	}
}

void DialogManager::SetMainColor(COLORREF col)
{
    ImVec4 color = ImVec4((col & 0xff) / 255.0f, ((col >> 8) & 0xff) / 255.0f, (col >> 16)/255.0f, 1.0f);
    ApplyGlowTheme(color);
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

void ImGuiDialog::Activate() {
	active = true;
	ImGui::SetWindowFocus(name.c_str());
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
	uint32_t type;

	Notification(uint32_t type_, const char *title_, const char *content_, size_t hash_):title(title_),content(content_),hash(hash_),type(type_)
	{
		if(type >= OAPINOTIF_INFO) type = OAPINOTIF_INFO;
		color = notifcolors[type];
		icon = notificons[type];
		duration = notifduration[type];
		creation = std::chrono::steady_clock::now();
		occurrences = 1;
		expired = false;
		permanent = type == OAPINOTIF_ERROR;
		speed = 0.0f;
		height = -1.0;
	}
	void ReTrigger() {
		occurrences++;
		creation = std::chrono::steady_clock::now() - FADE_TIME;
		duration = notifduration[type];
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
			speed += std::chrono::duration_cast<std::chrono::milliseconds>(dt).count()/100.0f;
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
	ImVec2 vp_size = ImGui::GetMainViewport()->Size + ImGui::GetMainViewport()->Pos;
	float height = 0.0f;

	int i = 0;
	static time_point_t last = std::chrono::steady_clock::now();
	auto now = std::chrono::steady_clock::now();
	duration_t dt = now - last;
	last = now;
	for (auto &notif: notifications)
	{
		notif.UpdateState(now, height, dt);

		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, notif.color.w);
		ImGui::PushStyleVar(ImGuiStyleVar_WindowRounding, 5.f);
		ImGui::SetNextWindowPos(ImVec2(vp_size.x - NOTIFY_PADDING_X, vp_size.y - NOTIFY_PADDING_Y - notif.height), ImGuiCond_Always, ImVec2(1.0f, 1.0f));

		// Generate new unique name for this notification
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
		ImGui::PopStyleVar(2);
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
	if(content == NULL) {
		content = "(null)";
	}
	const size_t hash = std::hash<std::string>{}(title) ^ std::hash<std::string>{}(content);
	for (auto &notif: notifications) {
		if(notif.hash == hash && notif.title == title && notif.content == content && notif.type == type) {
			notif.ReTrigger();
			return;
		}
	}
	notifications.emplace_back(type, title, content, hash);
}


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

	// Cheap double slider
	// It's provided only to be able to use sliders where a double is used but a float would have been enough
	// e.g. the stars brightness configuration
	DLLEXPORT bool SliderDouble(const char* label, double* v, double v_min, double v_max, const char* display_format)
	{
		float tmp = *v;
		bool ret = ImGui::SliderFloat(label, &tmp, v_min, v_max, display_format);
		if(ret) *v = tmp;
		return ret;
	}

	DLLEXPORT bool InputIntEx(const char* label, int* v, int v_min, int v_max, int step, int step_fast, ImGuiInputTextFlags flags)
	{
		bool ret = ImGui::InputInt(label, v, step, step_fast, flags);
		if(ret) {
			if(*v < v_min) *v = v_min;
			else if (*v > v_max) *v = v_max;
		}
		return ret;
	}
	DLLEXPORT bool InputDoubleEx(const char* label, double* v, double v_min, double v_max, double step, double step_fast, const char *fmt, ImGuiInputTextFlags flags)
	{
		bool ret = ImGui::InputDouble(label, v, step, step_fast, fmt, flags);
		if(ret) {
			if(*v < v_min) *v = v_min;
			else if (*v > v_max) *v = v_max;
		}
		return ret;
	}


	DLLEXPORT void PushFont(ImGuiFont f, float scale)
	{
		ImGuiContext& g = *GImGui;
		const ImGuiStyle& style = g.Style;
		ImGui::PushFont(g_pOrbiter->DlgMgr()->GetFont(f), style.FontSizeBase * scale);
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
		ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 0.0f);   // no border
		ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0,0,0,0));
		ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(0,0,0,0));    // invisible border color		bool ret = ImGui::Button( label );
		bool ret = ImGui::Button( label );
		ImGui::PopStyleColor(2);
		ImGui::PopStyleVar();
		if(tooltip && ImGui::IsItemHovered())
			ImGui::SetTooltip(tooltip);
		ImGui::PopClipRect();
        g.LastItemData = last_item_backup;
		return ret;
	}

	DLLEXPORT ImTextureID GetImTextureID (SURFHANDLE surf)
	{
		oapi::GraphicsClient *gc = g_pOrbiter->GetGraphicsClient();
		if (gc && surf)
			return gc->clbkImGuiSurfaceTexture (surf);
		return 0;
	}

	DLLEXPORT void Image(SURFHANDLE surf, const ImVec2& image_size, const ImVec2& uv0, const ImVec2& uv1, const ImVec4& tint_col, const ImVec4& border_col)
	{
		ImTextureID tex_id = GetImTextureID(surf);
		Image(tex_id, image_size, uv0, uv1, tint_col, border_col);
	}

	DLLEXPORT bool ImageButton(const char* str_id, SURFHANDLE surf, const ImVec2& image_size, const ImVec2& uv0, const ImVec2& uv1, const ImVec4& bg_col, const ImVec4& tint_col)
	{
		ImTextureID tex_id = GetImTextureID(surf);
		return ImageButton(str_id, tex_id, image_size, uv0, uv1, bg_col, tint_col);
	}

	DLLEXPORT bool SliderEnum(const char *label, int *v, const char *values[], int nvalues)
	{
        const char* elem_name = (*v >= 0 && *v < nvalues) ? values[*v] : "Unknown";
        return ImGui::SliderInt(label, v, 0, nvalues - 1, elem_name);
	}


	// "Animated" widgets for Combos and Popups

	struct FadingState
	{
		enum State { Closed, Opening, Open } state = Closed;
		float animstate = 0.0f;
	};
	static std::unordered_map<ImGuiID, FadingState> g_popupStates;

	// A popup that will fade-in when open (making it fade-out is too tricky)
	DLLEXPORT bool BeginAnimatedPopup(const char* name, float duration)
	{
		ImGuiContext& g = *GImGui;
		ImGuiID id = g.CurrentWindow->GetID(name);
		FadingState& S = g_popupStates[id];

		if (ImGui::IsPopupOpen(name) && S.state == FadingState::Closed)	{
			S.state = FadingState::Opening;
			S.animstate = 0.0f;
		}

		if (S.state == FadingState::Opening) {
			S.animstate += ImGui::GetIO().DeltaTime;
			if (S.animstate >= duration)
				S.state = FadingState::Open;
		}

		float x = ImClamp(S.animstate / duration, 0.0f, 1.0f);
		float alpha = x * x * (3.0f - 2.0f * x);

		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, alpha);

		if (!ImGui::BeginPopup(name)) {
			ImGui::PopStyleVar();
			S.state = FadingState::Closed;
			return false;
		}

		return true;
	}

	DLLEXPORT void EndAnimatedPopup()
	{
		ImGui::PopStyleVar();
		ImGui::EndPopup();
	}

	DLLEXPORT bool BeginAnimatedCombo(const char* label, const char* preview_value, ImGuiComboFlags flags, float duration)
	{
		ImGuiContext& g = *GImGui;
		ImGuiID id = g.CurrentWindow->GetID(label);
		FadingState& S = g_popupStates[id];

		float alpha = 1.0f;
		if (S.state == FadingState::Opening) {
			S.animstate += ImGui::GetIO().DeltaTime;
			if (S.animstate >= duration) {
				S.state = FadingState::Open;
			} else {
				float x = ImClamp(S.animstate / duration, 0.0f, 1.0f);
				alpha = x * x * (3.0f - 2.0f * x);
			}
		}

		ImVec4 bc = ImGui::GetStyleColorVec4(ImGuiCol_Border);
		ImVec4 bs = ImGui::GetStyleColorVec4(ImGuiCol_BorderShadow);
		bc.w *= alpha;
		bs.w *= alpha;
		ImGui::PushStyleColor(ImGuiCol_Border, bc);
		ImGui::PushStyleColor(ImGuiCol_BorderShadow, bs);
		ImGui::SetNextWindowBgAlpha(alpha);

		if (ImGui::BeginCombo(label, preview_value, flags)) {
			ImGui::PushStyleVar(ImGuiStyleVar_Alpha, alpha);

			if(S.state == FadingState::Closed) {
				S.state = FadingState::Opening;
				S.animstate = 0.0f;
			}
		} else {
			S.state = FadingState::Closed;
			ImGui::PopStyleColor(2);
			return false;
		}

		return true;
	}

    DLLEXPORT void EndAnimatedCombo()
	{
		ImGui::PopStyleColor(2);
		ImGui::PopStyleVar();
		ImGui::EndCombo();
	}

	struct AnimatedHeaderState
	{
		enum State { Closed, Opening, Closing, Open } state = Closed;
		float speed;
		float height = 0.0f;
		ImVec2 startPos;
		std::string childname;
	};
	static std::unordered_map<ImGuiID, AnimatedHeaderState> g_headerStates;
	static std::vector<ImGuiID> g_stackHeaders;
	DLLEXPORT bool BeginAnimatedCollapsingHeader(const char* label, ImGuiTreeNodeFlags flags, float speed)
	{
		ImGuiWindow* window = GetCurrentWindow();
		if (window->SkipItems)
			return false;

		ImGuiID id = ImGui::GetID(label);
		auto found = g_headerStates.find(id);
		auto& state = g_headerStates[id];
		state.speed = speed;

		// First time with see this widget,
		if(found == g_headerStates.end()) {
			state.childname = std::string(label) + "_child";
			// Check to see if it should be open
			if(flags & ImGuiTreeNodeFlags_DefaultOpen)
				state.state = AnimatedHeaderState::Open;
		}

		bool visible = ImGui::CollapsingHeader(label, flags);

		if(visible) {
			switch(state.state) {
				case AnimatedHeaderState::Closed:
					state.state = AnimatedHeaderState::Opening;
					break;
				case AnimatedHeaderState::Closing:
					state.state = AnimatedHeaderState::Opening;
					break;
				case AnimatedHeaderState::Opening:
				case AnimatedHeaderState::Open:
					break;
			}
		} else {
			switch(state.state) {
				case AnimatedHeaderState::Open:
					state.state = AnimatedHeaderState::Closing;
					break;
				case AnimatedHeaderState::Opening:
					state.state = AnimatedHeaderState::Closing;
					break;
				case AnimatedHeaderState::Closing:
				case AnimatedHeaderState::Closed:
					break;
			}
		}

		// If it's closed, EndAnimatedCollapsingHeader won't be called
		// so we need to exit before pushing the id to the stack
		if(state.state == AnimatedHeaderState::Closed) return false;
		g_stackHeaders.push_back(id);

		// If the block is fully deployed, we use a BeginGroup/EndGroup section
		// To compute the height of the whole block in EndAnimatedCollapsingHeader
		if(state.state == AnimatedHeaderState::Open) {
			state.startPos = ImGui::GetCursorScreenPos();
			ImGui::BeginGroup();
			return true;
		}

		// Use a child during the animation, we clamp its height to create
		// a sliding effect
		ImGui::BeginChild(state.childname.c_str(),
							ImVec2(-FLT_MIN, state.height), ImGuiChildFlags_AutoResizeY, ImGuiWindowFlags_NoDecoration);
			
		return true;
	}

	DLLEXPORT void EndAnimatedCollapsingHeader()
	{
		auto id = g_stackHeaders.back();
		g_stackHeaders.pop_back();

		auto& state = g_headerStates[id];
		// Compute the height of the block
		if(state.state == AnimatedHeaderState::Open) {
			ImGui::EndGroup();

			ImVec2 endPos = ImGui::GetItemRectMax();
			state.height = endPos.y - state.startPos.y;
			return;
		}

		ImVec2 available = ImGui::GetContentRegionAvail();

		switch(state.state) {
			case AnimatedHeaderState::Opening:
				// A negative available.y indicates that the content is
				// too tall to fit in the given heigth -> we increase it
				// Otherwise we can stop the animation
				if(available.y < 0) {
					state.height += ImGui::GetIO().DeltaTime * state.speed;
				} else {
					state.state = AnimatedHeaderState::Open;
				}
				break;
			case AnimatedHeaderState::Closing:
				state.height -= ImGui::GetIO().DeltaTime * state.speed;
				if(state.height <= 0.0) {
					state.height = 0;
					state.state = AnimatedHeaderState::Closed;
				}
				break;
			case AnimatedHeaderState::Open:
			case AnimatedHeaderState::Closed:
				break; // should not be here
		}
		ImGui::EndChild();
	}
}
