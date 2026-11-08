// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// Launchpad: the startup window for selecting scenarios and settings
// =======================================================================

#define OAPI_IMPLEMENTATION

#include "Launchpad.h"
#include "LaunchpadRenderer.h"
#include "LpadTab.h"
#include "TabScenario.h"
#include "TabOptions.h"
#include "TabModule.h"
#include "TabVideo.h"
#include "TabExtra.h"
#include "TabAbout.h"
#include "Orbiter.h"
#include "Memstat.h"
#include "Help.h"
#include "Log.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include "imgui_impl_orbiter.h"
#include "IconsFontAwesome6.h"
#include <filesystem>
#include <random>
#include <time.h>

namespace fs = std::filesystem;

extern HELPCONTEXT DefHelpContext;

static orbiter::LaunchpadDialog *s_launchpad = nullptr;

//=============================================================================
// class LaunchpadDialog

orbiter::LaunchpadDialog::LaunchpadDialog (Orbiter *app)
{
	pApp = app;
	pCfg = app->Cfg();
	hWnd = NULL;
	m_page = PG_SCN;
	m_requestPage = -1;
	pScenario = nullptr;
	pExtra = nullptr;
	m_bVisible = false;
	m_bLaunchEnabled = true;
	m_bWait = false;
	m_waitProgress = 0.0f;
	m_ctx = nullptr;
	m_renderer = nullptr;
	m_headingFont = nullptr;
	m_scale = 1.0f;
	mem_wait = 0;
	mem0 = 0;
	m_idleStart = time(NULL);
	s_launchpad = this;
}

orbiter::LaunchpadDialog::~LaunchpadDialog ()
{
	for (auto tab : TabList)
		delete tab;
	TabList.clear();
	ShutdownImGui();
	if (hWnd) {
		RECT r;
		GetWindowRect (hWnd, &r);
		pCfg->rLaunchpad = r;
		DestroyWindow (hWnd);
	}
	s_launchpad = nullptr;
}

bool orbiter::LaunchpadDialog::Create (bool startvideotab)
{
	if (hWnd) return true;

	oplat::WindowDesc desc;
	desc.title = "Orbiter Launchpad";
	float sc = oplat::GetBackingScale (NULL);
	desc.width = (int)(900 * sc);
	desc.height = (int)(620 * sc);
	const RECT &lr = pCfg->rLaunchpad;
	if (lr.right > lr.left && lr.bottom > lr.top) {
		desc.x = lr.left;
		desc.y = lr.top;
		desc.width = lr.right - lr.left;
		desc.height = lr.bottom - lr.top;
	}
	desc.proc = WndProc;
	desc.userData = this;
	desc.visible = false;
	hWnd = oplat::OpenWindow (desc);
	if (!hWnd) return false;
	oplat::SetMinClientSize (hWnd, (int)(720 * sc), (int)(480 * sc));

	InitImGui();

	AddTab (pScenario = new ScenarioTab (this));
	AddTab (new OptionsTab (this));
	AddTab (new ModuleTab (this));
	AddTab (new DefVideoTab (this));
	AddTab (pExtra = new ExtraTab (this));
	AddTab (new AboutTab (this));

	for (auto tab : TabList)
		tab->GetConfig (pCfg);

	if (pCfg->CfgDemoPrm.bDemo)
		SetDemoMode ();

	SwitchTabPage (startvideotab ? PG_VID : PG_SCN);

	if (pCfg->CfgCmdlinePrm.LaunchScenario.empty())
		Show ();
	return true;
}

void orbiter::LaunchpadDialog::InitImGui ()
{
	ImGuiContext *prev = ImGui::GetCurrentContext ();
	m_ctx = ImGui::CreateContext ();
	ImGui::SetCurrentContext (m_ctx);
	ImGuiIO &io = ImGui::GetIO ();
	io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
	io.IniFilename = nullptr;

	m_scale = oplat::GetBackingScale (hWnd);
	ImGui::StyleColorsDark ();
	ImGuiStyle &style = ImGui::GetStyle ();
	style.WindowRounding = 8.0f;
	style.ChildRounding = 6.0f;
	style.FrameRounding = 5.0f;
	style.PopupRounding = 6.0f;
	style.GrabRounding = 4.0f;
	style.TabRounding = 5.0f;
	style.WindowBorderSize = 0.0f;
	style.FrameBorderSize = 0.0f;
	style.WindowPadding = ImVec2 (14, 14);
	style.FramePadding = ImVec2 (8, 5);
	style.ItemSpacing = ImVec2 (8, 7);
	ImVec4 *c = style.Colors;
	c[ImGuiCol_WindowBg] = ImVec4 (0.11f, 0.11f, 0.12f, 1.0f);
	c[ImGuiCol_ChildBg] = ImVec4 (0.14f, 0.14f, 0.15f, 1.0f);
	c[ImGuiCol_PopupBg] = ImVec4 (0.16f, 0.16f, 0.17f, 1.0f);
	c[ImGuiCol_FrameBg] = ImVec4 (0.20f, 0.20f, 0.21f, 1.0f);
	c[ImGuiCol_FrameBgHovered] = ImVec4 (0.25f, 0.25f, 0.27f, 1.0f);
	c[ImGuiCol_FrameBgActive] = ImVec4 (0.28f, 0.28f, 0.30f, 1.0f);
	c[ImGuiCol_Header] = ImVec4 (0.22f, 0.36f, 0.62f, 1.0f);
	c[ImGuiCol_HeaderHovered] = ImVec4 (0.26f, 0.26f, 0.28f, 1.0f);
	c[ImGuiCol_HeaderActive] = ImVec4 (0.24f, 0.40f, 0.68f, 1.0f);
	c[ImGuiCol_Button] = ImVec4 (0.22f, 0.22f, 0.24f, 1.0f);
	c[ImGuiCol_ButtonHovered] = ImVec4 (0.28f, 0.28f, 0.30f, 1.0f);
	c[ImGuiCol_ButtonActive] = ImVec4 (0.32f, 0.32f, 0.35f, 1.0f);
	c[ImGuiCol_CheckMark] = ImVec4 (0.40f, 0.62f, 1.00f, 1.0f);
	c[ImGuiCol_SliderGrab] = ImVec4 (0.40f, 0.62f, 1.00f, 1.0f);
	c[ImGuiCol_SliderGrabActive] = ImVec4 (0.50f, 0.70f, 1.00f, 1.0f);
	c[ImGuiCol_Separator] = ImVec4 (0.24f, 0.24f, 0.26f, 1.0f);
	c[ImGuiCol_TitleBg] = ImVec4 (0.16f, 0.16f, 0.17f, 1.0f);
	c[ImGuiCol_TitleBgActive] = ImVec4 (0.18f, 0.18f, 0.20f, 1.0f);
	c[ImGuiCol_Tab] = ImVec4 (0.18f, 0.18f, 0.20f, 1.0f);
	c[ImGuiCol_TabHovered] = ImVec4 (0.28f, 0.28f, 0.30f, 1.0f);
	c[ImGuiCol_TabSelected] = ImVec4 (0.24f, 0.24f, 0.27f, 1.0f);
	style.ScaleAllSizes (m_scale);

	const CFG_FONTPRM &prm = pCfg->CfgFontPrm;
	float size = 14.0f * m_scale;
	const char *sysfont = "/System/Library/Fonts/SFNS.ttf";
	const char *body = fs::exists (sysfont) ? sysfont : prm.ImGui_DefaultFontFile;
	ImFontConfig icons;
	icons.MergeMode = true;
	icons.PixelSnapH = true;
	io.Fonts->AddFontFromFileTTF (body, size);
	if (fs::exists ("Fonts/fa-solid-900.ttf"))
		io.Fonts->AddFontFromFileTTF ("Fonts/fa-solid-900.ttf", size * 0.9f, &icons);
	m_headingFont = io.Fonts->AddFontFromFileTTF (body, size * 1.35f);

	ImGui_ImplOrbiter_Init (hWnd);
	m_renderer = new LaunchpadRenderer;
	m_renderer->Init (hWnd);

	ImGui::SetCurrentContext (prev);
}

void orbiter::LaunchpadDialog::ShutdownImGui ()
{
	if (!m_ctx) return;
	ImGuiContext *prev = ImGui::GetCurrentContext ();
	ImGui::SetCurrentContext (m_ctx);
	if (m_renderer) {
		delete m_renderer;
		m_renderer = nullptr;
	}
	ImGui_ImplOrbiter_Shutdown ();
	ImGui::DestroyContext (m_ctx);
	ImGui::SetCurrentContext (prev == m_ctx ? nullptr : prev);
	m_ctx = nullptr;
}

void orbiter::LaunchpadDialog::Show ()
{
	if (!hWnd) return;
	ShowWindow (hWnd, SW_SHOW);
	SetForegroundWindow (hWnd);
	m_bVisible = true;
	ResetIdleTimer ();
	for (auto tab : TabList)
		tab->LaunchpadShowing (true);
}

void orbiter::LaunchpadDialog::Hide ()
{
	if (!hWnd) return;
	ShowWindow (hWnd, SW_HIDE);
	m_bVisible = false;
	for (auto tab : TabList)
		tab->LaunchpadShowing (false);
}

void orbiter::LaunchpadDialog::AddTab (LaunchpadTab *tab)
{
	TabList.push_back (tab);
}

orbiter::LaunchpadTab *orbiter::LaunchpadDialog::GetTab (UINT i) const
{
	return i < TabList.size () ? TabList[i] : nullptr;
}

void orbiter::LaunchpadDialog::SwitchTabPage (int pg)
{
	if (pg < 0 || pg >= (int)TabList.size ()) return;
	m_page = pg;
	m_requestPage = pg;
	TabList[pg]->Activated ();
}

void orbiter::LaunchpadDialog::EnableLaunchButton (bool enable)
{
	m_bLaunchEnabled = enable;
}

void orbiter::LaunchpadDialog::UpdateConfig ()
{
	for (auto tab : TabList)
		tab->SetConfig (pCfg);
	if (hWnd) {
		RECT r;
		GetWindowRect (hWnd, &r);
		pCfg->rLaunchpad = r;
	}
}

void orbiter::LaunchpadDialog::LaunchScenario (const char *scenario)
{
	if (!m_bLaunchEnabled || !scenario || !scenario[0]) return;
	m_pendingLaunch = scenario;
}

void orbiter::LaunchpadDialog::RequestExit ()
{
	if (pCfg->CfgDemoPrm.bDemo && pCfg->CfgDemoPrm.bBlockExit) return;
	UpdateConfig ();
	PostQuitMessage (0);
}

//-----------------------------------------------------------------------------
// "Extra" item registry

orbiter::ExtraItem *orbiter::LaunchpadDialog::RegisterExtraParam (LaunchpadItem *item, ExtraItem *parent)
{
	const char *name = item->Name ();
	if (name)
		if (ExtraItem *existing = FindExtraParam (name, parent))
			return existing;
	auto node = std::make_unique<ExtraItem> ();
	node->item = item;
	node->parent = parent;
	ExtraItem *p = node.get ();
	(parent ? parent->children : m_extraRoot).push_back (std::move (node));
	item->hItem = (LAUNCHPADITEM_HANDLE)p;
	return p;
}

orbiter::ExtraItem *orbiter::LaunchpadDialog::FindNode (LaunchpadItem *item, std::list<std::unique_ptr<ExtraItem>> &list)
{
	for (auto &n : list) {
		if (n->item == item) return n.get ();
		if (ExtraItem *c = FindNode (item, n->children)) return c;
	}
	return nullptr;
}

bool orbiter::LaunchpadDialog::UnregisterExtraParam (LaunchpadItem *item)
{
	ExtraItem *node = FindNode (item, m_extraRoot);
	if (!node) return false;
	item->clbkWriteConfig ();
	auto &list = node->parent ? node->parent->children : m_extraRoot;
	for (auto it = list.begin (); it != list.end (); ++it) {
		if (it->get () == node) {
			list.erase (it);
			return true;
		}
	}
	return false;
}

orbiter::ExtraItem *orbiter::LaunchpadDialog::FindExtraParam (const char *name, const ExtraItem *parent)
{
	const auto &list = parent ? parent->children : m_extraRoot;
	for (auto &n : list) {
		const char *nm = n->item->Name ();
		if (nm && !strcmp (nm, name)) return n.get ();
	}
	return nullptr;
}

void orbiter::LaunchpadDialog::WriteExtraParams ()
{
	std::function<void (std::list<std::unique_ptr<ExtraItem>> &)> write = [&](std::list<std::unique_ptr<ExtraItem>> &list) {
		for (auto &n : list) {
			n->item->clbkWriteConfig ();
			write (n->children);
		}
	};
	write (m_extraRoot);
}

//-----------------------------------------------------------------------------
// Hosted dialogs

void orbiter::LaunchpadDialog::OpenDialog (ImGuiDialog *dlg)
{
	for (auto d : m_dialogs)
		if (d == dlg) return;
	m_dialogs.push_back (dlg);
}

bool orbiter::LaunchpadDialog::CloseDialog (ImGuiDialog *dlg)
{
	for (auto it = m_dialogs.begin (); it != m_dialogs.end (); ++it) {
		if (*it == dlg) {
			m_dialogs.erase (it);
			return true;
		}
	}
	return false;
}

//-----------------------------------------------------------------------------
// Images

void *orbiter::LaunchpadDialog::LoadImage (const char *path, int *width, int *height)
{
	oplat::Image img;
	if (!m_renderer || !oplat::LoadImageFile (path, img)) return nullptr;
	if (width) *width = img.width;
	if (height) *height = img.height;
	return m_renderer->CreateTexture (img.bgra.data (), img.width, img.height);
}

void orbiter::LaunchpadDialog::ReleaseImage (void *texture)
{
	if (m_renderer) m_renderer->ReleaseTexture (texture);
}

//-----------------------------------------------------------------------------
// Graphics client

void orbiter::LaunchpadDialog::OnGraphicsClientLoaded (oapi::GraphicsClient *gc, const char *moduleName)
{
	if (auto vt = dynamic_cast<DefVideoTab *> (GetTab (PG_VID)))
		vt->OnGraphicsClientLoaded (gc, moduleName);
}

//-----------------------------------------------------------------------------
// Wait page (shown while a simulation session is being closed)

void orbiter::LaunchpadDialog::ShowWaitPage (bool show, long mem_committed)
{
	m_bWait = show;
	if (show) {
		mem_wait = mem_committed / 1000;
		mem0 = pApp->memstat ? pApp->memstat->HeapUsage () / 1000 : 0;
		m_waitProgress = 0.0f;
		RenderFrame ();
	}
}

void orbiter::LaunchpadDialog::UpdateWaitProgress ()
{
	if (!m_bWait || !mem_wait) return;
	long mem = pApp->memstat ? pApp->memstat->HeapUsage () / 1000 : 0;
	m_waitProgress = (float)(mem0 - mem) / (float)mem_wait;
	if (m_waitProgress < 0.0f) m_waitProgress = 0.0f;
	if (m_waitProgress > 1.0f) m_waitProgress = 1.0f;
	static DWORD last = 0;
	DWORD now = GetTickCount ();
	if (now - last > 50) {
		last = now;
		RenderFrame ();
	}
}

//-----------------------------------------------------------------------------
// Demo mode

void orbiter::LaunchpadDialog::SetDemoMode ()
{
	ResetIdleTimer ();
}

void orbiter::LaunchpadDialog::ResetIdleTimer ()
{
	m_idleStart = time (NULL);
}

bool orbiter::LaunchpadDialog::SelectDemoScenario (std::string &scenario)
{
	std::vector<std::string> list;
	fs::path dir = fs::path (pCfg->CfgDirPrm.ScnDir) / "Demo";
	std::error_code ec;
	for (auto &e : fs::recursive_directory_iterator (dir, ec)) {
		if (e.path ().extension () == ".scn") {
			fs::path rel = fs::relative (e.path (), pCfg->CfgDirPrm.ScnDir, ec);
			rel.replace_extension ();
			list.push_back (rel.string ());
		}
	}
	if (list.empty ()) return false;
	static std::mt19937 rng ((unsigned)time (NULL));
	scenario = list[rng () % list.size ()];
	return true;
}

void orbiter::LaunchpadDialog::CheckDemoTimer ()
{
	if (!pCfg->CfgDemoPrm.bDemo) return;
	if (difftime (time (NULL), m_idleStart) > pCfg->CfgDemoPrm.LPIdleTime) {
		std::string scn;
		ResetIdleTimer ();
		if (SelectDemoScenario (scn))
			LaunchScenario (scn.c_str ());
	}
}

//-----------------------------------------------------------------------------
// Frame rendering

void orbiter::LaunchpadDialog::RenderFrame ()
{
	if (!m_ctx || !m_renderer || !m_bVisible) return;
	ImGuiContext *prev = ImGui::GetCurrentContext ();
	ImGui::SetCurrentContext (m_ctx);

	if (!m_renderer->BeginFrame ()) {
		ImGui::SetCurrentContext (prev);
		return;
	}
	ImGui_ImplOrbiter_NewFrame ();
	ImGui::NewFrame ();

	if (m_bWait) DrawWaitPage ();
	else DrawUI ();

	for (auto it = m_dialogs.begin (); it != m_dialogs.end ();) {
		auto cur = it++;
		if ((*cur)->IsActive ()) (*cur)->Display ();
	}

	ImGui::Render ();
	m_renderer->EndFrame (ImGui::GetDrawData ());
	ImGui::SetCurrentContext (prev == m_ctx ? m_ctx : prev);

	if (!m_bWait) CheckDemoTimer ();

	if (!m_pendingLaunch.empty ()) {
		std::string scn;
		scn.swap (m_pendingLaunch);
		UpdateConfig ();
		pApp->Launch (scn.c_str ());
	}
}

void orbiter::LaunchpadDialog::DrawWaitPage ()
{
	ImGuiViewport *vp = ImGui::GetMainViewport ();
	ImGui::SetNextWindowPos (vp->Pos);
	ImGui::SetNextWindowSize (vp->Size);
	ImGui::Begin ("##wait", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings);
	ImVec2 avail = ImGui::GetContentRegionAvail ();
	float w = avail.x * 0.5f;
	ImGui::SetCursorPos (ImVec2 ((vp->Size.x - w) * 0.5f, vp->Size.y * 0.45f));
	ImGui::BeginGroup ();
	ImGui::TextUnformatted ("Closing simulation session");
	if (mem_wait)
		ImGui::ProgressBar (m_waitProgress, ImVec2 (w, 0), "");
	ImGui::EndGroup ();
	ImGui::End ();
}

void orbiter::LaunchpadDialog::DrawUI ()
{
	ImGuiViewport *vp = ImGui::GetMainViewport ();
	ImGui::SetNextWindowPos (vp->Pos);
	ImGui::SetNextWindowSize (vp->Size);
	ImGui::Begin ("##launchpad", nullptr,
		ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoBringToFrontOnFocus);

	const float navw = 180.0f * m_scale;
	const ImGuiStyle &style = ImGui::GetStyle ();
	float buttonsH = 3.0f * (ImGui::GetFrameHeight () + style.ItemSpacing.y);

	ImGui::BeginChild ("##nav", ImVec2 (navw, 0), ImGuiChildFlags_None, ImGuiWindowFlags_NoBackground);
	ImGui::PushFont (m_headingFont);
	ImGui::TextUnformatted ("Orbiter");
	ImGui::PopFont ();
	ImGui::Spacing ();
	for (int i = 0; i < (int)TabList.size (); i++) {
		if (ImGui::Selectable (TabList[i]->Label (), m_page == i, 0, ImVec2 (0, ImGui::GetFrameHeight ()))) {
			if (m_page != i) SwitchTabPage (i);
		}
	}
	float y = ImGui::GetWindowHeight () - buttonsH;
	if (ImGui::GetCursorPosY () < y) ImGui::SetCursorPosY (y);
	float bw = ImGui::GetContentRegionAvail ().x;
	ImGui::BeginDisabled (!m_bLaunchEnabled);
	if (ImGui::Button ("Launch Orbiter", ImVec2 (bw, 0))) {
		if (pScenario) {
			std::string scn = pScenario->SelectedScenario ();
			if (!scn.empty ()) LaunchScenario (scn.c_str ());
		}
	}
	ImGui::EndDisabled ();
	if (ImGui::Button ("Help", ImVec2 (bw, 0)))
		TabList[m_page]->OpenHelp ();
	ImGui::BeginDisabled (pCfg->CfgDemoPrm.bDemo && pCfg->CfgDemoPrm.bBlockExit);
	if (ImGui::Button ("Exit", ImVec2 (bw, 0)))
		RequestExit ();
	ImGui::EndDisabled ();
	ImGui::EndChild ();

	ImGui::SameLine ();
	ImGui::BeginChild ("##page", ImVec2 (0, 0), ImGuiChildFlags_None, ImGuiWindowFlags_NoBackground);
	if (m_page >= 0 && m_page < (int)TabList.size ())
		TabList[m_page]->Draw ();
	ImGui::EndChild ();

	ImGui::End ();

	ImGuiIO &io = ImGui::GetIO ();
	if (io.MouseDelta.x != 0.0f || io.MouseDelta.y != 0.0f || ImGui::IsAnyItemActive ())
		ResetIdleTimer ();
}

LRESULT orbiter::LaunchpadDialog::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	LaunchpadDialog *lp = (LaunchpadDialog *)oplat::GetWindowUserData (hWnd);
	if (!lp) return DefWindowProc (hWnd, uMsg, wParam, lParam);
	switch (uMsg) {
	case WM_CLOSE:
		lp->RequestExit ();
		return 0;
	case WM_DESTROY:
		lp->hWnd = NULL;
		return 0;
	}
	if (lp->m_ctx) {
		ImGuiContext *prev = ImGui::GetCurrentContext ();
		ImGui::SetCurrentContext (lp->m_ctx);
		ImGui_ImplOrbiter_WndProcHandler (hWnd, uMsg, wParam, lParam);
		ImGui::SetCurrentContext (prev);
	}
	if (uMsg == WM_KEYDOWN || uMsg == WM_LBUTTONDOWN || uMsg == WM_MOUSEMOVE)
		lp->ResetIdleTimer ();
	return 0;
}

//=============================================================================
// class LaunchpadTab

orbiter::LaunchpadTab::LaunchpadTab (LaunchpadDialog *lp, const char *label, const char *helptopic)
{
	pLp = lp;
	pCfg = lp->Cfg ();
	m_label = label;
	m_helptopic = helptopic;
}

orbiter::LaunchpadTab::~LaunchpadTab ()
{
}

bool orbiter::LaunchpadTab::OpenHelp ()
{
	if (!m_helptopic) return false;
	OpenTabHelp (m_helptopic);
	return true;
}

void orbiter::LaunchpadTab::OpenTabHelp (const char *topic)
{
	char cbuf[256];
	snprintf (cbuf, sizeof (cbuf), "/%s.htm", topic);
	::OpenHelp (DefHelpContext.helpfile, cbuf);
}

//=============================================================================
// Route ImGui dialogs to the Launchpad when no simulation session is running

orbiter::LaunchpadDialog *LaunchpadInstance ()
{
	return s_launchpad;
}
