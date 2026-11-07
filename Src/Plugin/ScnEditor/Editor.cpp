// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//
// Editor.cpp
//
// Implementation of the ScnEditor dialog and its pages.
// ==============================================================

#include "orbitersdk.h"
#include "Editor.h"
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"
#include <dlfcn.h>
#include <algorithm>
#include <filesystem>
#include <functional>
#include <cstring>
#include <cstdio>
#include <cmath>

namespace fs = std::filesystem;
using std::min;
using std::max;

namespace {

const char *DEGSTR = "\xC2\xB0";

double lengthscale[4] = {1.0, 1e-3, 1.0/AU, 1.0};
const double anglescale[2] = {DEG, 1.0};

HELPCONTEXT g_hc = {
	(char*)"html/plugin/ScnEditor.chm",
	0,
	(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhc",
	(char*)"html/plugin/ScnEditor.chm::/ScnEditor.hhk"
};

const char *errmsg_dock[4] = {
	"Selected dock is already in use.",
	"Selected dock on target vessel is already in use.",
	"Target vessel is already part of the vessel superstructure.",
	"Target dock number invalid or out of range."
};

const char *propmode[4] = {
	"Maintain fixed state vectors",
	"Maintain fixed surface position",
	"Propagate along osculating elements",
	"Destroy vessel"
};

float Em () { return ImGui::GetFontSize (); }

float FieldWidth () { return Em () * 9.0f; }

int StepButtons (const char *id, bool coarse = false)
{
	const ImGuiStyle &style = ImGui::GetStyle ();
	const float sz = ImGui::GetFrameHeight ();
	const char *tip = "Ctrl+click for larger steps";
	int dir = 0;
	ImGui::PushID (id);
	ImGui::PushItemFlag (ImGuiItemFlags_ButtonRepeat, true);
	ImGui::SameLine (0, style.ItemInnerSpacing.x);
	if (ImGui::Button ("-", ImVec2 (sz, sz))) dir = -1;
	if (coarse && ImGui::IsItemHovered (ImGuiHoveredFlags_DelayNormal)) ImGui::SetTooltip ("%s", tip);
	ImGui::SameLine (0, style.ItemInnerSpacing.x);
	if (ImGui::Button ("+", ImVec2 (sz, sz))) dir = 1;
	if (coarse && ImGui::IsItemHovered (ImGuiHoveredFlags_DelayNormal)) ImGui::SetTooltip ("%s", tip);
	ImGui::PopItemFlag ();
	ImGui::PopID ();
	if (dir && coarse && ImGui::GetIO ().KeyCtrl) dir *= 2;
	return dir;
}

int ValueRow (const char *label, const char *id, double *v, const char *fmt, bool coarse, const char *unit = 0, bool *edited = 0)
{
	if (label) ImGui::TextUnformatted (label);
	ImGui::PushID (id);
	ImGui::SetNextItemWidth (FieldWidth ());
	bool e = ImGui::InputDouble ("##v", v, 0.0, 0.0, fmt);
	int d = StepButtons ("s", coarse);
	if (unit) {
		ImGui::SameLine ();
		ImGui::TextUnformatted (unit);
	}
	ImGui::PopID ();
	if (edited) *edited = e;
	return d;
}

bool FooterButton (const char *label, bool first = false)
{
	if (!first) ImGui::SameLine ();
	return ImGui::Button (label, ImVec2 (Em () * 6.0f, 0));
}

void ReadOnlyText (const char *id, const char *text)
{
	char cbuf[256];
	strncpy (cbuf, text ? text : "", 255);
	cbuf[255] = '\0';
	ImGui::SetNextItemWidth (-FLT_MIN);
	ImGui::InputText (id, cbuf, 256, ImGuiInputTextFlags_ReadOnly);
}

std::string ObjName (OBJHANDLE hObj)
{
	char cbuf[256] = "";
	if (hObj) oapiGetObjectName (hObj, cbuf, 256);
	return cbuf;
}

bool BodyCombo (const char *id, OBJHANDLE *hBody, bool noStars)
{
	bool changed = false;
	ImGui::SetNextItemWidth (-FLT_MIN);
	if (ImGui::BeginCombo (id, ObjName (*hBody).c_str ())) {
		std::vector<std::pair<std::string, OBJHANDLE>> list;
		for (DWORD n = 0; n < oapiGetGbodyCount (); n++) {
			OBJHANDLE h = oapiGetGbodyByIndex (n);
			if (noStars && oapiGetObjectType (h) == OBJTP_STAR) continue;
			list.emplace_back (ObjName (h), h);
		}
		std::sort (list.begin (), list.end ());
		for (auto &b : list) {
			if (ImGui::Selectable (b.first.c_str (), b.second == *hBody) && b.second != *hBody) {
				*hBody = b.second;
				changed = true;
			}
		}
		ImGui::EndCombo ();
	}
	return changed;
}

int VesselListBox (const char *id, OBJHANDLE *sel, const std::function<bool(OBJHANDLE)> &filter, const ImVec2 &size)
{
	int res = 0;
	std::vector<std::pair<std::string, OBJHANDLE>> list;
	for (DWORD i = 0; i < oapiGetVesselCount (); i++) {
		OBJHANDLE hV = oapiGetVesselByIndex (i);
		if (filter (hV)) list.emplace_back (ObjName (hV), hV);
	}
	std::sort (list.begin (), list.end ());
	if (ImGui::BeginListBox (id, size)) {
		for (auto &v : list) {
			if (ImGui::Selectable (v.first.c_str (), v.second == *sel, ImGuiSelectableFlags_AllowDoubleClick)) {
				*sel = v.second;
				res = ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left) ? 2 : 1;
			}
		}
		ImGui::EndListBox ();
	}
	return res;
}

bool BeginColumns (const char *id)
{
	return ImGui::BeginTable (id, 2, ImGuiTableFlags_SizingStretchSame | ImGuiTableFlags_PadOuterX);
}

void Crt2Pol (VECTOR3 &pos, VECTOR3 &vel)
{
	double r      = sqrt  (pos.x*pos.x + pos.y*pos.y + pos.z*pos.z);
	double lng    = atan2 (pos.z, pos.x);
	double lat    = asin  (pos.y/r);
	double drdt   = (vel.x*pos.x + vel.y*pos.y + vel.z*pos.z) / r;
	double dlngdt = (vel.z*pos.x - pos.z*vel.x) / (pos.x*pos.x + pos.z*pos.z);
	double dlatdt = (vel.y*r - pos.y*drdt) / (r*sqrt(r*r - pos.y*pos.y));
	pos.data[0] = r;
	pos.data[1] = lng;
	pos.data[2] = lat;
	vel.data[0] = drdt;
	vel.data[1] = dlngdt;
	vel.data[2] = dlatdt;
}

void Pol2Crt (VECTOR3 &pos, VECTOR3 &vel)
{
	double r   = pos.data[0];
	double lng = pos.data[1], clng = cos(lng), slng = sin(lng);
	double lat = pos.data[2], clat = cos(lat), slat = sin(lat);
	double x   = r * cos(lat) * cos(lng);
	double z   = r * cos(lat) * sin(lng);
	double y   = r * sin(lat);
	double dxdt = vel.data[0]*clat*clng - vel.data[1]*r*clat*slng - vel.data[2]*r*slat*clng;
	double dzdt = vel.data[0]*clat*slng + vel.data[1]*r*clat*clng - vel.data[2]*r*slat*slng;
	double dydt = vel.data[0]*slat + vel.data[2]*r*clat;
	pos.data[0] = x;
	pos.data[1] = y;
	pos.data[2] = z;
	vel.data[0] = dxdt;
	vel.data[1] = dydt;
	vel.data[2] = dzdt;
}

void OpenEditor (void *context)
{
	((ScnEditor*)context)->Open ();
}

}

// ==============================================================
// ScnEditor class definition
// ==============================================================

ScnEditor::ScnEditor (): ImGuiDialog ("Scenario editor", {560.0f, 540.0f})
{
	hVessel = 0;
	hEdLib  = 0;
	cur     = -1;
	pending = -1;

	dwCmd = oapiRegisterCustomCmd (
		(char*)"Scenario editor",
		(char*)"Create, delete and configure spacecraft",
		OpenEditor, this);

	dwMenuCmd = oapiRegisterCustomMenuCmd ("ScnEdit", "MenuInfoBar/ScnEdit.png", OpenEditor, this);
}

ScnEditor::~ScnEditor ()
{
	oapiCloseDialog (this);
	active = false;
	OnClose ();
	oapiUnregisterCustomCmd (dwCmd);
	oapiUnregisterCustomMenuCmd (dwMenuCmd);
}

void ScnEditor::Open ()
{
	if (!active) {
		pages.clear ();
		pages.emplace_back (new EditorPage_Vessel (this));
		pages.emplace_back (new EditorPage_New (this));
		pages.emplace_back (new EditorPage_Save (this));
		pages.emplace_back (new EditorPage_Edit (this));
		pages.emplace_back (new EditorPage_Elements (this));
		pages.emplace_back (new EditorPage_Statevec (this));
		pages.emplace_back (new EditorPage_Landed (this));
		pages.emplace_back (new EditorPage_Orientation (this));
		pages.emplace_back (new EditorPage_AngularVel (this));
		pages.emplace_back (new EditorPage_Propellant (this));
		pages.emplace_back (new EditorPage_Docking (this));
		pages.emplace_back (new EditorPage_Date (this));
		pending = -1;
		ShowPage (PG_VESSEL);
	}
	oapiOpenDialog (this);
}

void ScnEditor::Close ()
{
	active = false;
}

void ScnEditor::OnClose ()
{
	pages.clear ();
	cur = -1;
	pending = -1;
	FreeVesselLibrary ();
}

void ScnEditor::Display ()
{
	ImGui::SetNextWindowSize (ImVec2 (defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);
	bool visible = ImGui::Begin (name.c_str (), &active, ImGuiWindowFlags_NoCollapse);

	if (ImGui::MenuButton (ICON_FA_CIRCLE_QUESTION, "Help") && cur >= 0)
		pages[cur]->OpenHelp ();
	bool paused = oapiGetPause ();
	if (ImGui::MenuButton (paused ? ICON_FA_PLAY : ICON_FA_PAUSE, paused ? "Resume simulation" : "Pause simulation", Em () * 1.7f))
		oapiSetPause (!paused);

	if (cur >= PG_EDIT && cur != PG_DATE && !oapiIsVessel (hVessel))
		ShowPage (PG_VESSEL);

	if (visible && cur >= 0) {
		const ImGuiStyle &style = ImGui::GetStyle ();
		float footer = ImGui::GetFrameHeight () + style.ItemSpacing.y * 2.0f + 1.0f;
		ImGui::BeginChild ("page", ImVec2 (0, -footer));
		pages[cur]->Draw ();
		ImGui::EndChild ();
		ImGui::Separator ();
		pages[cur]->Footer ();
	}
	ImGui::End ();

	if (active && pending >= 0) {
		int pg = pending;
		pending = -1;
		ShowPage (pg);
	}
	if (!active) OnClose ();
}

void ScnEditor::ShowPage (int page)
{
	if (page >= 0 && page < (int)pages.size ()) {
		cur = page;
		pages[cur]->Init ();
	}
}

void ScnEditor::SwitchPage (int page)
{
	pending = page;
}

int ScnEditor::AddPage (ScnEditorPage *page)
{
	pages.emplace_back (page);
	return (int)pages.size () - 1;
}

void ScnEditor::DelCustomPages ()
{
	if (pages.size () > PG_COUNT) {
		if (cur >= PG_COUNT) cur = PG_EDIT;
		pages.resize (PG_COUNT);
	}
}

bool ScnEditor::CreateVessel (const char *name, const char *classname)
{
	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs));
	vs.version = 2;
	vs.rbody = oapiGetGbodyByName ((char*)"Earth");
	if (!vs.rbody) vs.rbody = oapiGetGbodyByIndex (0);
	double rad = 1.1 * oapiGetSize (vs.rbody);
	double vel = sqrt (GGRAV * oapiGetMass (vs.rbody) / rad);
	vs.rpos = _V(rad,0,0);
	vs.rvel = _V(0,0,vel);
	OBJHANDLE newvessel = oapiCreateVesselEx (name, classname, &vs);
	if (!newvessel) return false;
	hVessel = newvessel;
	return true;
}

void ScnEditor::VesselDeleted (OBJHANDLE hV)
{
	if (!active || pages.empty ()) return;
	if (hVessel == hV) {
		hVessel = 0;
		static_cast<EditorPage_Edit*>(pages[PG_EDIT].get ())->Reset ();
		DelCustomPages ();
		if (cur != PG_DATE && cur != PG_SAVE && cur != PG_NEW)
			ShowPage (PG_VESSEL);
	}
	static_cast<EditorPage_Vessel*>(pages[PG_VESSEL].get ())->VesselDeleted (hV);
}

void *ScnEditor::LoadVesselLibrary (const VESSEL *vessel)
{
	FreeVesselLibrary ();
	char cbuf[512];
	if (!vessel->GetEditorModule (cbuf)) return 0;
	std::string mod (cbuf);
	std::replace (mod.begin (), mod.end (), '\\', '/');
	size_t n = mod.size ();
	if (n > 4 && strcasecmp (mod.c_str () + n - 4, ".dll") == 0) mod.resize (n - 4);
	n = mod.size ();
	if (n <= 6 || strcasecmp (mod.c_str () + n - 6, ".dylib") != 0) mod += ".dylib";
	hEdLib = dlopen (("Modules/" + mod).c_str (), RTLD_NOW | RTLD_LOCAL);
	if (!hEdLib) hEdLib = dlopen (mod.c_str (), RTLD_NOW | RTLD_LOCAL);
	return hEdLib;
}

void ScnEditor::FreeVesselLibrary ()
{
	if (hEdLib) {
		dlclose (hEdLib);
		hEdLib = 0;
	}
}

bool ScnEditor::AddFuncButton (const EditorFuncSpec *spec)
{
	if (pages.size () <= PG_EDIT) return false;
	return static_cast<EditorPage_Edit*>(pages[PG_EDIT].get ())->AddFuncButton (spec);
}

bool ScnEditor::AddPageButton (const EditorPageSpec *spec)
{
	if (pages.size () <= PG_EDIT) return false;
	return static_cast<EditorPage_Edit*>(pages[PG_EDIT].get ())->AddPageButton (spec);
}

// ==============================================================
// ScnEditorPage class definition
// ==============================================================

void ScnEditorPage::OpenHelp ()
{
	g_hc.topic = (char*)HelpTopic ();
	oapiOpenHelp (&g_hc);
}

// ==============================================================
// EditorPage_Vessel class definition
// ==============================================================

EditorPage_Vessel::EditorPage_Vessel (ScnEditor *editor): ScnEditorPage (editor)
{
	sel = 0;
	track = true;
	deleteError = false;
}

void EditorPage_Vessel::Init ()
{
	SelectVessel (0);
}

void EditorPage_Vessel::SelectVessel (OBJHANDLE hV)
{
	if (!hV) hV = oapiCameraTarget ();
	if (!oapiIsVessel (hV)) hV = oapiGetFocusObject ();
	if (oapiIsVessel (hV)) {
		sel = hV;
		ed->hVessel = hV;
	}
}

void EditorPage_Vessel::VesselSelected ()
{
	if (sel && track) oapiCameraAttach (sel, 1);
}

void EditorPage_Vessel::VesselDeleted (OBJHANDLE hV)
{
	if (sel != hV) return;
	sel = 0;
	std::vector<std::pair<std::string, OBJHANDLE>> list;
	for (DWORD i = 0; i < oapiGetVesselCount (); i++) {
		OBJHANDLE h = oapiGetVesselByIndex (i);
		if (h != hV) list.emplace_back (ObjName (h), h);
	}
	if (list.empty ()) return;
	std::sort (list.begin (), list.end ());
	sel = list.front ().second;
	VesselSelected ();
}

void EditorPage_Vessel::EditVessel ()
{
	if (!oapiIsVessel (sel)) return;
	ed->hVessel = sel;
	ed->SwitchPage (PG_EDIT);
}

bool EditorPage_Vessel::CanDelete (OBJHANDLE hV)
{
	for (DWORD i = 0; i < oapiGetVesselCount (); i++) {
		OBJHANDLE obj = oapiGetVesselByIndex (i);
		if (obj == hV) continue;
		if (oapiGetVesselInterface (obj)->GetEnableFocus ()) return true;
	}
	return false;
}

void EditorPage_Vessel::DeleteVessel ()
{
	if (!oapiIsVessel (sel)) return;
	if (CanDelete (sel)) oapiDeleteVessel (sel);
	else deleteError = true;
}

void EditorPage_Vessel::Draw ()
{
	struct Entry { std::string name, cls, ref; OBJHANDLE h; };
	std::vector<Entry> list;
	for (DWORD i = 0; i < oapiGetVesselCount (); i++) {
		OBJHANDLE hV = oapiGetVesselByIndex (i);
		VESSEL *v = oapiGetVesselInterface (hV);
		const char *cls = v->GetClassName ();
		list.push_back ({v->GetName (), cls ? cls : "", ObjName (v->GetGravityRef ()), hV});
	}
	std::sort (list.begin (), list.end (), [](const Entry &a, const Entry &b) { return a.name < b.name; });

	const ImGuiStyle &style = ImGui::GetStyle ();
	const float bw = Em () * 6.0f;
	ImGui::TextUnformatted ("Vessels in current scenario");
	ImVec2 avail = ImGui::GetContentRegionAvail ();
	float listw = avail.x - bw - style.ItemSpacing.x;
	const ImGuiTableFlags flags = ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersOuter | ImGuiTableFlags_SizingStretchProp;
	if (ImGui::BeginTable ("vessels", 3, flags, ImVec2 (listw, avail.y))) {
		ImGui::TableSetupScrollFreeze (0, 1);
		ImGui::TableSetupColumn ("Name", 0, 1.2f);
		ImGui::TableSetupColumn ("Class", 0, 1.0f);
		ImGui::TableSetupColumn ("Reference", 0, 0.8f);
		ImGui::TableHeadersRow ();
		for (auto &e : list) {
			ImGui::TableNextRow ();
			ImGui::TableSetColumnIndex (0);
			ImGui::PushID (e.h);
			if (ImGui::Selectable (e.name.c_str (), e.h == sel, ImGuiSelectableFlags_SpanAllColumns | ImGuiSelectableFlags_AllowDoubleClick)) {
				if (e.h != sel) {
					sel = e.h;
					VesselSelected ();
				}
				if (ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left)) EditVessel ();
			}
			ImGui::PopID ();
			ImGui::TableSetColumnIndex (1);
			ImGui::TextUnformatted (e.cls.c_str ());
			ImGui::TableSetColumnIndex (2);
			ImGui::TextUnformatted (e.ref.c_str ());
		}
		ImGui::EndTable ();
	}

	ImGui::SameLine ();
	ImGui::BeginGroup ();
	float y0 = ImGui::GetCursorPosY ();
	if (ImGui::Checkbox ("Track", &track)) VesselSelected ();
	ImGui::SetCursorPosY (max (ImGui::GetCursorPosY (), y0 + avail.y - 3.0f * ImGui::GetFrameHeightWithSpacing () + style.ItemSpacing.y));
	if (ImGui::Button ("New...", ImVec2 (bw, 0))) ed->SwitchPage (PG_NEW);
	if (ImGui::Button ("Edit...", ImVec2 (bw, 0))) EditVessel ();
	if (ImGui::Button ("Delete", ImVec2 (bw, 0))) DeleteVessel ();
	ImGui::EndGroup ();

	if (deleteError) {
		ImGui::OpenPopup ("Delete vessel");
		deleteError = false;
	}
	if (ImGui::BeginPopupModal ("Delete vessel", NULL, ImGuiWindowFlags_AlwaysAutoResize)) {
		ImGui::TextUnformatted ("Cannot delete the last focusable vessel in a scenario.");
		if (ImGui::Button ("OK", ImVec2 (Em () * 6.0f, 0))) ImGui::CloseCurrentPopup ();
		ImGui::EndPopup ();
	}
}

void EditorPage_Vessel::Footer ()
{
	if (FooterButton ("Close", true)) ed->Close ();
	if (FooterButton ("Save...")) ed->SwitchPage (PG_SAVE);
	if (FooterButton ("Date...")) ed->SwitchPage (PG_DATE);
}

// ==============================================================
// EditorPage_New class definition
// ==============================================================

EditorPage_New::EditorPage_New (ScnEditor *editor): ScnEditorPage (editor)
{
	name[0] = '\0';
	camera = true;
	focus = false;
	img = 0;
	imgw = imgh = 0;
	create = false;
}

EditorPage_New::~EditorPage_New ()
{
	ReleaseImage ();
}

void EditorPage_New::Init ()
{
	camera = true;
	tree.clear ();
	ScanConfigDir ("Config/Vessels", "", tree);
	if (!seltype.empty ()) SelectType (seltype);
}

void EditorPage_New::ScanConfigDir (const char *dir, const std::string &prefix, std::vector<Node> &list)
{
	std::vector<Node> dirs, files;
	std::error_code ec;
	for (const auto &entry : fs::directory_iterator (dir, ec)) {
		std::string fname = entry.path ().filename ().string ();
		std::string path = prefix.empty () ? fname : prefix + "/" + fname;
		if (entry.is_directory (ec)) {
			Node node = {fname, path, true, {}};
			ScanConfigDir (entry.path ().string ().c_str (), path, node.children);
			dirs.push_back (std::move (node));
		} else if (entry.is_regular_file (ec) && entry.path ().extension ().string () == ".cfg") {
			bool skip = false;
			FILEHANDLE hFile = oapiOpenFile (entry.path ().string ().c_str (), FILE_IN);
			if (hFile) {
				bool b;
				skip = (oapiReadItem_bool (hFile, (char*)"EditorCreate", b) && !b);
				oapiCloseFile (hFile, FILE_IN);
			}
			if (skip) continue;
			std::string stem = entry.path ().stem ().string ();
			files.push_back ({stem, prefix.empty () ? stem : prefix + "/" + stem, false, {}});
		}
	}
	auto byName = [](const Node &a, const Node &b) { return a.name < b.name; };
	std::sort (files.begin (), files.end (), byName);
	std::sort (dirs.begin (), dirs.end (), byName);
	for (auto &f : files) list.push_back (std::move (f));
	for (auto &d : dirs) list.push_back (std::move (d));
}

void EditorPage_New::DrawTree (const std::vector<Node> &list)
{
	for (const Node &node : list) {
		ImGui::PushID (node.name.c_str ());
		if (node.dir) {
			if (ImGui::TreeNodeEx (node.name.c_str (), ImGuiTreeNodeFlags_SpanAvailWidth)) {
				DrawTree (node.children);
				ImGui::TreePop ();
			}
		} else {
			ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen | ImGuiTreeNodeFlags_SpanAvailWidth;
			if (node.path == seltype) flags |= ImGuiTreeNodeFlags_Selected;
			ImGui::TreeNodeEx (node.name.c_str (), flags);
			if (ImGui::IsItemClicked () && node.path != seltype) SelectType (node.path);
			if (ImGui::IsItemHovered () && ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left)) create = true;
		}
		ImGui::PopID ();
	}
}

void EditorPage_New::ReleaseImage ()
{
	if (img) {
		oapiDestroySurface (img);
		img = 0;
	}
	imgw = imgh = 0;
}

void EditorPage_New::SelectType (const std::string &path)
{
	seltype = path;
	ReleaseImage ();
	char pathname[512], imagename[256];
	snprintf (pathname, sizeof(pathname), "Vessels/%s.cfg", path.c_str ());
	FILEHANDLE hFile = oapiOpenFile (pathname, FILE_IN, CONFIG);
	if (!hFile) return;
	if (oapiReadItem_string (hFile, (char*)"ImageBmp", imagename)) {
		std::string fname (imagename);
		std::replace (fname.begin (), fname.end (), '\\', '/');
		img = oapiLoadSurfaceEx (fname.c_str (), OAPISURFACE_TEXTURE, true);
		if (img && !oapiGetSurfaceSize (img, &imgw, &imgh)) imgw = imgh = 0;
	}
	oapiCloseFile (hFile, FILE_IN);
}

bool EditorPage_New::CreateVessel ()
{
	if (name[0] == '\0') return false;
	if (oapiGetVesselByName (name)) return false;
	if (seltype.empty ()) return false;
	if (!ed->CreateVessel (name, seltype.c_str ())) return false;
	if (focus) oapiSetFocusObject (ed->hVessel);
	if (camera) oapiCameraAttach (ed->hVessel, 1);
	return true;
}

void EditorPage_New::Draw ()
{
	create = false;
	if (BeginColumns ("new")) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Vessel name");
		ImGui::SetNextItemWidth (-FLT_MIN);
		ImGui::InputText ("##name", name, sizeof(name));
		ImGui::TextUnformatted ("Vessel type");
		ImGui::BeginChild ("types", ImVec2 (0, ImGui::GetContentRegionAvail ().y), ImGuiChildFlags_Borders);
		DrawTree (tree);
		ImGui::EndChild ();

		ImGui::TableSetColumnIndex (1);
		ImGui::Checkbox ("Set camera", &camera);
		ImGui::Checkbox ("Set input focus", &focus);
		if (img && imgw > 0 && imgh > 0) {
			ImGui::Spacing ();
			float w = ImGui::GetContentRegionAvail ().x;
			float h = w * (float)imgh / (float)imgw;
			float hmax = ImGui::GetContentRegionAvail ().y;
			if (h > hmax) h = hmax, w = h * (float)imgw / (float)imgh;
			ImGui::Image (img, ImVec2 (w, h));
		}
		ImGui::EndTable ();
	}
	if (create && CreateVessel ()) ed->SwitchPage (PG_EDIT);
}

void EditorPage_New::Footer ()
{
	if (FooterButton ("Cancel", true)) ed->SwitchPage (PG_VESSEL);
	if (FooterButton ("Create")) {
		if (CreateVessel ()) ed->SwitchPage (PG_EDIT);
	}
}

// ==============================================================
// EditorPage_Save class definition
// ==============================================================

EditorPage_Save::EditorPage_Save (ScnEditor *editor): ScnEditorPage (editor)
{
	fname[0] = title[0] = text[0] = '\0';
}

bool EditorPage_Save::SaveScenario ()
{
	std::string desc;
	if (title[0]) {
		desc = "<h1>";
		desc += title;
		desc += "</h1>\n";
	}
	if (text[0]) {
		desc += "<p>";
		for (const char *c = text; *c; c++) {
			if (*c == '\r') continue;
			else if (*c == '\n') desc += "</p>\n<p>";
			else desc += *c;
		}
		desc += "</p>";
	}
	return oapiSaveScenario (fname, desc.c_str ());
}

void EditorPage_Save::Draw ()
{
	ImGui::SeparatorText ("Scenario file name");
	ImGui::SetNextItemWidth (-FLT_MIN);
	ImGui::InputText ("##fname", fname, sizeof(fname));
	ImGui::SeparatorText ("Description");
	ImGui::TextUnformatted ("Title");
	ImGui::SetNextItemWidth (-FLT_MIN);
	ImGui::InputText ("##title", title, sizeof(title));
	ImGui::TextUnformatted ("Text");
	ImGui::InputTextMultiline ("##text", text, sizeof(text), ImVec2 (-FLT_MIN, ImGui::GetContentRegionAvail ().y));
}

void EditorPage_Save::Footer ()
{
	if (FooterButton ("Save", true)) {
		if (SaveScenario ()) ed->SwitchPage (PG_VESSEL);
	}
	if (FooterButton ("Cancel")) ed->SwitchPage (PG_VESSEL);
}

// ==============================================================
// EditorPage_Date class definition
// ==============================================================

EditorPage_Date::EditorPage_Date (ScnEditor *editor): ScnEditorPage (editor)
{
	mjd = 0.0;
	memset (&date, 0, sizeof(date));
	memset (ut, 0, sizeof(ut));
	vmjd = vjd = vjc = vepoch = 0.0;
	propOrbital = 2;
	propSuborbital = 1;
}

void EditorPage_Date::Init ()
{
	Refresh ();
}

void EditorPage_Date::Apply ()
{
	static const int OrbitalMode[3] = {PROP_ORBITAL_FIXEDSTATE, PROP_ORBITAL_FIXEDSURF, PROP_ORBITAL_ELEMENTS};
	static const int SOrbitalMode[4] = {PROP_SORBITAL_FIXEDSTATE, PROP_SORBITAL_FIXEDSURF, PROP_SORBITAL_ELEMENTS, PROP_SORBITAL_DESTROY};
	oapiSetSimMJD (mjd, OrbitalMode[propOrbital] | SOrbitalMode[propSuborbital]);
}

void EditorPage_Date::Refresh ()
{
	SetMJD (oapiGetSimMJD ());
}

void EditorPage_Date::UpdateDateTime ()
{
	ut[0] = date.tm_mday;
	ut[1] = date.tm_mon;
	ut[2] = date.tm_year + 1900;
	ut[3] = date.tm_hour;
	ut[4] = date.tm_min;
	ut[5] = date.tm_sec;
}

void EditorPage_Date::SetUT (struct tm *new_date, bool reset_ut)
{
	mjd = date2mjd (new_date);
	vmjd = mjd;
	vjd = mjd + 2400000.5;
	vjc = MJD2JC (mjd);
	vepoch = MJD2Jepoch (mjd);
	if (reset_ut) UpdateDateTime ();
}

void EditorPage_Date::SetMJD (double new_mjd)
{
	mjd = new_mjd;
	memcpy (&date, mjddate (mjd), sizeof (date));
	UpdateDateTime ();
	vmjd = mjd;
	vjd = mjd + 2400000.5;
	vjc = MJD2JC (mjd);
	vepoch = MJD2Jepoch (mjd);
}

void EditorPage_Date::OnChangeDateTime ()
{
	int day = ut[0], month = ut[1], year = ut[2] - 1900, hour = ut[3], min = ut[4], sec = ut[5];
	if (day < 1 || day > 31 || month < 1 || month > 12) return;
	if (day == date.tm_mday && month == date.tm_mon && year == date.tm_year &&
		hour == date.tm_hour && min == date.tm_min && sec == date.tm_sec) return;
	date.tm_mday = day;
	date.tm_mon  = month;
	date.tm_year = year;
	date.tm_hour = hour;
	date.tm_min  = min;
	date.tm_sec  = sec;
	SetUT (&date);
}

void EditorPage_Date::Draw ()
{
	static const char *label[6] = {"Day", "Month", "Year", "Hour", "Minute", "Second"};
	ImGui::SeparatorText ("Date and time (UT)");
	bool changed = false;
	double dmjd = 0.0;
	bool dut = false;
	if (ImGui::BeginTable ("ut", 6, ImGuiTableFlags_SizingFixedFit)) {
		for (int i = 0; i < 6; i++) {
			ImGui::TableNextColumn ();
			ImGui::TextUnformatted (label[i]);
		}
		ImGui::TableNextRow ();
		for (int i = 0; i < 6; i++) {
			ImGui::TableNextColumn ();
			ImGui::PushID (i);
			ImGui::SetNextItemWidth (Em () * (i == 2 ? 3.0f : 2.0f));
			if (ImGui::InputScalar ("##v", ImGuiDataType_S32, &ut[i], NULL, NULL, i == 2 ? "%04d" : "%02d"))
				changed = true;
			int d = StepButtons ("s");
			ImGui::PopID ();
			if (!d) continue;
			switch (i) {
			case 0: dmjd = d; break;
			case 1:
				date.tm_mon += d;
				if (date.tm_mon < 1) date.tm_year--, date.tm_mon = 12;
				else if (date.tm_mon > 12) date.tm_year++, date.tm_mon = 1;
				dut = true;
				break;
			case 2: date.tm_year += d; dut = true; break;
			case 3: dmjd = d/24.0; break;
			case 4: dmjd = d/(24.0*60.0); break;
			case 5: dmjd = d/(24.0*3600.0); break;
			}
		}
		ImGui::EndTable ();
	}
	if (changed) OnChangeDateTime ();
	if (dmjd || dut) {
		if (dmjd) SetMJD (mjd + dmjd);
		else SetUT (&date, true);
		Apply ();
	}

	ImGui::Spacing ();
	if (BeginColumns ("jd")) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::SeparatorText ("Modified Julian Date (MJD)");
		ImGui::SetNextItemWidth (-FLT_MIN);
		if (ImGui::InputDouble ("##mjd", &vmjd, 0.0, 0.0, "%0.6f") && fabs (vmjd - mjd) > 1e-6) {
			double v = vmjd;
			SetMJD (v);
		}
		ImGui::TableSetColumnIndex (1);
		ImGui::SeparatorText ("Julian Date (JD)");
		ImGui::SetNextItemWidth (-FLT_MIN);
		if (ImGui::InputDouble ("##jd", &vjd, 0.0, 0.0, "%0.6f"))
			SetMJD (vjd - 2400000.5);
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::SeparatorText ("Julian Century (JC2000)");
		ImGui::SetNextItemWidth (-FLT_MIN);
		if (ImGui::InputDouble ("##jc", &vjc, 0.0, 0.0, "%0.10f"))
			SetMJD (JC2MJD (vjc));
		ImGui::TableSetColumnIndex (1);
		ImGui::SeparatorText ("Epoch");
		ImGui::SetNextItemWidth (-FLT_MIN);
		if (ImGui::InputDouble ("##epoch", &vepoch, 0.0, 0.0, "%0.8f"))
			SetMJD (Jepoch2MJD (vepoch));
		ImGui::EndTable ();
	}

	ImGui::SeparatorText ("Vessel state propagation");
	ImGui::TextUnformatted ("Orbital");
	ImGui::SetNextItemWidth (Em () * 16.0f);
	ImGui::Combo ("##orbital", &propOrbital, propmode, 3);
	ImGui::TextUnformatted ("Suborbital");
	ImGui::SetNextItemWidth (Em () * 16.0f);
	ImGui::Combo ("##suborbital", &propSuborbital, propmode, 4);
}

void EditorPage_Date::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_VESSEL);
	if (FooterButton ("Now")) {
		SetMJD (oapiGetSysMJD ());
		Apply ();
	}
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Edit class definition
// ==============================================================

EditorPage_Edit::EditorPage_Edit (ScnEditor *editor): ScnEditorPage (editor)
{
	hVessel = 0;
}

void EditorPage_Edit::Init ()
{
	VESSEL *vessel = Vessel ();
	if (!vessel || hVessel == ed->hVessel) return;
	hVessel = ed->hVessel;
	custom.clear ();
	ed->DelCustomPages ();
	if (void *hLib = ed->LoadVesselLibrary (vessel)) {
		SecInitFunc secInit = (SecInitFunc)dlsym (hLib, "secInit");
		if (secInit) secInit (ed, hVessel);
	}
}

bool EditorPage_Edit::AddFuncButton (const EditorFuncSpec *spec)
{
	if (!spec || !spec->func) return false;
	custom.push_back ({spec->btnlabel, spec->func, 0});
	return true;
}

bool EditorPage_Edit::AddPageButton (const EditorPageSpec *spec)
{
	if (!spec || !spec->func) return false;
	custom.push_back ({spec->btnlabel, 0, ed->AddPage (new EditorPage_Custom (ed, spec))});
	return true;
}

void EditorPage_Edit::Draw ()
{
	VESSEL *vessel = Vessel ();
	const ImVec2 bsize (-FLT_MIN, 0);

	if (BeginColumns ("id")) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Vessel");
		ReadOnlyText ("##name", vessel->GetName ());
		ImGui::TextUnformatted ("Status");
		ReadOnlyText ("##status", vessel->GetFlightStatus () & 1 ? "Inactive (landed)" : "Active (flight)");
		ImGui::TableSetColumnIndex (1);
		ImGui::TextUnformatted ("Class");
		ReadOnlyText ("##class", vessel->GetClassName ());
		ImGui::EndTable ();
	}

	ImGui::Spacing ();
	if (BeginColumns ("groups")) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::SeparatorText ("Free flight");
		if (ImGui::Button ("Orbital elements", bsize)) ed->SwitchPage (PG_ELEMENTS);
		if (ImGui::Button ("State vectors", bsize)) ed->SwitchPage (PG_STATEVEC);
		if (ImGui::Button ("Orientation", bsize)) ed->SwitchPage (PG_ORIENTATION);
		if (ImGui::Button ("Angular velocity", bsize)) ed->SwitchPage (PG_ANGVEL);

		ImGui::TableSetColumnIndex (1);
		ImGui::SeparatorText ("On the ground");
		if (ImGui::Button ("Location", bsize)) ed->SwitchPage (PG_LANDED);
		ImGui::SeparatorText ("Misc");
		ImGui::BeginDisabled (!(vessel->GetPropellantCount () > 0 && vessel->GetMaxFuelMass () > 0.0));
		if (ImGui::Button ("Propellant", bsize)) ed->SwitchPage (PG_PROPELLANT);
		ImGui::EndDisabled ();
		ImGui::BeginDisabled (vessel->DockCount () == 0);
		if (ImGui::Button ("Docking", bsize)) ed->SwitchPage (PG_DOCKING);
		ImGui::EndDisabled ();
		ImGui::EndTable ();
	}

	if (!custom.empty ()) {
		ImGui::Spacing ();
		ImGui::SeparatorText ("More");
		if (ImGui::BeginTable ("custom", 3, ImGuiTableFlags_SizingStretchSame | ImGuiTableFlags_PadOuterX)) {
			for (size_t i = 0; i < custom.size (); i++) {
				ImGui::TableNextColumn ();
				ImGui::PushID ((int)i);
				if (ImGui::Button (custom[i].label.c_str (), bsize)) {
					if (custom[i].page) ed->SwitchPage (custom[i].page);
					else custom[i].func (ed->hVessel);
				}
				ImGui::PopID ();
			}
			ImGui::EndTable ();
		}
	}
}

void EditorPage_Edit::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_VESSEL);
}

// ==============================================================
// EditorPage_Elements class definition
// ==============================================================

EditorPage_Elements::EditorPage_Elements (ScnEditor *editor): ScnEditorPage (editor)
{
	memset (&el, 0, sizeof(el));
	memset (&prm, 0, sizeof(prm));
	elmjd = oapiGetSimMJD ();
	hRef = 0;
	frm = 0;
	epc = 1;
	memset (unit, 0, sizeof(unit));
	memset (val, 0, sizeof(val));
	prec = 6;
	valid = false;
}

void EditorPage_Elements::Init ()
{
	memset (unit, 0, sizeof(unit));
	epc = 1;
	frm = 0;
	hRef = Vessel ()->GetGravityRef ();
	valid = false;
	Refresh ();
}

double EditorPage_Elements::Scale (int i) const
{
	if (i == 0) return lengthscale[unit[0]];
	if (i == 1) return 1.0;
	return anglescale[unit[i]];
}

double &EditorPage_Elements::Element (int i)
{
	switch (i) {
	case 0: return el.a;
	case 1: return el.e;
	case 2: return el.i;
	case 3: return el.theta;
	case 4: return el.omegab;
	default: return el.L;
	}
}

void EditorPage_Elements::Apply ()
{
	const double eps = 1e-7;
	if (!hRef) return;
	double mjd = (epc ? (elmjd ? elmjd : 1e-10) : 0.0);
	for (int i = 0; i < 6; i++)
		Element (i) = val[i] / Scale (i);
	if (el.e >= 1 && el.e < 1+eps) el.e = 1+eps;
	el.a = fabs (el.a);
	if (el.e > 1.0) el.a = -el.a;
	if (!Vessel ()->SetElements (hRef, el, &prm, mjd, frm))
		MessageBeep (-1);
	Refresh ();
}

void EditorPage_Elements::Refresh ()
{
	if (!hRef) return;
	double mjd = (epc ? (elmjd ? elmjd : 1e-10) : 0.0);
	if (!Vessel ()->GetElements (hRef, el, &prm, mjd, frm)) return;
	valid = true;
	prec = (el.e < 1.0 ? 6 : 10);
	lengthscale[3] = 1.0/oapiGetSize (hRef);
	for (int i = 0; i < 6; i++)
		val[i] = Element (i) * Scale (i);
}

void EditorPage_Elements::Draw ()
{
	static const char *label[6] = {
		"Semi-major axis [SMa]", "Eccentricity [Ecc]", "Inclination [Inc]",
		"Longitude of ascending node [LAN]", "Longitude of periapsis [LPe]", "Mean longitude at epoch [eps]"
	};
	static const char *lunit[4] = {"m", "km", "AU", "planet rad."};
	static const char *aunit[2] = {"deg", "rad"};
	char fmt[16];
	snprintf (fmt, sizeof(fmt), "%%0.%dg", prec);

	if (!BeginColumns ("elements")) return;
	ImGui::TableNextRow ();
	ImGui::TableSetColumnIndex (0);
	ImGui::SeparatorText ("Osculating elements");
	for (int i = 0; i < 6; i++) {
		ImGui::PushID (i);
		ImGui::TextUnformatted (label[i]);
		ImGui::SetNextItemWidth (FieldWidth ());
		ImGui::InputDouble ("##v", &val[i], 0.0, 0.0, i == 0 ? "%0.10g" : fmt);
		int d = StepButtons ("s");
		if (i != 1) {
			ImGui::SameLine ();
			ImGui::SetNextItemWidth (-FLT_MIN);
			if (i == 0 ? ImGui::Combo ("##u", &unit[0], lunit, 4) : ImGui::Combo ("##u", &unit[i], aunit, 2))
				val[i] = Element (i) * Scale (i);
		}
		ImGui::PopID ();
		if (!d) continue;
		switch (i) {
		case 0:
			el.a *= (1.0 + d*1e-4);
			break;
		case 1:
			el.e *= (1.0 + d*0.001);
			if (el.e < 0.0) el.e = 0.0;
			break;
		case 2:
			el.i += d*RAD*0.1;
			if      (el.i >  PI) el.i -= 2.0*PI;
			else if (el.i < -PI) el.i += 2.0*PI;
			break;
		default: {
			double &v = Element (i);
			v += d*RAD*0.1;
			if      (v >= 2.0*PI) v -= 2.0*PI;
			else if (v <  0.0)    v += 2.0*PI;
			} break;
		}
		val[i] = Element (i) * Scale (i);
		Apply ();
	}

	ImGui::TableSetColumnIndex (1);
	ImGui::SeparatorText ("Reference");
	ImGui::TextUnformatted ("Orbit reference");
	if (BodyCombo ("##ref", &hRef, false)) Refresh ();
	ImGui::TextUnformatted ("Frame");
	ImGui::SetNextItemWidth (-FLT_MIN);
	static const char *frame[2] = {"ecliptic", "ref. equator"};
	if (ImGui::Combo ("##frm", &frm, frame, 2)) Refresh ();
	ImGui::TextUnformatted ("Epoch");
	static const char *epoch[2] = {"current", "MJD"};
	ImGui::SetNextItemWidth (Em () * 5.5f);
	ImGui::Combo ("##epc", &epc, epoch, 2);
	ImGui::SameLine ();
	ImGui::BeginDisabled (epc == 0);
	ImGui::SetNextItemWidth (-FLT_MIN);
	ImGui::InputDouble ("##elmjd", &elmjd, 0.0, 0.0, "%0.5f");
	ImGui::EndDisabled ();

	ImGui::SeparatorText ("Secondary parameters");
	if (valid && ImGui::BeginTable ("secondary", 2, ImGuiTableFlags_SizingFixedFit)) {
		bool closed = (el.e < 1.0);
		auto row = [](const char *lbl, const char *fmt, double v, bool show) {
			ImGui::TableNextRow ();
			ImGui::TableSetColumnIndex (0);
			ImGui::TextUnformatted (lbl);
			ImGui::TableSetColumnIndex (1);
			if (show) ImGui::Text (fmt, v);
			else ImGui::TextUnformatted ("N/A");
		};
		char afmt[16];
		snprintf (afmt, sizeof(afmt), "%%0.3f%s", DEGSTR);
		row ("Orbit period", "%g s", prm.T, closed);
		row ("T. periapsis", "%g s", prm.PeT, true);
		row ("T. apoapsis", "%g s", prm.ApT, closed);
		row ("Periapsis", "%g m", prm.PeD, true);
		row ("Apoapsis", "%g m", prm.ApD, closed);
		row ("Mn anomaly", afmt, prm.MnA*DEG, true);
		row ("Tr anomaly", afmt, prm.TrA*DEG, true);
		row ("Mn longitude", afmt, prm.MnL*DEG, true);
		row ("Tr longitude", afmt, prm.TrL*DEG, true);
		ImGui::EndTable ();
	}
	ImGui::EndTable ();
}

void EditorPage_Elements::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Statevec class definition
// ==============================================================

EditorPage_Statevec::EditorPage_Statevec (ScnEditor *editor): ScnEditorPage (editor)
{
	hRef = 0;
	frm = 0;
	crd = 0;
	memset (val, 0, sizeof(val));
	hCopy = 0;
}

void EditorPage_Statevec::Init ()
{
	frm = 0;
	crd = 0;
	hCopy = 0;
	hRef = Vessel ()->GetGravityRef ();
	Refresh ();
}

void EditorPage_Statevec::Refresh (OBJHANDLE hV)
{
	if (!hV) hV = ed->hVessel;
	if (!hRef) return;
	VECTOR3 pos, vel;
	oapiGetRelativePos (hV, hRef, &pos);
	oapiGetRelativeVel (hV, hRef, &vel);
	if (frm) {
		MATRIX3 rot;
		if (frm == 1) oapiGetPlanetObliquityMatrix (hRef, &rot);
		else          oapiGetRotationMatrix (hRef, &rot);
		pos = tmul (rot, pos);
		vel = tmul (rot, vel);
	}
	if (crd) {
		Crt2Pol (pos, vel);
		pos.data[1] *= DEG; pos.data[2] *= DEG;
		vel.data[1] *= DEG; vel.data[2] *= DEG;
	}
	if (frm == 2) {
		double T = oapiGetPlanetPeriod (hRef);
		if (crd) {
			vel.data[1] -= 360.0/T;
		} else {
			double r   = std::hypot (pos.x, pos.z);
			double phi = atan2 (pos.z, pos.x);
			double v   = 2.0*PI*r/T;
			vel.x     += v*sin(phi);
			vel.z     -= v*cos(phi);
		}
	}
	for (int i = 0; i < 3; i++) {
		val[i] = pos.data[i];
		val[i+3] = vel.data[i];
	}
}

void EditorPage_Statevec::Apply ()
{
	if (!hRef) return;
	VESSEL *vessel = Vessel ();
	MATRIX3 rot;
	VECTOR3 pos = _V(val[0], val[1], val[2]);
	VECTOR3 vel = _V(val[3], val[4], val[5]);
	VECTOR3 refpos, refvel;
	VESSELSTATUS vs;
	if (frm == 2) {
		double T = oapiGetPlanetPeriod (hRef);
		if (crd) {
			vel.data[1] += 360.0/T;
		} else {
			double r   = std::hypot (pos.x, pos.z);
			double phi = atan2 (pos.z, pos.x);
			double v   = 2.0*PI*r/T;
			vel.x     -= v*sin(phi);
			vel.z     += v*cos(phi);
		}
	}
	if (crd) {
		pos.data[1] *= RAD, pos.data[2] *= RAD;
		vel.data[1] *= RAD, vel.data[2] *= RAD;
		Pol2Crt (pos, vel);
	}
	if (frm) {
		if (frm == 1) oapiGetPlanetObliquityMatrix (hRef, &rot);
		else          oapiGetRotationMatrix (hRef, &rot);
		pos = mul (rot, pos);
		vel = mul (rot, vel);
	}
	vessel->GetStatus (vs);
	oapiGetGlobalPos (hRef, &refpos);     pos += refpos;
	oapiGetGlobalVel (hRef, &refvel);     vel += refvel;
	oapiGetGlobalPos (vs.rbody, &refpos); pos -= refpos;
	oapiGetGlobalVel (vs.rbody, &refvel); vel -= refvel;
	if (vs.status != 0) {
		vs.status = 0;
		vessel->GetRotationMatrix (rot);
		vs.arot.x = atan2(rot.m23, rot.m33);
		vs.arot.y = -asin(rot.m13);
		vs.arot.z = atan2(rot.m12, rot.m11);
		vessel->GetAngularVel (vs.vrot);
	}
	veccpy (vs.rpos, pos);
	veccpy (vs.rvel, vel);
	vessel->DefSetState (&vs);
}

void EditorPage_Statevec::Draw ()
{
	static const char *lblCrt[6] = {"x", "y", "z", "dx / dt", "dy / dt", "dz / dt"};
	static const char *lblPol[6] = {"radius", "longitude", "latitude", "d radius / dt", "d longitude / dt", "d latitude / dt"};

	if (!BeginColumns ("statevec")) return;
	ImGui::TableNextRow ();
	ImGui::TableSetColumnIndex (0);
	ImGui::SeparatorText ("State vectors");
	for (int i = 0; i < 6; i++) {
		const char *unit;
		const char *fmt;
		double fine, coarse;
		switch (i) {
		case 0:  unit = "m";   fmt = "%0.1f"; fine = 1.0; coarse = 1000.0; break;
		case 1:
		case 2:  unit = crd ? "deg" : "m"; fmt = crd ? "%0.6f" : "%0.1f"; fine = crd ? 1e-4 : 1.0; coarse = crd ? 0.1 : 1000.0; break;
		case 3:  unit = "m/s"; fmt = "%0.2f"; fine = 0.1; coarse = 100.0; break;
		default: unit = crd ? "deg/s" : "m/s"; fmt = crd ? "%0.7f" : "%0.2f"; fine = crd ? 1e-5 : 0.1; coarse = crd ? 1e-2 : 100.0; break;
		}
		char id[8];
		snprintf (id, sizeof(id), "v%d", i);
		int d = ValueRow (crd ? lblPol[i] : lblCrt[i], id, &val[i], fmt, true, unit);
		if (d) {
			val[i] += (abs (d) == 2 ? coarse : fine) * (d > 0 ? 1.0 : -1.0);
			Apply ();
		}
	}

	ImGui::TableSetColumnIndex (1);
	ImGui::SeparatorText ("Reference");
	ImGui::TextUnformatted ("Orbit reference");
	if (BodyCombo ("##ref", &hRef, false)) Refresh ();
	ImGui::TextUnformatted ("Frame");
	static const char *frame[3] = {"ecliptic", "ref. equator (fixed)", "ref. equator (rotating)"};
	ImGui::SetNextItemWidth (-FLT_MIN);
	if (ImGui::Combo ("##frm", &frm, frame, 3)) Refresh ();
	ImGui::TextUnformatted ("Coordinates");
	static const char *coord[2] = {"cartesian", "polar"};
	ImGui::SetNextItemWidth (-FLT_MIN);
	if (ImGui::Combo ("##crd", &crd, coord, 2)) Refresh ();

	ImGui::SeparatorText ("Copy state from");
	OBJHANDLE self = ed->hVessel;
	int res = VesselListBox ("##copy", &hCopy, [self](OBJHANDLE hV) {
		return hV != self && (oapiGetVesselInterface (hV)->GetFlightStatus () & 1) == 0;
	}, ImVec2 (-FLT_MIN, ImGui::GetContentRegionAvail ().y));
	if (res) {
		Refresh (hCopy);
		if (res == 2) Apply ();
	}
	ImGui::EndTable ();
}

void EditorPage_Statevec::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Landed class definition
// ==============================================================

EditorPage_Landed::EditorPage_Landed (ScnEditor *editor): ScnEditorPage (editor)
{
	hRef = 0;
	hBase = 0;
	pad = 0;
	lng = lat = hdg = 0.0;
	hCopy = 0;
}

void EditorPage_Landed::Init ()
{
	hRef = Vessel ()->GetGravityRef ();
	hBase = 0;
	pad = 0;
	hCopy = 0;
	Refresh ();
}

void EditorPage_Landed::SelectCBody (OBJHANDLE hBody)
{
	if (!hBody) return;
	hRef = hBody;
	hBase = 0;
	pad = 0;
}

void EditorPage_Landed::SelectBase (OBJHANDLE hNewBase)
{
	if (hNewBase == hBase) return;
	hBase = hNewBase;
	pad = (hBase && oapiGetBasePadCount (hBase) ? 1 : 0);
}

void EditorPage_Landed::SetBasePosition ()
{
	if (!hRef || !hBase) return;
	if (pad >= 1 && pad <= (int)oapiGetBasePadCount (hBase))
		oapiGetBasePadEquPos (hBase, pad-1, &lng, &lat);
	else
		oapiGetBaseEquPos (hBase, &lng, &lat);
	lng *= DEG;
	lat *= DEG;
}

void EditorPage_Landed::Refresh (OBJHANDLE hV)
{
	if (hV && hV != ed->hVessel) SelectCBody (oapiGetVesselInterface (hV)->GetSurfaceRef ());
	if (!hV) hV = ed->hVessel;
	if (!hRef) return;
	VESSEL *vessel = oapiGetVesselInterface (hV);
	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs));
	vs.version = 2;
	vessel->GetStatusEx (&vs);
	if (vs.rbody == hRef) {
		lng = vs.surf_lng * DEG;
		lat = vs.surf_lat * DEG;
		SelectBase (vs.base);
	} else {
		VECTOR3 pos;
		MATRIX3 rot;
		oapiGetRelativePos (ed->hVessel, hRef, &pos);
		oapiGetRotationMatrix (hRef, &rot);
		pos = tmul (rot, pos);
		VECTOR3 vel = _V(0,0,0);
		Crt2Pol (pos, vel);
		lng = pos.data[1] * DEG;
		lat = pos.data[2] * DEG;
	}
	hdg = vs.surf_hdg * DEG;
}

void EditorPage_Landed::Apply ()
{
	if (!hRef) return;
	VESSELSTATUS2 vs;
	memset (&vs, 0, sizeof(vs));
	vs.version = 2;
	vs.rbody = hRef;
	vs.status = 1;
	vs.arot.x = 10;
	vs.surf_lng = lng * RAD;
	vs.surf_lat = lat * RAD;
	vs.surf_hdg = hdg * RAD;
	Vessel ()->DefSetStateEx (&vs);
}

void EditorPage_Landed::Draw ()
{
	if (!BeginColumns ("landed")) return;
	ImGui::TableNextRow ();
	ImGui::TableSetColumnIndex (0);
	ImGui::SeparatorText ("Surface position");
	int d;
	if ((d = ValueRow ("Longitude", "lng", &lng, "%0.6f", true, "deg"))) {
		lng += (abs (d) == 2 ? 0.001 : 0.00001) * (d > 0 ? 1.0 : -1.0);
		if      (lng < -180.0) lng += 360.0;
		else if (lng > +180.0) lng -= 360.0;
		Apply ();
	}
	if ((d = ValueRow ("Latitude", "lat", &lat, "%0.6f", true, "deg"))) {
		lat += (abs (d) == 2 ? 0.001 : 0.00001) * (d > 0 ? 1.0 : -1.0);
		lat = min (90.0, max (-90.0, lat));
		Apply ();
	}
	if ((d = ValueRow ("Heading", "hdg", &hdg, "%0.6f", true, "deg"))) {
		hdg += (abs (d) == 2 ? 1.0 : 0.01) * (d > 0 ? 1.0 : -1.0);
		if      (hdg <   0.0) hdg += 360.0;
		else if (hdg > 360.0) hdg -= 360.0;
		Apply ();
	}

	ImGui::TableSetColumnIndex (1);
	ImGui::SeparatorText ("Reference");
	ImGui::TextUnformatted ("Celestial body");
	if (BodyCombo ("##ref", &hRef, true)) {
		hBase = 0;
		pad = 0;
		Apply ();
	}
	ImGui::TextUnformatted ("Surface base");
	ImGui::SetNextItemWidth (-FLT_MIN);
	if (ImGui::BeginCombo ("##base", ObjName (hBase).c_str ())) {
		for (DWORD n = 0; hRef && n < oapiGetBaseCount (hRef); n++) {
			OBJHANDLE h = oapiGetBaseByIndex (hRef, n);
			if (ImGui::Selectable (ObjName (h).c_str (), h == hBase)) {
				hBase = 0;
				SelectBase (h);
				SetBasePosition ();
				Apply ();
			}
		}
		ImGui::EndCombo ();
	}
	ImGui::TextUnformatted ("Landing pad");
	char cbuf[16] = "";
	if (pad) snprintf (cbuf, sizeof(cbuf), "%d", pad);
	ImGui::SetNextItemWidth (Em () * 5.0f);
	if (ImGui::BeginCombo ("##pad", cbuf)) {
		int npad = hBase ? (int)oapiGetBasePadCount (hBase) : 0;
		for (int n = 1; n <= npad; n++) {
			snprintf (cbuf, sizeof(cbuf), "%d", n);
			if (ImGui::Selectable (cbuf, n == pad)) {
				pad = n;
				SetBasePosition ();
				Apply ();
			}
		}
		ImGui::EndCombo ();
	}

	ImGui::SeparatorText ("Copy state from");
	OBJHANDLE self = ed->hVessel;
	int res = VesselListBox ("##copy", &hCopy, [self](OBJHANDLE hV) {
		return hV != self && (oapiGetVesselInterface (hV)->GetFlightStatus () & 1) == 1;
	}, ImVec2 (-FLT_MIN, ImGui::GetContentRegionAvail ().y));
	if (res) {
		Refresh (hCopy);
		if (res == 2) Apply ();
	}
	ImGui::EndTable ();
}

void EditorPage_Landed::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Orientation class definition
// ==============================================================

EditorPage_Orientation::EditorPage_Orientation (ScnEditor *editor): ScnEditorPage (editor)
{
	arot[0] = arot[1] = arot[2] = 0.0;
}

void EditorPage_Orientation::Refresh ()
{
	VECTOR3 a;
	Vessel ()->GetGlobalOrientation (a);
	for (int i = 0; i < 3; i++) arot[i] = a.data[i] * DEG;
}

void EditorPage_Orientation::Apply ()
{
	VECTOR3 a;
	for (int i = 0; i < 3; i++) a.data[i] = arot[i] * RAD;
	Vessel ()->SetGlobalOrientation (a);
}

void EditorPage_Orientation::Rotate (int axis, double da)
{
	MATRIX3 R, R2;
	VESSEL *vessel = Vessel ();
	vessel->GetRotationMatrix (R);
	double sina = sin(da), cosa = cos(da);
	switch (axis) {
	case 0:  R2 = _M(1,0,0,  0,cosa,sina,  0,-sina,cosa); break;
	case 1:  R2 = _M(cosa,0,sina,  0,1,0,  -sina,0,cosa); break;
	default: R2 = _M(cosa,sina,0,  -sina,cosa,0,  0,0,1); break;
	}
	vessel->SetRotationMatrix (mul (R,R2));
	Refresh ();
}

void EditorPage_Orientation::Draw ()
{
	static const char *label[3] = {"alpha", "beta", "gamma"};
	if (!BeginColumns ("orientation")) return;
	ImGui::TableNextRow ();
	ImGui::TableSetColumnIndex (0);
	ImGui::SeparatorText ("Euler angles");
	for (int i = 0; i < 3; i++) {
		int d = ValueRow (label[i], label[i], &arot[i], "%0.6f", true, "deg");
		if (!d) continue;
		double &v = arot[i];
		v += (abs (d) == 2 ? 0.1 : 0.001) * (d > 0 ? 1.0 : -1.0);
		switch (i) {
		case 0:
			if      (v < -180.0) v += 360.0;
			else if (v > +180.0) v -= 360.0;
			break;
		case 1:
			v = min (90.0, max (-90.0, v));
			break;
		case 2:
			if      (v <   0.0) v += 360.0;
			else if (v > 360.0) v -= 360.0;
			break;
		}
		Apply ();
	}

	ImGui::TableSetColumnIndex (1);
	ImGui::SeparatorText ("Rotate");
	static const char *axis[3] = {"Pitch", "Yaw", "Bank"};
	static const char *dirs[3][2] = {{"Up", "Down"}, {"Left", "Right"}, {"Left", "Right"}};
	if (ImGui::BeginTable ("rotate", 3, ImGuiTableFlags_SizingFixedFit)) {
		ImGui::PushItemFlag (ImGuiItemFlags_ButtonRepeat, true);
		for (int i = 0; i < 3; i++) {
			ImGui::PushID (i);
			ImGui::TableNextRow ();
			ImGui::TableSetColumnIndex (0);
			ImGui::AlignTextToFramePadding ();
			ImGui::TextUnformatted (axis[i]);
			ImGui::TableSetColumnIndex (1);
			if (ImGui::Button (dirs[i][0], ImVec2 (Em () * 4.0f, 0))) Rotate (i, 0.005);
			ImGui::TableSetColumnIndex (2);
			if (ImGui::Button (dirs[i][1], ImVec2 (Em () * 4.0f, 0))) Rotate (i, -0.005);
			ImGui::PopID ();
		}
		ImGui::PopItemFlag ();
		ImGui::EndTable ();
	}
	ImGui::EndTable ();
}

void EditorPage_Orientation::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_AngularVel class definition
// ==============================================================

EditorPage_AngularVel::EditorPage_AngularVel (ScnEditor *editor): ScnEditorPage (editor)
{
	avel[0] = avel[1] = avel[2] = 0.0;
}

void EditorPage_AngularVel::Refresh ()
{
	VECTOR3 a;
	Vessel ()->GetAngularVel (a);
	for (int i = 0; i < 3; i++) avel[i] = a.data[i] * DEG;
}

void EditorPage_AngularVel::Apply ()
{
	VECTOR3 a;
	for (int i = 0; i < 3; i++) a.data[i] = avel[i] * RAD;
	Vessel ()->SetAngularVel (a);
}

void EditorPage_AngularVel::Killrot ()
{
	avel[0] = avel[1] = avel[2] = 0.0;
	Vessel ()->SetAngularVel (_V(0,0,0));
}

void EditorPage_AngularVel::Draw ()
{
	static const char *label[3] = {"Pitch (omega x)", "Yaw (omega y)", "Bank (omega z)"};
	static const char *id[3] = {"x", "y", "z"};
	ImGui::SeparatorText ("Angular velocity");
	for (int i = 0; i < 3; i++) {
		int d = ValueRow (label[i], id[i], &avel[i], "%0.6f", true, "deg/s");
		if (d) {
			avel[i] += (abs (d) == 2 ? 0.1 : 0.001) * (d > 0 ? 1.0 : -1.0);
			Apply ();
		}
	}
	ImGui::Spacing ();
	if (ImGui::Button ("Kill rotation")) Killrot ();
}

void EditorPage_AngularVel::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Propellant class definition
// ==============================================================

EditorPage_Propellant::EditorPage_Propellant (ScnEditor *editor): ScnEditorPage (editor)
{
	tank = 1;
	ntank = 0;
	level = mass = maxmass = 0.0;
	gauge = 0;
	massEdited = false;
}

void EditorPage_Propellant::Init ()
{
	massEdited = false;
	Refresh ();
}

void EditorPage_Propellant::Refresh ()
{
	VESSEL *vessel = Vessel ();
	ntank = (int)vessel->GetPropellantCount ();
	if (tank < 1 || tank > ntank) tank = 1;
	PROPELLANT_HANDLE hP = vessel->GetPropellantHandleByIndex (tank-1);
	if (!hP) return;
	maxmass = vessel->GetPropellantMaxMass (hP);
	mass = vessel->GetPropellantMass (hP);
	level = (maxmass > 0.0 ? mass/maxmass : 0.0);
	gauge = (int)(level*100.0+0.5);
}

void EditorPage_Propellant::Apply ()
{
	double lvl;
	if (!massEdited) lvl = level;
	else if (maxmass > 0.0) lvl = mass/maxmass;
	else return;
	SetLevel (max (0.0, min (1.0, lvl)));
}

void EditorPage_Propellant::SetLevel (double lvl, bool setall)
{
	VESSEL *vessel = Vessel ();
	ntank = (int)vessel->GetPropellantCount ();
	if (!ntank || tank < 1 || tank > ntank) return;
	int k0 = (setall ? 0 : tank-1);
	int k1 = (setall ? ntank : tank);
	for (int k = k0; k < k1; k++) {
		PROPELLANT_HANDLE hP = vessel->GetPropellantHandleByIndex (k);
		double m0 = vessel->GetPropellantMaxMass (hP);
		vessel->SetPropellantMass (hP, lvl*m0);
		if (k == tank-1) {
			level = lvl;
			maxmass = m0;
			mass = lvl*m0;
			gauge = (int)(lvl*100.0+0.5);
		}
	}
}

void EditorPage_Propellant::Draw ()
{
	VESSEL *vessel = Vessel ();
	ImGui::SeparatorText ("Propellant levels");
	ImGui::TextUnformatted ("Tank no.");
	ImGui::SetNextItemWidth (Em () * 3.0f);
	if (ImGui::InputInt ("##tank", &tank, 0, 0) && tank >= 1 && tank <= ntank) Refresh ();
	if (int d = StepButtons ("tank")) {
		tank = max (1, min (ntank, tank + d));
		Refresh ();
	}
	ImGui::SameLine ();
	ImGui::Text ("of %d", ntank);

	ImGui::Spacing ();
	if (ImGui::Button ("Empty")) SetLevel (0.0);
	ImGui::SameLine ();
	ImGui::SetNextItemWidth (Em () * 14.0f);
	if (ImGui::SliderInt ("##gauge", &gauge, 0, 100, "%d %%")) SetLevel (gauge*0.01);
	ImGui::SameLine ();
	if (ImGui::Button ("Full")) SetLevel (1.0);

	ImGui::Spacing ();
	if (BeginColumns ("values")) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Fill level (0-1)");
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputDouble ("##level", &level, 0.0, 0.0, "%0.4f")) massEdited = false;
		ImGui::TableSetColumnIndex (1);
		ImGui::Text ("Mass (0-%0.2f kg)", maxmass);
		ImGui::SetNextItemWidth (FieldWidth ());
		if (ImGui::InputDouble ("##mass", &mass, 0.0, 0.0, "%0.2f")) massEdited = true;
		ImGui::EndTable ();
	}

	ImGui::Spacing ();
	if (ImGui::BeginTable ("totals", 2, ImGuiTableFlags_SizingFixedFit)) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Total propellant mass");
		ImGui::TableSetColumnIndex (1);
		ImGui::Text ("%0.2f kg", vessel->GetTotalPropellantMass ());
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Total vessel mass");
		ImGui::TableSetColumnIndex (1);
		ImGui::Text ("%0.2f kg", vessel->GetMass ());
		ImGui::EndTable ();
	}
	ImGui::Spacing ();
	if (ImGui::Button ("Empty all")) SetLevel (0.0, true);
	ImGui::SameLine ();
	if (ImGui::Button ("Fill all")) SetLevel (1.0, true);
}

void EditorPage_Propellant::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Apply")) Apply ();
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Docking class definition
// ==============================================================

EditorPage_Docking::EditorPage_Docking (ScnEditor *editor): ScnEditorPage (editor)
{
	dock = 1;
	hTarget = 0;
	tgtdock = 0;
	mode = 1;
	freq = 0.0;
	freqEditing = false;
}

void EditorPage_Docking::Init ()
{
	dock = 1;
	if (hTarget && !oapiIsVessel (hTarget)) hTarget = 0, tgtdock = 0;
	Refresh ();
}

int EditorPage_Docking::DockNo ()
{
	return (dock >= 1 && dock <= (int)Vessel ()->DockCount () ? dock : 0);
}

void EditorPage_Docking::Refresh ()
{
	errmsg.clear ();
	int ndock = (int)Vessel ()->DockCount ();
	dock = max (1, min (ndock, dock));
}

void EditorPage_Docking::SetTargetDock (int d)
{
	int n = 0;
	if (hTarget && oapiIsVessel (hTarget)) {
		int ndock = (int)oapiGetVesselInterface (hTarget)->DockCount ();
		if (ndock) n = max (1, min (ndock, d));
	}
	tgtdock = n;
}

void EditorPage_Docking::SetIDSChannel (double f, int dch)
{
	int n = DockNo ();
	if (!n) return;
	DOCKHANDLE hDock = Vessel ()->GetDockHandle (n-1);
	int ch = (int)((f-108.0)*20.0+0.5);
	ch = max (0, min (639, ch+dch));
	Vessel ()->SetIDSChannel (hDock, ch);
}

void EditorPage_Docking::Dock ()
{
	if (!hTarget || !oapiIsVessel (hTarget)) return;
	VESSEL *target = oapiGetVesselInterface (hTarget);
	int n = DockNo ();
	if (!n) return;
	if (tgtdock < 1 || tgtdock > (int)target->DockCount ()) {
		errmsg = errmsg_dock[3];
		return;
	}
	int res = Vessel ()->Dock (hTarget, n-1, tgtdock-1, mode);
	Refresh ();
	if (res >= 1 && res <= 3) errmsg = errmsg_dock[res-1];
}

void EditorPage_Docking::Undock ()
{
	int n = DockNo ();
	if (n) Vessel ()->Undock (n-1);
}

void EditorPage_Docking::Draw ()
{
	VESSEL *vessel = Vessel ();
	int ndock = (int)vessel->DockCount ();
	ImGui::SeparatorText ("Docking status");

	if (ImGui::BeginTable ("dock", 2, ImGuiTableFlags_SizingFixedFit)) {
		ImGui::TableNextRow ();
		ImGui::TableSetColumnIndex (0);
		ImGui::TextUnformatted ("Dock no.");
		ImGui::SetNextItemWidth (Em () * 3.0f);
		if (ImGui::InputInt ("##dock", &dock, 0, 0) && DockNo ()) errmsg.clear ();
		if (int d = StepButtons ("dock")) {
			dock += d;
			Refresh ();
		}
		ImGui::SameLine ();
		ImGui::Text ("of %d", ndock);

		int n = DockNo ();
		DOCKHANDLE hDock = (n ? vessel->GetDockHandle (n-1) : 0);
		NAVHANDLE hIDS = (hDock ? vessel->GetIDS (hDock) : 0);
		ImGui::TableSetColumnIndex (1);
		ImGui::BeginDisabled (!hDock);
		bool ids = (hIDS != 0);
		if (ImGui::Checkbox ("IDS frequency", &ids)) {
			vessel->EnableIDS (hDock, ids);
			errmsg.clear ();
		}
		ImGui::EndDisabled ();
		if (hIDS) {
			if (!freqEditing) freq = oapiGetNavFreq (hIDS);
			ImGui::SetNextItemWidth (Em () * 5.0f);
			ImGui::InputDouble ("##freq", &freq, 0.0, 0.0, "%0.2f");
			freqEditing = ImGui::IsItemActive ();
			if (ImGui::IsItemDeactivatedAfterEdit ()) SetIDSChannel (freq, 0);
			if (int d = StepButtons ("freq", true)) SetIDSChannel (oapiGetNavFreq (hIDS), abs (d) == 2 ? 20*(d > 0 ? 1 : -1) : d);
		} else {
			ImGui::BeginDisabled ();
			ReadOnlyText ("##freq", "<none>");
			ImGui::EndDisabled ();
		}
		ImGui::EndTable ();

		ImGui::Spacing ();
		OBJHANDLE hMate = (hDock ? vessel->GetDockStatus (hDock) : 0);
		if (hMate) {
			ImGui::TextUnformatted ("Currently docked to");
			std::string mate = ObjName (hMate);
			char cbuf[256];
			strncpy (cbuf, mate.c_str (), 255);
			cbuf[255] = '\0';
			ImGui::SetNextItemWidth (Em () * 14.0f);
			ImGui::InputText ("##mate", cbuf, 256, ImGuiInputTextFlags_ReadOnly);
			ImGui::SameLine ();
			if (ImGui::Button ("Undock", ImVec2 (Em () * 6.0f, 0))) {
				Undock ();
				Refresh ();
			}
		} else if (hDock) {
			ImGui::TextUnformatted ("Establish docking connection with");
			ImGui::SetNextItemWidth (Em () * 14.0f);
			std::string tgtname = (hTarget && oapiIsVessel (hTarget) ? ObjName (hTarget) : std::string ());
			if (ImGui::BeginCombo ("##target", tgtname.c_str ())) {
				std::vector<std::pair<std::string, OBJHANDLE>> list;
				for (DWORD i = 0; i < oapiGetVesselCount (); i++) {
					OBJHANDLE hV = oapiGetVesselByIndex (i);
					if (hV != ed->hVessel && oapiGetVesselInterface (hV)->DockCount () > 0)
						list.emplace_back (ObjName (hV), hV);
				}
				std::sort (list.begin (), list.end ());
				for (auto &v : list) {
					if (ImGui::Selectable (v.first.c_str (), v.second == hTarget)) {
						hTarget = v.second;
						SetTargetDock (1);
					}
				}
				ImGui::EndCombo ();
			}
			ImGui::SameLine ();
			ImGui::TextUnformatted ("using dock");
			ImGui::SameLine ();
			ImGui::SetNextItemWidth (Em () * 3.0f);
			ImGui::InputInt ("##tgtdock", &tgtdock, 0, 0);
			if (int d = StepButtons ("tgtdock")) SetTargetDock (tgtdock + d);
			ImGui::RadioButton ("Bring target to my current position", &mode, 1);
			ImGui::RadioButton ("Move me to current target position", &mode, 2);
			if (ImGui::Button ("Dock", ImVec2 (Em () * 6.0f, 0))) Dock ();
		}
	}

	if (!errmsg.empty ()) {
		ImGui::Spacing ();
		ImGui::TextWrapped ("Error: %s", errmsg.c_str ());
	}
}

void EditorPage_Docking::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
	if (FooterButton ("Refresh")) Refresh ();
}

// ==============================================================
// EditorPage_Custom class definition
// ==============================================================

EditorPage_Custom::EditorPage_Custom (ScnEditor *editor, const EditorPageSpec *spec): ScnEditorPage (editor)
{
	func = spec->func;
	help = spec->help;
}

void EditorPage_Custom::Draw ()
{
	func (ed->hVessel);
}

void EditorPage_Custom::Footer ()
{
	if (FooterButton ("Done", true)) ed->SwitchPage (PG_EDIT);
}

void EditorPage_Custom::OpenHelp ()
{
	if (help) oapiOpenHelp (help);
	else ScnEditorPage::OpenHelp ();
}
