// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Meshdebug
//                  Part of the ORBITER SDK
//
// Meshdebug.cpp
//
// A simple tool for marking mesh groups during a simulation.
// May be useful for debugging meshes.
// ==============================================================

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include <algorithm>
#include <cmath>
#include <cstring>

ORBITER_MODULE_INFO("Developer resources and samples",
	"MESH DEBUG:\r\n\r\nA small utility for developers. Meshdebug can be used to visually identify a mesh group in a vessel mesh. Set the camera focus on the vessel whose meshes you want to inspect. Then open the mesh debugger dialog from the Custom functions list (Ctrl-F4). The vessel meshes will become semi-transparent, and the selected mesh group will blink.");

// ==============================================================
// Global variables
// ==============================================================

class MeshDebugDialog;

MeshDebugDialog *g_dlg = 0; // dialog
DWORD g_dwCmd;        // custom function identifier
VESSEL *g_vessel;     // focus vessel interface
VISHANDLE *g_visptr;  // pointer to visual of current focus object
MESHHANDLE g_mesh;    // current mesh handle
DWORD g_nmesh, g_ngrp;
int   g_imesh, g_igrp;
DWORD g_flag;
DWORD g_origmat;
int g_tmpmat;
float g_opac = 0.5f;
double g_t;
bool blink;

// ==============================================================
// Local prototypes
// ==============================================================

void GetMeshParams ();
void ChangeMesh (int idx);
void ChangeGroup (int idx);
void OpenDlgClbk (void *context);
void SetMaterialOpacity (MESHHANDLE hMesh, float opac);

// ==============================================================
// The dialog
// ==============================================================

class MeshDebugDialog: public ImGuiDialog {
public:
	MeshDebugDialog (): ImGuiDialog ("Mesh debugger", {260.0f, 130.0f}) {}

	void Open ()
	{
		if (!IsActive()) {
			GetMeshParams ();
			oapiGetObjectName (g_vessel->GetHandle(), vesselName, 256);
		}
		oapiOpenDialog (this);
	}

	void Reset ()
	{
		g_mesh = 0;
		g_imesh = g_igrp = -1;
		active = false;
		oapiCloseDialog (this);
	}

protected:
	void OnDraw () override
	{
		bool hasVis = (*g_visptr != 0);
		ImGui::Text ("Visual: %s", hasVis ? vesselName : "[None]");
		ImGui::BeginDisabled (!hasVis || !g_nmesh);
		int m = g_imesh;
		ImGui::SetNextItemWidth (ImGui::GetFontSize() * 7.0f);
		if (ImGui::InputInt ("Mesh", &m)) {
			m = std::min ((int)g_nmesh-1, std::max (0, m));
			if (m != g_imesh) ChangeMesh (m);
		}
		ImGui::SameLine ();
		ImGui::TextDisabled ("(0 to %d)", (int)g_nmesh-1);
		int g = g_igrp;
		ImGui::SetNextItemWidth (ImGui::GetFontSize() * 7.0f);
		if (ImGui::InputInt ("Group", &g)) {
			g = std::min ((int)g_ngrp-1, std::max (0, g));
			if (g != g_igrp) ChangeGroup (g);
		}
		ImGui::SameLine ();
		ImGui::TextDisabled ("(0 to %d)", (int)g_ngrp-1);
		ImGui::EndDisabled ();
	}

	void OnClose () override
	{
		ChangeMesh (-1);
	}

private:
	char vesselName[256] = "";
};

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (HINSTANCE hModule)
{
	g_dlg = new MeshDebugDialog;
	g_dwCmd = oapiRegisterCustomCmd ("Mesh debugger",
		"Mark individual mesh groups in a vessel mesh",
		OpenDlgClbk, NULL);
}


// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

DLLCLBK void ExitModule (HINSTANCE hModule)
{
	oapiUnregisterCustomCmd (g_dwCmd);
	delete g_dlg;
	g_dlg = 0;
}


// ==============================================================
// Session end: the meshes are gone, so drop the dialog state

DLLCLBK void opcCloseRenderViewport ()
{
	if (g_dlg && g_dlg->IsActive()) g_dlg->Reset ();
}


// ==============================================================
// frame step

DLLCLBK void opcPreStep (double simt, double simdt, double mjd)
{
	if (!g_dlg || !g_dlg->IsActive() || !g_mesh) return;
	double d, t = oapiGetSysTime();
	bool bshow = (modf (t-g_t, &d) > 0.5);
	if (bshow != blink) {
		blink = bshow;
		if (g_igrp >= 0) {
			MESHGROUP *grp = oapiMeshGroup (g_mesh, g_igrp);
			grp->UsrFlag = (blink ? g_flag:3);
		}
	}
}

// ==============================================================
// Open the dialog window

void OpenDlgClbk (void *context)
{
	g_dlg->Open ();
}


// ==============================================================

void GetMeshParams ()
{
	g_vessel = oapiGetFocusInterface();
	g_visptr = oapiObjectVisualPtr (oapiGetFocusObject());
	g_mesh = 0;
	g_nmesh = g_ngrp =  0;
	g_imesh = g_igrp = -1;
	g_tmpmat = -1;
	if (*g_visptr) {
		for (; g_vessel->GetMesh (*g_visptr, g_nmesh); g_nmesh++);
		ChangeMesh (0);
	}
}

void ChangeMesh (int idx)
{
	if (*g_visptr && idx != g_imesh) {
		if (g_mesh) {
			ChangeGroup (-1);
			SetMaterialOpacity (g_mesh, 1.0f/g_opac);
			oapiDeleteMaterial (g_mesh, g_tmpmat);
			oapiSetMeshProperty (g_mesh, MESHPROPERTY_MODULATEMATALPHA, 0);
		}
		g_imesh = idx;
		g_mesh = (idx >= 0 ? g_vessel->GetMesh (*g_visptr, g_imesh) : 0);
		if (g_mesh) {
			g_ngrp = oapiMeshGroupCount (g_mesh);
			SetMaterialOpacity (g_mesh, g_opac);
			oapiSetMeshProperty (g_mesh, MESHPROPERTY_MODULATEMATALPHA, 1);
			MATERIAL mat;
			g_tmpmat = oapiAddMaterial (g_mesh, &mat);
			ChangeGroup (0);
		}
	}
}

void ChangeGroup (int idx)
{
	if (*g_visptr) {
		MESHGROUP *grp;
		g_t = oapiGetSysTime();
		blink = true;
		if (g_igrp >= 0) {
			grp = oapiMeshGroup (g_mesh, g_igrp);
			grp->UsrFlag = g_flag;
			grp->MtrlIdx = g_origmat;
		}
		g_igrp = idx;
		if (g_igrp >= 0) {
			grp = oapiMeshGroup (g_mesh, g_igrp);
			g_flag = grp->UsrFlag; // save original flag
			grp->UsrFlag = 3;      // hide group
			g_origmat = grp->MtrlIdx;
			if (g_origmat >= oapiMeshMaterialCount (g_mesh)) g_origmat = 0;
			MATERIAL *mat1 = oapiMeshMaterial (g_mesh, g_tmpmat);
			MATERIAL *mat2 = oapiMeshMaterial (g_mesh, g_origmat);
			memcpy (mat1, mat2, sizeof(MATERIAL));
			mat1->diffuse.a /= g_opac;
			mat1->ambient.a /= g_opac;
			mat1->specular.a /= g_opac;
			mat1->emissive.a /= g_opac;
			grp->MtrlIdx = g_tmpmat;
		}
	}
}

void SetMaterialOpacity (MESHHANDLE hMesh, float opac)
{
	DWORD i, nmat = oapiMeshMaterialCount (hMesh);
	for (i = 0; i < nmat; i++) {
		MATERIAL *mat = oapiMeshMaterial (hMesh, i);
		mat->diffuse.a  *= opac;
		mat->ambient.a  *= opac;
		mat->specular.a *= opac;
		mat->emissive.a *= opac;
	}
}
