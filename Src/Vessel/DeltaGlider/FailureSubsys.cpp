// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// FailureSubsys.cpp
// Subsystem for failure and warning displays
// ==============================================================

#include "FailureSubsys.h"
#include "meshres_p0.h"

// ==============================================================
// Failure subsystem
// ==============================================================

FailureSubsystem::FailureSubsystem (DeltaGlider *v)
: DGSubsystem (v)
{
	bMWSActive = false;
	ELID_MWS = AddElement (mws = new MwsButton (this));
}

// --------------------------------------------------------------

void FailureSubsystem::clbkPostStep (double simt, double simdt, double mjd)
{
	if (bMWSActive) {
		double di;
		bool mwson = (modf (simt, &di) < 0.5);
		if (mwson != bMWSOn) {
			bMWSOn = mwson;
		}
	}
}

// --------------------------------------------------------------

bool FailureSubsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);

	// MWS button
	DG()->RegisterPanelArea (hPanel, ELID_MWS, _R(1071,6,1098,32), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN, panel2dtex, mws);

	return true;
}

// ==============================================================

MwsButton::MwsButton (FailureSubsystem *_subsys)
: PanelElement(_subsys->DG()), subsys(_subsys)
{
}

// --------------------------------------------------------------

void MwsButton::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_INSTRUMENTS_ABOVE_P0);
	vtxofs = 184;
	active = islit = false;
}

// --------------------------------------------------------------

bool MwsButton::ProcessMouse2D (int event, int mx, int my)
{
	subsys->MWSReset();
	return false;
}

// --------------------------------------------------------------

bool MwsButton::ProcessMouseVC (int event, VECTOR3 &p)
{
	subsys->MWSReset();
	return false;
}

// --------------------------------------------------------------

bool MwsButton::Redraw2D (SURFHANDLE surf)
{
	bool light;
	if (subsys->MWSActive()) {
		double di, simt = oapiGetSimTime();
		light = (modf (simt, &di) < 0.5);
	} else light = false;

	if (light != islit) {
		static const float texh = (float)PANEL2D_TEXH;
		static const float th = 30.0f;
		static const float ty0 = (texh-643.0f)/texh;
		static const float ty1 = ty0 + th/texh;
		int i;
		float tv = (light ? ty1 : ty0);
		for (i = 2; i < 4; i++)
			grp->Vtx[vtxofs+i].tv = tv;
		islit = light;
	}
	return false;
}

// --------------------------------------------------------------

bool MwsButton::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
#ifdef UNDEF // TODO
	bool light;
	if (dg->MWSActive()) {
		double di, simt = oapiGetSimTime();
		light = (modf (simt, &di) < 0.5);
	} else light = false;

	if (light != islit) {
		NTVERTEX vtx[4];
		static WORD vidx[4] = {0,1,2,3};
		GROUPEDITSPEC ges;
		ges.flags = GRPEDIT_VTXTEXU;
		ges.nVtx = 4;
		ges.vIdx = vidx;
		ges.Vtx = vtx;
		float xofs = 0.2246f + (light ? 0.12891f : 0.0f);
		vtx[0].tu = vtx[1].tu = xofs;
		vtx[2].tu = vtx[3].tu = xofs + 0.125f;
		oapiEditMeshGroup (hMesh, GRP_MWS_VC, &ges);
		islit = light;
	}
#endif
	return false;
}
