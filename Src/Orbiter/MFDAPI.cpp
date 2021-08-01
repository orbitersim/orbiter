// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION
#include "Orbitersdk.h"
#include "Mfd.h"

extern Vessel *g_focusobj;
extern Pane *g_pane;

// ======================================================================
// class ExternMFD
// ======================================================================

ExternMFD::ExternMFD (const MFDSPEC &spec)
{
	instr = 0;
	pmode = 0;
	btpressed = -1;
	hVessel = (OBJHANDLE)g_focusobj;
	DW = spec.pos.right - spec.pos.left;
	DH = spec.pos.bottom - spec.pos.top;
	nbt1 = spec.nbt_left;
	nbt2 = spec.nbt_right;
	bty0 = spec.bt_yofs;
	btdy = spec.bt_ydist;
}

ExternMFD::~ExternMFD ()
{
	if (instr) delete instr;
}

#ifdef _WIN64
size_t
#else
UINT
#endif
ExternMFD::Id () const
{
	return (
#ifdef _WIN64
		size_t
#else
		UINT
#endif
		)this;
}

bool ExternMFD::Active () const
{
	return (instr != 0);
}

OBJHANDLE ExternMFD::GetVessel () const
{
	return hVessel;
}

void ExternMFD::SetVessel (OBJHANDLE hV)
{
	if (hV != hVessel) {
		hVessel = hV;
		if (instr) SetMode (instr->Type());
	}
}

SURFHANDLE ExternMFD::GetDisplaySurface () const
{
	return (instr ? instr->Surface() : 0);
}

const char *ExternMFD::GetButtonLabel (int bt) const
{
	return (instr ? instr->ButtonLabel (bt) : 0);
}

bool ExternMFD::ProcessButton (int bt, int event)
{
	if (event & PANEL_MOUSE_LBDOWN)
		btpressed = bt;
	else if (event & PANEL_MOUSE_LBUP)
		btpressed = -1;
	return (instr ? instr->ConsumeButton (bt, event) : false);
}

bool ExternMFD::SendKey (DWORD key)
{
	if (key == OAPI_KEY_ESCAPE) { // needs thought
		SetMode (instr ? MFD_NONE : pmode);
		if (!instr) clbkRefreshDisplay (0);
		return true;
	} else if (instr) {
		if (instr->ConsumeKeyBuffered (key)) {
			clbkRefreshDisplay ((SURFHANDLE)(instr->Surface()));
			return true;
		}
	}
	return false;
}

bool ExternMFD::Resize (const MFDSPEC &spec)
{
	bool active = (instr != 0);
	if (active) {
		pmode = instr->Type();
		delete instr;
		instr = 0;
	}
	nbt1 = spec.nbt_left;
	nbt2 = spec.nbt_right;
	bty0 = spec.bt_yofs;
	btdy = spec.bt_ydist;
	DW = spec.pos.right - spec.pos.left;
	DH = spec.pos.bottom - spec.pos.top;

	if (active) SetMode (pmode);
	return true;
}

bool ExternMFD::SetMode (int mode)
{
	Instrument::Spec spec;
	spec.w = DW;
	spec.h = DH;
	spec.nbtl = nbt1;
	spec.nbtr = nbt2;
	spec.bt_y0 = bty0;
	spec.bt_dy = btdy;
	Instrument *newinstr = 0;
	if (mode != MFD_NONE) {
		newinstr = Instrument::Create (mode, g_pane, Id(), spec, (Vessel*)hVessel);
		if (!newinstr) return false;
	}
	if (instr) {
		pmode = instr->Type();
		delete instr;
	}
	instr = newinstr;
	clbkRefreshButtons();
	return true;
}

bool ExternMFD::OpenModeHelp () const
{
	if (!instr) return false;
	HELPCONTEXT *hc = instr->HelpTopic();
	if (hc) return oapiOpenHelp (hc);
	else return false;
}

void ExternMFD::clbkUpdate ()
{
	if (btpressed >= 0)
		ProcessButton (btpressed, PANEL_MOUSE_LBPRESSED);
	if (instr && instr->Update(1.0))
		clbkRefreshDisplay ((SURFHANDLE)(instr->Surface()));
}

void ExternMFD::clbkRefreshDisplay (SURFHANDLE hSurf)
{}

void ExternMFD::clbkRefreshButtons ()
{}

void ExternMFD::clbkFocusChanged (OBJHANDLE hFocus)
{
	// default behaviour: switch MFD to new focus vessel
	SetVessel (hFocus);
}