// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//             ORBITER MODULE: Common vessel tools
//                  Part of the ORBITER SDK
//
// Instrument.cpp
// Implementation for class PanelElement:
//   Base class for panel and VC instrument visualisations
// ==============================================================

#include "Instrument.h"
#include "Orbitersdk.h"

PanelElement::PanelElement (VESSEL3 *v)
{
	vessel = v;
	grp = 0;
	vtxofs = 0;
	mesh = 0;
	gidx = 0;
}

// --------------------------------------------------------------

PanelElement::~PanelElement ()
{
}

// --------------------------------------------------------------

void PanelElement::Reset2D (int panelid)
{
}

// --------------------------------------------------------------

void PanelElement::Reset2D (int panelid, MESHHANDLE hMesh)
{
}

// --------------------------------------------------------------

void PanelElement::ResetVC (DEVMESHHANDLE hMesh)
{
}

// --------------------------------------------------------------

void PanelElement::LoadVC (int vcid)
{
}

// --------------------------------------------------------------

void PanelElement::LoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
}

// --------------------------------------------------------------

bool PanelElement::Redraw2D (SURFHANDLE surf)
{
	return false;
}

// --------------------------------------------------------------

bool PanelElement::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	return false;
}

// --------------------------------------------------------------

bool PanelElement::ProcessMouse2D (int event, int mx, int my)
{
	return false;
}

// --------------------------------------------------------------

bool PanelElement::ProcessMouseVC (int event, VECTOR3 &p)
{
	return false;
}

// --------------------------------------------------------------

void PanelElement::AddGeometry (MESHHANDLE hMesh, DWORD grpidx, const NTVERTEX *vtx, DWORD nvtx, const WORD *idx, DWORD nidx)
{
	mesh = hMesh;
	gidx = grpidx;
	grp  = oapiMeshGroup (hMesh, grpidx);
	vtxofs = grp->nVtx;
	oapiAddMeshGroupBlock (hMesh, grpidx, vtx, nvtx, idx, nidx);
}

// --------------------------------------------------------------

void PanelElement::SelectGeometry (MESHHANDLE hMesh, DWORD grpidx, int vofs)
{
	mesh = hMesh;
	gidx = grpidx;
	grp  = oapiMeshGroup (hMesh, grpidx);
	vtxofs = vofs;
}

// --------------------------------------------------------------

char *PanelElement::DispStr (double dist, int precision)
{
	static char strbuf[32];
	double absd = fabs (dist);
	if (absd < 1e4) {
		if      (absd < 1e3)  sprintf (strbuf, "% 6.*f ", precision-3, dist);
		else                  sprintf (strbuf, "% 0.*fk", precision-1, dist*1e-3);
	} else if (absd < 1e7) {
		if      (absd < 1e5)  sprintf (strbuf, "% 0.*fk", precision-2, dist*1e-3);
		else if (absd < 1e6)  sprintf (strbuf, "% 0.*fk", precision-3, dist*1e-3);
		else                  sprintf (strbuf, "% 0.*fM", precision-1, dist*1e-6);
	} else if (absd < 1e10) {
		if      (absd < 1e8)  sprintf (strbuf, "% 0.*fM", precision-2, dist*1e-6);
		else if (absd < 1e9)  sprintf (strbuf, "% 0.*fM", precision-3, dist*1e-6);
		else                  sprintf (strbuf, "% 0.*fG", precision-1, dist*1e-9);
	} else {
		if      (absd < 1e11) sprintf (strbuf, "% 0.*fG", precision-2, dist*1e-9);
		else if (absd < 1e12) sprintf (strbuf, "% 0.*fG", precision-3, dist*1e-9);
		else                  strcpy (strbuf, "--.--");
	}
	return strbuf;
}

// ==============================================================

AnimState2::AnimState2 ()
{
	state = 0.0;
	speed = 0.0;
	inc_speed =  1.0;
	dec_speed = -1.0;
}

// --------------------------------------------------------------

AnimState2::AnimState2 (double operating_speed, double initial_state)
{
	state = initial_state;
	speed = 0.0;
	inc_speed =  operating_speed;
	dec_speed = -operating_speed;
}

// --------------------------------------------------------------

void AnimState2::SetOperatingSpeed (double opspeed)
{
	inc_speed =  opspeed;
	dec_speed = -opspeed;
}

// --------------------------------------------------------------

void AnimState2::SetState (double _state, double _speed)
{
	state = _state;
	speed = _speed;
	if (state >= 1.0) {
		state = 1.0;
		if (speed > 0.0)
			speed = 0.0;
	} else if (state <= 0.0) {
		state = 0.0;
		if (speed < 0.0)
			speed = 0.0;
	}
}

// --------------------------------------------------------------

void AnimState2::Open ()
{
	if (state < 1.0)
		speed = inc_speed;
}

// --------------------------------------------------------------

void AnimState2::Close ()
{
	if (state > 0.0)
		speed = dec_speed;
}

// --------------------------------------------------------------

void AnimState2::Stop ()
{
	speed = 0.0;
}

// --------------------------------------------------------------

void AnimState2::SetOpened ()
{
	state = 1.0;
	speed = 0.0;
}

// --------------------------------------------------------------

void AnimState2::SetClosed ()
{
	state = 0.0;
	speed = 0.0;
}

// --------------------------------------------------------------

bool AnimState2::Process (double dt)
{
	if (speed) {
		state += speed * dt;
		if (state <= 0.0) {
			state = 0.0;
			speed = 0.0;
		} else if (state >= 1.0) {
			state = 1.0;
			speed = 0.0;
		}
		return true;
	} else {
		return false;
	}
}

// --------------------------------------------------------------

void AnimState2::SaveState (FILEHANDLE scn, const char *label)
{
	if (state) {
		char cbuf[256];
		snprintf (cbuf, sizeof(cbuf) - 1, "%0.4lf %0.4lf", state, speed);
		oapiWriteScenario_string (scn, (char*)label, cbuf);
	}
}

// --------------------------------------------------------------

bool AnimState2::ParseScenarioLine (const char *line, const char *label)
{
	if (!_strnicmp (line, label, strlen(label))) {
		sscanf (line+strlen(label), "%lf%lf", &state, &speed);
		return true;
	}
	return false;
}
// ==============================================================

Subsystem::Subsystem (ComponentVessel *v)
: vessel(v)
{
	parent = 0;              // top-level subsystem
	id = v->next_ssys_id++;  // assign a top-level subsystem id
}

// --------------------------------------------------------------

Subsystem::Subsystem (Subsystem *p)
: parent(p)
{
	vessel = p->vessel;
	id = p->id;    // inherit the parent id
}

// --------------------------------------------------------------

Subsystem::~Subsystem ()
{
	// delete child systems
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		delete *it;

	// delete panel elements
	for (std::vector<PanelElement*>::iterator it = element.begin(); it != element.end(); ++it)
		delete *it;
}

// --------------------------------------------------------------

void Subsystem::AddSubsystem (Subsystem *subsys)
{
	child.push_back (subsys);
}

// --------------------------------------------------------------

int Subsystem::AddElement (PanelElement *el)
{
	// panel elements are always managed by the top-level subsystem
	if (parent) {
		return parent->AddElement (el);
	} else {
		element.push_back (el);
		return element.size()-1 + (id+1)*1000; // create unique id
	}
}

// --------------------------------------------------------------

void Subsystem::clbkPostCreation ()
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkPostCreation();
}

// --------------------------------------------------------------

void Subsystem::clbkSaveState (FILEHANDLE scn)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkSaveState (scn);
}

// --------------------------------------------------------------

bool Subsystem::clbkParseScenarioLine (const char *line)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		if ((*it)->clbkParseScenarioLine (line))
			return true;
	return false;
}

// --------------------------------------------------------------

void Subsystem::clbkPreStep (double simt, double simdt, double mjd)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkPreStep (simt, simdt, mjd);
}

// --------------------------------------------------------------

void Subsystem::clbkPostStep (double simt, double simdt, double mjd)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkPostStep (simt, simdt, mjd);
}

// --------------------------------------------------------------

bool Subsystem::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	for (std::vector<PanelElement*>::iterator it = element.begin(); it != element.end(); ++it)
		(*it)->LoadPanel2D (panelid, hPanel, viewW, viewH);

	bool b = false;
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it) {
		bool bi = (*it)->clbkLoadPanel2D (panelid, hPanel, viewW, viewH);
		b = b || bi;
	}
	return b;
}

// --------------------------------------------------------------

bool Subsystem::clbkLoadVC (int vcid)
{
	for (std::vector<PanelElement*>::iterator it = element.begin(); it != element.end(); ++it)
		(*it)->LoadVC (vcid);

	bool b = false;
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it) {
		bool bi = (*it)->clbkLoadVC (vcid);
		b = b || bi;
	}
	return b;
}

// --------------------------------------------------------------

void Subsystem::clbkReset2D (int panelid, MESHHANDLE hMesh)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkReset2D (panelid, hMesh);

	for (std::vector<PanelElement*>::iterator it = element.begin(); it != element.end(); ++it)
		(*it)->Reset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void Subsystem::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkResetVC (vcid, hMesh);

	for (std::vector<PanelElement*>::iterator it = element.begin(); it != element.end(); ++it)
		(*it)->ResetVC (hMesh);
}

// --------------------------------------------------------------

bool Subsystem::clbkVCRedrawEvent (int elid, int event, DEVMESHHANDLE hMesh, SURFHANDLE hSurf)
{
	// note: this callback is not distributed to children, because the top-level
	// subsystem manages all panel elements

	elid -= (id+1)*1000; // convert to index
	return (elid >= 0 && elid < element.size() ? element[elid]->RedrawVC (hMesh, hSurf) : false);
}

// --------------------------------------------------------------

bool Subsystem::clbkVCMouseEvent (int elid, int event, VECTOR3 &p)
{
	// note: this callback is not distributed to children, because the top-level
	// subsystem manages all panel elements

	elid -= (id+1)*1000; // convert to index
	return (elid >= 0 && elid < element.size() ? element[elid]->ProcessMouseVC (event, p) : false);
}

// --------------------------------------------------------------

bool Subsystem::clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp)
{
	bool b = false;
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it) {
		bool bi = (*it)->clbkDrawHUD (mode, hps, skp);
		b = b || bi;
	}
	return b;
}

// --------------------------------------------------------------

void Subsystem::clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		(*it)->clbkRenderHUD (mode, hps, hTex);
}

// --------------------------------------------------------------

bool Subsystem::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		if ((*it)->clbkPlaybackEvent (simt, event_t, event_type, event))
			return true;
	return false;
}

// --------------------------------------------------------------

int Subsystem::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	int res;
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		if (res = (*it)->clbkConsumeBufferedKey (key, down, kstate))
			return res;
	return 0;
}

// --------------------------------------------------------------

int Subsystem::clbkConsumeDirectKey (char *kstate)
{
	int res;
	for (std::vector<Subsystem*>::iterator it = child.begin(); it != child.end(); ++it)
		if (res = (*it)->clbkConsumeDirectKey (kstate))
			return res;
	return 0;
}

// ==============================================================

ComponentVessel::ComponentVessel (OBJHANDLE hVessel, int fmodel)
: VESSEL4 (hVessel, fmodel)
{
	next_ssys_id = 0;
}

// --------------------------------------------------------------

ComponentVessel::~ComponentVessel ()
{
	// delete subsystems
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		delete *it;

}

// --------------------------------------------------------------

void ComponentVessel::AddSubsystem (Subsystem *subsys)
{
	ssys.push_back (subsys);
}

// --------------------------------------------------------------

void ComponentVessel::clbkSaveState (FILEHANDLE scn)
{
	// Write default vessel parameters
	VESSEL4::clbkSaveState (scn);

	// Let subsystems write their parameters
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkSaveState (scn);
}

// --------------------------------------------------------------

bool ComponentVessel::clbkParseScenarioLine (const char *line)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		if ((*it)->clbkParseScenarioLine (line))
			return true;
	return false;
}

// --------------------------------------------------------------

void ComponentVessel::clbkPostCreation ()
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkPostCreation ();
}

// --------------------------------------------------------------

bool ComponentVessel::clbkDrawHUD (int mode, const HUDPAINTSPEC *hps, oapi::Sketchpad *skp)
{
	VESSEL4::clbkDrawHUD (mode, hps, skp); // allow default HUD elements

	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkDrawHUD (mode, hps, skp);

	return true;
}

// --------------------------------------------------------------

void ComponentVessel::clbkRenderHUD (int mode, const HUDPAINTSPEC *hps, SURFHANDLE hTex)
{
	VESSEL4::clbkRenderHUD (mode, hps, hTex); // allow default HUD elements

	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkRenderHUD (mode, hps, hTex);
}

// --------------------------------------------------------------

bool ComponentVessel::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		if ((*it)->clbkPlaybackEvent (simt, event_t, event_type, event))
			return true;
	return false;
}

// --------------------------------------------------------------

void ComponentVessel::clbkPreStep (double simt, double simdt, double mjd)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkPreStep (simt, simdt, mjd);
}

// --------------------------------------------------------------

void ComponentVessel::clbkPostStep (double simt, double simdt, double mjd)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkPostStep (simt, simdt, mjd);
}

// --------------------------------------------------------------

void ComponentVessel::clbkReset2D (int panelid, MESHHANDLE hMesh)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkReset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void ComponentVessel::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it)
		(*it)->clbkResetVC (vcid, hMesh);
}

// --------------------------------------------------------------

bool ComponentVessel::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	bool b = false;
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it) {
		bool bi = (*it)->clbkLoadPanel2D (panelid, hPanel, viewW, viewH);
		b = b || bi;
	}
	return b;
}

// --------------------------------------------------------------

bool ComponentVessel::clbkLoadVC (int vcid)
{
	bool b = false;
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it) {
		bool bi = (*it)->clbkLoadVC (vcid);
		b = b || bi;
	}
	return b;
}

// --------------------------------------------------------------

bool ComponentVessel::clbkVCMouseEvent (int elid, int event, VECTOR3 &p)
{
	int subsys = elid/1000-1;
	if (subsys >= 0 && subsys < ssys.size())
		return ssys[subsys]->clbkVCMouseEvent (elid, event, p);
	return false;
}

// --------------------------------------------------------------

bool ComponentVessel::clbkVCRedrawEvent (int elid, int event, DEVMESHHANDLE hMesh, SURFHANDLE hSurf)
{
	int subsys = elid/1000-1;
	if (subsys >= 0 && subsys < ssys.size())
		return ssys[subsys]->clbkVCRedrawEvent (elid, event, hMesh, hSurf);
	return false;
}

// --------------------------------------------------------------

int ComponentVessel::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	int res;
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it) {
		if (res = (*it)->clbkConsumeBufferedKey (key, down, kstate))
			return res;
	}
	return 0;
}

// --------------------------------------------------------------

int ComponentVessel::clbkConsumeDirectKey (char *kstate)
{
	int res;
	for (std::vector<Subsystem*>::iterator it = ssys.begin(); it != ssys.end(); ++it) {
		if (res = (*it)->clbkConsumeDirectKey (kstate))
			return res;
	}
	return 0;
}
