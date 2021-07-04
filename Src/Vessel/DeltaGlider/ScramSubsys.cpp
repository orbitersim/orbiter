// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// ScramSubsys.cpp
// Implementation for the delta glider ramjet engine
// ==============================================================

#include "ScramSubsys.h"
#include "meshres_p0.h"
#include "meshres_vc.h"

// --------------------------------------------------------------
// constructor

Scramjet::Scramjet (VESSEL *v)
: vessel(v)
{
	nthdef = 0;    // no thrusters associated yet
}

// --------------------------------------------------------------
// destructor

Scramjet::~Scramjet ()
{
	if (nthdef) {  // delete list of thruster definitions
		for (UINT i = 0; i < nthdef; i++)
			delete thdef[i];
		delete []thdef;
	}
}

// --------------------------------------------------------------
// add new thruster definition to list

void Scramjet::AddThrusterDefinition (THRUSTER_HANDLE th,
	double Qr, double Ai, double Tb_max, double dmf_max)
{
	THDEF *thd   = new THDEF;
	thd->th      = th;
	thd->Qr      = Qr;
	thd->Ai      = Ai;
	thd->Tb_max  = Tb_max;
	thd->dmf_max = dmf_max;
	thd->dmf     = 0.0;
	thd->F       = 0.0;
	for (int i = 0; i < 3; i++) thd->T[i] = 0.0;

	THDEF **tmp = new THDEF*[nthdef+1];
	if (nthdef) {
		memcpy (tmp, thdef, nthdef*sizeof (THDEF*));
		delete []thdef;
	}
	thdef = tmp;
	thdef[nthdef++] = thd;
}

// --------------------------------------------------------------
// calculate current thrust force for all engines

void Scramjet::Thrust (double *F) const
{
	const OBJHANDLE hBody = vessel->GetAtmRef();
	const ATMCONST *atm = (hBody ? oapiGetPlanetAtmConstants (hBody) : 0);

	if (atm) { // atmospheric parameters available
		
		double M, Fs, T0, Td, Tb, Tb0, Te, p0, pd, D, rho, cp, v0, ve, tr, lvl, dma, dmf, precov, dmafac;
		const double eps = 1e-4;
		const double dma_scale = 2.7e-4;

		M   = vessel->GetMachNumber();                     // Mach number
		T0  = vessel->GetAtmTemperature();                 // freestream temperature
		p0  = vessel->GetAtmPressure();                    // freestream pressure
		rho = vessel->GetAtmDensity();                     // freestream density
		cp  = atm->gamma * atm->R / (atm->gamma-1.0);      // specific heat (pressure)
		v0  = M * sqrt (atm->gamma * atm->R * T0);         // freestream velocity
		tr  = (1.0 + 0.5*(atm->gamma-1.0) * M*M);          // temperature ratio
		Td  = T0 * tr;                                     // diffuser temperature
		pd  = p0 * pow (Td/T0, atm->gamma/(atm->gamma-1.0)); // diffuser pressure
		precov = max (0.0, 1.0-0.075*pow (max(M,1.0)-1.0, 1.35)); // pressure recovery
		dmafac = dma_scale*precov*pd;

		for (UINT i = 0; i < nthdef; i++) {
			Tb0 = thdef[i]->Tb_max;                        // max burner temperature
			if (Tb0 > Td) {                                // we are within operational range
				lvl  = vessel->GetThrusterLevel (thdef[i]->th); // throttle level
				D    = (Tb0-Td) / (thdef[i]->Qr/cp - Tb0); // max fuel-to-air ratio (what if negative?)
				D   *= lvl;                                // actual fuel-to-air ratio

				dma = dmafac * thdef[i]->Ai;               // air mass flow rate [kg/s]
				//dma  = rho * v0 * thdef[i]->Ai;            // air mass flow rate
				dmf  = D * dma;                            // fuel mass flow rate
				if (dmf > thdef[i]->dmf_max) {             // max fuel rate exceeded
					dmf = thdef[i]->dmf_max;
					D = dmf/dma;
				}
				Tb   = (D*thdef[i]->Qr/cp + Td) / (1.0+D); // actual burner temperature
				Te   = Tb * pow (p0/pd, (atm->gamma-1.0)/atm->gamma); // exhaust temperature
				ve   = sqrt (2.0*cp*(Tb-Te));              // exhaust velocity
			    Fs  = (1.0+D)*ve - v0;                     // specific thrust
				thdef[i]->F = F[i] = max (0.0, Fs*dma);    // thrust force
				thdef[i]->dmf = dmf;
				thdef[i]->T[1] = Tb;
				thdef[i]->T[2] = Te;

			} else {                                       // overheating!

				thdef[i]->F = F[i] = 0.0;
				thdef[i]->dmf = 0.0;
				thdef[i]->T[1] = thdef[i]->T[2] = Td;

			}
			thdef[i]->T[0] = Td;
		}

	} else {   // no atmospheric parameters

		for (UINT i = 0; i < nthdef; i++) {
			thdef[i]->F = F[i] = 0.0;
			thdef[i]->dmf = 0.0;
		}

	}
}

// --------------------------------------------------------------

double Scramjet::TSFC (UINT idx) const
{
	const double eps = 1e-5;
	return thdef[idx]->dmf/(thdef[idx]->F+eps);
}

// ==============================================================
// Scramjet subsystem
// ==============================================================

ScramSubsystem::ScramSubsystem (DeltaGlider *dg)
: DGSubsystem (dg)
{
	modelidx = dg->FlightModel();
	scram = new Scramjet (dg);
	hProp = dg->CreatePropellantResource (fuel_maxmass = TANK2_CAPACITY);
	VECTOR3 dir = {0.0, sin(SCRAM_DEFAULT_DIR), cos(SCRAM_DEFAULT_DIR)};
	PSTREAM_HANDLE ph;
	PARTICLESTREAMSPEC exhaust_scram = {
		0, 2.0, 10, 150, 0.1, 0.2, 16, 1.0, PARTICLESTREAMSPEC::EMISSIVE,
		PARTICLESTREAMSPEC::LVL_SQRT, 0, 1,
		PARTICLESTREAMSPEC::ATM_PLOG, 1e-5, 0.1
	};
	for (int i = 0; i < 2; i++) {
		hScram[i] = dg->CreateThruster (_V(i?0.9:-0.9, -0.8, -5.6), dir, 0, hProp, 0);
		scram->AddThrusterDefinition (hScram[i], SCRAM_FHV[modelidx],
			SCRAM_INTAKE_AREA, SCRAM_TEMAX[modelidx], SCRAM_MAX_DMF[modelidx]);
		ph = DG()->AddExhaustStream (hScram[0], _V(i?1:-1,-1.1,-5.4), &exhaust_scram);
		if (ph) oapiParticleSetLevelRef (ph, scram_intensity+i);
		scram_max[i] = scram_intensity[i] = 0.0;
	}

	AddSubsystem (throttle = new ScramThrottle (this));
}

// --------------------------------------------------------------

ScramSubsystem::~ScramSubsystem ()
{
	delete scram;
}

// --------------------------------------------------------------

void ScramSubsystem::SetPropellantMaxMass (double mass)
{
	DG()->SetPropellantMaxMass (hProp, fuel_maxmass = mass);
}

// --------------------------------------------------------------

void ScramSubsystem::IncThrusterLevel (int which, double dlvl)
{
	for (int i = 0; i < 2; i++) {
		if (which != 1-i) {
			DG()->IncThrusterLevel (hScram[i], dlvl);
			scram_intensity[i] = DG()->GetThrusterLevel (hScram[i]) * scram_max[i];
		}
	}
}

// --------------------------------------------------------------

void ScramSubsystem::SetThrusterLevel (int which, double lvl)
{
	for (int i = 0; i < 2; i++) {
		if (which != 1-i) {
			DG()->SetThrusterLevel (hScram[i], lvl);
			scram_intensity[i] = lvl * scram_max[i];
		}
	}
}

// --------------------------------------------------------------

void ScramSubsystem::clbkPostStep (double simt, double simdt, double mjd)
{
	DGSubsystem::clbkPostStep (simt, simdt, mjd);

	// compute scramjet parameters
	const double eps = 1e-8;
	const double Fnominal = 2.5*MAX_MAIN_THRUST[modelidx];

	double Fscram[2];
	scram->Thrust (Fscram);

	for (int i = 0; i < 2; i++) {
		double level = DG()->GetThrusterLevel (hScram[i]);
		double Fmax  = Fscram[i]/(level+eps);
		DG()->SetThrusterMax0 (hScram[i], Fmax);
		DG()->SetThrusterIsp (hScram[i], max (1.0, Fscram[i]/(scram->DMF(i)+eps))); // don't allow ISP=0

		// the following are used for calculating exhaust density
		scram_max[i] = min (Fmax/Fnominal, 1.0);
		scram_intensity[i] = level * scram_max[i];
	}
}

// --------------------------------------------------------------

int ScramSubsystem::clbkConsumeDirectKey (char *kstate)
{
	if (KEYMOD_ALT (kstate)) {
		if (KEYDOWN (kstate, OAPI_KEY_ADD)) { // increment scram thrust
			IncThrusterLevel (2, 0.3 * oapiGetSimStep());
			RESETKEY (kstate, OAPI_KEY_ADD);
		}
		if (KEYDOWN (kstate, OAPI_KEY_SUBTRACT)) { // decrement scram thrust
			IncThrusterLevel (2, -0.3 * oapiGetSimStep());
			RESETKEY (kstate, OAPI_KEY_SUBTRACT);
		}
	}
	return 0;
}

// ==============================================================
// Throttle control
// ==============================================================

ScramThrottle::ScramThrottle (DGSubsystem *_subsys)
: DGSubsystem(_subsys)
{
	ELID_LEVER = AddElement (lever = new ScramThrottleLever (this));

	// VC animation: Left scram engine throttle
	static UINT ScramThrottleLGrp[2] = {GRP_THROTTLE_SCRAM_L1_VC,GRP_THROTTLE_SCRAM_L2_VC};
	static MGROUP_ROTATE ScramThrottleL (1, ScramThrottleLGrp, 2,
		_V(0,0.7849,6.96), _V(1,0,0), (float)(30*RAD));
	anim_lever[0] = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_lever[0], 0, 1, &ScramThrottleL);

	// VC animation: Right scram engine throttle
	static UINT ScramThrottleRGrp[2] = {GRP_THROTTLE_SCRAM_R1_VC,GRP_THROTTLE_SCRAM_R2_VC};
	static MGROUP_ROTATE ScramThrottleR (1, ScramThrottleRGrp, 2,
		_V(0,0.7849,6.96), _V(1,0,0), (float)(30*RAD));
	anim_lever[1] =  DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_lever[1], 0, 1, &ScramThrottleR);
}

// --------------------------------------------------------------

bool ScramThrottle::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	if (panelid != 0) return false;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh0,1);
	DG()->RegisterPanelArea (hPanel, ELID_LEVER, _R(4,456,57,558), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED, panel2dtex, lever);

	return true;
}

// --------------------------------------------------------------

bool ScramThrottle::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Throttle lever animations
	oapiVCRegisterArea (ELID_LEVER, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBPRESSED);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_LEVER, _V(-0.45,0.98,6.94), _V(-0.39,0.98,6.94), _V(-0.45,0.95,7.07), _V(-0.39,0.95,7.07));

	return true;
}

// ==============================================================

ScramThrottleLever::ScramThrottleLever (ScramThrottle *comp)
: PanelElement(comp->DG()), component(comp)
{
	for (int i = 0; i < 2; i++) ppos[i] = 0.0f;
}

// --------------------------------------------------------------

void ScramThrottleLever::Reset2D (int panelid, MESHHANDLE hMesh)
{
	grp = oapiMeshGroup (hMesh, GRP_SCRAM_INSTRUMENTS_P0);
	vtxofs = 0;
}

// --------------------------------------------------------------

void ScramThrottleLever::ResetVC (DEVMESHHANDLE hMesh)
{
	for (int i = 0; i < 2; i++)
		sliderpos[i] = (UINT)-1;
}

// --------------------------------------------------------------

bool ScramThrottleLever::Redraw2D (SURFHANDLE surf)
{
	static const float tx_dy = 18.0f;
	static const float bb_y0 = 541.5f;

	int i, j, vofs;
	float pos;
	static const float sy[4] = {bb_y0,bb_y0,bb_y0+tx_dy,bb_y0+tx_dy};

	for (i = 0; i < 2; i++) {
		double level = ((ScramSubsystem*)component->Parent())->GetThrusterLevel(i);
		pos = (float)(-level*84.0);
		if (pos != ppos[i]) {
			vofs = vtxofs+i*4;
			for (j = 0; j < 4; j++) grp->Vtx[vofs+j].y = sy[j]+pos;
			ppos[i] = pos;
		}
	}
	return false;
}

// --------------------------------------------------------------

bool ScramThrottleLever::ProcessMouse2D (int event, int mx, int my)
{
	static int ctrl = 0;
	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		if      (mx <  12) ctrl = 0; // left engine
		else if (mx >= 37) ctrl = 1; // right engine
		else               ctrl = 2; // both
	}
	((ScramSubsystem*)component->Parent())->SetThrusterLevel (ctrl, max (0.0, min (1.0, 1.0-my/84.0)));
	return true;
}

// --------------------------------------------------------------

bool ScramThrottleLever::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE hSurf)
{
	UINT i, pos;

	for (i = 0; i < 2; i++) {
		double level = ((ScramSubsystem*)component->Parent())->GetThrusterLevel (i);
		UINT pos = (UINT)(level*500.0);
		if (pos != sliderpos[i]) {
			component->DG()->SetAnimation (component->anim_lever[i], level);
			sliderpos[i] = pos;
		}
	}
	return true;
}

// --------------------------------------------------------------

bool ScramThrottleLever::ProcessMouseVC (int event, VECTOR3 &p)
{
	static int ctrl = 0;
	static double py = 0.0;

	if (event & PANEL_MOUSE_LBDOWN) { // record which slider to operate
		if      (p.x < 0.3) ctrl = 0; // left engine
		else if (p.x > 0.7) ctrl = 1; // right engine
		else                ctrl = 2; // both
		py = p.y;
	} else {
		for (int i = 0; i < 2; i++) {
			if (ctrl == i || ctrl == 2) {
				double lvl = ((ScramSubsystem*)component->Parent())->GetThrusterLevel (i);
				lvl = max (0.0, min (1.0, lvl + (p.y-py)));
				if (lvl < 0.01) lvl = 0.0;
				((ScramSubsystem*)component->Parent())->SetThrusterLevel (i, lvl);
			}
		}
		py = p.y;
	}
	return true;
}