// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//
// ThermalSubsys.cpp
// Subsystem for coolant loop controls
// ==============================================================

#define STRICT 1

#include "ThermalSubsys.h"
#include "PressureSubsys.h"
#include "ScramSubsys.h"
#include "meshres.h"
#include "meshres_p1.h"
#include "meshres_vc.h"
#include "dg_vc_anim.h"

using std::min;
using std::max;

static const double sigma = 5.670e-8;   // Boltzmann constant

// heat capacity coefficients [J kg^-1 K^-1]
const double c_metal = 0.6e3;
const double c_propellant = 4.181e3;
const double c_ceramic = 0.85e3;
const double c_air = 1.01e3;
const double c_radiator = 0.2e3;

// ==============================================================
// Thermal control subsystem
// ==============================================================

const double ThermalSubsystem::Ax_fuselage = 27.6;
const double ThermalSubsystem::Ay_fuselage = 62.4;
const double ThermalSubsystem::Az_fuselage = 10.6;
const double ThermalSubsystem::Ay_wing = 58.8;
const double ThermalSubsystem::A_radiatorpanel1 = 3.58;
const double ThermalSubsystem::A_radiatorpanel2 = 4.4;
const double ThermalSubsystem::A_maintank = 30.0;
const double ThermalSubsystem::A_cabin = 80.0;
const double ThermalSubsystem::A_avionics = 5.0;
const double ThermalSubsystem::alpha_upper = 0.5;
const double ThermalSubsystem::alpha_lower = 0.6;
const double ThermalSubsystem::alpha_radiator = 0.2;
const double ThermalSubsystem::eps_radiator = 0.95;
const double ThermalSubsystem::k_upper = 0.34;
const double ThermalSubsystem::k_lower = 0.034;
const double ThermalSubsystem::k_cabin = 0.024;
const double ThermalSubsystem::k_convect = 5e-4;

ThermalSubsystem::ThermalSubsystem (DeltaGlider *v)
  : DGSubsystem (v)
{
	// compartment masses
	double m0 = v->GetEmptyMass();
	cprm[SURFUPPERFUSELAGE].mass    = m0*0.2;
	cprm[SURFLOWERFUSELAGE].mass    = m0*0.15;
	cprm[SURFUPPERLEFTWING].mass    = m0*0.075;
	cprm[SURFLOWERLEFTWING].mass    = m0*0.075;
	cprm[SURFUPPERRIGHTWING].mass   = m0*0.075;
	cprm[SURFLOWERRIGHTWING].mass   = m0*0.075;
	cprm[INTERIORFUSELAGE].mass     = m0*0.31;
	cprm[AVIONICS].mass             = m0*0.03;
	cprm[CABIN].mass                = 0.0;
	cprm[PROPELLANT_LEFTWING].mass  = 0.0;
	cprm[PROPELLANT_RIGHTWING].mass = 0.0;
	cprm[PROPELLANT_MAIN].mass      = 0.0;
	cprm[RADIATOR].mass             = m0*0.01;

	// compartment heat capacity coefficients
	cprm[SURFUPPERFUSELAGE].cp      = c_metal;
	cprm[SURFLOWERFUSELAGE].cp      = c_ceramic;
	cprm[SURFUPPERLEFTWING].cp      = c_metal;
	cprm[SURFLOWERLEFTWING].cp      = c_ceramic;
	cprm[SURFUPPERRIGHTWING].cp     = c_metal;
	cprm[SURFLOWERRIGHTWING].cp     = c_ceramic;
	cprm[INTERIORFUSELAGE].cp       = c_metal;
	cprm[AVIONICS].cp               = c_metal;
	cprm[CABIN].cp                  = c_air;
	cprm[PROPELLANT_LEFTWING].cp    = c_propellant;
	cprm[PROPELLANT_RIGHTWING].cp   = c_propellant;
	cprm[PROPELLANT_MAIN].cp        = c_propellant;
	cprm[RADIATOR].cp               = c_radiator;

	// thermal parameter state defaults - overwritten by scenario
	for (Compartment c = SURFUPPERFUSELAGE; c <= RADIATOR; c = (Compartment)(c+1))
		cprm[c].T = 293.0;
	cprm[PROPELLANT_LEFTWING].T = 240.0;
	cprm[PROPELLANT_RIGHTWING].T = 240.0;
	cprm[PROPELLANT_MAIN].T = 240.0;
	cprm[AVIONICS].T = 500.0;

	eps = 0.7;
	sr_updt = -1e10;

	// create component instances
	AddSubsystem (coolantloop = new CoolantLoop (this));
	AddSubsystem (radiatorctrl = new RadiatorControl (this));
}

// --------------------------------------------------------------

void ThermalSubsystem::OpenRadiator ()
{
	radiatorctrl->OpenRadiator();
}

// --------------------------------------------------------------

void ThermalSubsystem::CloseRadiator ()
{
	radiatorctrl->CloseRadiator();
}

// --------------------------------------------------------------

const AnimState2 &ThermalSubsystem::RadiatorState() const
{
	return radiatorctrl->State();
}

// --------------------------------------------------------------

void ThermalSubsystem::clbkPreStep (double simt, double simdt, double mjd)
{
	if (!simdt) return; // sanity check

	// constants and spacecraft parameters
	static const double R = 287.0;          // gas constant [J/kg/K]
	static const double v_cabin = 24.0;     // cabin volume
	static const double v_airlock = 4.0;    // airlock volume

	// dynamic compartment masses
	cprm[CABIN].mass = (DG()->SubsysPressure()->PCabin() * v_cabin + DG()->SubsysPressure()->PAirlock() * v_airlock)/(R*cprm[CABIN].T);
	if (DG()->ScramVersion()) {
		cprm[PROPELLANT_LEFTWING].mass = cprm[PROPELLANT_RIGHTWING].mass = 0.5*DG()->GetPropellantMass(DG()->ph_main);
		cprm[PROPELLANT_MAIN].mass = DG()->GetPropellantMass(DG()->ph_rcs) + DG()->SubsysScram()->GetPropellantMass();
	} else {
		cprm[PROPELLANT_LEFTWING].mass = cprm[PROPELLANT_RIGHTWING].mass = 0.35*DG()->GetPropellantMass(DG()->ph_main);
		cprm[PROPELLANT_MAIN].mass = DG()->GetPropellantMass(DG()->ph_main)*0.3 + DG()->GetPropellantMass(DG()->ph_rcs);
	}

	atm_p = DG()->GetAtmPressure();
	atm_T = DG()->GetAtmTemperature();
	eps = (atm_p ? exp(-0.5e-5*atm_p)*0.7 : 0.7);

	int i;

	DGSubsystem::clbkPreStep (simt, simdt, mjd);

	if (simt > sr_updt + 1.0 || simt < sr_updt) {
		// compute solar irradiance at vessel position (0 if in shadow)
		H0 = SolarRadiation (&sdir);
		if (atm_p) H0 *= exp(-3.63e-6*atm_p);
		// compute IR irradiation from orbited body
		H1 = PlanetRadiation(&pdir);
		sr_updt = simt;
	}

	double dQ[13];
	for (i = 0; i < 13; i++) dQ[i] = 0.0;

	// irradiance
	if (H0) AddIrradiance (H0, sdir, dQ); // add solar irrandiance
	if (H1) AddIrradiance (H1, pdir, dQ); // add planet IR irrandiance
	if (H0 && H1) AddAlbedoReflection (H0, pdir, dQ); // add reflected planet irradiance

	// add internal heat generation
	bool active = (DG()->GetFlightStatus() & 1) == 0;
	dQ[AVIONICS] = (active ? 6e3 : 2e3); // simplistic power management for now

	// heat exchange from crew to cabin atmosphere (convection)
	double p = DG()->SubsysPressure()->PCabin();
	double dQ_crew = 1e-3 * p; // heat exchange is pressure-dependent
	dQ[CABIN] = dQ_crew; // pilot
	for (i = 0; i < 4; i++)
		if (DG()->psngr[i]) dQ[CABIN] += dQ_crew;

	// subtract black-body radiation
	SubtractBlackbodyRadiation (dQ);

	// atmospheric heat convection
	if (atm_p && atm_T)
		AtmosphericConvection (dQ);

	// internal heat conduction
	HeatConduction (dQ);

	// compute temperature change
	for (i = 0; i < 13; i++)
		if (cprm[i].mass)
			cprm[i].T += dQ[i] * simdt / (cprm[i].mass * cprm[i].cp);

	//sprintf(oapiDebugString(), "T(inner)=%lf, T(outer)=%lf", cprm[INTERIORFUSELAGE].T, cprm[SURFUPPERFUSELAGE].T);
}

// --------------------------------------------------------------

void ThermalSubsystem::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[1024];
	sprintf (cbuf, "%0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf %0.2lf",
		cprm[0].T, cprm[1].T, cprm[2].T, cprm[3].T, cprm[4].T, cprm[5].T, cprm[6].T, cprm[7].T, cprm[8].T, cprm[9].T, cprm[10].T, cprm[11].T, cprm[12].T);
	oapiWriteScenario_string (scn, (char*)"COMPARTMENT_TEMP", cbuf);
	DGSubsystem::clbkSaveState (scn);
}

// --------------------------------------------------------------

bool ThermalSubsystem::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp(line, "COMPARTMENT_TEMP", 16)) {
		sscanf(line+16, "%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf%lf",
			&cprm[0].T, &cprm[1].T, &cprm[2].T, &cprm[3].T, &cprm[4].T, &cprm[5].T,
			&cprm[6].T, &cprm[7].T, &cprm[8].T, &cprm[9].T, &cprm[10].T, &cprm[11].T,
			&cprm[12].T);
		return true;
	}
	return DGSubsystem::clbkParseScenarioLine (line);
}

// --------------------------------------------------------------

double ThermalSubsystem::SolarRadiation(VECTOR3 *sdir)
{
	// Check if we are in the shadow of the closest celestial body
	// For simplicity, assume sun position is at origin
	VECTOR3 Ppos, Vpos;
	double vdist, pdist, prad, srad;
	DG()->GetGlobalPos(Vpos);
	vdist = length(Vpos); // distance from sun
	OBJHANDLE hObj = DG()->GetSurfaceRef();
	while (hObj && oapiGetObjectType(hObj) == OBJTP_PLANET) {
		prad = oapiGetSize(hObj);
		oapiGetGlobalPos(hObj, &Ppos);
		pdist = length(Ppos);
		if (vdist > pdist) {
			double d = length(crossp(Ppos, Ppos-Vpos))/vdist;
			if (d < prad) return 0.0;
		}
		hObj = oapiGetGbodyParent(hObj);
	}
	if (sdir) {
		MATRIX3 Vrot;
		DG()->GetRotationMatrix(Vrot);
		*sdir = tmul(Vrot, Vpos / -vdist);
	}
	const OBJHANDLE hSun = oapiGetGbodyByIndex(0);
	srad = oapiGetSize(hSun);
	const double Hsun = 62499432.6; // should be queried from hSun
	return Hsun * (srad*srad)/(vdist*vdist);
}

// --------------------------------------------------------------

double ThermalSubsystem::PlanetRadiation(VECTOR3 *pdir)
{
	OBJHANDLE hObj = DG()->GetSurfaceRef();
	if (!hObj) return 0.0;	
	
	VECTOR3 Ppos, Vpos;
	double prad = oapiGetSize(hObj);
	oapiGetGlobalPos(hObj, &Ppos);
	DG()->GetGlobalPos(Vpos);
	double pdist = length(Ppos-Vpos);
	if (pdir) *pdir = (Ppos-Vpos)/pdist;
	double alt_ratio = prad/pdist;
	const double Hplanet = 237.0; // W/m^2; only true for Earth
	return Hplanet * (alt_ratio*alt_ratio);
}

// --------------------------------------------------------------

void ThermalSubsystem::AddIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const
{
	AddFuselageIrradiance (rPower, dir, compartmentQ);
	AddWingIrradiance (rPower, dir, compartmentQ);
	AddRadiatorIrradiance (rPower, dir, compartmentQ);
}

// --------------------------------------------------------------

void ThermalSubsystem::AddAlbedoReflection (double rPower, const VECTOR3 &dir, double *compartmentQ) const
{
	const double albedo = 0.3;  // for now

	VECTOR3 Ppos, Vpos;
	OBJHANDLE hObj = DG()->GetSurfaceRef();
	oapiGetGlobalPos(hObj, &Ppos);
	DG()->GetGlobalPos(Vpos);
	double cosphi = dotp(unit(Vpos-Ppos), unit(-Ppos));
	double irelalt = oapiGetSize(hObj)/length(Vpos-Ppos);
	rPower *= albedo * irelalt*irelalt * cosphi;
	AddIrradiance (rPower, dir, compartmentQ);
}

// --------------------------------------------------------------

void ThermalSubsystem::AddFuselageIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const
{
	double cs, dq;
	if (dir.y >= 0.0) { // fuselage irradiated from above
		cs = fabs(dir.x)*Ax_fuselage + dir.y*Ay_fuselage + fabs(dir.z)*Az_fuselage; // projected cross section
		dq = cs * alpha_upper * rPower;
		compartmentQ[SURFUPPERFUSELAGE] += dq;
	} else { // fuselage irradiated from below
		cs = fabs(dir.x)*Ax_fuselage*0.4 + fabs(dir.z)*Az_fuselage; // Ax*0.4: assume shadowing by wing
		dq = cs * alpha_upper * rPower;
		compartmentQ[SURFUPPERFUSELAGE] += dq;
		cs = -dir.y*Ay_fuselage;
		dq = cs * alpha_lower * rPower;
		compartmentQ[SURFLOWERFUSELAGE] += dq;
	}
}

// --------------------------------------------------------------

void ThermalSubsystem::AddWingIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const
{
	double cs, dq;
	if (dir.y >= 0.0) {  // wings irradiated from above
		cs = dir.y * Ay_wing; // projected cross section
		dq = cs * alpha_upper * rPower;
		if (dir.y > 0.3 || fabs(dir.x) < 0.3) {
			compartmentQ[SURFUPPERLEFTWING] += dq;
			compartmentQ[SURFUPPERRIGHTWING] += dq;
		} else { // wing shadowed by fuselage
			compartmentQ[dir.x > 0.0 ? SURFUPPERRIGHTWING : SURFUPPERLEFTWING] += dq;
		}
	} else { // wings irradiated from below
		cs = -dir.y * Ay_wing;
		dq = cs * alpha_lower * rPower;
		compartmentQ[SURFLOWERLEFTWING] += dq;
		compartmentQ[SURFLOWERRIGHTWING] += dq;
	}
}

// --------------------------------------------------------------

void ThermalSubsystem::AddRadiatorIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const
{
	double rstate = RadiatorState().State();
	if (!rstate) return;

	double pprog, alpha, cosa, cs, dq;
	VECTOR3 paneldir;

	// panel 1
	pprog = min(1.0, rstate/0.33);
	alpha = (pprog*175.0-100.0)*RAD;
	paneldir = _V(0, sin(alpha), -cos(alpha));
	cosa = dotp(dir, paneldir); // irradiance cosine
	if (cosa > 0.0) {
		if (pprog < 0.5) cosa *= pprog*2.0;
		cs = cosa * A_radiatorpanel2;
		dq = cs * alpha_radiator * rPower;
		compartmentQ[RADIATOR] += dq;
	}

	// panel 2
	pprog = max (0.0, min(1.0, (rstate-0.5)*4.0));
	alpha = pprog*145.0*RAD;
	paneldir = _V(-sin(alpha), -cos(alpha), 0);
	cosa = dotp(dir, paneldir);
	if (cosa > 0.0) {
		if (pprog < 0.5) cosa *= pprog*2.0;
		cs = cosa * A_radiatorpanel1;
		dq = cs * alpha_radiator * rPower;
		compartmentQ[RADIATOR] += dq;
	}

	// panel 3
	pprog = max (0.0, min(1.0, (rstate-0.75)*4.0));
	alpha = pprog*145.0*RAD;
	paneldir = _V(sin(alpha), -cos(alpha), 0);
	cosa = dotp(dir, paneldir);
	if (cosa > 0.0) {
		if (pprog < 0.5) cosa *= pprog*2.0;
		cs = cosa * A_radiatorpanel1;
		dq = cs * alpha_radiator * rPower;
		compartmentQ[RADIATOR] += dq;
	}
}

// --------------------------------------------------------------

void ThermalSubsystem::SubtractBlackbodyRadiation (double *compartmentQ) const
{
	// radiation from vessel surface
	compartmentQ[SURFUPPERFUSELAGE] -= (Ax_fuselage*2.0 + Ay_fuselage + Az_fuselage*2.0)
		* eps * sigma * pow(cprm[SURFUPPERFUSELAGE].T, 4.0);
	compartmentQ[SURFLOWERFUSELAGE]  -= Ay_fuselage * eps * sigma * pow(cprm[SURFLOWERFUSELAGE].T, 4.0);
	compartmentQ[SURFUPPERLEFTWING]  -= Ay_wing * eps * sigma * pow(cprm[SURFUPPERLEFTWING].T, 4.0);
	compartmentQ[SURFLOWERLEFTWING]  -= Ay_wing * eps * sigma * pow(cprm[SURFLOWERLEFTWING].T, 4.0);
	compartmentQ[SURFUPPERRIGHTWING] -= Ay_wing * eps * sigma * pow(cprm[SURFUPPERRIGHTWING].T, 4.0);
	compartmentQ[SURFLOWERRIGHTWING] -= Ay_wing * eps * sigma * pow(cprm[SURFLOWERRIGHTWING].T, 4.0);
	
	double rstate = RadiatorState().State();
	if (rstate) {
		static double A_radiator = A_radiatorpanel2 + 2.0*A_radiatorpanel1 * 1.4; // 1.4: assume fractional emission from lower panel surfaces
		compartmentQ[RADIATOR] -= rstate * A_radiator * eps_radiator * sigma * pow(cprm[RADIATOR].T, 4.0);
	}
}

// --------------------------------------------------------------

void ThermalSubsystem::AtmosphericConvection (double *compartmentQ)
{
	double dq;
	dq = (cprm[SURFLOWERFUSELAGE].T - atm_T) * k_convect * atm_p * Ay_fuselage;
	compartmentQ[SURFLOWERFUSELAGE] -= dq;
	dq = (cprm[SURFUPPERFUSELAGE].T - atm_T) * k_convect * atm_p * (Ay_fuselage + 2.0*Ax_fuselage + 2.0*Az_fuselage);
	compartmentQ[SURFUPPERFUSELAGE] -= dq;
	dq = (cprm[SURFUPPERLEFTWING].T - atm_T) * k_convect * atm_p * Ay_wing;
	compartmentQ[SURFUPPERLEFTWING] -= dq;
	dq = (cprm[SURFLOWERLEFTWING].T - atm_T) * k_convect * atm_p * Ay_wing;
	compartmentQ[SURFLOWERLEFTWING] -= dq;
	dq = (cprm[SURFUPPERRIGHTWING].T - atm_T) * k_convect * atm_p * Ay_wing;
	compartmentQ[SURFUPPERRIGHTWING] -= dq;
	dq = (cprm[SURFLOWERRIGHTWING].T - atm_T) * k_convect * atm_p * Ay_wing;
	compartmentQ[SURFLOWERRIGHTWING] -= dq;
	
	double rstate = RadiatorState().State();
	if (rstate) {
		static double A_radiator = A_radiatorpanel2 + 4.0*A_radiatorpanel1;
		dq = (cprm[RADIATOR].T - atm_T) * k_convect * atm_p * A_radiator;
		compartmentQ[RADIATOR] -= dq;
	}

	const PressureSubsystem *pssys = DG()->SubsysPressure();
	if (pssys->HatchState().IsOpen() || (pssys->OLockState().IsOpen() && pssys->ILockState().IsOpen())) {
		dq = (cprm[CABIN].T - atm_T) * k_convect * atm_p * cprm[CABIN].mass;
		compartmentQ[CABIN] -= dq;
	}
}

// --------------------------------------------------------------

void ThermalSubsystem::HeatConduction (double *compartmentQ) const
{
	double q;

	//   left wing surface <--> left wing tank
	if (cprm[PROPELLANT_LEFTWING].mass) {
		q = (cprm[SURFUPPERLEFTWING].T - cprm[PROPELLANT_LEFTWING].T) * k_upper * Ay_wing;
		compartmentQ[SURFUPPERLEFTWING] -= q;   compartmentQ[PROPELLANT_LEFTWING] += q;
		q = (cprm[SURFLOWERLEFTWING].T - cprm[PROPELLANT_LEFTWING].T) * k_lower * Ay_wing;
		compartmentQ[SURFLOWERLEFTWING] -= q;   compartmentQ[PROPELLANT_LEFTWING] += q;
	}
	//   right wing surface <--> right wing tank
	if (cprm[PROPELLANT_RIGHTWING].mass) {
		q = (cprm[SURFUPPERRIGHTWING].T - cprm[PROPELLANT_RIGHTWING].T) * k_upper * Ay_wing;
		compartmentQ[SURFUPPERRIGHTWING] -= q;   compartmentQ[PROPELLANT_RIGHTWING] += q;
		q = (cprm[SURFLOWERRIGHTWING].T - cprm[PROPELLANT_RIGHTWING].T) * k_lower * Ay_wing;
		compartmentQ[SURFLOWERRIGHTWING] -= q;   compartmentQ[PROPELLANT_RIGHTWING] += q;
	}
	// fuselage surface <--> fuselage interior
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[INTERIORFUSELAGE].T) * k_upper * (Ax_fuselage*2.0 + Ay_fuselage + Az_fuselage*2.0);
	compartmentQ[SURFUPPERFUSELAGE] -= q;   compartmentQ[INTERIORFUSELAGE] += q;
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[INTERIORFUSELAGE].T) * k_lower * Ay_fuselage;
	compartmentQ[SURFLOWERFUSELAGE] -= q;   compartmentQ[INTERIORFUSELAGE] += q;
	// fuselage <--> wings
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[SURFUPPERLEFTWING].T) * k_upper * 3.0;
	compartmentQ[SURFUPPERFUSELAGE] -= q;   compartmentQ[SURFUPPERLEFTWING] += q;
	q = (cprm[SURFUPPERFUSELAGE].T - cprm[SURFUPPERRIGHTWING].T) * k_upper * 3.0;
	compartmentQ[SURFUPPERFUSELAGE] -= q;   compartmentQ[SURFUPPERRIGHTWING] += q;
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[SURFLOWERLEFTWING].T) * k_lower * 3.0;
	compartmentQ[SURFLOWERFUSELAGE] -= q;   compartmentQ[SURFLOWERLEFTWING] += q;
	q = (cprm[SURFLOWERFUSELAGE].T - cprm[SURFLOWERRIGHTWING].T) * k_lower * 3.0;
	compartmentQ[SURFLOWERFUSELAGE] -= q;   compartmentQ[SURFLOWERRIGHTWING] += q;
	// fuselage interior <--> interior tank
	if (cprm[PROPELLANT_MAIN].mass) {
		q = (cprm[INTERIORFUSELAGE].T - cprm[PROPELLANT_MAIN].T) * k_upper * A_maintank;
		compartmentQ[INTERIORFUSELAGE] -= q;   compartmentQ[PROPELLANT_MAIN] += q;
	}
	// fuselage interior <--> cabin
	q = (cprm[INTERIORFUSELAGE].T - cprm[CABIN].T) * k_cabin * A_cabin * cprm[CABIN].mass;
	compartmentQ[INTERIORFUSELAGE] -= q;   compartmentQ[CABIN] += q;
	// fuselage interior <--> avionics
	q = (cprm[INTERIORFUSELAGE].T - cprm[AVIONICS].T) * k_upper * A_avionics;
	compartmentQ[INTERIORFUSELAGE] -= q;   compartmentQ[AVIONICS] += q;
	// cabin <--> avionics
	q = (cprm[CABIN].T - cprm[AVIONICS].T) * k_cabin * A_avionics * cprm[CABIN].mass;
	compartmentQ[CABIN] -= q;   compartmentQ[AVIONICS] += q;
	// radiator <--> fuselage exterior
	double rstate = RadiatorState().State();
	if (!rstate) {
		q = (cprm[RADIATOR].T - cprm[SURFUPPERFUSELAGE].T) * 0.02 * A_radiatorpanel2;
		compartmentQ[RADIATOR] -= q;   compartmentQ[SURFUPPERFUSELAGE] += q;
	}
}


// ==============================================================
// Coolant loop
// ==============================================================

const double CoolantLoop::cp = 0.935e3;

CoolantLoop::CoolantLoop (ThermalSubsystem *_subsys)
  : DGSubsystem(_subsys), ssys_th(_subsys), nnode(12)
{
	extern GDIParams g_Param;

	bPumpActive = false;
	ELID_PUMPSWITCH = AddElement (psw = new CoolantPumpSwitch (this));
	ELID_PUMPDIAL = AddElement (pdial = new CoolantPumpDial (this));
	ELID_REFTEMPDIAL = AddElement (tdial = new CoolantReftempDial (this));
	ELID_DISPLAY = AddElement (disp = new CoolantLoopDisplay (this, g_Param.surf));

	// pump dial animation
	static UINT PumpDialGrp = GRP_COOLING_PUMP_DIAL_VC;
	static MGROUP_ROTATE PumpDialTransform (1, &PumpDialGrp, 1,
		VC_COOLING_PUMP_DIAL_ref, VC_COOLING_PUMP_DIAL_axis, (float)(-280*RAD));
	anim_vc_pumpdial = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_vc_pumpdial, 0, 1, &PumpDialTransform);

	// reference temperature dial animation
	static UINT ReftempDialGrp = GRP_COOLING_REFTEMP_DIAL_VC;
	static MGROUP_ROTATE ReftempDialTransform (1, &ReftempDialGrp, 1,
		VC_COOLING_REFTEMP_DIAL_ref, VC_COOLING_REFTEMP_DIAL_axis, (float)(-280*RAD));
	anim_vc_reftempdial = DG()->CreateAnimation (0.5);
	DG()->AddAnimationComponent (anim_vc_reftempdial, 0, 1, &ReftempDialTransform);

	// configure connections
	node[PUMP].nodetype = NodeParam::PUMP;
	node[PUMP].upstream[0] = &node[EXCHANGER_AVIONICSCOLDPLATE];
	node[PUMP].dnstream[0] = &node[SPLITTER_HEATSINKBYPASS];
	node[PUMP].pumprate = 0.0;

	node[SPLITTER_HEATSINKBYPASS].nodetype = NodeParam::SPLITTER;
	node[SPLITTER_HEATSINKBYPASS].upstream[0] = &node[PUMP];
	node[SPLITTER_HEATSINKBYPASS].dnstream[0] = &node[EXCHANGER_RADIATOR];
	node[SPLITTER_HEATSINKBYPASS].dnstream[1] = &node[MERGER_HEATSINKBYPASS];
	node[SPLITTER_HEATSINKBYPASS].split = 1.0;

	node[EXCHANGER_RADIATOR].nodetype = NodeParam::EXCHANGER;
	node[EXCHANGER_RADIATOR].upstream[0] = &node[SPLITTER_HEATSINKBYPASS];
	node[EXCHANGER_RADIATOR].dnstream[0] = &node[SPLITTER_WINGBYPASS];
	node[EXCHANGER_RADIATOR].cprm = &ssys_th->cprm[ThermalSubsystem::RADIATOR];
	node[EXCHANGER_RADIATOR].k = 300.0;

	node[SPLITTER_WINGBYPASS].nodetype = NodeParam::SPLITTER;
	node[SPLITTER_WINGBYPASS].upstream[0] = &node[EXCHANGER_RADIATOR];
	node[SPLITTER_WINGBYPASS].dnstream[0] = &node[SPLITTER_WINGDISTRIBUTE];
	node[SPLITTER_WINGBYPASS].dnstream[1] = &node[MERGER_WINGBYPASS];
	node[SPLITTER_WINGBYPASS].split = 1.0;

	node[SPLITTER_WINGDISTRIBUTE].nodetype = NodeParam::SPLITTER;
	node[SPLITTER_WINGDISTRIBUTE].upstream[0] = &node[SPLITTER_WINGBYPASS];
	node[SPLITTER_WINGDISTRIBUTE].dnstream[0] = &node[EXCHANGER_PROPLWING];
	node[SPLITTER_WINGDISTRIBUTE].dnstream[1] = &node[EXCHANGER_PROPRWING];
	node[SPLITTER_WINGDISTRIBUTE].split = 0.5;

	node[EXCHANGER_PROPLWING].nodetype = NodeParam::EXCHANGER;
	node[EXCHANGER_PROPLWING].upstream[0] = &node[SPLITTER_WINGDISTRIBUTE];
	node[EXCHANGER_PROPLWING].dnstream[0] = &node[MERGER_WINGDISTRIBUTE];
	node[EXCHANGER_PROPLWING].cprm = &ssys_th->cprm[ThermalSubsystem::PROPELLANT_LEFTWING];
	node[EXCHANGER_PROPLWING].k = 40.0;

	node[EXCHANGER_PROPRWING].nodetype = NodeParam::EXCHANGER;
	node[EXCHANGER_PROPRWING].upstream[0] = &node[SPLITTER_WINGDISTRIBUTE];
	node[EXCHANGER_PROPRWING].dnstream[0] = &node[MERGER_WINGDISTRIBUTE];
	node[EXCHANGER_PROPRWING].cprm = &ssys_th->cprm[ThermalSubsystem::PROPELLANT_LEFTWING];
	node[EXCHANGER_PROPRWING].k = 40.0;

	node[MERGER_WINGDISTRIBUTE].nodetype = NodeParam::MERGER;
	node[MERGER_WINGDISTRIBUTE].upstream[0] = &node[EXCHANGER_PROPLWING];
	node[MERGER_WINGDISTRIBUTE].upstream[1] = &node[EXCHANGER_PROPRWING];
	node[MERGER_WINGDISTRIBUTE].dnstream[0] = &node[MERGER_WINGBYPASS];

	node[MERGER_WINGBYPASS].nodetype = NodeParam::MERGER;
	node[MERGER_WINGBYPASS].upstream[0] = &node[MERGER_WINGDISTRIBUTE];
	node[MERGER_WINGBYPASS].upstream[1] = &node[SPLITTER_WINGBYPASS];
	node[MERGER_WINGBYPASS].dnstream[0] = &node[MERGER_HEATSINKBYPASS];

	node[MERGER_HEATSINKBYPASS].nodetype = NodeParam::MERGER;
	node[MERGER_HEATSINKBYPASS].upstream[0] = &node[MERGER_WINGBYPASS];
	node[MERGER_HEATSINKBYPASS].upstream[1] = &node[SPLITTER_HEATSINKBYPASS];
	node[MERGER_HEATSINKBYPASS].dnstream[0] = &node[EXCHANGER_CABIN];

	node[EXCHANGER_CABIN].nodetype = NodeParam::EXCHANGER;
	node[EXCHANGER_CABIN].upstream[0] = &node[MERGER_HEATSINKBYPASS];
	node[EXCHANGER_CABIN].dnstream[0] = &node[EXCHANGER_AVIONICSCOLDPLATE];
	node[EXCHANGER_CABIN].cprm = &ssys_th->cprm[ThermalSubsystem::CABIN];
	node[EXCHANGER_CABIN].k = 100.0;

	node[EXCHANGER_AVIONICSCOLDPLATE].nodetype = NodeParam::EXCHANGER;
	node[EXCHANGER_AVIONICSCOLDPLATE].upstream[0] = &node[EXCHANGER_CABIN];
	node[EXCHANGER_AVIONICSCOLDPLATE].dnstream[0] = &node[PUMP];
	node[EXCHANGER_AVIONICSCOLDPLATE].cprm = &ssys_th->cprm[ThermalSubsystem::AVIONICS];
	node[EXCHANGER_AVIONICSCOLDPLATE].k = 10.0;

	// default propellant temperature
	for (int i = 0; i < nnode; i++)
		node[i].T0 = node[i].T1 = 293.0;

	Tref_tgt = 287.0;
	pumprate = 0.5;
}

// --------------------------------------------------------------

void CoolantLoop::ActivatePump (bool on)
{
	bPumpActive = on;
	node[PUMP].pumprate = (on ? pumprate : 0.0);
}

// --------------------------------------------------------------

void CoolantLoop::SetPumprate (double rate)
{
	pumprate = rate;
	if (bPumpActive) node[PUMP].pumprate = pumprate;
	DG()->SetAnimation (anim_vc_pumpdial, pumprate*1.0);
	disp->Refresh();
}

// --------------------------------------------------------------

void CoolantLoop::IncPumprate (bool increase)
{
	double rate, dT = oapiGetSimStep()*0.2;
	if (increase) rate = min(1.0, pumprate+dT);
	else          rate = max(0.0, pumprate-dT);
	SetPumprate (rate);
}

// --------------------------------------------------------------

void CoolantLoop::SetReftemp (double temp)
{
	Tref_tgt = temp;
	DG()->SetAnimation (anim_vc_reftempdial, (Tref_tgt-282.0)*0.1);
	disp->Refresh();
}

// --------------------------------------------------------------

void CoolantLoop::IncReftemp (bool increase)
{
	double temp, dT = oapiGetSimStep()*2.0;
	if (increase) temp = min(292.0, Tref_tgt+dT);
	else          temp = max(282.0, Tref_tgt-dT);
	SetReftemp (temp);
}

// --------------------------------------------------------------

void CoolantLoop::clbkPreStep (double simt, double simdt, double mjd)
{
	node[EXCHANGER_CABIN].k = node[EXCHANGER_CABIN].cprm->mass*3.7;

	// set the wing tank bypass rate
	double Ta = node[MERGER_WINGBYPASS].upstream[0]->T1;
	double Tb = node[MERGER_WINGBYPASS].upstream[1]->T1;
	node[SPLITTER_WINGBYPASS].split = ((Ta != Tb) ? min (1.0, max (0.0, (Tref_tgt-Tb)/(Ta-Tb))) : 1.0);

	// set the heatsink bypass rate
	Ta = node[MERGER_HEATSINKBYPASS].upstream[0]->T1;
	Tb = node[MERGER_HEATSINKBYPASS].upstream[1]->T1;
	node[SPLITTER_HEATSINKBYPASS].split = ((Ta != Tb) ? min(1.0, max(0.0, (Tref_tgt-Tb)/(Ta-Tb))) : 1.0);

	for (int i = 0; i < nnode; i++)
		node[i].Update (simdt);
}

// --------------------------------------------------------------

bool CoolantLoop::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	bool res = DGSubsystem::clbkLoadPanel2D (panelid, hPanel, viewW, viewH);

	if (panelid != 1) return res;

	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);

	// Temperature display
	SURFHANDLE surf = oapiGetTextureHandle(DG()->panelmesh1,2);
	DG()->RegisterPanelArea (hPanel, ELID_DISPLAY, _R(0,0,0,0), PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE, surf, disp);

	// Coolant pump switch
	DG()->RegisterPanelArea (hPanel, ELID_PUMPSWITCH, _R(708,9,734,61), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, psw);
	psw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 48);

	return true;
}

// --------------------------------------------------------------

bool CoolantLoop::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// coolant loop display
	oapiVCRegisterArea (ELID_DISPLAY, PANEL_REDRAW_ALWAYS, PANEL_MOUSE_IGNORE);

	// coolant pump switch
	oapiVCRegisterArea (ELID_PUMPSWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_PUMPSWITCH, VC_PUMP_SWITCH_mousearea[0], VC_PUMP_SWITCH_mousearea[1], VC_PUMP_SWITCH_mousearea[2], VC_PUMP_SWITCH_mousearea[3]);
	psw->DefineAnimationVC (VC_PUMP_SWITCH_ref, VC_PUMP_SWITCH_axis, GRP_SWITCH1_VC, VC_PUMP_SWITCH_vofs);

	// coolant pump dial
	oapiVCRegisterArea (ELID_PUMPDIAL, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_PUMPDIAL, VC_COOLING_PUMP_DIAL_mousearea[0], VC_COOLING_PUMP_DIAL_mousearea[1], VC_COOLING_PUMP_DIAL_mousearea[2], VC_COOLING_PUMP_DIAL_mousearea[3]);

	// coolant reference temperature dial
	oapiVCRegisterArea (ELID_REFTEMPDIAL, PANEL_REDRAW_NEVER, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBPRESSED | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_REFTEMPDIAL, VC_COOLING_REFTEMP_DIAL_mousearea[0], VC_COOLING_REFTEMP_DIAL_mousearea[1], VC_COOLING_REFTEMP_DIAL_mousearea[2], VC_COOLING_REFTEMP_DIAL_mousearea[3]);

	return true;
}

// --------------------------------------------------------------

void CoolantLoop::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[1024];
	sprintf (cbuf, "%d %0.3lf %0.3lf", bPumpActive ? 1:0, pumprate, Tref_tgt);
	oapiWriteScenario_string (scn, (char*)"COOLANT_STATE", cbuf);
	DGSubsystem::clbkSaveState (scn);
}

// --------------------------------------------------------------

bool CoolantLoop::clbkParseScenarioLine (const char *line)
{
	if (!_strnicmp(line, "COOLANT_STATE", 13)) {
		int i;
		double rate, temp;
		sscanf(line+13, "%d%lf%lf", &i, &rate, &temp);
		ActivatePump (i != 0);
		SetPumprate (rate);
		SetReftemp (temp);
		return true;
	}
	return DGSubsystem::clbkParseScenarioLine (line);
}

// --------------------------------------------------------------

double CoolantLoop::NodeParam::Flowrate (const NodeParam *dn)
{
	switch (nodetype) {
	case PUMP:
		return pumprate;
	case EXCHANGER:
		return upstream[0]->Flowrate(this);
	case SPLITTER:
		return upstream[0]->Flowrate(this) * (dn == dnstream[0] ? split : 1.0-split);
	case MERGER:
		return upstream[0]->Flowrate(this) + upstream[1]->Flowrate(this);
	default:
		return 0.0;
	}
}

// --------------------------------------------------------------

void CoolantLoop::NodeParam::Update (double simdt)
{
	switch (nodetype) {
	case PUMP:
	case SPLITTER:
		if (upstream[0]->Flowrate(this))
			T0 = T1 = upstream[0]->T1;
		break;
	case MERGER: {
		double flow0 = upstream[0]->Flowrate(this);
		double flow1 = upstream[1]->Flowrate(this);
		double flow = flow0 + flow1;
		if (flow)
			T0 = T1 = (upstream[0]->T1*flow0 + upstream[1]->T1*flow1)/flow;
		}
		break;
	case EXCHANGER:
		if (cprm) {
			double T = cprm->T; // reservoir temperature
			double flowrate = Flowrate(this);
			if (flowrate) {
				T0 = upstream[0]->T1;
				double efficiency = flowrate*cp;   // transfer efficiency
				double dTrel = exp(-k/efficiency); // 0: T1=T(reservoir); 1: T1=T0
				T1 = T + dTrel*(T0-T);             // coolant exit temp
				double q = (T1-T0)*efficiency;     // heat flow rate [W]
				cprm->T -= q * simdt / (cprm->mass * cprm->cp); // update reservoir temp

				// DEBUG
				if (fabs(k-15.0) < 1e-6)
					sprintf(oapiDebugString(), "T0=%lf, T1=%lf, T(av)=%lf, Q=%lf", T0, T1, cprm->T, q);
			} else {
				// todo
			}
		}
		break;
	}
}

// ==============================================================

CoolantLoopDisplay::CoolantLoopDisplay (CoolantLoop *comp, SURFHANDLE blitsrc)
: PanelElement(comp->DG()), component(comp), bsrc(blitsrc)
{
	btgt = 0;
}

// --------------------------------------------------------------

void CoolantLoopDisplay::ResetVC (DEVMESHHANDLE hMesh)
{
	upt = 0.0;
	btgt = oapiGetTextureHandle (component->DG()->vcmesh_tpl, 14);
	for (int i = 0; i < 15; i++)
		for (int j = 0; j < 8; j++)
			label[i][j] = ' ';
}

// --------------------------------------------------------------

bool CoolantLoopDisplay::Redraw2D (SURFHANDLE surf)
{
	btgt = surf;
	return Redraw();
}

// --------------------------------------------------------------

bool CoolantLoopDisplay::RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf)
{
	return Redraw();
}

// --------------------------------------------------------------

bool CoolantLoopDisplay::Redraw ()
{
	if (!btgt) return false;

	double t = oapiGetSimTime();
	if (t < upt && t > upt-1.0) return false;
	upt = t + 0.5;

	char cbuf[16];
	snprintf (cbuf, sizeof(cbuf) - 1, "%5.2lf", component->node[CoolantLoop::PUMP].pumprate);
	BlitReadout (0, 67, 78, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->Tref_tgt);
	BlitReadout (1, 67, 227, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::EXCHANGER_CABIN].T0);
	BlitReadout (2, 67, 200, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::EXCHANGER_CABIN].T1);
	BlitReadout (3, 67, 149, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::EXCHANGER_CABIN].cprm->T);
	BlitReadout (4, 67, 178, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::EXCHANGER_AVIONICSCOLDPLATE].T1);
	BlitReadout (5, 67, 98, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::EXCHANGER_RADIATOR].T1);
	BlitReadout (6, 238, 113, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::MERGER_WINGDISTRIBUTE].T1);
	BlitReadout (7, 238, 189, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", component->node[CoolantLoop::MERGER_WINGBYPASS].T1);
	BlitReadout (8, 238, 211, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%5.1lf", 0.5*(component->node[CoolantLoop::EXCHANGER_PROPRWING].cprm->T+component->node[CoolantLoop::EXCHANGER_PROPLWING].cprm->T));
	BlitReadout (10, 238, 169, cbuf);

	snprintf (cbuf, sizeof(cbuf) - 1, "%4.2lf", component->node[CoolantLoop::SPLITTER_WINGBYPASS].Flowrate(&component->node[CoolantLoop::MERGER_WINGBYPASS]));
	BlitReadout (11, 167, 125, cbuf, 4);

	snprintf (cbuf, sizeof(cbuf) - 1, "%4.2lf", component->node[CoolantLoop::SPLITTER_WINGBYPASS].Flowrate(&component->node[CoolantLoop::SPLITTER_WINGDISTRIBUTE]));
	BlitReadout (12, 202, 125, cbuf, 4);

	snprintf (cbuf, sizeof(cbuf) - 1, "%4.2lf", component->node[CoolantLoop::SPLITTER_HEATSINKBYPASS].Flowrate(&component->node[CoolantLoop::MERGER_HEATSINKBYPASS]));
	BlitReadout (13, 145, 60, cbuf, 4);

	snprintf (cbuf, sizeof(cbuf) - 1, "%4.2lf", component->node[CoolantLoop::SPLITTER_HEATSINKBYPASS].Flowrate(&component->node[CoolantLoop::EXCHANGER_RADIATOR]));
	BlitReadout (14, 180, 60, cbuf, 4);

	return false;
}

// --------------------------------------------------------------

void CoolantLoopDisplay::BlitReadout (int which, int tgtx, int tgty, const char *str, int maxchar)
{
	int srcx, srcy = 0;
	int w = 8;
	int h = 11;
	char c, *tgtstr = label[which];
	for (int i = 0; i < maxchar; i++) {
		w = 8;
		if ((c=str[i]) != tgtstr[i]) {
			if (c >= '0' && c <= '9') srcx = (c-'0')*8;
			else switch (c) {
				case '.': srcx = 10*8+2; w = 4; break;
				case '+': srcx = 11*8; break;
				case '-': srcx = 12*8; break;
				case 'k': srcx = 13*8; break;
				case 'M': srcx = 14*8; break;
				case 'G': srcx = 15*8; break;
				default:  srcx = 16*8; break;
			}
			oapiBlt (btgt, bsrc, tgtx, tgty, srcx, srcy, w, h);
			tgtstr[i] = c;
		}
		tgtx += (c == '.' ? 4 : w);
	}
}

// ==============================================================

CoolantPumpSwitch::CoolantPumpSwitch (CoolantLoop *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void CoolantPumpSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState (component->PumpActive() ? UP : DOWN);
}

// --------------------------------------------------------------

void CoolantPumpSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	DGSwitch1::ResetVC (hMesh);
	SetState (component->PumpActive() ? UP : DOWN);
}

// --------------------------------------------------------------

bool CoolantPumpSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		component->ActivatePump (GetState() == UP);
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool CoolantPumpSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		component->ActivatePump (GetState() == UP);
		return true;
	}
	return false;
}

// ==============================================================

CoolantPumpDial::CoolantPumpDial (CoolantLoop *comp)
: PanelElement (comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

bool CoolantPumpDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	component->IncPumprate (p.x > 0.5);
	return true;
}

// ==============================================================

CoolantReftempDial::CoolantReftempDial (CoolantLoop *comp)
: PanelElement (comp->DG()), component(comp)
{
}

// --------------------------------------------------------------

bool CoolantReftempDial::ProcessMouseVC (int event, VECTOR3 &p)
{
	component->IncReftemp (p.x > 0.5);
	return true;
}

// ==============================================================
// Radiator control
// ==============================================================

RadiatorControl::RadiatorControl (ThermalSubsystem *_subsys)
  : DGSubsystem(_subsys)
{
	radiator_state.SetOperatingSpeed (RADIATOR_OPERATING_SPEED);
	radiator_extend = false;

	ELID_SWITCH = AddElement (sw = new RadiatorSwitch (this));

	// Radiator animation
	static UINT RaddoorGrp[3] = {GRP_Raddoor1,GRP_Raddoor2,GRP_Radiator4};
	static MGROUP_ROTATE Raddoor (0, RaddoorGrp, 3,
		_V(0,1.481,-3.986), _V(1,0,0), (float)(175*RAD));
	static UINT FRadiatorGrp[1] = {GRP_Radiator4};
	static MGROUP_ROTATE FRadiator (0, FRadiatorGrp, 1,
		_V(0,1.91,-2.965), _V(1,0,0), (float)(185*RAD));
	static UINT RadiatorGrp[7] = {GRP_Radiator1,GRP_Radiator1a,GRP_Radiator1b,
		GRP_Radiator2,GRP_Radiator2a,GRP_Radiator2b,GRP_Radiator3};
	static MGROUP_TRANSLATE Radiator (0, RadiatorGrp, 7,
		_V(0,0.584,-0.157));
	static UINT LRadiatorGrp[3] = {GRP_Radiator1,GRP_Radiator1a,GRP_Radiator1b};
	static MGROUP_ROTATE LRadiator (0, LRadiatorGrp, 3,
		_V(-0.88,1.94,-4.211), _V(0,0.260,0.966), (float)(145*RAD));
	static UINT RRadiatorGrp[3] = {GRP_Radiator2,GRP_Radiator2a,GRP_Radiator2b};
	static MGROUP_ROTATE RRadiator (0, RRadiatorGrp, 3,
		_V(0.93,1.91,-4.211), _V(0,0.260,0.966), (float)(-145*RAD));
	static VECTOR3 axis1 = {cos(145*RAD),sin(145*RAD)*cos(0.26292),sin(145*RAD)*sin(-0.26292)};
	static UINT LaRadiatorGrp[1] = {GRP_Radiator1a};
	static MGROUP_ROTATE LaRadiator (0, LaRadiatorGrp, 1,
		_V(-0.91, 1.86, -5.055), axis1, (float)(180*RAD));
	static UINT LbRadiatorGrp[1] = {GRP_Radiator1b};
	static MGROUP_ROTATE LbRadiator (0, LbRadiatorGrp, 1,
		_V(-0.91, 2.075, -4.315), axis1, (float)(-180*RAD));
	static VECTOR3 axis2 = {cos(-145*RAD),sin(-145*RAD)*cos(0.26292),sin(-145*RAD)*sin(-0.26292)};
	static UINT RaRadiatorGrp[1] = {GRP_Radiator2a};
	static MGROUP_ROTATE RaRadiator (0, RaRadiatorGrp, 1,
		_V(0.91, 1.675, -5.01), axis2, (float)(180*RAD));
	static UINT RbRadiatorGrp[1] = {GRP_Radiator2b};
	static MGROUP_ROTATE RbRadiator (0, RbRadiatorGrp, 1,
		_V(0.91, 1.89, -4.27), axis2, (float)(-180*RAD));
	anim_radiator = DG()->CreateAnimation (0);
	DG()->AddAnimationComponent (anim_radiator, 0, 0.25, &Raddoor);
	DG()->AddAnimationComponent (anim_radiator, 0.28, 0.53, &FRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.20, 0.4, &Radiator);
	DG()->AddAnimationComponent (anim_radiator, 0.4, 0.6, &RRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.6, 0.8, &LRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.9, 1.0, &LaRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.8, 0.9, &LbRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.9, 1.0, &RaRadiator);
	DG()->AddAnimationComponent (anim_radiator, 0.8, 0.9, &RbRadiator);
}

// --------------------------------------------------------------

void RadiatorControl::OpenRadiator ()
{
	radiator_extend = true;
	radiator_state.Open();
	if (sw->GetState() != DGSwitch1::UP) {
		sw->SetState(DGSwitch1::UP);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("RADIATOR", "OPEN");
}

// --------------------------------------------------------------

void RadiatorControl::CloseRadiator ()
{
	radiator_extend = false;
	radiator_state.Close();
	if (sw->GetState() != DGSwitch1::DOWN) {
		sw->SetState(DGSwitch1::DOWN);
		DG()->TriggerRedrawArea(1, 0, ELID_SWITCH);
	}
	DG()->UpdateStatusIndicators();
	DG()->RecordEvent ("RADIATOR", "CLOSE");
}

// --------------------------------------------------------------

void RadiatorControl::Revert (void)
{
	if (radiator_state.IsOpen() || radiator_state.IsOpening())
		CloseRadiator();
	else
		OpenRadiator();
}

// --------------------------------------------------------------

void RadiatorControl::clbkPostCreation ()
{
	DG()->SetAnimation (anim_radiator, radiator_state.State());
	radiator_extend = (radiator_state.IsOpen() || radiator_state.IsOpening());
}

// --------------------------------------------------------------

void RadiatorControl::clbkSaveState (FILEHANDLE scn)
{
	radiator_state.SaveState (scn, "RADIATOR");
}

// --------------------------------------------------------------

bool RadiatorControl::clbkParseScenarioLine (const char *line)
{
	return radiator_state.ParseScenarioLine (line, "RADIATOR");
}

// --------------------------------------------------------------

void RadiatorControl::clbkPostStep (double simt, double simdt, double mjd)
{
	// animate radiator
	if (radiator_state.Process (simdt)) {
		DG()->SetAnimation (anim_radiator, radiator_state.State());
		DG()->UpdateStatusIndicators();
	}
}

// --------------------------------------------------------------

bool RadiatorControl::clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH)
{
	bool res = DGSubsystem::clbkLoadPanel2D (panelid, hPanel, viewW, viewH);

	if (panelid != 1) return res;

	// Radiator switch
	SURFHANDLE panel2dtex = oapiGetTextureHandle(DG()->panelmesh1,1);
	DG()->RegisterPanelArea (hPanel, ELID_SWITCH, _R(846,192,872,244), PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN|PANEL_MOUSE_LBUP, panel2dtex, sw);
	sw->DefineAnimation2D (DG()->panelmesh1, GRP_INSTRUMENTS_ABOVE_P1, 44);

	return true;
}


// --------------------------------------------------------------

bool RadiatorControl::clbkLoadVC (int vcid)
{
	if (vcid != 0) return false;

	// Radiator switch
	oapiVCRegisterArea (ELID_SWITCH, PANEL_REDRAW_MOUSE, PANEL_MOUSE_LBDOWN | PANEL_MOUSE_LBUP);
	oapiVCSetAreaClickmode_Quadrilateral (ELID_SWITCH, VC_RADIATOR_SWITCH_mousearea[0], VC_RADIATOR_SWITCH_mousearea[1], VC_RADIATOR_SWITCH_mousearea[2], VC_RADIATOR_SWITCH_mousearea[3]);
	sw->DefineAnimationVC (VC_RADIATOR_SWITCH_ref, VC_RADIATOR_SWITCH_axis, GRP_SWITCH1_VC, VC_RADIATOR_SWITCH_vofs);

	return true;
}

// --------------------------------------------------------------

void RadiatorControl::clbkResetVC (int vcid, DEVMESHHANDLE hMesh)
{
	//if (radiator_extend) OpenRadiator();
	//else                 CloseRadiator();
}

// --------------------------------------------------------------

bool RadiatorControl::clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event)
{
	if (!_stricmp (event_type, "RADIATOR")) {
		if (!_stricmp (event, "CLOSE")) CloseRadiator();
		else                            OpenRadiator();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

int RadiatorControl::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (KEYMOD_ALT(kstate) || KEYMOD_CONTROL(kstate) || KEYMOD_SHIFT(kstate))
		return 0;

	if (key == OAPI_KEY_D) {
		Revert();
		return 1;
	}
	return 0;
}

// ==============================================================

RadiatorSwitch::RadiatorSwitch (RadiatorControl *comp)
: DGSwitch1(comp->DG(), DGSwitch1::TWOSTATE), component(comp)
{
}

// --------------------------------------------------------------

void RadiatorSwitch::Reset2D (int panelid, MESHHANDLE hMesh)
{
	SetState ((DGSwitch1::State)component->GetRadiator() ? DGSwitch1::UP : DGSwitch1::DOWN);
	DGSwitch1::Reset2D (panelid, hMesh);
}

// --------------------------------------------------------------

void RadiatorSwitch::ResetVC (DEVMESHHANDLE hMesh)
{
	SetState (component->GetRadiator() ? UP:DOWN);
	DGSwitch1::ResetVC (hMesh);
}

// --------------------------------------------------------------

bool RadiatorSwitch::ProcessMouse2D (int event, int mx, int my)
{
	if (DGSwitch1::ProcessMouse2D (event, mx, my)) {
		if (GetState() == DGSwitch1::UP) component->OpenRadiator();
		else                             component->CloseRadiator();
		return true;
	}
	return false;
}

// --------------------------------------------------------------

bool RadiatorSwitch::ProcessMouseVC (int event, VECTOR3 &p)
{
	if (DGSwitch1::ProcessMouseVC (event, p)) {
		if (GetState() == DGSwitch1::UP) component->OpenRadiator();
		else                             component->CloseRadiator();
		return true;
	}
	return false;
}
