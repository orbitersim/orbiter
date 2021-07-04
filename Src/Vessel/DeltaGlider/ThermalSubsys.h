// ==============================================================
//                ORBITER MODULE: DeltaGlider
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// ThermalSubsys.h
// Subsystem for coolant loop controls
// ==============================================================

#ifndef __THERMALSUBSYS_H
#define __THERMALSUBSYS_H

#include "DGSubsys.h"
#include "DGSwitches.h"

// ==============================================================
// Thermal control subsystem
// ==============================================================

class RadiatorControl;

class ThermalSubsystem: public DGSubsystem {
	friend class CoolantLoop;

public:
	ThermalSubsystem (DeltaGlider *v);
	void OpenRadiator ();
	void CloseRadiator ();
	const AnimState2 &RadiatorState() const;
	void clbkPreStep (double simt, double simdt, double mjd);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);

private:
	/**
	 * Density of solar irradiance [W/m^2] at the current vessel location.
	 * Drops to 0 in planet shadow.
	 * \param [out] sdir If provided, is set to direction of sun in vessel
     *   frame on exit.
	 */
	double SolarRadiation(VECTOR3 *sdir=0);
	double PlanetRadiation(VECTOR3 *pdir=0);
	void AddIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const;
	void AddAlbedoReflection (double rPower, const VECTOR3 &dir, double *compartmentQ) const;
	void AddFuselageIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const;
	void AddWingIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const;
	void AddRadiatorIrradiance (double rPower, const VECTOR3 &dir, double *compartmentQ) const;
	void SubtractBlackbodyRadiation (double *compartmentQ) const;
	void AtmosphericConvection (double *compartmentQ);
	void HeatConduction (double *compartmentQ) const;

	enum Compartment {
		SURFUPPERFUSELAGE,
		SURFLOWERFUSELAGE,
		SURFUPPERLEFTWING,
		SURFLOWERLEFTWING,
		SURFUPPERRIGHTWING,
		SURFLOWERRIGHTWING,
		INTERIORFUSELAGE,
		AVIONICS,
		CABIN,
		PROPELLANT_LEFTWING,
		PROPELLANT_RIGHTWING,
		PROPELLANT_MAIN,
		RADIATOR
	};

	// thermal parameters of the vessel compartments
	struct CompartmentParam {
		double mass; // compartment mass [kg]
		double cp;   // compartment heat capacity coefficient [J/kg/K]
		double T;    // compartment temperature [K]
	} cprm[13];

	// some DG-specific thermal parameters
	static const double Ax_fuselage;     // fuselage x-cross section
	static const double Ay_fuselage;     // fuselage y-cross section
	static const double Az_fuselage;     // fuselage z-cross section
	static const double Ay_wing;         // effective wing area for thermal exchange [m^2]
	static const double A_radiatorpanel1;// area of a side radiator panel (sum of 3 subpanels)
	static const double A_radiatorpanel2;// area of central radiator panel (sum of 2 subpanels)
	static const double A_maintank;      // surface area interior tank
	static const double A_cabin;         // surface area of cabin
	static const double A_avionics;      // effective instrumentation surface area
	static const double alpha_upper;     // absorptivity upper surface (white paint = 0.4)
	static const double alpha_lower;     // absorptivity lower surface
	static const double alpha_radiator;  // absorptivity radiator panels
	static const double eps_radiator;    // IR emissivity of radiator panels
	static const double k_upper;         // heat conductivity between upper surface and interior [W/m/K]
	static const double k_lower;         // heat conductivity between lower surface and interior [W/m/K]
	static const double k_cabin;         // heat conductivity between fuselage and cabin
	static const double k_convect;       // atmospheric convection coefficient from the surface surface 

	double eps;     // IR emissivity
	double sr_updt;
	double atm_p;   // ambient pressure [Pa]
	double atm_T;   // ambient temperature [K]
	double H0;      // solar irradiance at vessel position (0 if in shadow)
	double H1;      // IR irradiation from orbited body
	VECTOR3 sdir;   // current sun direction in vessel frame
	VECTOR3 pdir;   // current planet direction in vessel frame

	CoolantLoop *coolantloop;
	RadiatorControl *radiatorctrl;
};

// ==============================================================
// Coolant loop
// ==============================================================

class CoolantLoop: public DGSubsystem {
	friend class CoolantLoopDisplay;
	friend class CoolantPumpSwitch;
	friend class CoolantPumpDial;
	friend class CoolantReftempDial;

public:
	CoolantLoop (ThermalSubsystem *_subsys);
	inline bool PumpActive() const { return bPumpActive; }
	void ActivatePump (bool on);
	void SetPumprate (double rate);
	void IncPumprate (bool increase);
	void SetReftemp (double temp);
	void IncReftemp (bool increase);
	void clbkPreStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);

private:
	ThermalSubsystem *ssys_th;

	enum CoolantNode {
		PUMP,
		SPLITTER_HEATSINKBYPASS,    // radiator+wing tank bypass
		EXCHANGER_RADIATOR,         // radiator heat exchanger
		SPLITTER_WINGBYPASS,        // wing tank bypass
		SPLITTER_WINGDISTRIBUTE,    // wing tank distribute
		EXCHANGER_PROPLWING,        // left wing tank heat exchanger
		EXCHANGER_PROPRWING,        // right wing tank heat exchanger
		MERGER_WINGDISTRIBUTE,      // wing tank distribute
		MERGER_WINGBYPASS,          // wing tank bypass
		MERGER_HEATSINKBYPASS,      // radiator+wing tank bypass
		EXCHANGER_CABIN,            // cabin heat exchanger
		EXCHANGER_AVIONICSCOLDPLATE // avionics/instrument block cold plate
	};

	struct NodeParam {
		enum NodeType { PUMP, EXCHANGER, SPLITTER, MERGER } nodetype;
		NodeParam *upstream[2]; // connection to upstream nodes (only MERGER types use both)
		NodeParam *dnstream[2]; // connection to downstream nodes (only SPLITTER types use both)
		double T0, T1;      // entry/exit temperature [K]
		// pump parameters (all other types use Flowrate() function)
		double pumprate;    // coolant pump rate [kg/s]
		// exchanger parameters
		ThermalSubsystem::CompartmentParam *cprm; // connected compartment for heat exchangers
		double k;           // transfer coefficient [W/K]
		// splitter parameters
		double split;       // downstream flow split: outflow[0] = inflow*(1-split); outflow[1] = inflow*split

		double Flowrate(const NodeParam *dn=0); // outgoing flowrate (only splitters use branch!=0)
		void Update (double simdt);             // update node
	} node[12];

	const int nnode;         // number of nodes in the loop
	static const double cp;  // coolant heat capacity [J/kg/K]
	double Tref_tgt;         // target temperature at heat sink exit [K]
	bool bPumpActive;
	double pumprate;
	CoolantLoopDisplay *disp;
	CoolantPumpSwitch *psw;
	CoolantPumpDial *pdial;
	CoolantReftempDial *tdial;
	int ELID_DISPLAY;
	int ELID_PUMPSWITCH;
	int ELID_PUMPDIAL;
	int ELID_REFTEMPDIAL;
	UINT anim_vc_pumpdial;
	UINT anim_vc_reftempdial;
};

// ==============================================================

class CoolantLoopDisplay: public PanelElement {
public:
	CoolantLoopDisplay (CoolantLoop *comp, SURFHANDLE blitsrc);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool Redraw2D (SURFHANDLE surf);
	bool RedrawVC (DEVMESHHANDLE hMesh, SURFHANDLE surf);
	void Refresh() { upt = 0.0; }

private:
	bool Redraw ();
	void BlitReadout (int which, int tgtx, int tgty, const char *str, int maxchar = 5);
	CoolantLoop *component;
	SURFHANDLE bsrc, btgt;
	double upt;
	char label[15][8];
};

// ==============================================================

class CoolantPumpSwitch: public DGSwitch1 {
public:
	CoolantPumpSwitch (CoolantLoop *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	CoolantLoop *component;
};

// ==============================================================

class CoolantPumpDial: public PanelElement {
public:
	CoolantPumpDial (CoolantLoop *comp);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	CoolantLoop *component;
};

// ==============================================================

class CoolantReftempDial: public PanelElement {
public:
	CoolantReftempDial (CoolantLoop *comp);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	CoolantLoop *component;
};

// ==============================================================
// Radiator control
// ==============================================================

class RadiatorControl: public DGSubsystem {
	friend class RadiatorSwitch;

public:
	RadiatorControl (ThermalSubsystem *_subsys);
	void OpenRadiator();
	void CloseRadiator();
	void Revert ();
	inline const AnimState2 &State() const { return radiator_state; }
	inline bool GetRadiator () const { return radiator_extend; }
	void clbkPostCreation();
	void clbkSaveState (FILEHANDLE scn);
	bool clbkParseScenarioLine (const char *line);
	void clbkPostStep (double simt, double simdt, double mjd);
	bool clbkLoadPanel2D (int panelid, PANELHANDLE hPanel, DWORD viewW, DWORD viewH);
	bool clbkLoadVC (int vcid);
	void clbkResetVC (int vcid, DEVMESHHANDLE hMesh);
	bool clbkPlaybackEvent (double simt, double event_t, const char *event_type, const char *event);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);

private:
	bool radiator_extend;
	AnimState2 radiator_state;
	RadiatorSwitch *sw;
	int ELID_SWITCH;
	UINT anim_radiator;         // handle for radiator animation
};

// ==============================================================

class RadiatorSwitch: public DGSwitch1 {
public:
	RadiatorSwitch (RadiatorControl *comp);
	void Reset2D (int panelid, MESHHANDLE hMesh);
	void ResetVC (DEVMESHHANDLE hMesh);
	bool ProcessMouse2D (int event, int mx, int my);
	bool ProcessMouseVC (int event, VECTOR3 &p);

private:
	RadiatorControl *component;
};

#endif // !__COOLINGSUBSYS_H