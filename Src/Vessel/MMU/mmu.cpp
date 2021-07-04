// ==============================================================
// ORBITER vessel module: NASA Manned Manuveuring Unit (MMU)
// ==============================================================

#include "orbitersdk.h"

const double slThrust = 367455;
const double vacThrust = 414340;

// ==============================================================
// MMU class interface

class MMU: public VESSEL2 {
public:
	MMU (OBJHANDLE hVessel, int fmodel): VESSEL2 (hVessel, fmodel) {}
	void clbkSetClassCaps (FILEHANDLE cfg);
	//int clbkConsumeDirectKey (char *kstate);
	int clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
};


PROPELLANT_HANDLE main_tank;
THRUSTER_HANDLE thruster[24];


double GetCurrentValue(VESSEL *vessel, double sl, double vac)
{
	double c_atm=0;
	double c_value=0;
	c_atm = vessel->GetAtmPressure() / 101.4e3; 
	c_value = c_atm * (vac-sl);				
	c_value += sl;								
	return c_value;
}

void AddAttitudeJets(VESSEL *vessel)
{
	VECTOR3 m_exhaust_pos;
	VECTOR3 m_exhaust_ref;
	THRUSTER_HANDLE a_thruster[4];
	const double JET_THRUST = 1.50;
	const double JET_ISP = 45.0*G;
	main_tank = vessel->CreatePropellantResource(11.8);

	
	m_exhaust_pos= _V(.37,0.64,-.22);
	m_exhaust_ref = _V(0,0,1);
	thruster[0] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[0],0.2,0.01);
	
	m_exhaust_pos= _V(-.37,0.64,-.22);
	m_exhaust_ref = _V(0,0,1);
	thruster[6] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[6],0.2,0.01);
	
	m_exhaust_pos= _V(.37,-0.64,-.22);
	m_exhaust_ref = _V(0,0,1);
	thruster[12] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[12],0.2,0.01);
	
	m_exhaust_pos= _V(-.37,-0.64,-.22);
	m_exhaust_ref = _V(0,0,1);
	thruster[18] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[18],0.2,0.01);

	m_exhaust_pos= _V(.37,.64,0.22);
	m_exhaust_ref = _V(0,0,-1);
	thruster[1] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[1],0.2,0.01);
	
	m_exhaust_pos= _V(-.37,0.64, 0.22);
	m_exhaust_ref = _V(0,0,-1);
	thruster[7] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[7],0.2,0.01);
	
	m_exhaust_pos= _V(.37,-0.64,0.22);
	m_exhaust_ref = _V(0,0,-1);
	thruster[13] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[13],0.2,0.01);
	
	m_exhaust_pos= _V(-.37,-0.64,0.22);
	m_exhaust_ref = _V(0,0,-1);
	thruster[19] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[19],0.2,0.01);

	
	m_exhaust_pos= _V(.40,.64,0.17);
	m_exhaust_ref = _V(-1,0,0);
	thruster[2] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[2],0.2,0.01);

	m_exhaust_pos= _V(.40,.64,-0.17);
	m_exhaust_ref = _V(-1,0,0);
	thruster[3] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[3],0.2,0.01);

	m_exhaust_pos= _V(.37,.68,0.17);
	m_exhaust_ref = _V(0,-1,0);
	thruster[4] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[4],0.2,0.01);

	m_exhaust_pos= _V(.37,.68,-0.17);
	m_exhaust_ref = _V(0,-1,0);
	thruster[5] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[5],0.2,0.01);

	m_exhaust_pos= _V(-.40,0.64, 0.17);
	m_exhaust_ref = _V(1,0,0);
	thruster[8] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[8],0.2,0.01);

	m_exhaust_pos= _V(-.40,0.64, -0.17);
	m_exhaust_ref = _V(1,0,0);
	thruster[9] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[9],0.2,0.01);

	m_exhaust_pos= _V(-.37,0.68, 0.17);
	m_exhaust_ref = _V(0,-1,0);
	thruster[10] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[10],0.2,0.01);

	m_exhaust_pos= _V(-.37,0.68, -0.17);
	m_exhaust_ref = _V(0,-1,0);
	thruster[11] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[11],0.2,0.01);
	
	m_exhaust_pos= _V(.40,-0.64,0.17);
	m_exhaust_ref = _V(-1,0,0);
	thruster[14] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[14],0.2,0.01);

	m_exhaust_pos= _V(.40,-0.64,-0.17);
	m_exhaust_ref = _V(-1,0,0);
	thruster[15] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[15],0.2,0.01);
	
	m_exhaust_pos= _V(.37,-0.68,0.17);
	m_exhaust_ref = _V(0,1,0);
	thruster[16] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[16],0.2,0.01);

	m_exhaust_pos= _V(.37,-0.68,-0.17);
	m_exhaust_ref = _V(0,1,0);
	thruster[17] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[17],0.2,0.01);

	m_exhaust_pos= _V(-.40,-0.68,0.17);
	m_exhaust_ref = _V(1,0,0);
	thruster[20] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[20],0.2,0.01);

	m_exhaust_pos= _V(-.40,-0.68,-0.17);
	m_exhaust_ref = _V(1,0,0);
	thruster[21] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[21],0.2,0.01);

	m_exhaust_pos= _V(-.37,-0.68,0.17);
	m_exhaust_ref = _V(0,1,0);
	thruster[22] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[22],0.2,0.01);

	m_exhaust_pos= _V(-.37,-0.68,-0.17);
	m_exhaust_ref = _V(0,1,0);
	thruster[23] = vessel->CreateThruster(m_exhaust_pos, m_exhaust_ref, JET_THRUST, main_tank, JET_ISP);
	vessel->AddExhaust(thruster[23],0.2,0.01);

	a_thruster[0] = thruster[0];
	a_thruster[1] = thruster[6];
	a_thruster[2] = thruster[12];
	a_thruster[3] = thruster[18];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_FORWARD);

	a_thruster[0] = thruster[1];
	a_thruster[1] = thruster[7];
	a_thruster[2] = thruster[13];
	a_thruster[3] = thruster[19];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_BACK);

	a_thruster[0] = thruster[0];
	a_thruster[1] = thruster[6];
	a_thruster[2] = thruster[13];
	a_thruster[3] = thruster[19];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_PITCHDOWN);

	a_thruster[0] = thruster[1];
	a_thruster[1] = thruster[7];
	a_thruster[2] = thruster[12];
	a_thruster[3] = thruster[18];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_PITCHUP);



	a_thruster[0] = thruster[2];
	a_thruster[1] = thruster[3];
	a_thruster[2] = thruster[14];
	a_thruster[3] = thruster[15];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_LEFT);

	a_thruster[0] = thruster[8];
	a_thruster[1] = thruster[9];
	a_thruster[2] = thruster[20];
	a_thruster[3] = thruster[21];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_RIGHT);

	a_thruster[0] = thruster[2];
	a_thruster[1] = thruster[9];
	a_thruster[2] = thruster[14];
	a_thruster[3] = thruster[21];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_YAWLEFT);

	a_thruster[0] = thruster[8];
	a_thruster[1] = thruster[3];
	a_thruster[2] = thruster[20];
	a_thruster[3] = thruster[15];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_YAWRIGHT);


	
	a_thruster[0] = thruster[4];
	a_thruster[1] = thruster[5];
	a_thruster[2] = thruster[10];
	a_thruster[3] = thruster[11];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_DOWN);

	a_thruster[0] = thruster[16];
	a_thruster[1] = thruster[17];
	a_thruster[2] = thruster[22];
	a_thruster[3] = thruster[23];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_UP);

	a_thruster[0] = thruster[4];
	a_thruster[1] = thruster[5];
	a_thruster[2] = thruster[22];
	a_thruster[3] = thruster[23];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_BANKRIGHT);

	a_thruster[0] = thruster[16];
	a_thruster[1] = thruster[17];
	a_thruster[2] = thruster[10];
	a_thruster[3] = thruster[11];
	vessel->CreateThrusterGroup(a_thruster,4,THGROUP_ATT_BANKLEFT);



}

void SetMMU (VESSEL *vessel)
{
	VECTOR3 mesh_pos;
// ==============================================================
// These numbers are based off of the MS Space Simulator MMU
	vessel->SetSize (1.25);
	vessel->SetCOG_elev (0.75);
	vessel->SetEmptyMass (200.0);
//	vessel->SetMaxFuelMass (200.0);
//	vessel->SetFuelMass(200.0);
//	vessel->SetISP (350.0*9.8);
//	vessel->SetMaxThrust (ENGINE_MAIN,  31.5);
//	vessel->SetMaxThrust (ENGINE_RETRO, 31.5);
//	vessel->SetMaxThrust (ENGINE_HOVER, 0);
//	vessel->SetMaxThrust (ENGINE_ATTITUDE, 31.5);
// ==============================================================
	vessel->SetPMI (_V(.35,.13,0.35));
	vessel->SetCrossSections (_V(1.64,.77,1.64));
// ==============================================================
	vessel->SetCW (0.3, 0.3, .3, .3);
	vessel->SetRotDrag (_V(0.7,0.7,1.2));
	vessel->SetPitchMomentScale (0);
	vessel->SetYawMomentScale (0);
	vessel->SetLiftCoeffFunc (0); 
	vessel->SetDockParams (_V(0,0,.44), _V(0,0,1), _V(0,1,0));

// ==============================================================
	vessel->ClearMeshes();
	vessel->ClearExhaustRefs();
	vessel->ClearAttExhaustRefs();
	vessel->ClearPropellantResources();
	vessel->ClearThrusterDefinitions();
	mesh_pos = _V(0,-0.24,0.16);
	vessel->AddMesh("mmu", &mesh_pos);
	AddAttitudeJets(vessel);
	vessel->SetDockParams(_V(0,0,0.5),_V(0,0,1),_V(0,1,0));
	//vessel->SetDockParams(_V(0,0,0),_V(0,-1,0),_V(0,0,1));
	//vessel->CreateDock(_V(0,0,0.22),_V(0,0,1),_V(0,1,0));
}



// ==============================================================
// MMU class implementation

// Set the capabilities of the vessel class
void MMU::clbkSetClassCaps (FILEHANDLE cfg)
{
	SetMMU (this);
}

#ifdef UNDEF
// Keyboard interface handler
int MMU::clbkConsumeDirectKey (char *kstate)
{
	OBJHANDLE hvessel;
	if (KEYMOD_SHIFT (kstate)) 
	{
		return 0; // shift combinations are reserved
	}
	else if (KEYMOD_CONTROL (kstate)) 
	{
		// insert ctrl key combinations here
	}
	else 
	{ // unmodified keys
		if (KEYDOWN (kstate, OAPI_KEY_E)) { // "End EVA"
			if (oapiAcceptDelayedKey (OAPI_KEY_E, 1.0))
			{
				char name[256];
				char vname[256];
				UINT i;
				strcpy (vname, GetName());
				for (i=0;i<strlen(vname)+1;i++)
					name[i]=0;

				for (i=0;i<strlen(vname)-4;i++)
					name[i]=vname[i];
				//strncpy (name, vname,strlen(vname)+1-4);
				hvessel=oapiGetVesselByName(name);
				if (hvessel != 0)
					oapiSetFocusObject(hvessel);
			};
			return 1;
		}
	}
	return 0;
}
#endif

int MMU::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	if (!down) return 0; // only process keydown events

	if (KEYMOD_SHIFT (kstate)) {
	} else if (KEYMOD_CONTROL (kstate)) {
	} else { // unmodified keys
		switch (key) {
		case OAPI_KEY_E: {  // end EVA
			DOCKHANDLE hDock = GetDockHandle (0);
			OBJHANDLE mate = GetDockStatus (hDock);
			if (mate) {
				oapiSetFocusObject(mate);
				oapiDeleteVessel (GetHandle(), mate);
			}
			} return 1;
		}
	}
	return 0;
}

// ==============================================================
// API interface
// ==============================================================

// Initialisation
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new MMU (hvessel, flightmodel);
}

// Cleanup
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (MMU*)vessel;
}
