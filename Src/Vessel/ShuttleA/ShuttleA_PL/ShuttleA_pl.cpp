// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE:  ShuttleA Payload
//
// ShuttleA_pl.cpp
// Reference implementation of Shuttle-A Payload vessel class module
// ==============================================================

#define STRICT
#define ORBITER_MODULE

#include "ShuttleA_pl.h"
#include <math.h>
#include <stdio.h>


//Payload parachute airfoil definitions
void None_MomentCoeff (double aoa,double M,double Re,double *cl,double *cm,double *cd0)
{
	*cl = 0;

	*cm = 0;

	*cd0 = 0;
}
void Drogue_MomentCoeff (double aoa,double M,double Re,double *cl,double *cm,double *cd0)
{
	int i;
	const int nlift = 9;
	static const double AOA[nlift] = {-90*RAD,-85*RAD,-75*RAD,0*RAD,90*RAD,180*RAD,255*RAD,265*RAD,270*RAD};
	static const double CM[nlift]  = {       0,  -0.01,    -0.1,  -0.2,  0,    0.2,   0.1,     0.01,      0};

	static const double SCM[nlift] = {(CM[1]-CM[0])/(AOA[1]-AOA[0]), (CM[2]-CM[1])/(AOA[2]-AOA[1]),
		                              (CM[3]-CM[2])/(AOA[3]-AOA[2]), (CM[4]-CM[3])/(AOA[4]-AOA[3]),
									  (CM[5]-CM[4])/(AOA[5]-AOA[4]), (CM[6]-CM[5])/(AOA[6]-AOA[5]),
									  (CM[7]-CM[6])/(AOA[7]-AOA[6]), (CM[8]-CM[7])/(AOA[8]-AOA[7])};
	for (i = 0; i < nlift-1 && AOA[i+1] < aoa; i++);
	*cl= 0;

	*cm=CM[i] + (aoa-AOA[i])*SCM[i];

	*cd0 = 0.005  + oapiGetWaveDrag (M, 0.75, 1.0, 1.1, 0.04);
}
void Parachute_MomentCoeff (double aoa,double M,double Re,double *cl,double *cm,double *cd0)
{
	int i;
	const int nabsc = 9;
	static const double AOA[nabsc] = {-90*RAD,-85*RAD,-75*RAD,0*RAD,90*RAD,180*RAD,255*RAD,265*RAD,270*RAD};
	static const double CM[nabsc]  = {       0,  -0.01,    -0.1,  -0.2,  0,    0.2,   0.1,     0.01,      0};
	static const double SCM[nabsc] = {(CM[1]-CM[0])/(AOA[1]-AOA[0]), (CM[2]-CM[1])/(AOA[2]-AOA[1]),
		                              (CM[3]-CM[2])/(AOA[3]-AOA[2]), (CM[4]-CM[3])/(AOA[4]-AOA[3]),
									  (CM[5]-CM[4])/(AOA[5]-AOA[4]), (CM[6]-CM[5])/(AOA[6]-AOA[5]),
									  (CM[7]-CM[6])/(AOA[7]-AOA[6]), (CM[8]-CM[7])/(AOA[8]-AOA[7])};
	for (i = 0; i < nabsc-1 && AOA[i+1] < aoa; i++);
	*cl=0;
	
	*cd0 = 0.025;

	*cm= CM[i] + (aoa-AOA[i])*SCM[i];
}


// ==============================================================
// Specialised vessel class ShuttleA Payload Container
// ==============================================================

// --------------------------------------------------------------
// Constructor
// --------------------------------------------------------------
ShuttleA_PL::ShuttleA_PL (OBJHANDLE hObj, int fmodel)
: VESSEL2 (hObj, fmodel)
{
	Parachute_mode	= 0;
	timer			= 0.0f;

}

ShuttleA_PL::~ShuttleA_PL ()
{

}

void ShuttleA_PL::SetNormalConfig()
{

	SetMeshVisibilityMode (AddMesh (mesh_main), MESHVIS_EXTERNAL);
	
	ClearAirfoilDefinitions();
	CreateAirfoil (LIFT_VERTICAL, _V(0,-4,4), None_MomentCoeff,  4, 2500, 0.5);
}

void ShuttleA_PL::SetDrogueConfig()
{
	ClearMeshes();
	SetMeshVisibilityMode (AddMesh (mesh_main), MESHVIS_EXTERNAL);
	SetMeshVisibilityMode (AddMesh (mesh_drogue), MESHVIS_EXTERNAL);
	
	ClearAirfoilDefinitions();
	CreateAirfoil (LIFT_VERTICAL, _V(0.0f,-2.0f,0.0f), Drogue_MomentCoeff,  4, 2500, 0.5);


}

void ShuttleA_PL::SetParachuteConfig()
{
	ClearMeshes();
	SetMeshVisibilityMode (AddMesh (mesh_main), MESHVIS_EXTERNAL);
	SetMeshVisibilityMode (AddMesh (mesh_parachute), MESHVIS_EXTERNAL);
	
	ClearAirfoilDefinitions();
	CreateAirfoil (LIFT_VERTICAL, _V(0.0f,-2.0f,2.0f), Parachute_MomentCoeff,  4, 2500, 0.5);
}

void ShuttleA_PL::SetPostLandingConfig()
{
	ClearMeshes();
	SetMeshVisibilityMode (AddMesh (mesh_main), MESHVIS_EXTERNAL);

	ClearAirfoilDefinitions();

}
// ==============================================================
// Overloaded callback functions
// ==============================================================

// --------------------------------------------------------------
// Set vessel class caps
// --------------------------------------------------------------
// --------------------------------------------------------------
// Read status from scenario file
// --------------------------------------------------------------
void ShuttleA_PL::clbkLoadStateEx (FILEHANDLE scn, void *vs)
{
	char *line;

	while (oapiReadScenario_nextline (scn, line)) {
		if (!_strnicmp (line, "PARACHUTE", 9)) {
			sscanf (line+9, "%d", &Parachute_mode);
		} else if (!_strnicmp (line, "TIMER", 5)) {
			sscanf (line+5, "%lf", &timer);
		
		} else {
			ParseScenarioLineEx (line, vs);
			// unrecognised option - pass to Orbiter's generic parser
		}
	}

	switch (Parachute_mode)
	{
		case 0:
			SetNormalConfig();
			break;
		case 1:
			SetDrogueConfig();
			break;
		case 2:
			SetParachuteConfig();
			break;
		case 3:
			SetPostLandingConfig();
			break;
	}
}

// --------------------------------------------------------------
// Write status to scenario file
// --------------------------------------------------------------
void ShuttleA_PL::clbkSaveState (FILEHANDLE scn)
{
	char cbuf[256];

	// default vessel parameters
	VESSEL2::clbkSaveState (scn);

	// custom parameters
	
	sprintf (cbuf, "%d", Parachute_mode);
	oapiWriteScenario_string (scn, "PARACHUTE", cbuf);

	sprintf (cbuf, "%0.1f", timer);
	oapiWriteScenario_string (scn, "TIMER", cbuf);
}

// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA_PL::clbkSetClassCaps (FILEHANDLE cfg)
{
	
	//Common stuff on all configurations
	SetSize (5.0);
	SetPMI (_V(2.56,2.39,2.02));
	SetEmptyMass (20000.0f);
	SetCrossSections (_V(17.61f, 17.44f, 13.09f));
	SetRotDrag (_V(0.7, 0.7, 0.3));
	SetSurfaceFrictionCoeff (0.5, 0.5);
	SetCameraOffset (_V(0.0f,0.0f,0.0f));
	SetDockParams (_V(2.00f,0.00f,0.00f), _V(1,0,0), _V(0,1,0));

	char attach_id[8]={"SH"};
	payload_attachment[0]=CreateAttachment (true,_V(-0.319f, 0.0f,   2.464f),_V(0.0f,0.0f,1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[1]=CreateAttachment (true,_V(-0.319f, 0.0f,  -2.464f),_V(0.0f,0.0f,-1.0f),_V(0.0f,1.0f,0.0f),attach_id);
	payload_attachment[2]=CreateAttachment (true,_V(-0.319f, 2.0f,   0.0f  ),_V(0.0f,1.0f, 0.0f),_V(1.0f,0.0f,0.0f),"GS"); // MS 060906: added by request

	TOUCHDOWNVTX tdvtx[8] = {
		{{-2,-2, 2.5}, 2e5, 3e4, 5},
		{{-2,-2,-2.5}, 2e5, 3e4, 5},
		{{ 2,-2,-2.5}, 2e5, 3e4, 5},
		{{ 2,-2, 2.5}, 2e5, 3e4, 5},
		{{-2, 2, 2.5}, 2e5, 3e4, 5},
		{{-2, 2,-2.5}, 2e5, 3e4, 5},
		{{ 2, 2,-2.5}, 2e5, 3e4, 5},
		{{ 2, 2, 2.5}, 2e5, 3e4, 5}
	};
	SetTouchdownPoints (tdvtx, 8);

	EnableTransponder (true);

	mesh_main = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA_pl");
	mesh_drogue = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA_chpr");
	mesh_parachute = oapiLoadMeshGlobal ("ShuttleA\\ShuttleA_chmain");

	

}


// --------------------------------------------------------------
// 
// --------------------------------------------------------------
void ShuttleA_PL::clbkPostStep(double simt,double simdt,double mjd)
{
	
	double DPress;
	double Altiutde;
	
	switch (Parachute_mode)
	{
		case 0: // awaiting dyn_press to deploy the drogue

			// if we are nolonger attach to the shuttle.. activate the timer
			if ((GetAttachmentStatus (payload_attachment[0]) == NULL) && 
				(GetAttachmentStatus (payload_attachment[1]) == NULL))
												timer+=simdt;
			else  timer=0.0f;// reset timer upon recapture
			
			//do not deploy before we clear the shuttle
			if (timer <DROGUE_DEPLOY_DELAY) return; 

			//if timer has ellapsed.. pressure sensors should activate the drogue 
			DPress=GetDynPressure();

			if ((DPress>50000)||((DPress>1000)&&(GetAltitude()<5000.0f)))
			{
				SetDrogueConfig();
				Parachute_mode++;
			};
			break;

		case 1: //drogue is deployed.. awaiting 3k altitude for main chute
			Altiutde=GetAltitude();
			if (Altiutde<3000.0f) 
			{
				SetParachuteConfig();
				Parachute_mode++;
			}
			break;
		case 2: //main deployed.. awaiting 0 alt to release chute
			//Altiutde=GetAltitude();
			//if (Altiutde<10.0f) 
			if (GroundContact())
			{ 
				SetPostLandingConfig();
				Parachute_mode++;
			}
			break;
		case 3: //we've landed.. nothing to do 
			break;

	}
}


// ==============================================================
// API callback interface
// ==============================================================

// --------------------------------------------------------------
// Module initialisation
// --------------------------------------------------------------
DLLCLBK void InitModule (HINSTANCE hModule)
{
   
}

// --------------------------------------------------------------
// Module cleanup
// --------------------------------------------------------------
DLLCLBK void ExitModule (HINSTANCE hModule)
{

	 int d=0;
}

// --------------------------------------------------------------
// Vessel initialisation
// --------------------------------------------------------------
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{
	return new ShuttleA_PL (hvessel, flightmodel);
}

// --------------------------------------------------------------
// Vessel cleanup
// --------------------------------------------------------------
DLLCLBK void ovcExit (VESSEL *vessel)
{
	if (vessel) delete (ShuttleA_PL*)vessel;
}
