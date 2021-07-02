// ==============================================================
//                 ORBITER MODULE: Dragonfly
//                  Part of the ORBITER SDK
//            Copyright (C) 2002 Martin Schweiger
//                   All rights reserved
//
// Dragonfly.h
// Class interface of "Dragonfly" class space tug
// ==============================================================

#ifndef __DRAGONFLY_H
#define __DRAGONFLY_H

#include "orbitersdk.h"
#include "panel.h"
#include "internal.h"
// ==========================================================
// Some vessel class caps
// ==========================================================

const double EMPTY_MASS = 7e3;
// Vessel empty mass [kg]

const double MAX_MAIN_FUEL = 4e3;
// Max fuel capacity: main tank [kg]

const double MAX_RCS_THRUST = 1e3;
// Max thrust rating [N] for Reaction Control System thrusters

const double ISP = 4e3;
// Vacuum Isp for all thrusters [m/s]

// ==========================================================
// Interface for derived vessel class: Dragonfly
// ==========================================================

class Dragonfly: public VESSEL2 {
public:
	Dragonfly (OBJHANDLE hObj, int fmodel);
	void SetClassCaps (FILEHANDLE cfg);

	//-----------------this added for internals
	int CurrentPANEL;
	ShipInternal Internals;
	void LoadState (FILEHANDLE scn, void *vs);
	void SaveState (FILEHANDLE scn);
	void LoadPanel(int id);
	void PanelEvent(int id,int event,SURFHANDLE surf);
	void MouseEvent(int id,int event,int mx,int my);
    
	void DockEvent (int dock, OBJHANDLE connected);
	bool SetCGMode (int mode);
	void MoveCGOfs (int dir);
	void Timestep (double simt);
   
	void RegisterAnimations();
	void RedrawPanel_SensorInfo (SURFHANDLE surf);
	void RedrawPanel_CGIndicator (SURFHANDLE surf);

	void SetNormalRCS();
	void SetManualRCS();
	void DeleteAutoRCS();
	void SetZeroManualRCS();	// they constantly need to be set to 0 in manual mode!!!!
	double cgofs;               // longitudinal offset of CG when attached to other object
	float dock_latched;			//dock 0 can be latched
    int latch_handle;			//handle to latch/unlatch the docking port;
	int UAnt_handle;
	int LAnt_handle;
	int UP_handle;
	float UP_pos;
	int UY_handle;
	float UY_pos;
	int LP_handle;
	float LP_pos;
	int LY_handle;
	float LY_pos;
	float UP_trg;
	float UY_trg;
	float LP_trg;
	float LY_trg;
	float LAnt_SStr;
	float UAnt_SStr;		//diverse flags and pos for the upper and lower antenas
	int NAV_handle;			//lin,rot,diabled flag
	int VERN_handle;		//vernier or normal flag
	int Manual_RCS;			//are we in manual or normal RCS thruster groups mode ?
	int INTR_handle;		//RCS mode, normal, pulse, rate
	int Kill_rot;			//Kill rot nav mode flag
	int killset;			//diferential kill rot

	OBJHANDLE Dock_target_object;
	DOCKHANDLE Dock_target_dock;
	char Dock_dist[20];
    char Dock_vel[20];
    char Dock_x_vel[20];
	char Dock_y_vel[20];
	char Dock_z_vel[20];
    char spare_memory[20];
	float signal_flag;
	PROPELLANT_HANDLE ph_main;
	float *AC_power;
	float *DC_power;
	HDC openGLhDC;

	// overloaded VESSEL2 callback functions
	void clbkSetClassCaps (FILEHANDLE cfg);
	void clbkSaveState (FILEHANDLE scn);
	void clbkLoadStateEx (FILEHANDLE scn, void *vs);
	void clbkPostStep (double simt, double simdt, double mjd);
	void clbkMFDMode (int mfd, int mode);
	void clbkDockEvent (int dock, OBJHANDLE mate);
	int  clbkConsumeBufferedKey (DWORD key, bool down, char *kstate);
	bool clbkLoadPanel (int id);
	bool clbkPanelRedrawEvent (int id, int event, SURFHANDLE surf);
	bool clbkPanelMouseEvent (int id, int event, int mx, int my);

private:
	THRUSTER_HANDLE th_ap[4];   // handles of counter-balance engines in tail fin
	THRUSTER_HANDLE th_lp[5], th_rp[5], th[2];

	THGROUP_HANDLE th_lin_left,th_lin_right,th_lin_up,th_lin_down,th_lin_forward,th_lin_back;
	THGROUP_HANDLE th_rot_left,th_rot_right,th_rot_up,th_rot_down,th_rot_rllft,th_rot_rlrgt;
 	
	int cgmode;                 // 0=auto, 1=manual
	int cgswitch;               // -1=left, 0=center, 1=right
	bool undockflap;            // status of undock button cover

	int sensormode;             // docking sensor mode (local/remote)
	int remoteport;             // input reference to remote dock

	UINT anim_latch;			// handle to latch animation
	UINT anim_UY_ant;			
	UINT anim_UP_ant;			
	UINT anim_LY_ant;			
	UINT anim_LP_ant;			
	ANIMCOMP Upper_ant_pitch;
	ANIMCOMP Lower_ant_pitch;



};

#endif // !__DRAGONFLY_H