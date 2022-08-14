// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                 ORBITER MODULE: Dragonfly
//                  Part of the ORBITER SDK
//
// Dragonfly.cpp
// Reference implementation of "Dragonfly" class space tug
// ==============================================================

#define STRICT

#include "Dragonfly.h"
#include <stdio.h>
#include <math.h>
#include "internal.h"
//#include "glstuff.cpp"
HINSTANCE hDLL; 
double Lsim;

// ==============================================================
// Specialised vessel class Dragonfly
// ==============================================================

// Constructor
Dragonfly::Dragonfly (OBJHANDLE hObj, int fmodel)
: VESSEL2 (hObj, fmodel)
{ 
	Internals.Init(this);
	Internals.MakePanels(this);
	cgmode = 0;cgofs = 0.0;
	dock_latched=0;latch_handle=0;
	UP_handle=0;UP_pos=0;
	UY_handle=0;UY_pos=0.5;
    LP_handle=0;LP_pos=0;
	LY_handle=0;LY_pos=0.5;
	UAnt_handle=0;UAnt_SStr=0.0;
	LAnt_handle=0;LAnt_SStr=0.0;
	NAV_handle=3;		//no nav set
	VERN_handle=1;	    //normal jets
	INTR_handle=1;	    //normal rot
	killset=0;			//killrot mode is not saved 
	Kill_rot=0;
	Dock_target_object=NULL;
	RegisterAnimations();
//	openGLhDC=hDC;
}

void Dragonfly::RegisterAnimations()
{	static UINT Latch1[1] = {52};
    static UINT Latch2[1] = {53};//53
    static UINT Latch3[1] = {55};
	static UINT U_Antena[2]= {60,105};
	static UINT L_Antena[2]= {61,106};
	static ANIMCOMP latch1 = { // outer airlock
		Latch1, 1, 0.0, 0.6,
		0.25,0,3.06,
		0,1,0,
		(float)(45.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
	static ANIMCOMP latch2 = { // outer airlock
		Latch2, 1, 0.0, 0.6,
		-0.25,0,3.06,
		0,1,0,
		(float)(-45.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
	static ANIMCOMP latch3 = { // outer airlock
		Latch3, 1, 0.5, 1.0,
		0,0,0.5,
		0,
		0,
		MESHGROUP_TRANSFORM::TRANSLATE
	};
	static ANIMCOMP U_yaw_ant = { // outer airlock
	    U_Antena, 2, 0.0, 1.0,
		0,0,0,
		0,1,0,
		(float)(300.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
	static ANIMCOMP U_pitch_ant = { // outer airlock
	    U_Antena, 2, 0.0, 1.0,
		0,3.18,0,
		1,0,0,
		(float)(-75.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
		Upper_ant_pitch=U_pitch_ant;
	
	
	static ANIMCOMP L_yaw_ant = { // outer airlock
	    L_Antena, 2, 0.0, 1.0,
		0,0,0,
		0,1,0,
		(float)(300.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
	static ANIMCOMP L_pitch_ant = { // outer airlock
	    L_Antena, 2, 0.0, 1.0,
		0,-3.18,0,
		1,0,0,
		(float)(75.0/180.0*acos(-1.0)),
		0,
		0,
		MESHGROUP_TRANSFORM::ROTATE
	};
	Lower_ant_pitch=L_pitch_ant;

	// Register animation for dock latching
	anim_latch = RegisterAnimSequence (0.0); // gear is fully extended in the mesh
	AddAnimComp (anim_latch, &latch1);   
	AddAnimComp (anim_latch, &latch2); 
	AddAnimComp (anim_latch, &latch3); 
	
	anim_UP_ant = RegisterAnimSequence(0);	//forward
	AddAnimComp(anim_UP_ant,&Upper_ant_pitch);

	anim_UY_ant = RegisterAnimSequence(0.5);	//forward
	AddAnimComp(anim_UY_ant,&U_yaw_ant);
		

	anim_LP_ant = RegisterAnimSequence(0);	//forward
	AddAnimComp(anim_LP_ant,&Lower_ant_pitch);

	anim_LY_ant = RegisterAnimSequence(0.5);	//forward
	AddAnimComp(anim_LY_ant,&L_yaw_ant);
		
}
// Set vessel class parameters
void Dragonfly::SetClassCaps (FILEHANDLE cfg)
{
	
	int i;

	SetSize (4.0);
	SetEmptyMass (EMPTY_MASS);
	SetCrossSections (_V(23.7,22.5,17.3));
	SetPMI (_V(5.4,5.4,2.5));
	SetCameraOffset (_V(0,1.5,0));

	// ************************* propellant specs **********************************

	ph_main = CreatePropellantResource (MAX_MAIN_FUEL);

	// *********************** thruster definitions ********************************

	// thrusters in left pod
	th_lp[0] = CreateThruster (_V(-3.5,0,0), _V(1,0,0), 2*MAX_RCS_THRUST, ph_main, ISP);
	th_lp[1] = CreateThruster (_V(-2.98,0,-0.8), _V(0,0,1), MAX_RCS_THRUST, ph_main, ISP);
	th_lp[2] = CreateThruster (_V(-2.98,0,0.8), _V(0,0,-1), MAX_RCS_THRUST, ph_main, ISP);
	th_lp[3] = CreateThruster (_V(-2.98,-0.8,0), _V(0,1,0), MAX_RCS_THRUST, ph_main, ISP);
	th_lp[4] = CreateThruster (_V(-2.98,0.8,0), _V(0,-1,0), MAX_RCS_THRUST, ph_main, ISP);

	// thrusters in right pod
	th_rp[0] = CreateThruster (_V(3.5,0,0), _V(-1,0,0), 2*MAX_RCS_THRUST, ph_main, ISP);
	th_rp[1] = CreateThruster (_V(2.98,0,-0.8), _V(0,0,1), MAX_RCS_THRUST, ph_main, ISP);
	th_rp[2] = CreateThruster (_V(2.98,0,0.8), _V(0,0,-1), MAX_RCS_THRUST, ph_main, ISP);
	th_rp[3] = CreateThruster (_V(2.98,-0.8,0), _V(0,1,0), MAX_RCS_THRUST, ph_main, ISP);
	th_rp[4] = CreateThruster (_V(2.98,0.8,0), _V(0,-1,0), MAX_RCS_THRUST, ph_main, ISP);

	// thrusters in aft pod (rotational RCS)
	th_ap[0] = CreateThruster (_V(-0.8,0,-11.1), _V(1,0,0), MAX_RCS_THRUST, ph_main, ISP);
	th_ap[1] = CreateThruster (_V(0.8,0,-11.1), _V(-1,0,0), MAX_RCS_THRUST, ph_main, ISP);
	th_ap[2] = CreateThruster (_V(0,-0.8,-11.1), _V(0,1,0), MAX_RCS_THRUST, ph_main, ISP);
	th_ap[3] = CreateThruster (_V(0,0.8,-11.1), _V(0,-1,0), MAX_RCS_THRUST, ph_main, ISP);

	// exhaust definitions for left pod
	AddExhaust (th_lp[0], 1, 0.15, _V(-3.5,0.18,-0.18), _V(-1,0,0));
	AddExhaust (th_lp[0], 1, 0.15, _V(-3.5,-0.18,0.18), _V(-1,0,0));
	for (i = 1; i < 5; i++) AddExhaust (th_lp[i], 1, 0.15);

	// exhaust definitions for right pod
	AddExhaust (th_rp[0], 1, 0.15, _V(3.5,-0.18,-0.18), _V(1,0,0));
	AddExhaust (th_rp[0], 1, 0.15, _V(3.5,0.18,0.18), _V(1,0,0));
	for (i = 1; i < 5; i++) AddExhaust (th_rp[i], 1, 0.15);

	// exhaust definitions for aft pod
	for (i = 0; i < 4; i++) AddExhaust (th_ap[i], 1, 0.15);
	//thruster definitions. Both manual and "automatic" RCS
	SetManualRCS();
	SetNormalRCS();//
	
	// *************************** docking port ************************************

	SetDockParams (_V(0,0,3.2), _V(0,0,1), _V(0,1,0));
    
	// ******************************** mesh ***************************************

	AddMesh (oapiLoadMeshGlobal ("Dragonfly\\Dragonfly"));
};

void Dragonfly::LoadState (FILEHANDLE scn, void *vs)
{
    char *line;
	while (oapiReadScenario_nextline (scn, line)) {
        if (!strnicmp (line, "UPPERANT", 8)) {
			sscanf (line+8, "%f %f %i %i %i", &UP_pos ,&UY_pos,&UP_handle, &UY_handle,&UAnt_handle);
		} else if (!strnicmp (line, "LOWERANT", 8)) {
			sscanf (line+8, "%f %f %i %i %i", &LP_pos, &LY_pos,&LP_handle,&LY_handle,&LAnt_handle);
			//SetGearParameters (gear_proc);
		} else if (!strnicmp (line, "HATCH", 5)) {
			sscanf (line+5, "%f %i", &dock_latched, &latch_handle);
		} else if (!strnicmp (line, "ANTTRG", 6)) {
			Dock_target_object=oapiGetObjectByName(line+7);
			//sscanf (line+5, "%f %i", &dock_latched, &latch_handle);
        } else {
            ParseScenarioLineEx (line, vs);
			// unrecognised option - pass to Orbiter's generic parser
        }
    }

	    SetAnimState (anim_UY_ant, UY_pos);
		float ang=(150-UY_pos*300.0)/180.0*acos(-1.0);
	    Upper_ant_pitch.trans.P.rotparam.axis=_V(cos(ang),0,-sin(ang));	 
        SetAnimState (anim_UP_ant, UP_pos);		
	    
		SetAnimState (anim_LY_ant, LY_pos);
		ang=(150-LY_pos*300.0)/180.0*acos(-1.0);
	    Lower_ant_pitch.trans.P.rotparam.axis=_V(cos(ang),0,-sin(ang));	 
        SetAnimState (anim_LP_ant, LP_pos);		

	    SetAnimState (anim_latch, dock_latched);
};

void Dragonfly::SaveState (FILEHANDLE scn)
{
	char cbuf[256];

	// custom parameters
	sprintf (cbuf, "%f %f %i %i %i", UP_pos ,UY_pos,UP_handle, UY_handle,UAnt_handle);
	oapiWriteScenario_string (scn, "UPPERANT", cbuf);
	sprintf (cbuf, "%f %f %i %i %i", LP_pos ,LY_pos,LP_handle, LY_handle,LAnt_handle);
	oapiWriteScenario_string (scn, "LOWERANT", cbuf);
    sprintf(cbuf,"%f %i",dock_latched,latch_handle);
    oapiWriteScenario_string (scn, "HATCH", cbuf);
	//char *name;
	if (Dock_target_object) {
		 oapiGetObjectName(Dock_target_object,cbuf,256);
		 oapiWriteScenario_string (scn, "ANTTRG", cbuf);
	}
}
void Dragonfly::DockEvent (int dock, OBJHANDLE connected)
{
	if (sensormode == 1) { // we are in remote sensing mode
		struct { OBJHANDLE hObj; int dock; } dockspec;
		dockspec.hObj = connected;
		dockspec.dock = remoteport = (connected ? 0 : -1);
		oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);
	}
//	oapiTriggerPanelRedrawArea (0, AID_DOCK_SENSORINFO);
};

bool Dragonfly::SetCGMode (int mode)
{
	if (mode == cgmode) return false; // nothing to do
	cgmode = mode;
	return true;
};

void Dragonfly::MoveCGOfs (int dir)
{
	if (cgmode == 0) return;
	if (dir < 0) {
		if (cgofs <= 0.0) return;
		cgofs = max (0.0, cgofs - oapiGetSysStep() * 0.5);
	} else {
		if (cgofs >= 22.2) return;
		cgofs = min (22.2, cgofs + oapiGetSysStep() * 0.5);
	}
};
void Dragonfly::DeleteAutoRCS() //this removes normal control of Orbiter over RCS
{
  DelThrusterGroup(THGROUP_ATT_PITCHUP);
  DelThrusterGroup(THGROUP_ATT_PITCHDOWN);
  DelThrusterGroup(THGROUP_ATT_YAWRIGHT);
  DelThrusterGroup(THGROUP_ATT_YAWLEFT);
  DelThrusterGroup(THGROUP_ATT_BANKRIGHT);
  DelThrusterGroup(THGROUP_ATT_BANKLEFT);
  DelThrusterGroup(THGROUP_ATT_LEFT);
  DelThrusterGroup(THGROUP_ATT_RIGHT);
  DelThrusterGroup(THGROUP_ATT_UP);
  DelThrusterGroup(THGROUP_ATT_DOWN);
  DelThrusterGroup(THGROUP_ATT_FORWARD);
  DelThrusterGroup(THGROUP_ATT_BACK);
  Manual_RCS=1;//we've gone to manual
}
void Dragonfly::SetZeroManualRCS()
{

		//set all manual thrust to 0
	SetThrusterGroupLevel(th_lin_left,0.0);
	SetThrusterGroupLevel(th_lin_right,0.0);
	SetThrusterGroupLevel(th_lin_up,0.0);
	SetThrusterGroupLevel(th_lin_down,0.0);
	SetThrusterGroupLevel(th_lin_forward,0.0);
	SetThrusterGroupLevel(th_lin_back,0.0);
	
	
    SetThrusterGroupLevel(th_rot_left,0.0);
	SetThrusterGroupLevel(th_rot_right,0.0);
	SetThrusterGroupLevel(th_rot_up,0.0);
	SetThrusterGroupLevel(th_rot_down,0.0);
    SetThrusterGroupLevel(th_rot_rllft,0.0);
	SetThrusterGroupLevel(th_rot_rlrgt,0.0);
};
void Dragonfly::SetNormalRCS()
{
	DeleteAutoRCS();	//??need to delete them again?
	SetZeroManualRCS();	//just in case
	
	// thruster group definitions (or re-definition)
	th[0] = th_lp[0];
	CreateThrusterGroup (th, 1, THGROUP_ATT_RIGHT);
	// need to add counter-balance in aft pod

	th[0] = th_rp[0];
	CreateThrusterGroup (th, 1, THGROUP_ATT_LEFT);

	th[0] = th_lp[3];
	th[1] = th_rp[3];
	CreateThrusterGroup (th, 2, THGROUP_ATT_UP);

	th[0] = th_lp[4];
	th[1] = th_rp[4];
	CreateThrusterGroup (th, 2, THGROUP_ATT_DOWN);

	th[0] = th_lp[1];
	th[1] = th_rp[1];
	CreateThrusterGroup (th, 2, THGROUP_ATT_FORWARD);

	th[0] = th_lp[2];
	th[1] = th_rp[2];
	CreateThrusterGroup (th, 2, THGROUP_ATT_BACK);

	th[0] = th_lp[3];
	th[1] = th_rp[4];
	CreateThrusterGroup (th, 2, THGROUP_ATT_BANKRIGHT);

	th[0] = th_lp[4];
	th[1] = th_rp[3];
	CreateThrusterGroup (th, 2, THGROUP_ATT_BANKLEFT);

	CreateThrusterGroup (th_ap+0, 1, THGROUP_ATT_YAWLEFT);
	CreateThrusterGroup (th_ap+1, 1, THGROUP_ATT_YAWRIGHT);
	CreateThrusterGroup (th_ap+2, 1, THGROUP_ATT_PITCHDOWN);
	CreateThrusterGroup (th_ap+3, 1, THGROUP_ATT_PITCHUP);
	Manual_RCS=0;//we've gone to Auto

};
void Dragonfly::SetManualRCS()
{//we define manual RCS groups
	// thruster group definitions
	th[0] = th_lp[0];
	th_lin_right=CreateThrusterGroup (th, 1, THGROUP_USER);
	// need to add counter-balance in aft pod

	th[0] = th_rp[0];
	th_lin_left=CreateThrusterGroup (th, 1, THGROUP_USER);

	th[0] = th_lp[3];
	th[1] = th_rp[3];
	th_lin_up=CreateThrusterGroup (th, 2, THGROUP_USER);

	th[0] = th_lp[4];
	th[1] = th_rp[4];
	th_lin_down=CreateThrusterGroup (th, 2, THGROUP_USER);

	th[0] = th_lp[1];
	th[1] = th_rp[1];
	th_lin_forward=CreateThrusterGroup (th, 2, THGROUP_USER);

	th[0] = th_lp[2];
	th[1] = th_rp[2];
	th_lin_back=CreateThrusterGroup (th, 2, THGROUP_USER);

	th[0] = th_lp[3];
	th[1] = th_rp[4];
	th_rot_rlrgt=CreateThrusterGroup (th, 2, THGROUP_USER);

	th[0] = th_lp[4];
	th[1] = th_rp[3];
	th_rot_rllft=CreateThrusterGroup (th, 2, THGROUP_USER);

	th_rot_left=CreateThrusterGroup (th_ap+0, 1, THGROUP_USER);
	th_rot_right=CreateThrusterGroup (th_ap+1, 1, THGROUP_USER);
	th_rot_down=CreateThrusterGroup (th_ap+2, 1, THGROUP_USER);
	th_rot_up=CreateThrusterGroup (th_ap+3, 1, THGROUP_USER);
}



void Dragonfly::Timestep (double simt)
{
	VECTOR3 cg;
	VECTOR3 dist,pos;
	double dlevel, ratio;
	float line;
	static int vern;
	static int intr;
	static int rotmode;
// ******************** Set linear, rotation, disabled RCS mode 
	if (NAV_handle<3)
		{SetAttitudeMode(NAV_handle+1);
		 NAV_handle=3;
		};
// ******************** Kill rot mode can be called via a CB 
	//sprintf(oapiDebugString(),"%0.4f %0.4f %0.4f",Internals.Valves[23]->Temp,Internals.Tanks[11]->Temp,Internals.Tanks[12]->Temp);
	if (Kill_rot==1) 
	{ if (killset==0) { killset=1; //we've set the killrot
	  rotmode= Internals.Nav_mode_switch->pos;	
	  Internals.Nav_mode_switch->pos=0;//go to rot mode
	  oapiTriggerPanelRedrawArea(0,Internals.Nav_mode_switch->idx);//and redraw it
	  vern=VERN_handle;
	  intr=INTR_handle;
	  if (Manual_RCS) {
			VERN_handle=1;//no vernier
			Internals.Vern_mode_switch->pos=1;//no vernier
			oapiTriggerPanelRedrawArea(0,Internals.Vern_mode_switch->idx);
			INTR_handle=1;//normal mode
			Internals.Intr_mode_switch->pos=1;
			oapiTriggerPanelRedrawArea(0,Internals.Intr_mode_switch->idx);
			SetNormalRCS(); //go to auto mode
						};

	 SetAttitudeMode(1);//go to rot mode
	 ActivateNavmode(NAVMODE_KILLROT); //stop all rotation;
						}//end of killset
	if (killset==1){		//we are killrotting, monitoring the end of it
	   if (GetNavmodeState(NAVMODE_KILLROT)==0)//no longer killrotting
		     {//go back to previously saved RCS mode
			  Internals.Nav_mode_switch->pos=rotmode;
			  oapiTriggerPanelRedrawArea(0,Internals.Nav_mode_switch->idx);
			  SetAttitudeMode(rotmode+1);
			  
			  Internals.Vern_mode_switch->pos=vern;
			  VERN_handle=vern;
			  oapiTriggerPanelRedrawArea(0,Internals.Vern_mode_switch->idx);
			  
			  Internals.Intr_mode_switch->pos=intr;
			  INTR_handle=intr;
			  oapiTriggerPanelRedrawArea(0,Internals.Intr_mode_switch->idx);
			  //then inform the loop that killrot has ended
		      Kill_rot=0;
			  killset=0;
				};
				}//end of killset
	}; //end of killrot=0;
	if ((killset)&&(Kill_rot==0)) //we've been asked to stop killrot
	{//go back to previously saved RCS mode
			  Internals.Nav_mode_switch->pos=rotmode;
			  oapiTriggerPanelRedrawArea(0,Internals.Nav_mode_switch->idx);
			  SetAttitudeMode(rotmode+1);
			  
			  Internals.Vern_mode_switch->pos=vern;
			  VERN_handle=vern;
			  oapiTriggerPanelRedrawArea(0,Internals.Vern_mode_switch->idx);
			  
			  Internals.Intr_mode_switch->pos=intr;
			  INTR_handle=intr;
			  oapiTriggerPanelRedrawArea(0,Internals.Intr_mode_switch->idx);
		
	  DeactivateNavmode(NAVMODE_KILLROT);
	  Kill_rot=0; //not doing anything
	  killset=0;
	};

//********************* Handle the vessel animations. Docking latch mecanism and the 2 antenas
	if (Dock_target_object)	{
	//Dock_target_object=oapiGetVesselByIndex(1);
	
		oapiGetGlobalPos(Dock_target_object,&dist);
		Global2Local(dist,pos);//now we have a position w.r.t ship
		line=_vector3(pos.x,pos.y,pos.z).mod();
		UP_trg=1-acos(pos.y/line)/acos(-1.0)*180.0/75.0;
		UY_trg=atan2(pos.x/line,pos.z/line);
		if (UY_trg>acos(-1.0)) UY_trg-=2*acos(-1.0);
		   UY_trg=0.5-UY_trg/acos(-1.0)*180.0/300.0;
		
		LP_trg=acos(pos.y/line)/acos(-1.0)*180.0/75.0-1;
		LY_trg=UY_trg;//same azimuth;

	if (UAnt_handle==-1) {//Aquire & Track
		if (UY_trg<UY_pos) UY_handle=-1;
		if (UY_trg>UY_pos) UY_handle=1;
		if (UP_trg<UP_pos) UP_handle=-1;
		if (UP_trg>UP_pos) UP_handle=1;
	};
	if (LAnt_handle==-1) {//Aquire & Track
		if (LY_trg<LY_pos) LY_handle=-1;
		if (LY_trg>LY_pos) LY_handle=1;
		if (LP_trg<LP_pos) LP_handle=-1;
		if (LP_trg>LP_pos) LP_handle=1;
	};
	LAnt_SStr=1.0-(fabs(LY_trg-LY_pos)+fabs(LP_trg-LP_pos))/0.04;
	UAnt_SStr=1.0-(fabs(UY_trg-UY_pos)+fabs(UP_trg-UP_pos))/0.04;
	};
	if (UAnt_handle==1) //bring upper antena home 
	{ 
	  if (UP_pos>0) UP_handle=-1;
	  else UP_handle=0;

	  if (UY_pos>0.51) UY_handle=-1;
	  else if (UY_pos<0.49) UY_handle=1;
		   else {UY_pos=0.5;;UY_handle=0;}
	};

	if (LAnt_handle==1) //bring lower antena home 
	{ if (LP_pos>0) LP_handle=-1;
	  else LP_handle=0;
	  if (LY_pos>0.51) LY_handle=-1;
	  else if (LY_pos<0.49) LY_handle=1;
		   else {LY_pos=0.5;;LY_handle=0;}
	};
	//stop all motion 
    if (UAnt_handle==0)
		{UP_handle=0; UY_handle=0; UAnt_handle=2;};
	if (LAnt_handle==0)
		{LP_handle=0; LY_handle=0; LAnt_handle=2;};


	if (latch_handle)
	{	if ((latch_handle<0)&&(dock_latched>0)) dock_latched-=oapiGetSysStep()/10.0;
	    if ((latch_handle>0)&&(dock_latched<1)) dock_latched+=oapiGetSysStep()/10.0;
		if (dock_latched<0) dock_latched=0;
		if(dock_latched>1) dock_latched=1;
	SetAnimState (anim_latch, dock_latched);
	}

	
	if ((*AC_power>0)&&(UY_handle))
		{if ((UY_handle<0)&&(UY_pos>0)) UY_pos-=oapiGetSysStep()/18.0;
	     if ((UY_handle>0)&&(UY_pos<1)) UY_pos+=oapiGetSysStep()/18.0;
		 if (UY_pos<0) UY_pos=0;
		 if (UY_pos>1) UY_pos=1;
		SetAnimState (anim_UY_ant, UY_pos);
		float ang=(150-UY_pos*300.0)/180.0*acos(-1.0);
	    Upper_ant_pitch.trans.P.rotparam.axis=_V(cos(ang),0,-sin(ang));	 
	};
	if ((*AC_power>0)&&(UP_handle))
		{if ((UP_handle<0)&&(UP_pos>0)) UP_pos-=oapiGetSysStep()/18.0;
	     if ((UP_handle>0)&&(UP_pos<1)) UP_pos+=oapiGetSysStep()/18.0;
		 if (UP_pos<0) UP_pos=0;
		 if (UP_pos>1) UP_pos=1;
		 SetAnimState (anim_UP_ant, UP_pos);
	};
	if ((*AC_power>0)&&(LY_handle))
		{if ((LY_handle<0)&&(LY_pos>0)) LY_pos-=oapiGetSysStep()/18.0;
	     if ((LY_handle>0)&&(LY_pos<1)) LY_pos+=oapiGetSysStep()/18.0;
		 if (LY_pos<0) LY_pos=0;
		 if (LY_pos>1) LY_pos=1;
		SetAnimState (anim_LY_ant, LY_pos);
		float ang=(150-LY_pos*300.0)/180.0*acos(-1.0);
	    Lower_ant_pitch.trans.P.rotparam.axis=_V(cos(ang),0,-sin(ang));	 
	};
	if ((*AC_power>0)&&(LP_handle))
		{if ((LP_handle<0)&&(LP_pos>0)) LP_pos-=oapiGetSysStep()/18.0;
	     if ((LP_handle>0)&&(LP_pos<1)) LP_pos+=oapiGetSysStep()/18.0;
		 if (LP_pos<0) LP_pos=0;
		 if (LP_pos>1) LP_pos=1;
		SetAnimState (anim_LP_ant, LP_pos);
	};
signal_flag=0;
if ((*AC_power>0)&&((UAnt_SStr>0.9)||(LAnt_SStr>0.9)))
{	
	sprintf(Dock_dist,"%5.0f",_vector3(pos.x,pos.y,pos.z).mod());//radar dist
	VECTOR3 rel_vel;
	oapiGetRelativeVel(GetHandle(),Dock_target_object,&rel_vel);
    rel_vel=rel_vel+dist; //this is global;
    VECTOR3 local_vel;
	Global2Local(rel_vel,local_vel);//local frame vel
	local_vel=local_vel-pos;	//minus position is actual V vector
	sprintf(Dock_vel,"%5.2f",_vector3(local_vel.x,local_vel.y,local_vel.z).mod());//total closure
	sprintf(Dock_x_vel,"%3.0f",fabs(local_vel.x*100));
	sprintf(Dock_y_vel,"%3.0f",fabs(local_vel.y*100));
	sprintf(Dock_z_vel,"%3.0f",fabs(local_vel.z*100));
	signal_flag=1;	
}
//adjust for diferent RCS modes
//NAV HANDLE - 1,lin, 0,rot, -1,off
//VERN HANDLE - 1,norm, -1,vernier
//INTR HANDLE - 1,command, 0, pulse, -1, rate
if (Manual_RCS==0) //we are in auto mode
	{if (VERN_handle<1) DeleteAutoRCS();// go to manual
     if (INTR_handle<1) DeleteAutoRCS();// go to manual
	};
double tlevel;
double tlevel_r;
static int lin_x,lin_y,lin_z;
static int rot_x,rot_y,rot_z;
if (Manual_RCS)
{//now's the tricky part
if (INTR_handle>-1) SetZeroManualRCS();//needs to be zeroed;!
if ((VERN_handle==1) &&(INTR_handle==1)) SetNormalRCS();//go to auto
else
{
 switch (INTR_handle)
 { 
 case 1: // normal command
	 dlevel=(VERN_handle>0?1:0.1); //vernier or not! :-) must be vernier, or this would not be manual command ...
	 if (Internals.Nav_mode_switch->pos==0){
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_up,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_down,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_right,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_left,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_rlrgt,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) SetThrusterGroupLevel(th_rot_rllft,tlevel*dlevel);
	 } else if (Internals.Nav_mode_switch->pos==1) {
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_left,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_right,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_up,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_down,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_forward,tlevel*dlevel);
	 if (tlevel=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) SetThrusterGroupLevel(th_lin_back,tlevel*dlevel);
	 }//the left/right forward/back are changed a bit.. but there is no "9" key processed
	 break;
 case 0: //pulse type command
	 dlevel=(VERN_handle>0?1:0.1); //vernier or not!
	 if (Internals.Nav_mode_switch->pos==0){
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_x++)) SetThrusterGroupLevel(th_rot_up,tlevel*dlevel); 
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_x++)) SetThrusterGroupLevel(th_rot_down,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_x=0;
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_z++)) SetThrusterGroupLevel(th_rot_right,tlevel*dlevel);
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_z++)) SetThrusterGroupLevel(th_rot_left,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_z=0;
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_y++)) SetThrusterGroupLevel(th_rot_rlrgt,tlevel*dlevel);
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_y++)) SetThrusterGroupLevel(th_rot_rllft,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_y=0;
	 } else if (Internals.Nav_mode_switch->pos==1) {
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_x++)) SetThrusterGroupLevel(th_lin_left,tlevel*dlevel);
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_x++)) SetThrusterGroupLevel(th_lin_right,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_x=0;
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_z++)) SetThrusterGroupLevel(th_lin_up,tlevel*dlevel);
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_z++)) SetThrusterGroupLevel(th_lin_down,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_z=0;
	 if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_y++)) SetThrusterGroupLevel(th_lin_forward,tlevel*dlevel);
	 if ((tlevel_r=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE))&&(!rot_y++)) SetThrusterGroupLevel(th_lin_back,tlevel_r*dlevel);
	 if (tlevel+tlevel_r==0) rot_y=0;
	 }//the left/right forward/back are changed a bit.. but there is no "9" key processed :-(
	 break;
 case -1: //rate command.. this is overly-complicated
	dlevel=(VERN_handle>0?1:0.1); //vernier or not!
	 if (Internals.Nav_mode_switch->pos==0){
		 //pitch -up
		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!rot_x++))
		 {
			tlevel_r=GetThrusterGroupLevel(th_rot_down);
			if (tlevel_r) {SetThrusterGroupLevel(th_rot_down,0.0);lin_x++;}
			else IncThrusterGroupLevel(th_rot_up,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_x=0;
		//pitch-down
		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE))&&(!lin_x++))
			{
			tlevel_r=GetThrusterGroupLevel(th_rot_up);
			if (tlevel_r) {SetThrusterGroupLevel(th_rot_up,0.0);rot_x++;}
			else IncThrusterGroupLevel(th_rot_down,tlevel*dlevel/10.0);
			}
		 if(!tlevel) lin_x=0;
		 //yaw right
		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) &&(!rot_z++))
			{
			tlevel_r=GetThrusterGroupLevel(th_rot_left);
			if (tlevel_r){ SetThrusterGroupLevel(th_rot_left,0.0);lin_z++;}
			else IncThrusterGroupLevel(th_rot_right,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_z=0;
		//yaw right
		if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) &&(!lin_z++))
			{
			tlevel_r=GetThrusterGroupLevel(th_rot_right);
			if (tlevel_r){ SetThrusterGroupLevel(th_rot_right,0.0);rot_z++;}
			else IncThrusterGroupLevel(th_rot_left,tlevel*dlevel/10.0);
			}
		 if (!tlevel) lin_z=0;
		//roll right
		if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) && (!rot_y++))
		 {
			tlevel_r=GetThrusterGroupLevel(th_rot_rllft);
			if (tlevel_r){ SetThrusterGroupLevel(th_rot_rllft,0.0);lin_y++;}
			else IncThrusterGroupLevel(th_rot_rlrgt,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_y=0;
		 //roll left
		if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_ROTMODE)) &&(!lin_y++))
		{
			tlevel_r=GetThrusterGroupLevel(th_rot_rlrgt);
			if (tlevel_r){ SetThrusterGroupLevel(th_rot_rlrgt,0.0);rot_z++;}
			else IncThrusterGroupLevel(th_rot_rllft,tlevel*dlevel/10.0);
			}
		 if (!tlevel) lin_y=0;
	 } else if (Internals.Nav_mode_switch->pos==1) {
		if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) &&(!rot_x++))
			 {
			tlevel_r=GetThrusterGroupLevel(th_lin_right);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_right,0.0);lin_x++;}
			else IncThrusterGroupLevel(th_lin_left,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_x=0;

		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_BANKRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) &&(!lin_x++))
			 {
			tlevel_r=GetThrusterGroupLevel(th_lin_left);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_left,0.0);rot_x++;}
			else IncThrusterGroupLevel(th_lin_right,tlevel*dlevel/10.0);
			}
		 if (!tlevel) lin_x=0;

		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHUP, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) &&(!rot_z++))
	 		 {
			tlevel_r=GetThrusterGroupLevel(th_lin_down);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_down,0.0);lin_z++;}
			else IncThrusterGroupLevel(th_lin_up,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_z=0;

		 if ((tlevel=GetManualControlLevel(THGROUP_ATT_PITCHDOWN, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) &&(!lin_z++))
			 {
			tlevel_r=GetThrusterGroupLevel(th_lin_up);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_up,0.0);rot_z++;}
			else IncThrusterGroupLevel(th_lin_down,tlevel*dlevel/10.0);
			}
		 if (!tlevel) lin_z=0;

		if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWLEFT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) &&(!rot_y++))
			 {
			tlevel_r=GetThrusterGroupLevel(th_lin_back);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_back,0.0);lin_y++;}
			else IncThrusterGroupLevel(th_lin_forward,tlevel*dlevel/10.0);
			}
		 if (!tlevel) rot_y=0;

		if ((tlevel=GetManualControlLevel(THGROUP_ATT_YAWRIGHT, MANCTRL_ANYDEVICE, MANCTRL_LINMODE)) && (!lin_y++))
			 {
			tlevel_r=GetThrusterGroupLevel(th_lin_forward);
			if (tlevel_r) {SetThrusterGroupLevel(th_lin_forward,0.0);rot_y++;}
			else IncThrusterGroupLevel(th_lin_back,tlevel*dlevel/10.0);
			}
		 if (!tlevel) lin_y=0;

	 }//the left/right forward/back are changed a bit.. but there is no "9" key processed
	 break;
 };//end INTR switch
}//end else
}//end manual RCS


//*******************************  THIS TAKES CARE OF AUTOMATIC GC MANAGEMENT...
	if (!cgmode) // auto
		cgofs = (GetSuperstructureCG (cg) ? cg.z : 0.0);
	if (Manual_RCS==0) { //auto mode we use normal Orbiter channels
	if (cgofs) {
		ratio = 2.0*cgofs/(cgofs+11.1); // counter-balance level

		dlevel = GetThrusterGroupLevel (THGROUP_ATT_LEFT) - GetThrusterGroupLevel (THGROUP_ATT_RIGHT);
		if (dlevel > 0)      IncThrusterLevel_SingleStep (th_ap[0],  dlevel*ratio);
		else if (dlevel < 0) IncThrusterLevel_SingleStep (th_ap[1], -dlevel*ratio);

		dlevel = GetThrusterGroupLevel (THGROUP_ATT_DOWN) - GetThrusterGroupLevel (THGROUP_ATT_UP);
		if      (dlevel > 0) IncThrusterLevel_SingleStep (th_ap[2],  dlevel*ratio);
		else if (dlevel < 0) IncThrusterLevel_SingleStep (th_ap[3], -dlevel*ratio);
	}}
	else { //we are in manual mode, using custom thruster groups
	if (cgofs) {
		ratio = 2.0*cgofs/(cgofs+11.1); // counter-balance level

		dlevel = GetThrusterGroupLevel (th_lin_left) - GetThrusterGroupLevel (th_lin_right);
		if (dlevel > 0)      IncThrusterLevel_SingleStep (th_ap[0],  dlevel*ratio);
		else if (dlevel < 0) IncThrusterLevel_SingleStep (th_ap[1], -dlevel*ratio);

		dlevel = GetThrusterGroupLevel (th_lin_down) - GetThrusterGroupLevel (th_lin_up);
		if      (dlevel > 0) IncThrusterLevel_SingleStep (th_ap[2],  dlevel*ratio);
		else if (dlevel < 0) IncThrusterLevel_SingleStep (th_ap[3], -dlevel*ratio);
	}


	}
};
void Dragonfly::RedrawPanel_SensorInfo (SURFHANDLE surf)
{
	bool engaged = false;
	OBJHANDLE hObj = GetDockStatus (GetDockHandle (0));
	HDC hDC = oapiGetDC (surf);
//	SelectObject (hDC, g_Param.font[1]);
	SetTextColor (hDC, RGB(0,255,0));
	SetBkMode (hDC, TRANSPARENT);
	if (!sensormode) {
		TextOut (hDC, 0, 0, "LOCAL", 5);
		engaged = (hObj != NULL);
	} else {
		TextOut (hDC, 0, 0, "REMOTE", 6);
		if (remoteport >= 0) {
			char cbuf[20];
			sprintf (cbuf, "DOCK %d", remoteport+1);
			TextOut (hDC, 0, 10, cbuf, strlen(cbuf));
			engaged = (GetDockStatus (oapiGetDockHandle (hObj, remoteport)) != NULL);
		} else 
			TextOut (hDC, 0, 10, "NO DATA", 7);
	}
	if (engaged) {
		SetTextColor (hDC, 0);
		SetBkColor (hDC, RGB(255,255,0));
		SetBkMode (hDC, OPAQUE);
		TextOut (hDC, 50, 0, "ENG", 3);
	}
	oapiReleaseDC (surf, hDC);
};

void Dragonfly::RedrawPanel_CGIndicator (SURFHANDLE surf)
{
	char cbuf[20];
	HDC hDC = oapiGetDC (surf);
//	SelectObject (hDC, g_Param.font[1]);
	SetTextColor (hDC, RGB(0,255,0));
	SetBkMode (hDC, TRANSPARENT);
	sprintf (cbuf, "%0.1f m", cgofs);
	TextOut (hDC, 30, 0, cbuf, strlen (cbuf));
	int loc = 4+min ((int)(cgofs*3.784), 74);
//	SelectObject (hDC, g_Param.pen[0]);
	MoveToEx (hDC, loc, 15, NULL); LineTo (hDC, loc-3, 22); LineTo (hDC, loc+3, 22); LineTo (hDC, loc, 15);
	oapiReleaseDC (surf, hDC);
};

void Dragonfly::LoadPanel(int id)
{ 
	Internals.PanelList[id].MakeYourBackground();
	oapiRegisterPanelBackground(Internals.PanelList[id].hBitmap,Internals.PanelList[id].ATT_mode,0xFFFFFF);
	oapiSetPanelNeighbours(Internals.PanelList[id].neighbours[0],Internals.PanelList[id].neighbours[1],Internals.PanelList[id].neighbours[2],Internals.PanelList[id].neighbours[3]);
	Internals.PanelList[id].RegisterYourInstruments();
    CurrentPANEL=id;
}

void Dragonfly::PanelEvent(int id,int event,SURFHANDLE surf)
{
Internals.PanelList[CurrentPANEL].surf=surf;	//get the surface handle, since intruments will be using this
switch (event) {
	case PANEL_REDRAW_INIT:
		Internals.PanelList[CurrentPANEL].Paint(id);
		break;
	case PANEL_REDRAW_ALWAYS:
		Internals.PanelList[CurrentPANEL].Refresh(id);
		break;
	case PANEL_REDRAW_USER:
		Internals.PanelList[CurrentPANEL].Paint(id);
		break;

};

};



void Dragonfly::MouseEvent(int id,int event,int mx,int my)
{ 
	switch (event)
	{
	  case PANEL_MOUSE_LBDOWN:
		Internals.PanelList[CurrentPANEL].LBD(id,mx,my);
		break;
	  case PANEL_MOUSE_RBDOWN:
		Internals.PanelList[CurrentPANEL].RBD(id,mx,my);
		break;
	  case PANEL_MOUSE_LBUP:
	  case PANEL_MOUSE_RBUP:
		Internals.PanelList[CurrentPANEL].BU(id);
		break;
	}

};

// ==============================================================
// MS-070214: Added VESSEL2 callback functions to replace the
// ovc* nonmember functions
// ==============================================================

// Set the capabilities of the vessel class
void Dragonfly::clbkSetClassCaps (FILEHANDLE cfg)
{
	SetClassCaps (cfg);
}

// Save status to scenario file
void Dragonfly::clbkSaveState (FILEHANDLE scn)
{
	Internals.Save (scn);
	VESSEL2::clbkSaveState (scn);
	SaveState (scn);
}

// Read status from scenario file
void Dragonfly::clbkLoadStateEx (FILEHANDLE scn, void *vs)
{
	Internals.Load(scn,vs);
	LoadState (scn, vs);
	//remember to change RCS status after loadin the RCS handles
	//setting wether it's lin/rot/disbl.
	SetAttitudeMode(Internals.Nav_mode_switch->pos+1);//?? needed ??	
	VERN_handle=Internals.Vern_mode_switch->pos;
	INTR_handle=Internals.Intr_mode_switch->pos;
}

void Dragonfly::clbkPostStep (double simt, double simdt, double mjd)
{
	VESSEL2::clbkPostStep (simt, simdt, mjd);
	Internals.Refresh (simt-Lsim);
	Lsim = simt;
}

void Dragonfly::clbkMFDMode (int mfd, int mode)
{
	if (mfd == 0) {
		int idx = Internals.MFDS[mfd]->idx; //repaint the MFD just in case
		oapiTriggerPanelRedrawArea(0,idx);
	}
}

void Dragonfly::clbkDockEvent (int dock, OBJHANDLE mate)
{
	int index = Internals.Dk[0]->idx;
	oapiTriggerPanelRedrawArea (0, index);
}

int Dragonfly::clbkConsumeBufferedKey (DWORD key, bool down, char *kstate)
{
	int idx, mode;
	if (!down) return 0; // only process keydown events

	if (KEYMOD_SHIFT (kstate)) {
	} else if (KEYMOD_CONTROL (kstate)) {
		switch (key) {
		case OAPI_KEY_DIVIDE:
		 	if (SetAttitudeMode (GetAttitudeMode() >= 1 ? 0 : 1))
				mode = Internals.Nav_mode_switch->pos = GetAttitudeMode()-1;
				idx = Internals.Nav_mode_switch->idx;
				oapiTriggerPanelRedrawArea (0, idx);
				
			return 1;
		}
	} else {
		switch (key) {
		case OAPI_KEY_DIVIDE:
			if (ToggleAttitudeMode()) {
				mode = Internals.Nav_mode_switch->pos = GetAttitudeMode()-1;
				idx = Internals.Nav_mode_switch->idx;
				oapiTriggerPanelRedrawArea (0, idx);

				return 1;
			}
		}//end switch
	}		
	return 0;
}

bool Dragonfly::clbkLoadPanel (int id)
{
	LoadPanel (id);
	return true;
}

bool Dragonfly::clbkPanelRedrawEvent (int id, int event, SURFHANDLE surf)
{
	PanelEvent (id, event, surf);
	return true;
}

bool Dragonfly::clbkPanelMouseEvent (int id, int event, int mx, int my)
{
	MouseEvent (id, event, mx, my);
	return true;
}

// ==============================================================
// API interface
// ==============================================================

// Initialisation
DLLCLBK VESSEL *ovcInit (OBJHANDLE hvessel, int flightmodel)
{	PANEL_InitGDIResources(hDLL);
	return new Dragonfly (hvessel, flightmodel);
}

// Cleanup
DLLCLBK void ovcExit (VESSEL *vessel)
{   PANEL_ReleaseGDIResources();
	if (vessel) delete (Dragonfly*)vessel;
}

BOOL WINAPI DllMain (HINSTANCE hModule,
					 DWORD ul_reason_for_call,
					 LPVOID lpReserved)
{
	switch (ul_reason_for_call) {
	case DLL_PROCESS_ATTACH:
	    hDLL = hModule;
		// allocate GDI resources
		
		PANEL_DLLAtach();
	//	hInstance=hDLL;
//		CreateGLWindow("no title", 200, 200, 16);

	//	PANEL_InitGDIResources(hModule);
	
	//	Internals.Init();
	//	Internals.MakePanels();

			break;
	case DLL_PROCESS_DETACH:
		// deallocate GDI resources
//		CloseGLWindow();
		break;
	}
	return TRUE;
}
