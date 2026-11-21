#include "NodeLegacyControls.h"

#include "Camera.h"
#include "Pane.h"
#include "TimeData.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

extern Camera *g_camera;
extern Pane *g_pane;
extern TimeData td;
extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;

NodeLegacyControls::NodeLegacyControls() : Node("Legacy Controller")
{
}

void NodeLegacyControls::RegisterPins()
{
	RegisterPin("ModKey", ModKey);
	RegisterPin("Main engine", MainEngine);
	RegisterPin("Bank/Yaw", BankYaw);
	RegisterPin("Pitch/Vertical translation", PitchVert);
	RegisterPin("Yaw/Lateral translation", YawLat);
	RegisterPin("Camera/Panel", CamPanel);
}

void NodeLegacyControls::Update(bool dryrun)
{
	if(dryrun) return;

	Config *pConfig = g_pOrbiter->Cfg();

	int hat = CamPanel;
	if (hat) {
		if (g_camera->IsExternal()) {  // use the joystick's coolie hat to rotate external camera
			if (ModKey) { // shift instrument panel
				if      (hat & SDL_HAT_UP)    g_camera->Rotate (0,  td.SysDT);
				else if (hat & SDL_HAT_DOWN)  g_camera->Rotate (0, -td.SysDT);
				if      (hat & SDL_HAT_RIGHT) g_camera->Rotate (-td.SysDT, 0);
				else if (hat & SDL_HAT_LEFT)  g_camera->Rotate ( td.SysDT, 0);
			} else {
				if      (hat & SDL_HAT_UP)    g_camera->AddTheta (-td.SysDT);
				else if (hat & SDL_HAT_DOWN)  g_camera->AddTheta ( td.SysDT);
				if      (hat & SDL_HAT_RIGHT) g_camera->AddPhi   ( td.SysDT);
				else if (hat & SDL_HAT_LEFT)  g_camera->AddPhi   (-td.SysDT);
			}
		} else { // internal view
			if (ModKey) { // shift instrument panel
				if      (hat & SDL_HAT_UP)    g_pane->ShiftPanel (0.0,  td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
				else if (hat & SDL_HAT_DOWN)  g_pane->ShiftPanel (0.0, -td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed);
				if      (hat & SDL_HAT_RIGHT) g_pane->ShiftPanel (-td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
				else if (hat & SDL_HAT_LEFT)  g_pane->ShiftPanel ( td.SysDT*pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
			} else {                 // rotate camera
				if      (hat & SDL_HAT_UP)    g_camera->Rotate (0,  td.SysDT, true);
				else if (hat & SDL_HAT_DOWN)  g_camera->Rotate (0, -td.SysDT, true);
				if      (hat & SDL_HAT_RIGHT) g_camera->Rotate (-td.SysDT, 0, true);
				else if (hat & SDL_HAT_LEFT)  g_camera->Rotate ( td.SysDT, 0, true);
			}
		}
	}
}

void NodeLegacyControls::GetJoystickCtrl(float ctrlJoystick[15])
{
	ctrlJoystick[THGROUP_MAIN] = -MainEngine;
	
	float x = BankYaw;
	float y = PitchVert;
	float Rz = YawLat;

	if (x) {
		if (ModKey) { // emulate rudder control
			if (x > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT] =   x;
			else            ctrlJoystick[THGROUP_ATT_YAWLEFT]  =  -x;
		} else {                 // rotation (bank)
			if (x > 0) ctrlJoystick[THGROUP_ATT_BANKRIGHT] =  x;
			else            ctrlJoystick[THGROUP_ATT_BANKLEFT]  = -x;
		}
	}
	if (y) {                // rotation (pitch) or translation (vertical)
		if (y > 0) ctrlJoystick[THGROUP_ATT_PITCHUP]   = ctrlJoystick[THGROUP_ATT_UP]    =  y;
		else            ctrlJoystick[THGROUP_ATT_PITCHDOWN] = ctrlJoystick[THGROUP_ATT_DOWN]  = -y;
	}
	if (Rz) {               // rotation (yaw) or translation (transversal)
		if (Rz > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT] = ctrlJoystick[THGROUP_ATT_RIGHT] =  Rz;
		else             ctrlJoystick[THGROUP_ATT_YAWLEFT]  = ctrlJoystick[THGROUP_ATT_LEFT]  = -Rz;
	}
}
