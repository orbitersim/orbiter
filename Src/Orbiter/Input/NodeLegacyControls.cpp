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

// /!\ Must match the order in the constructor
enum {
	ModKey,
	MainEngine,
	BankYaw,
	PitchVert,
	YawLat,
	CamPanel,
};

NodeLegacyControls::NodeLegacyControls(int id) : Node("Legacy Controller", id)
{
	AddInput("ModKey", Pin::type::Button);
	AddInput("Main engine", Pin::type::Axis);
	AddInput("Bank/Yaw", Pin::type::Axis);
	AddInput("Pitch/Vertical translation", Pin::type::Axis);
	AddInput("Yaw/Lateral translation", Pin::type::Axis);
	AddInput("Camera/Panel", Pin::type::Hat);
}

void NodeLegacyControls::Update(bool dryrun)
{
	if(dryrun) return;

	Config *pConfig = g_pOrbiter->Cfg();

	uint8_t hat = inputs[CamPanel].hVal;
	if (hat) {
		if (g_camera->IsExternal()) {  // use the joystick's coolie hat to rotate external camera
			if (inputs[ModKey].bVal) { // shift instrument panel
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
			if (inputs[ModKey].bVal) { // shift instrument panel
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
	ctrlJoystick[THGROUP_MAIN] = -inputs[MainEngine].fVal;
	
	float x = inputs[BankYaw].fVal;
	float y = inputs[PitchVert].fVal;
	float Rz = inputs[YawLat].fVal;
	bool alt = inputs[ModKey].bVal;

	if (x) {
		if (alt) { // emulate rudder control
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
