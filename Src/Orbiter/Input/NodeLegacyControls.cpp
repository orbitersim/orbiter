#include "NodeLegacyControls.h"

#include "Camera.h"
#include "Pane.h"
#include "TimeData.h"

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
    RegisterPinWithDefault("Select", select, true);
    RegisterPin("ModKey", ModKey);
    RegisterPin("Main engine", MainEngine);
    RegisterPin("Bank/Yaw", BankYaw);
    RegisterPin("Pitch/Vertical translation", PitchVert);
    RegisterPin("Yaw/Lateral translation", YawLat);
    RegisterPin("Camera/Panel", CamPanel);
}

void NodeLegacyControls::UpdateOutputs(bool dryrun)
{
    if(dryrun || !select) return;

    Config *pConfig = g_pOrbiter->Cfg();

    Pin::hat hat = CamPanel;
    if (hat != Pin::hat::CENTERED) {
        if (g_camera->IsExternal()) {  // use the joystick's coolie hat to rotate external camera
            if (ModKey) { // shift instrument panel
                if      (hat & Pin::hat::UP)    g_camera->Rotate (0,  td.SysDT);
                else if (hat & Pin::hat::DOWN)  g_camera->Rotate (0, -td.SysDT);
                if      (hat & Pin::hat::RIGHT) g_camera->Rotate (-td.SysDT, 0);
                else if (hat & Pin::hat::LEFT)  g_camera->Rotate ( td.SysDT, 0);
            } else {
                if      (hat & Pin::hat::UP)    g_camera->AddTheta (-td.SysDT);
                else if (hat & Pin::hat::DOWN)  g_camera->AddTheta ( td.SysDT);
                if      (hat & Pin::hat::RIGHT) g_camera->AddPhi   ( td.SysDT);
                else if (hat & Pin::hat::LEFT)  g_camera->AddPhi   (-td.SysDT);
            }
        } else { // internal view
            if (ModKey) { // shift instrument panel
                if      (hat & Pin::hat::UP)    g_pane->ShiftPanel (0.0,  td.SysDT * pConfig->CfgLogicPrm.PanelScrollSpeed);
                else if (hat & Pin::hat::DOWN)  g_pane->ShiftPanel (0.0, -td.SysDT * pConfig->CfgLogicPrm.PanelScrollSpeed);
                if      (hat & Pin::hat::RIGHT) g_pane->ShiftPanel (-td.SysDT * pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
                else if (hat & Pin::hat::LEFT)  g_pane->ShiftPanel ( td.SysDT * pConfig->CfgLogicPrm.PanelScrollSpeed, 0.0);
            } else {                 // rotate camera
                if      (hat & Pin::hat::UP)    g_camera->Rotate (0,  td.SysDT, true);
                else if (hat & Pin::hat::DOWN)  g_camera->Rotate (0, -td.SysDT, true);
                if      (hat & Pin::hat::RIGHT) g_camera->Rotate (-td.SysDT, 0, true);
                else if (hat & Pin::hat::LEFT)  g_camera->Rotate ( td.SysDT, 0, true);
            }
        }
    }
}

void NodeLegacyControls::AddJoystickCtrl(float ctrlJoystick[15])
{
    if(!select) return;

    ctrlJoystick[THGROUP_MAIN] = -MainEngine;
    
    float x = BankYaw;
    float y = PitchVert;
    float Rz = YawLat;

    if (x) {
        if (ModKey) { // emulate rudder control
            if (x > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT]  += x;
            else       ctrlJoystick[THGROUP_ATT_YAWLEFT]   -= x;
        } else {      // rotation (bank)
            if (x > 0) ctrlJoystick[THGROUP_ATT_BANKRIGHT] += x;
            else       ctrlJoystick[THGROUP_ATT_BANKLEFT]  -= x;
        }
    }
    if (y) {          // rotation (pitch) or translation (vertical)
        if (y > 0)  ctrlJoystick[THGROUP_ATT_PITCHUP]   = (ctrlJoystick[THGROUP_ATT_UP]    += y);
        else        ctrlJoystick[THGROUP_ATT_PITCHDOWN] = (ctrlJoystick[THGROUP_ATT_DOWN]  -= y);
    }
    if (Rz) {         // rotation (yaw) or translation (transversal)
        if (Rz > 0) ctrlJoystick[THGROUP_ATT_YAWRIGHT]  = (ctrlJoystick[THGROUP_ATT_RIGHT] += Rz);
        else        ctrlJoystick[THGROUP_ATT_YAWLEFT]   = (ctrlJoystick[THGROUP_ATT_LEFT]  -= Rz);
    }
}
