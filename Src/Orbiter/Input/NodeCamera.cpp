#include "NodeCamera.h"
#include "Camera.h"
#include "TimeData.h"
#include "Orbiter.h"
#include "Vessel.h"

extern Camera *g_camera;
extern TimeData td;
extern Orbiter *g_pOrbiter;
extern Vessel *g_focusobj;

void NodeCamera::RegisterPins()
{
    RegisterPinWithDefault("Select", select, true);
    RegisterPin("Hat", hat);
    RegisterPin("Reset", reset);
    RegisterPin("RotX", rotX);
    RegisterPin("RotY", rotY);
    RegisterPin("Toggle internal/external", toggleView);
}

void NodeCamera::UpdateOutputs(bool dryrun)
{
    if(!select) return;

    float rX = rotX;
    float rY = rotY;

    if(hat & Pin::hat::DOWN)  rY += 1.0;
    if(hat & Pin::hat::UP)    rY -= 1.0;
    if(hat & Pin::hat::RIGHT) rX += 1.0;
    if(hat & Pin::hat::LEFT)  rX -= 1.0;

    if (g_camera->IsExternal()) {
        g_camera->AddPhi   (td.SysDT * rX);
        g_camera->AddTheta (td.SysDT * rY);
    } else {
        if(reset) {
            g_camera->ResetCockpitDir();
        } else {
            g_camera->Rotate (-td.SysDT * rX,  -td.SysDT * rY, true);
        }
    }
    if(toggleView) g_pOrbiter->SetView(g_focusobj, !g_camera->IsExternal());
}
