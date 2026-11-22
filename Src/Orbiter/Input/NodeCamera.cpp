#include "NodeCamera.h"
#include "Camera.h"
#include "TimeData.h"

extern Camera *g_camera;
extern TimeData td;

void NodeCamera::RegisterPins()
{
    RegisterPinWithDefault("Enable", enable, true);
    RegisterPin("Hat", hat);
    RegisterPin("Reset", reset);
    RegisterPin("RotX", rotX);
    RegisterPin("RotY", rotY);
}

void NodeCamera::UpdateOutputs(bool dryrun)
{
    if(!enable) return;

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
}
