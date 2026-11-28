#pragma once

#include <string>
#include <unordered_map>
#include <memory>
#include "Joystick.h"
#include <windows.h>
#include "OrbiterSDK.h"

class ControllerGraph;
class NodeLegacyControls;

class ControllerEditor : public ImGuiDialog
{
public:
    ControllerEditor():ImGuiDialog("Joystick Editor") {}
    void OnDraw() override;
};

class InputController {
    static inline ControllerGraph *graph;
    static inline struct {
        float x,y,Rz,throttle;
        int alt, hat;
    } state;
    static inline DWORD m_cmd;
    // The key should be an SDL_JoystickID but we don't want to include SDL headers just for that
    // There's a static_assert in the .cpp file to detect if SDL_JoystickID changes type.
    static inline std::unordered_map<uint32_t, std::unique_ptr<Joystick>> joysticks;
    static inline std::unique_ptr<ControllerEditor> editor;
public:
    static void GlobalInit();
    static void GlobalExit();
    static void GetJoystickCtrl(float ctrl[15]);
    static ControllerGraph *GetGraph() { return graph; }
    static void ScanControllers();
};
