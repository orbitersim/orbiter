#pragma once

#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

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
    static inline std::unordered_map<SDL_JoystickID, std::unique_ptr<Joystick>> joysticks;
    static inline std::unique_ptr<ControllerEditor> editor;
public:
    static void GlobalInit();
    static void GlobalExit();
    static void GetJoystickCtrl(float ctrl[15]);
    static ControllerGraph *GetGraph() { return graph; }
    static void ScanControllers();
};
