/**
 * \file InputAPI.h
 * \brief Input handling API functions
*/

#ifndef __ORBITER_INPUTAPI_H
#define __ORBITER_INPUTAPI_H

#include "OrbiterAPI.h"
#include <SDL3/SDL.h>
#include <optional>

struct Keybind {
    SDL_Keycode key;
    SDL_Keymod mods;

    static const Keybind NONE;
};

struct AxisInput {
    const char* id;
    const char* name;
};

typedef size_t AxisInputId;

OAPIFUNC AxisInputId oapiRegisterAxisInput(AxisInput input);
OAPIFUNC void oapiUnregisterAxisInput(AxisInputId input);
OAPIFUNC Sint16 oapiPollAxisInput(AxisInputId input);
OAPIFUNC const AxisInput* oapiGetAxisInputById(AxisInputId id);

struct ButtonInput {
    const char* id;
    const char* name;
    Keybind defaultBinding;
};

typedef size_t ButtonInputId;

OAPIFUNC ButtonInputId oapiRegisterButtonInput(ButtonInput input);
OAPIFUNC void oapiUnregisterButtonInput(ButtonInputId input);
OAPIFUNC bool oapiPollButtonInput(ButtonInputId input);
OAPIFUNC const ButtonInput* oapiGetButtonInputById(ButtonInputId id);

struct BuiltinInputs {
    AxisInputId CockpitCamRotateX;
    AxisInputId CockpitCamRotateY;

    AxisInputId CockpitCamLeanZ;
    AxisInputId CockpitCamLeanX;

    ButtonInputId CockpitResetCam;

    ButtonInputId PanelShiftLeft;
    ButtonInputId PanelShiftRight;
    ButtonInputId PanelShiftUp;
    ButtonInputId PanelShiftDown;
    ButtonInputId PanelSwitchLeft;
    ButtonInputId PanelSwitchRight;
    ButtonInputId PanelSwitchUp;
    ButtonInputId PanelSwitchDown;

    AxisInputId TrackCamRotateX;
    AxisInputId TrackCamRotateY;
    AxisInputId TrackCamZ;

    AxisInputId GroundCamTiltX;
    AxisInputId GroundCamTiltY;

    AxisInputId MainThrust;
    ButtonInputId KillMainRetroThrust;
    ButtonInputId OverrideFullMainThrust;
    ButtonInputId OverrideFullRetroThrust;

    AxisInputId HoverThrust;

    ButtonInputId RCSEnable;
    ButtonInputId RCSMode;

    AxisInputId RCSPitch;
    AxisInputId RCSYaw;
    AxisInputId RCSBank;
    AxisInputId RCSPitchX;
    AxisInputId RCSPitchY;
    AxisInputId RCSPitchZ;

    AxisInputId LPRCSPitch;
    AxisInputId LPRCSYaw;
    AxisInputId LPRCSBank;
    AxisInputId LPRCSPitchX;
    AxisInputId LPRCSPitchY;
    AxisInputId LPRCSPitchZ;

    ButtonInputId NMHoldAltitude;
    ButtonInputId NMHLevel;
    ButtonInputId NMPrograde;
    ButtonInputId NMRetrograde;
    ButtonInputId NMNormal;
    ButtonInputId NMAntinormal;
    ButtonInputId NMKillrot;

    ButtonInputId Undock;

    AxisInputId ElevatorTrim;
    AxisInputId WheelbrakeLeft;
    AxisInputId WheelbrakeRight;

    ButtonInputId HUD;
    ButtonInputId HUDMode;
    ButtonInputId HUDReference;
    ButtonInputId HUDTarget;
    ButtonInputId HUDColour;

    ButtonInputId IncSimSpeed;
    ButtonInputId DecSimSpeed;

    AxisInputId FOV;
    ButtonInputId StepIncFOV;
    ButtonInputId StepDecFOV;

    ButtonInputId MainMenu;
    ButtonInputId DlgHelp;
    ButtonInputId DlgCamera;
    ButtonInputId DlgSimspeed;
    ButtonInputId DlgCustomCmd;
    ButtonInputId DlgVisualHelpers;
    ButtonInputId DlgRecorder;
    ButtonInputId DlgInfo;
    ButtonInputId DlgMap;
    ButtonInputId ToggleCamInternal;
    ButtonInputId ToggleTrackMode;
    ButtonInputId TogglePanelMode;
    ButtonInputId TogglePlanetarium;
    ButtonInputId ToggleSurfaceLabels;
    ButtonInputId ToggleRecPlay;
    ButtonInputId Pause;
    ButtonInputId Quicksave;
    ButtonInputId Quit;
    ButtonInputId DlgSelectVessel;
    ButtonInputId SelectPrevVessel;
    ButtonInputId DlgCapture;
    ButtonInputId DlgOptions;

    static const BuiltinInputs* Get();
private:
    static BuiltinInputs* INSTANCE;
    BuiltinInputs();
};

#endif //__ORBITER_INPUTAPI_H
