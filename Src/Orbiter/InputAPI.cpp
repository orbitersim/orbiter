#include "InputAPI.h"
#include "OrbiterAPI.h"

#include <cassert>
#define TODO for (;;) { assert(false); }

DLLEXPORT AxisInputId oapiRegisterAxisInput(AxisInput input) {
    TODO
}

DLLEXPORT void oapiUnregisterAxisInput(AxisInputId input) {
    TODO
}

DLLEXPORT Sint16 oapiPollAxisInput(AxisInputId input) {
   TODO
}

DLLEXPORT const AxisInput* oapiGetAxisInputById(AxisInputId input) {
    TODO
}

DLLEXPORT ButtonInputId oapiRegisterButtonInput(ButtonInput input) {
    TODO
}

DLLEXPORT void oapiUnregisterButtonInput(ButtonInputId input) {
    TODO
}

DLLEXPORT bool oapiPollButtonInput(ButtonInputId input) {
    TODO
}

DLLEXPORT const ButtonInput* oapiGetButtonInputById(ButtonInputId input) {
    TODO
}