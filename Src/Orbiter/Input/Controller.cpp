#include "Controller.h"
#include "Orbiter.h"
#include "Config.h"
#include "Graph.h"
#include "NodeCamera.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

static_assert(std::is_same_v<SDL_JoystickID, uint32_t>, "SDL_JoystickID is no longer uint32_t");

extern Orbiter* g_pOrbiter;

static void OpenDlgClbk(void *context)
{
    ControllerEditor *ed = (ControllerEditor *)context;
    oapiOpenDialog(ed);
}

void ControllerEditor::OnDraw()
{
    ControllerGraph *graph = InputController::GetGraph();
    graph->DrawEditor();
}

void InputController::GlobalInit()
{
    editor = std::make_unique<ControllerEditor>();
    ControllerGraph::GlobalInit();
    graph = new ControllerGraph();

    m_cmd = oapiRegisterCustomCmd((char*)"Joystick configuration", (char*)"Advanced joystick configuration", OpenDlgClbk, editor.get());

}

void InputController::GlobalExit()
{
    editor.reset();
    delete graph;
}

void InputController::GetJoystickCtrl(float ctrl[15])
{
    SDL_UpdateJoysticks();
    ScanControllers();
    graph->Update(false);
    graph->GetJoystickCtrl(ctrl);
}

// We scan ourselves for now because we still use the Windows message loop instead of SDL event polling
void InputController::ScanControllers()
{
    int count = 0;
    SDL_JoystickID* ids = SDL_GetJoysticks(&count);

    if (!ids)
        return;

    // Check if a known controller has been removed
    for (auto it = joysticks.begin(); it != joysticks.end(); ) {
        if (std::find(ids, ids + count, it->first) == ids + count) {
            graph->DetachJoystick(it->second.get());
            it = joysticks.erase(it);
        } else {
            ++it;
        }
    }

    // Search for new controllers
    for (int i = 0; i < count; ++i) {
        SDL_JoystickID id = ids[i];

        if (joysticks.find(id) != joysticks.end())
            continue;

        auto js = std::make_unique<Joystick>(id);
        if (!js->IsConnected())
            continue;

        Joystick *ptr = js.get();
        joysticks.emplace(id, std::move(js));
        graph->AttachJoystick(ptr);
    }

    SDL_free(ids);
}
