#include "Controller.h"
#include "OrbiterSDK.h"
#include "Orbiter.h"
#include "Config.h"
#include "Graph.h"
#include "NodeLegacyControls.h"
#include "NodeJoystick.h"

extern Orbiter* g_pOrbiter;

void InputController::GlobalInit()
{
	ControllerGraph::GlobalInit();
	graph = new ControllerGraph();
	auto node = std::make_unique<NodeLegacyControls>();
	node->InitDefault(graph->nodeidgen++);
	graph->AddNode(std::move(node));
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

void InputController::DetachControllers()
{
	joysticks.clear();
}