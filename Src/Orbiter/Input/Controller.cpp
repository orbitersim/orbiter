#include "Controller.h"
#include "OrbiterSDK.h"
#include "Orbiter.h"
#include "Config.h"
#include "Graph.h"
#include "NodeJoystick.h"
#include "NodeLegacyControls.h"

extern Orbiter* g_pOrbiter;

void InputController::GlobalInit()
{
	graph = new ControllerGraph();
	legacyCtrl = graph->AddNode<NodeLegacyControls>();
}

void InputController::GetJoystickCtrl(float ctrl[15])
{
	SDL_UpdateJoysticks();
	ScanControllers();
	graph->Update(false);
	legacyCtrl->GetJoystickCtrl(ctrl);
}

void InputController::GetInputs(float *x, float *y, float *Rz, int *hat, float *throttle, int *alt)
{
	auto normalize = [](int v) {
		return  v < 0 ? v / 32768.0f : v / 32767.0f;
	};

	*x = normalize(state.x);
	*y = normalize(state.y);
	*Rz = normalize(state.Rz);
	*hat = state.hat;
	*throttle = normalize(state.throttle);
	*alt = state.alt;
}

void InputController::ScanControllers()
{
	int count;
	SDL_JoystickID *ids = SDL_GetJoysticks(&count);
	// Search for new controllers
	for (int i = 0; i < count; ++i) {
		SDL_GUID guid = SDL_GetJoystickGUIDForID(ids[i]);
		std::string key(reinterpret_cast<const char*>(guid.data), sizeof(guid.data));
		auto it = joyMap.find(key);
		if(it == joyMap.end()) {
			oapiAddNotification(OAPINOTIF_INFO, "New joystick connected", SDL_GetJoystickNameForID(ids[i]));
			joyMap[key] = graph->AddNode<NodeJoystick>(ids[i]);
		} else {
			NodeJoystick *js = it->second;
			if(!js->IsConnected()) {
				oapiAddNotification(OAPINOTIF_INFO, "Joystick reconnected", SDL_GetJoystickNameForID(ids[i]));
				if(!js->Attach(ids[i])) {
					// If this fails for whatever reason, delete the node and create a new one for this GUID
					graph->DeleteNode(js);
					joyMap[key] = graph->AddNode<NodeJoystick>(ids[i]);
				}
			}
		}
	}

	// Check if a known controller has been removed
	for(auto &[k, node]: joyMap) {
		if(node->IsAttached() && !node->IsConnected()) {
			oapiAddNotification(OAPINOTIF_INFO, "Joystick disconnected", node->name.c_str());
			node->Detach();
		}
	}
	SDL_free(ids);
}