#include "Graph.h"
#include "Node.h"
#include "Pin.h"
#include "NodeJoystick.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>

ImVec4 GetStateColor(const Pin &pin) {
    ImVec4 color = {0.5,0.5,0.5,1.0};
    switch(pin.ptype) {
        case Pin::type::Axis:
            if(pin.fVal > 0.0f) {
                color.x = 0.5f * (1.0f-pin.fVal) + 0.902 * pin.fVal;
                color.y = 0.5f * (1.0f-pin.fVal) + 0.624 * pin.fVal;
                color.z = 0.5f * (1.0f-pin.fVal) + 0.000 * pin.fVal;
            } else {
                color.x = 0.5f * (1.0f+pin.fVal) - 0.337 * pin.fVal;
                color.y = 0.5f * (1.0f+pin.fVal) - 0.706 * pin.fVal;
                color.z = 0.5f * (1.0f+pin.fVal) - 0.914 * pin.fVal;
            }
            break;
        case Pin::type::HalfAxis:
            color.x = 0.5f * (1.0f-pin.fVal) + 0.902 * pin.fVal;
            color.y = 0.5f * (1.0f-pin.fVal) + 0.624 * pin.fVal;
            color.z = 0.5f * (1.0f-pin.fVal) + 0.000 * pin.fVal;
            break;
        case Pin::type::Button:
            if(pin.bVal) {
                color = {0.902,0.624,0.000,1.0};
            }
            break;
        case Pin::type::Hat:
            if(pin.hVal) {
                color = {0.902,0.624,0.000,1.0};
            }
            break;
    }
    return color;
}

ControllerGraph::ControllerGraph()
{
	int count = 0;
	SDL_JoystickID* ids = SDL_GetJoysticks(&count);
	
	for (int i = 0; i < count; ++i) {
		Node *n = new NodeJoystick(ids[i], i);
		AddNode(n);
	}

	SDL_free(ids);
}

ControllerGraph::~ControllerGraph()
{
}
void ControllerGraph::AddNode(Node *node) { 
    nodes.push_back(node);
    for(Pin &pin:node->inputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }
    for(Pin &pin:node->outputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }
}

bool ControllerGraph::CanLink(const Pin *a, const Pin *b) {
    const Pin *in = a;
    const Pin *out = b;

    if(a->parent == b->parent) return false;

    if(a->pkind == Pin::kind::Output) {
        in = b;
        out = a;
    }
    if(in->pkind == out->pkind) return false;

    if(in->ptype != out->ptype) return false;

    return true;
}

void ControllerGraph::Draw()
{
    const auto filter = [this](const Pin *pin) {
        if(!draggedPin) return true;
        return CanLink(draggedPin, pin);
    };
    for(Node *n: nodes) {
        n->Draw(filter);
    }
    for (const Link& link : links)
    {
        ImVec4 color = GetStateColor(PinFromId(link.InputId));
        ed::Link(link.Id, link.InputId, link.OutputId, color);
    }

    if (ed::BeginCreate())
    {
        ed::PinId startPinId, endPinId;

        ed::PinId newnodepin;
        if(ed::QueryNewNode(&newnodepin)) {
             draggedPin = &PinFromId(newnodepin);
        }

        if (ed::QueryNewLink(&startPinId, &endPinId))
        {
            if(startPinId != ed::PinId::Invalid && endPinId != ed::PinId::Invalid) {
                Pin *ip = &PinFromId(startPinId);
                Pin *op = &PinFromId(endPinId);
                // We need startPinId/ip to be the input pin
                if(ip->pkind != Pin::kind::Input) {
                    std::swap(ip, op);
                    std::swap(startPinId, endPinId);
                }

                if(!CanLink(ip, op)) {
                    ed::RejectNewItem(ImColor(255, 0, 0), 2.0f);
                } else if(ed::AcceptNewItem(ImColor(128, 255, 128), 2.0f)) {
                       links.push_back({ ed::LinkId(linkidgen++), startPinId, endPinId });
                        ed::Link(links.back().Id, links.back().InputId, links.back().OutputId);
                }
            }
        }
    } else {
        draggedPin = NULL;
    }
    ed::EndCreate(); // Wraps up object creation action handling.
}

void ControllerGraph::Update(bool dryrun)
{
	for(Node *n: nodes) {
		n->Update(dryrun);
	}
}