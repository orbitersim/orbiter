#include "Graph.h"
#include "Node.h"
#include "Pin.h"

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

void ControllerGraph::Draw()
{
	for(Node *n: nodes) {
		n->Draw();
	}
    for (const Link& link : links)
    {
        ImVec4 color = GetStateColor(PinFromId(link.InputId));
        ed::Link(link.Id, link.InputId, link.OutputId, color);
    }

	if (ed::BeginCreate())
    {
        ed::PinId startPinId, endPinId;
        if (ed::QueryNewLink(&startPinId, &endPinId))
        {
            if(startPinId != ed::PinId::Invalid && endPinId != ed::PinId::Invalid) {
                auto ip = &PinFromId(startPinId);
                auto op = &PinFromId(endPinId);

				ed::PinId inputPinId = startPinId;
				ed::PinId outputPinId = endPinId;
                if (ip->pkind != Pin::kind::Input) {
                    std::swap(ip, op);
                    std::swap(inputPinId, outputPinId);
                }

                if(ed::AcceptNewItem(ImColor(128, 255, 128), 2.0f)) {
                       links.push_back({ ed::LinkId(linkidgen++), inputPinId, outputPinId });
                        ed::Link(links.back().Id, links.back().InputId, links.back().OutputId);
                }
            }
        }
    }

    ed::EndCreate(); // Wraps up object creation action handling.



}
