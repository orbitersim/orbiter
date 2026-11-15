#include "Graph.h"
#include "Node.h"
#include "Pin.h"
#include "NodeJoystick.h"
#include "NodeLegacyControls.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>
#include <set>
#include "OrbiterAPI.h"

static ImVec4 GetStateColor(const Pin *pin) {
    ImVec4 color = {0.5,0.5,0.5,1.0};
    switch(pin->ptype) {
        case Pin::type::Axis:
            if(pin->fVal > 0.0f) {
                color.x = 0.5f * (1.0f-pin->fVal) + 0.902 * pin->fVal;
                color.y = 0.5f * (1.0f-pin->fVal) + 0.624 * pin->fVal;
                color.z = 0.5f * (1.0f-pin->fVal) + 0.000 * pin->fVal;
            } else {
                color.x = 0.5f * (1.0f+pin->fVal) - 0.337 * pin->fVal;
                color.y = 0.5f * (1.0f+pin->fVal) - 0.706 * pin->fVal;
                color.z = 0.5f * (1.0f+pin->fVal) - 0.914 * pin->fVal;
            }
            break;
        case Pin::type::HalfAxis:
            color.x = 0.5f * (1.0f-pin->fVal) + 0.902 * pin->fVal;
            color.y = 0.5f * (1.0f-pin->fVal) + 0.624 * pin->fVal;
            color.z = 0.5f * (1.0f-pin->fVal) + 0.000 * pin->fVal;
            break;
        case Pin::type::Button:
            if(pin->bVal) {
                color = {0.902,0.624,0.000,1.0};
            }
            break;
        case Pin::type::Hat:
            if(pin->hVal) {
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
	for(Node *n: nodes) {
		delete n;
	}
}

void ControllerGraph::AddNode(Node *node) { 
    nodes.push_back(node);
    for(Pin &pin: node->inputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }
    for(Pin &pin: node->outputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }

	dirty = true;
}

void ControllerGraph::DeleteNode(Node *node) {

	const auto eraseLinks = [this](ed::PinId id) {
        links.erase(std::remove_if(links.begin(), 
                        links.end(),
                        [&](Link &x){return x.InputId == id || x.OutputId == id;}),
                        links.end());
	};
	
	for(Pin &pin: node->inputs) {
        assert(pins.count(pin.id.AsPointer()) == 1);
		eraseLinks(pin.id);
        pins.erase(pin.id.AsPointer());
    }
    for(Pin &pin: node->outputs) {
        assert(pins.count(pin.id.AsPointer()) == 1);
		eraseLinks(pin.id);
        pins.erase(pin.id.AsPointer());
    }

	auto it = std::find(nodes.begin(), nodes.end(), node);
	assert(it != nodes.end());
	nodes.erase(it);
	delete node;

	dirty = true;
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
    for (const Link& link : links) {
        ImVec4 color = GetStateColor(PinFromId(link.OutputId));
        ed::Link(link.Id, link.InputId, link.OutputId, color);
    }

    if (ed::BeginCreate()) {
        ed::PinId newnodepin;
        if(ed::QueryNewNode(&newnodepin)) {
             draggedPin = PinFromId(newnodepin);
        }

        ed::PinId startPinId, endPinId;
        if (ed::QueryNewLink(&startPinId, &endPinId)) {
            if(startPinId != ed::PinId::Invalid && endPinId != ed::PinId::Invalid) {
                Pin *ip = PinFromId(startPinId);
                Pin *op = PinFromId(endPinId);
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
					TopologicalSort();
                }
            }
        }
    } else {
        draggedPin = NULL;
    }
    ed::EndCreate(); // Wraps up object creation action handling.

    if (ed::BeginDelete()) {
        ed::LinkId linkId = 0;
        while (ed::QueryDeletedLink(&linkId)) {
            if (ed::AcceptDeletedItem()) {
                auto id = std::find_if(links.begin(), links.end(), [linkId](auto& link) { return link.Id == linkId; });
                if (id != links.end()) {
                    dirty = true;
                    links.erase(id);
                }
            }
        }
    }
    ed::EndDelete();


	static ed::LinkId contextLinkId;
    ImVec2 openPopupPosition = ImGui::GetMousePos();
    ed::Suspend();
    if (ed::ShowLinkContextMenu(&contextLinkId)) {
        ImGui::OpenPopup("Link Context Menu");
    }

    if (ImGui::BeginPopup("Link Context Menu"))
    {
        if (ImGui::MenuItem("Delete link")) {
            ed::DeleteLink(contextLinkId);
            dirty = true;
        }
        ImGui::EndPopup();
    }
	ed::Resume();
}

void ControllerGraph::Update(bool dryrun)
{
	if(dirty) TopologicalSort();

	for(Node *n: nodesSorted) {
		n->UpdateInputs();
		n->Update(dryrun);
	}
}

// Kahn's algorithm from https://en.wikipedia.org/wiki/Topological_sorting
void ControllerGraph::TopologicalSort()
{
retry:
    // For the algorithm we need the incoming/outgoing edges for each node
    // but we only know links between pins from the links container.
    // We temporarily reconstruct that information here because we only
    // need it when updating the graph and it's not used anywhere else.
    // We also update here the list of predecessors for each input Pin
    // since the information is handy.

    nodesSorted.clear();
    std::set<Node *> noincoming;

    struct NodeInfo {
        std::set<Node*> children;
        std::set<Node*> parents;
    };

    std::unordered_map<Node*, NodeInfo> nodeInfo;
	std::unordered_map<Pin*, Node*> pinOwners;

    for(Node *n: nodes) {
		for (Pin& i : n->inputs) {
			i.inputs.clear();
			pinOwners[&i] = n;
		}

		for (Pin& o : n->outputs) {
			pinOwners[&o] = n;
		}
	
	}
    for(const Link &l: links) {
        Pin *to = PinFromId(l.InputId);
        Pin *from = PinFromId(l.OutputId);
        Node *parent = pinOwners[from];
        Node *child = pinOwners[to];
        nodeInfo[parent].children.insert(child);
        nodeInfo[child].parents.insert(parent);
		to->inputs.emplace_back(from);
	}

	// Actual algorithm
	for(Node *n: nodes) {
        if(nodeInfo[n].parents.empty())
            noincoming.insert(n);
    }

    while(!noincoming.empty()) {
        const auto &n = noincoming.begin();
        Node *node = *n;
        noincoming.erase(n);
        nodesSorted.push_back(node);
        for(Node *child: nodeInfo[node].children) {
            nodeInfo[child].parents.erase(node);
            if(nodeInfo[child].parents.empty())
                noincoming.insert(child);
        }
    }

	// If a cycle was introduced by the last added link, remove it
	if(nodesSorted.size() != nodes.size()) {
        oapiAddNotification(OAPINOTIF_ERROR, "Cannot link back to an ancestor node");
		links.pop_back();
		goto retry;
    }
	dirty = false;
}
