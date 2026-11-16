#include "Graph.h"
#include "Node.h"
#include "Pin.h"
#include "NodeJoystick.h"
#include "NodeLegacyControls.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>
#include <set>
#include <algorithm>
#include <unordered_set>
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
}

void ControllerGraph::AddNode(std::unique_ptr<Node> node) {
    for(Pin &pin: node->inputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }
    for(Pin &pin: node->outputs) {
        assert(pins.count(pin.id.AsPointer()) == 0);
        pins[pin.id.AsPointer()] = &pin;
    }
    nodes.emplace_back(std::move(node));

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

	auto it = std::find_if(nodes.begin(), nodes.end(), [node](const std::unique_ptr<Node>& p) { return p.get() == node; });
	assert(it != nodes.end());

	nodes.erase(it);

	dirty = true;
}

// TODO: profile this thing when we have more nodes to play with
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

	// Duplicate link
    if (std::any_of(links.begin(), links.end(),
        [&](const auto& link) {
            return link.InputId == in->id && link.OutputId == out->id;
        }))
        return false;

	// Check if adding this link would create a cycle
	// i.e. if the input node can already reach the output node.
	// Adapted from https://en.wikipedia.org/wiki/Depth-first_search
    std::vector<Node*> pending = { in->parent };
    std::unordered_set<Node*> visited;

    while (!pending.empty()) {
        Node *node = pending.back();
        pending.pop_back();

        if (node == out->parent) return false;
        if (!visited.insert(node).second) continue;

        for (const auto& link : links) {
            Pin *from = PinFromId(link.OutputId);
            if (from->parent != node) continue;

            Pin *to = PinFromId(link.InputId);
            pending.push_back(to->parent);
        }
    }

    return true;
}

void ControllerGraph::Draw()
{
    const auto filter = [this](const Pin *pin) {
        if(!draggedPin) return true;
        return CanLink(draggedPin, pin);
    };
    for(auto &n: nodes) {
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
	static ed::PinId contextPinId;

    ImVec2 openPopupPosition = ImGui::GetMousePos();
    ed::Suspend();
    if (ed::ShowPinContextMenu(&contextPinId)) {
        Pin *pin = PinFromId(contextPinId);
        if (pin->cbContextMenu) {
            ImGui::OpenPopup("Pin Context Menu");
        }
    } else if (ed::ShowLinkContextMenu(&contextLinkId)) {
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
    if (ImGui::BeginPopup("Pin Context Menu"))
    {
        Pin *pin = PinFromId(contextPinId);

        if (pin->cbContextMenu) pin->cbContextMenu(pin);

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

    for(auto &n: nodes) {
		for (Pin& i : n->inputs) {
			i.inputs.clear();
			pinOwners[&i] = n.get();
		}

		for (Pin& o : n->outputs) {
			pinOwners[&o] = n.get();
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
	for(auto &n: nodes) {
        if(nodeInfo[n.get()].parents.empty())
            noincoming.insert(n.get());
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

	// This should not happen but just in case...
	if(nodesSorted.size() != nodes.size()) {
        oapiAddNotification(OAPINOTIF_ERROR, "Cycle detected in controller graph, removing last link");
		links.pop_back();
		goto retry;
    }
	dirty = false;
}
