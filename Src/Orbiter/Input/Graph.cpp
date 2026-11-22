#include "Graph.h"
#include "Node.h"
#include "Pin.h"
#include "NodeJoystick.h"
#include "NodeCamera.h"
#include "NodeLegacyControls.h"
#include "Joystick.h"
#include <set>
#include <algorithm>
#include <unordered_set>
#include "OrbiterAPI.h"
#include "crude_json.h"
#include "Controller.h"
#include <IconsFontAwesome6.h>
#include "imgui_extras.h"

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
            if(pin->hVal != Pin::hat::CENTERED) {
                color = {0.902,0.624,0.000,1.0};
            }
            break;
    }
    return color;
}

ControllerGraph::ControllerGraph()
{
    ed::Config config;
    config.SettingsFile = NULL;
    m_Context = ed::CreateEditor(&config);

    ed::SetCurrentEditor(m_Context);
    Load("graph.json");

    for(auto &node: nodes) {
    }
}

ControllerGraph::~ControllerGraph()
{
    ed::DestroyEditor(m_Context);
}

void ControllerGraph::AddNode(std::unique_ptr<Node> node) {
    assert(node->initialized);
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

    // Button press/released can only be hooks to a Button source
    bool trigger = ((in->ptype == Pin::type::ButtonPressed || in->ptype == Pin::type::ButtonReleased) && out->ptype == Pin::type::Button);

    if(!trigger && in->ptype != out->ptype) return false;

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

bool ControllerGraph::NodeHasRoom(ImVec2 pos, ImVec2 size)
{
    const float spacing = 10.0f;
    for(auto &node: nodes) {
        if(node->placed) {
            ImVec2 otherPos = ed::GetNodePosition(node->id);
            ImVec2 otherSize = ed::GetNodeSize(node->id);

            if (pos.x < otherPos.x + otherSize.x + spacing &&
                pos.x + size.x + spacing > otherPos.x &&
                pos.y < otherPos.y + otherSize.y + spacing &&
                pos.y + size.y + spacing > otherPos.y) {
                return false;
            }
        }
    }
    return true;
}

void ControllerGraph::DrawEditor()
{
    ed::SetCurrentEditor(m_Context);

    if(ImGui::Button("Save")) {
        Save("graph.json");
    }
    ImGui::SameLine();
    if(ImGui::Button("Reload")) {
        if(!Load("graph.json")) {
            oapiAddNotification(OAPINOTIF_ERROR, "Error loading file", "graph.json");
        }
    }
    ImGui::SameLine();
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::BeginAnimatedCombo("##joysticks", ICON_FA_GAMEPAD" Joysticks", ImGuiComboFlags_HeightLargest)) {
        for(auto &n: nodes) {
            if(dynamic_cast<NodeJoystick *>(n.get())) {
                if(ImGui::Checkbox(n->name.c_str(), &n->enabled)) {
                    // Don't take into account for free room checks
                    if(!n->enabled) n->placed = false;
                }
            }
        }
        ImGui::EndAnimatedCombo();
    }
    ImGui::SameLine();
	ImGui::SetNextItemWidth(160.0f);
	if(ImGui::BeginAnimatedCombo("##actions", ICON_FA_SLIDERS" Actions", ImGuiComboFlags_HeightLargest)) {
        for(auto &n: nodes) {
            if(n->IsSingleton()) {
                if(ImGui::Checkbox(n->name.c_str(), &n->enabled)) {
                    // Don't take into account for free room checks
                    if(!n->enabled) n->placed = false;
                }
            }
        }
        ImGui::EndAnimatedCombo();
    }

    ImGui::Separator();

    ed::Begin("My Editor", ImVec2(0.0, 0.0f));

    const auto filter = [this](const Pin *pin) {
        if(!draggedPin) return true;
        return CanLink(draggedPin, pin);
    };

    // By default, all nodes are created at {0,0}
    // In order not to have a bunch of nodes on top of each other
    // we check in a circular pattern from the barycenter of existing nodes
    // until we find enough room to place a new node.
    // It's not great but better than everything being piled up.
    bool allplaced = true;
    ImVec2 barycenter{0.0f,0.0f};
    int npoints = 0;
    for(auto &n: nodes) {
        if(n->enabled) {
            ImVec2 pos = ed::GetNodePosition(n->id);
            n->Draw(filter);
            if(n->placed) {
                npoints++;
                barycenter.x += pos.x;
                barycenter.y += pos.y;
            } else {
                allplaced = false;
            }
        }
    }

    if(!allplaced) {
        if(npoints) {
            barycenter.x /= npoints;
            barycenter.y /= npoints;
        }
        auto fibospiral = [](ImVec2 origin, int index) {
            float angle = index * 2.39996323f; // Golden angle
            float radius = 30.0f * sqrt(index);

            return ImVec2{
                origin.x + radius * cos(angle),
                origin.y + radius * sin(angle)
            };
        };

        for(auto &n: nodes) {
            if(!n->placed) {
                for(int i = 0;;i++) {
                    ImVec2 pos = fibospiral(barycenter, i);
                    ImVec2 sz = ed::GetNodeSize(n->id);
                    if(NodeHasRoom(pos, sz)) {
                        ed::SetNodePosition(n->id, pos);
                        n->placed = true;
                        break;
                    }
                }
            }
        }
        ed::NavigateToContent();
    }

    for (const Link& link : links) {
        Pin *pin = PinFromId(link.OutputId);
        if(!pin) {
            // Protection against invalid JSON
            ed::DeleteLink(link.Id);
        } else {
            ImVec4 color = GetStateColor(pin);
            ed::Link(link.Id, link.InputId, link.OutputId, color);
        }
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
    ed::End();

    ed::SetCurrentEditor(NULL);
}

void ControllerGraph::Update(bool dryrun)
{
    if(dirty) TopologicalSort();

    for(Node *n: nodesSorted) {
        if(n->enabled) {
            n->UpdateInputs();
            n->UpdateOutputs(dryrun);
        }
    }
}
void ControllerGraph::GetJoystickCtrl(float ctrl[15])
{
    // ctrl is cleared by Orbiter before calling this
    // We can just add every legacy controllers outputs and clamp the result
    for(Node *n: nodesSorted) {
        if(!n->enabled) continue;
        if(INodeLegacyController *lc = dynamic_cast<INodeLegacyController *>(n)) {
            lc->AddJoystickCtrl(ctrl);
        }
    }
    for(int i = 0; i < 15; i++) {
        ctrl[i] = std::clamp(ctrl[i], -1.0f, 1.0f);
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

        // Protects against JSON files containing links to pins no longer exposed by a Node
        if(!to || !from) {
            ed::DeleteLink(l.Id);
            continue;
        }

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

void ControllerGraph::AttachJoystick(Joystick *js)
{
    for (auto& node : nodes) {
        NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node.get());

        if (jsnode && !jsnode->IsAttached() && jsnode->CanAttach(js)) {
            jsnode->Attach(js);
            oapiAddNotification(OAPINOTIF_INFO, "Joystick reconnected", js->name.c_str());
            return;
        }
    }

    oapiAddNotification(OAPINOTIF_INFO, "New joystick connected", js->name.c_str());
    auto node = std::make_unique<NodeJoystick>();
    node->InitFromJoystick(js, nodeidgen++);
    AddNode(std::move(node));
}

void ControllerGraph::DetachJoystick(Joystick *js)
{
    for (auto& node : nodes) {
        NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node.get());

        if (jsnode && jsnode->GetJoystick() == js) {
            jsnode->Detach();
            oapiAddNotification(OAPINOTIF_INFO, "Joystick disconnected", js->name.c_str());
            return;
        }
    }
}
bool ControllerGraph::HasNodeOfType(const std::string &type) {
    for(auto &n: nodes) {
        if(type == n->GetTypeName()) {
            return true;
        }
    }
    return false;
}

void ControllerGraph::CreateSingletonNodes()
{
    for(auto &nodeinfo: nodeFactory.types) {
        NodeFactory::NodeInfo ni = nodeinfo.second;
        if(ni.isSingleton && ! HasNodeOfType(nodeinfo.first)) {
            Node *node = ni.maker();
            node->InitDefault(nodeidgen++);
            node->enabled = false;
            AddNode(std::unique_ptr<Node>(node));
        }
    }
}

void ControllerGraph::Save(const char *path)
{
    crude_json::value out;
    for(auto &n: nodes) {
        crude_json::value node;
        n->ToJSON(node);
        out["nodes"].push_back(node);
    }
    for(auto &l: links) {
        crude_json::value link;
        link["id"] = std::to_string((ptrdiff_t)l.Id.AsPointer());
        link["in"] = std::to_string((ptrdiff_t)l.InputId.AsPointer());
        link["out"] = std::to_string((ptrdiff_t)l.OutputId.AsPointer());
        out["links"].push_back(link);
    }

    out["linkidgen"] = std::to_string(linkidgen);
    out["nodeidgen"] = std::to_string(nodeidgen);
    out.save(path, 4);
}

bool ControllerGraph::Load(const char *path)
{
    std::pair<crude_json::value, bool> ret = crude_json::value::load(path);
    if(ret.second) {
        // Save attached joysticks so we can reattach them after the graph is recreated
        std::vector<Joystick *> joysticks;
        for(auto &node: nodes) {
            NodeJoystick *njs = dynamic_cast<NodeJoystick *>(node.get());
            if(njs && njs->js) {
                joysticks.push_back(njs->js);
            }
        }

        crude_json::value &json = ret.first;
        nodes.clear();
        links.clear();
        pins.clear();
        nodesSorted.clear();
        linkidgen = std::stoi(json["linkidgen"].get<std::string>());
        nodeidgen = std::stoi(json["nodeidgen"].get<std::string>());
        if(json.contains("nodes")) {
            for(auto &n: json["nodes"].get<crude_json::array>()) {
                const std::string &name = n["typename"].get<std::string>();
                Node *node = nodeFactory.MakeNode(name.c_str());
                if(node == NULL) {
                    oapiAddNotification(OAPINOTIF_ERROR, "Unknown node", name.c_str());
                    continue;
                }
                node->InitFromJSON(n);
                if(n.contains("xpos")) {
                    ImVec2 pos;
                    pos.x = n["xpos"].get<crude_json::number>();
                    pos.y = n["ypos"].get<crude_json::number>();
                    ed::SetNodePosition(node->id, pos);
                    node->placed = true;
                }
                AddNode(std::unique_ptr<Node>(node));
                dirty = true;

            }
        }

        if(json.contains("links")) {
            for(auto &l: json["links"].get<crude_json::array>()) {
                ed::LinkId id = std::stoi(l["id"].get<std::string>());
                ed::PinId inputPinId = std::stoi(l["in"].get<std::string>());
                ed::PinId outputPinId = std::stoi(l["out"].get<std::string>());
                links.push_back({ id, inputPinId, outputPinId });
                dirty = true;
            }
        }

        CreateSingletonNodes();

        TopologicalSort();

        for(Joystick *js: joysticks) {
            AttachJoystick(js);
        }

        ed::NavigateToContent();
        return true;
    } else {
        CreateSingletonNodes();
        TopologicalSort();
        return false;
    }

}
void ControllerGraph::GlobalInit()
{
    nodeFactory.Register<NodeLegacyControls>();
    nodeFactory.Register<NodeJoystick>();
    nodeFactory.Register<NodeCamera>();
}
