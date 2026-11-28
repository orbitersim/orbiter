#pragma once
#include <vector>
#include <string>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <imgui_node_editor.h>
#include <functional>
#include "crude_json.h"

namespace ed = ax::NodeEditor;

struct Link
{
    ed::LinkId Id;
    ed::PinId  InputId;
    ed::PinId  OutputId;
};

class Node;
class Pin;
class Joystick;
class NodeJoystick;

class NodeFactory {
public:
    struct NodeInfo {
        std::function<Node *()> maker;
        bool isSingleton;
    };
    std::map<std::string, NodeInfo> types;

    template <typename T> void Register() {
        types[NodeTraits<T>::TypeName]={[]()->Node * { return new T(); }, NodeTraits<T>::IsSingleton};
    }

    Node *MakeNode(const char *type) {
        auto it = types.find(type);
        if(it == types.end()) return NULL;
        return it->second.maker();
    }
};


class AxisAssignment {
public:
    NodeJoystick *jsnode;
    int axis = 0;
    bool invert = false;
    bool rescale = false;
    float deadzone = 0.0f;
    float saturation = 1.0f;

    void ToJSON(crude_json::value &out);
    bool FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId);
};

class HatAssignment {
public:
    NodeJoystick *jsnode;
    int hat = 0;
    bool flip;

    void ToJSON(crude_json::value &out);
    bool FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId);
};

class WizardConfig {
public:
    std::optional<AxisAssignment> pitch;
    std::optional<AxisAssignment> bank;
    std::optional<AxisAssignment> yaw;
    std::optional<AxisAssignment> thrusters;
    std::optional<HatAssignment> camera;

    void ToJSON(crude_json::value &out);
    bool FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId);
};


// The graph owns the Nodes and the links
// The Nodes own the Pins
class ControllerGraph final {
public:
    WizardConfig wcfg;
    ed::EditorContext* m_Context = NULL;
    std::vector<Link> links;
    std::vector<std::unique_ptr<Node>> nodes;
    std::unordered_map<void *, Pin *> pins; // Map between ed::PinId and Pins
    std::list<Node *> nodesSorted;
    Pin *draggedPin = NULL;
    int linkidgen = 1;
    int nodeidgen = 1;
    bool dirty = true;
    ControllerGraph();
    ~ControllerGraph();
    void Update(bool dryrun);
    void DrawConfiguration();
    bool wizardMode = true;
    void DrawEditor();
    void DrawWizard();
    void AddNode(std::unique_ptr<Node> node);
    void DeleteNode(Node *node);
    bool NodeHasRoom(ImVec2 pos, ImVec2 size);
    Pin *PinFromId(ed::PinId id) {
        auto it = pins.find(id.AsPointer());
        if(it == pins.end()) return NULL;
        return it->second;
    }
    bool CanLink(const Pin *a, const Pin *b);
    void TopologicalSort();
    bool Load(const char *path);
    void Save(const char *path);
    void AttachJoystick(Joystick *joystick);
    void DetachJoystick(Joystick *joystick);
    void GetJoystickCtrl(float ctrl[15]);
    static inline NodeFactory nodeFactory;
    static void GlobalInit();
    bool HasNodeOfType(const std::string &type);
    Node *GetNodeOfType(const std::string &type);
    void CreateSingletonNodes();
    void CreateGraphFromWizard();
};
