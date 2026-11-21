#pragma once
#include <vector>
#include <string>
#include <list>
#include <unordered_map>
#include <memory>
#include <imgui_node_editor.h>
#include <functional>

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
class NodeLegacyControls;

class NodeFactory {
public:
    std::unordered_map<std::string, std::function<Node *()>> types;

    template <typename T> void Register() {
        types[T::TypeName]=[]()->Node * { return new T(); };
    }

    Node *MakeNode(const char *type) {
        return types.at(type)();
    }
};


// The graph owns the Nodes and the links
// The Nodes own the Pins
class ControllerGraph final {
    public:
    ed::EditorContext* m_Context = NULL;
    std::vector<Link> links;
    std::vector<std::unique_ptr<Node>> nodes;
    std::unordered_map<void *, Pin *> pins; // Map between ed::PinId and Pins
    std::list<Node *> nodesSorted;
    Pin *draggedPin = NULL;
    NodeLegacyControls *legacyControls = NULL;
    int linkidgen = 1;
    int nodeidgen = 1;
    bool dirty = true;
    ControllerGraph();
    ~ControllerGraph();
    void Update(bool dryrun);
    void DrawEditor();
    void AddNode(std::unique_ptr<Node> node);
    void DeleteNode(Node *node);
    bool NodeHasRoom(ImVec2 pos, ImVec2 size);
    Pin *PinFromId(ed::PinId id) { return pins.at(id.AsPointer()); }
    bool CanLink(const Pin *a, const Pin *b);
    void TopologicalSort();
    bool Load(const char *path);
    void Save(const char *path);
    void AttachJoystick(Joystick *joystick);
    void DetachJoystick(Joystick *joystick);
    void GetJoystickCtrl(float ctrl[15]);
    static inline NodeFactory nodeFactory;
    static void GlobalInit();
};
