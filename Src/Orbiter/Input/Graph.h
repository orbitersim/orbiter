#pragma once
#include <vector>
#include <string>
#include <list>
#include <map>
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
    void CreateSingletonNodes();
};
