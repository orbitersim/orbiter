#pragma once
#include <vector>
#include <string>
#include <list>
#include <unordered_map>
#include <imgui_node_editor.h>
namespace ed = ax::NodeEditor;

struct Link
{
    ed::LinkId Id;
    ed::PinId  InputId;
    ed::PinId  OutputId;
};

class Node;
class Pin;

// The graph owns the Nodes and the links
// The Nodes own the Pins
class ControllerGraph final {
    public:
    std::vector<Link> links;
    std::vector<Node *> nodes;
	std::unordered_map<void *, Pin *> pins; // Map between ed::PinId and Pins
    std::list<Node *> nodesSorted;
	Pin *draggedPin = NULL;
	int linkidgen = 1;
	int nodeidgen = 1;
    ControllerGraph();
    ~ControllerGraph();
	void Update(bool dryrun);
    void Draw();
	void AddNode(Node *node);
	void DeleteNode(Node *node);
	Pin *PinFromId(ed::PinId id) { return pins.at(id.AsPointer()); }
	bool CanLink(const Pin *a, const Pin *b);
	void TopologicalSort();
	template <typename T, typename... Args>
	T *AddNode(Args&&... args)
	{
		static_assert(std::is_base_of<Node, T>::value, "T must derive from Node");
		T* n = new T(std::forward<Args>(args)..., nodeidgen++);
		AddNode(static_cast<Node*>(n));
		return n;
	}
};
