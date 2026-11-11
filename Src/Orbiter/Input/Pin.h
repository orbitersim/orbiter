#pragma once

#include <list>
#include <string>
#include <functional>
#include <imgui_node_editor.h>

namespace ed = ax::NodeEditor;

// Pins are interactive elements of a Node
// They can be inputs or outputs
// They have a type that defines what value they can take
// An input Pin knows which other Pins are connected to it, this is
// used to update its value on demand via the ReadInputs method.
class Node;
class Pin final
{
public:
    enum class type {
        Button,
        HalfAxis,    // A pin that takes a value in [0;1]
        Axis,        // A pin that takes a value in [-1;1]
        Hat,
    };
    enum class kind {
        Input,
        Output
    };
    Pin() = delete;
    Pin(Pin &) = delete;
    Pin(Pin &&from) noexcept;
    Pin& operator=(Pin&& from) noexcept;
    ~Pin();
    Pin(std::string n, Pin::type t, Pin::kind k, Node *p, ed::PinId id = ed::PinId::Invalid);
    bool operator==(const Pin &other);
    void ReadInputs();
    bool hasInputs() { return !inputs.empty(); }
    void Draw(std::function<bool(const Pin *)> filter);
    ed::PinId id;
    std::string name;
    Pin::type ptype;
    Pin::kind pkind;
    Node *parent;

    float fVal;     // Axis/HalfAxis
    bool bVal;      // Button
    int hVal;       // Hat

    std::list<Pin *> inputs;
};
