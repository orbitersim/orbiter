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
    Pin(Pin &&) = delete;
    Pin& operator=(Pin&&) = delete;
    ~Pin();
    Pin(std::string iname, std::string dname, Pin::type t, Pin::kind k, Node *p, ed::PinId id = ed::PinId::Invalid);
    bool operator==(const Pin &other);
    void ReadInputs();
    void Draw(std::function<bool(const Pin *)> filter);
    void DrawIcon();
    std::function<void(Pin *)> cbContextMenu;

    ed::PinId id;
    std::string name;        // Name in JSON file
    std::string displayname; // Name in the editor
    Pin::type ptype;
    Pin::kind pkind;
    Node *parent;

    float fVal;     // Axis/HalfAxis
    float fValRaw;  // value before deadzone/flipping/rescaling
    bool bVal;      // Button
    int hVal;       // Hat

    struct Context {
        float deadzone = 0.0f;
        float saturation = 1.0f;
        bool flip = false;
        bool rescale = false;
    } context;

    std::list<Pin *> inputs;
};


template<Pin::type Type>
class InputPin {
    using ValueType = std::conditional_t<
        Type == Pin::type::Button, bool,
        std::conditional_t<
            Type == Pin::type::Axis, float,
            int // Hat
        >
    >;

    ValueType *value = NULL;

public:
    InputPin() = default;

    operator ValueType&() { return *value; }
    operator const ValueType&() const { return *value; }

	void Bind(ValueType &v) { value = &v; }
};

template<Pin::type Type>
class OutputPin {
    using ValueType = std::conditional_t<
        Type == Pin::type::Button, bool,
        std::conditional_t<
            Type == Pin::type::Axis, float,
            int // Hat
        >
    >;

    ValueType *value = NULL;

public:
    OutputPin() = default;

    operator ValueType&() { return *value; }
    operator const ValueType&() const { return *value; }

	void Bind(ValueType &v) { value = &v; }
};
