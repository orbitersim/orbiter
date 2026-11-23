#pragma once

#include <deque>
#include <string>

#include "Pin.h"
#include "crude_json.h"

#define RegisterPin(name, var) RegisterPinInternal(#var, name, var)
#define RegisterPinWithDefault(name, var, def) RegisterPinInternal(#var, name, var, def)

#define DECLARE_NODE_SINGLETON(T) \
class T; \
    template <> struct NodeTraits<T> { \
        static constexpr const char* TypeName = #T; \
        static constexpr bool IsSingleton = true; \
    };

#define DECLARE_NODE(T) \
class T; \
    template <> struct NodeTraits<T> { \
        static constexpr const char* TypeName = #T; \
        static constexpr bool IsSingleton = false; \
    };

#define DECLARE_NODE_TRAITS(T) \
    const char* GetTypeName() const override { return NodeTraits<T>::TypeName; } \
    bool IsSingleton() const override { return NodeTraits<T>::IsSingleton; }

template <typename T>
struct NodeTraits; // Intentionally undefined


// For controllers that go through the legacy API
// It automatically takes into account the Lin/Rot/Off RCS switch and AF control surfaces modes
class INodeLegacyController {
public:
    virtual void AddJoystickCtrl(float ctrl[15]) = 0;
};

class Node {
protected:
    bool grayed = false;
public:
    std::string name;
    // Use deque to make sure pointers to Pins stay valid
    std::deque<Pin> inputs;
    std::deque<Pin> outputs;
    float maxWidthOutput = 0.0f; // For right alignment of the text
    float extraspacing;          // Spacing between input and output groups
    int id;
    bool dirty;
    int pinidgen;
    bool placed = false; // Has been positioned in the editor
    bool initialized = false;
    bool enabled = true; // if false : not shown on editor and ignored when executing the graph
    bool singleton = false; // 
    Node(const char *n);
    virtual ~Node() = default;
    template<Pin::type Type>
    void RegisterPinInternal(const char *name, const char *displayname, InputPin<Type> &ip) {
        Pin *p = AddInput(name, displayname, Type);

        if constexpr (Type == Pin::type::Button || Type == Pin::type::ButtonPressed || Type == Pin::type::ButtonReleased)
            ip.Bind(p->bVal);
        else if constexpr (Type == Pin::type::Axis)
            ip.Bind(p->fVal);
        else if constexpr (Type == Pin::type::Hat)
            ip.Bind(p->hVal);
    }
    template<Pin::type Type, typename DefaultValue>
    void RegisterPinInternal(const char *name, const char *displayname, InputPin<Type> &ip, DefaultValue def) {
        Pin *p = AddInput(name, displayname, Type);

        if constexpr (Type == Pin::type::Button || Type == Pin::type::ButtonPressed || Type == Pin::type::ButtonReleased) {
            ip.Bind(p->bVal);
            p->bValDefault = def;
        } else if constexpr (Type == Pin::type::Axis) {
            ip.Bind(p->fVal);
            p->fValDefault = def;
        } else if constexpr (Type == Pin::type::Hat) {
            ip.Bind(p->hVal);
            p->hValDefault = def;
        }
    }
    template<Pin::type Type>
    void RegisterPinInternal(const char *name, const char *displayname, OutputPin<Type> &ip) {
        Pin *p = AddOutput(name, displayname, Type);

        if constexpr (Type == Pin::type::Button)
            ip.Bind(p->bVal);
        else if constexpr (Type == Pin::type::Axis)
            ip.Bind(p->fVal);
        else if constexpr (Type == Pin::type::Hat)
            ip.Bind(p->hVal);
    }
    Pin *AddInput(const char *iname, const char *dname, Pin::type type);
    Pin *AddOutput(const char *iname, const char *dname, Pin::type type);
    void ComputeLayout();
    void UpdateInputs();
    void InitDefault(int id);
    virtual void RegisterPins() = 0;
    virtual void Draw(std::function<bool(const Pin *)> filter);
    virtual void UpdateOutputs(bool dryrun) = 0;

    // Serialization
    virtual const char* GetTypeName() const = 0;
    virtual bool IsSingleton() const = 0;
    virtual void ToJSON(crude_json::value &out);
    virtual void InitFromJSON(const crude_json::value &in);
    const crude_json::value *jsonTemplate = NULL;
};
