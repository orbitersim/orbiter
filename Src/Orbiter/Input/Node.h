#pragma once

#include <deque>
#include <string>

#include "Pin.h"
#include "crude_json.h"

#define RegisterPin(name, var) RegisterPinInternal(#var, name, var)

class Node {
protected:
    bool grayed = false;
public:
    std::string name;
    // Use deque to make sure pointers to Pins stay valid
    std::deque<Pin> inputs;
    std::deque<Pin> outputs;
    float widthName = 0.0f;
    float maxWidthInput = 0.0f;
    float maxWidthOutput = 0.0f;
    int id;
    bool dirty;
    int pinidgen;
    bool placed = false; // Has been positioned in the editor
    bool initialized = false;
    Node(const char *n);
    virtual ~Node() = default;
    template<Pin::type Type>
    void RegisterPinInternal(const char *name, const char *displayname, InputPin<Type> &ip) {
        Pin *p = AddInput(name, displayname, Type);

        if constexpr (Type == Pin::type::Button)
            ip.Bind(p->bVal);
        else if constexpr (Type == Pin::type::Axis)
            ip.Bind(p->fVal);
        else if constexpr (Type == Pin::type::Hat)
            ip.Bind(p->hVal);
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
    virtual void Update(bool dryrun) = 0;

    // Serialization
    virtual const char* GetTypeName() const = 0;
    virtual void ToJSON(crude_json::value &out);
    virtual void InitFromJSON(const crude_json::value &in);
    const crude_json::value *jsonTemplate = NULL;
};
