#include "Node.h"

Node::Node(const char *n) {
    dirty = true;
    name = n;
}

Pin *Node::AddInput(const char *iname, const char *dname, Pin::type type) {
    dirty = true;

    if(jsonTemplate) {
        if(jsonTemplate->contains("inputs")) {
            for(auto &i: (*jsonTemplate)["inputs"].get<crude_json::array>()) {
                std::string jsonname = i["name"].get<std::string>();
                enum Pin::type jsontype = (enum Pin::type)stoi(i["type"].get<std::string>());
                if(type == jsontype && jsonname == iname) {
                    ed::PinId id = std::stoi(i["id"].get<std::string>());
                    inputs.emplace_back(iname, dname, type, Pin::kind::Input, this, id);
                    return &inputs.back();;
                }
            }
        }
    }

    inputs.emplace_back(iname, dname, type, Pin::kind::Input, this, pinidgen++);
    return &inputs.back();
}
Pin *Node::AddOutput(const char *iname, const char *dname, Pin::type type) {
    dirty = true;

    if(jsonTemplate) {
        if(jsonTemplate->contains("outputs")) {
            for(auto &o: (*jsonTemplate)["outputs"].get<crude_json::array>()) {
                std::string jsonname = o["name"].get<std::string>();
                enum Pin::type jsontype = (enum Pin::type)stoi(o["type"].get<std::string>());
                if(type == jsontype && jsonname == iname) {
                    ed::PinId id = std::stoi(o["id"].get<std::string>());
                    outputs.emplace_back(iname, dname, type, Pin::kind::Output, this, id);
                    Pin *ret = &outputs.back();
                    if(o.contains("deadzone")) {
                        float deadzone = o["deadzone"].get<crude_json::number>();
                        ret->context.deadzone = deadzone;
                    }
                    if(o.contains("saturation")) {
                        float saturation = o["saturation"].get<crude_json::number>();
                        ret->context.saturation = saturation;
                    }
                    if(o.contains("flip")) {
                        bool flip = o["flip"].get<crude_json::boolean>();
                        ret->context.flip = flip;
                    }
                    if(o.contains("rescale")) {
                        bool rescale = o["rescale"].get<crude_json::boolean>();
                        ret->context.rescale = rescale;
                    }

                    return ret;
                }
            }
        }
    }

    outputs.emplace_back(iname, dname, type, Pin::kind::Output, this, pinidgen++);
    return &outputs.back();
}

void Node::ComputeLayout() {
    float widthName = ImGui::CalcTextSize(name.c_str()).x;

    float maxWidthInput = 0.0f;
    for(Pin &input: inputs) {
        float sz = ImGui::CalcTextSize(input.displayname.c_str()).x;
        if(sz > maxWidthInput) maxWidthInput = sz;
    }

    maxWidthOutput = 0.0f;
    for(Pin &output: outputs) {
        float sz = ImGui::CalcTextSize(output.displayname.c_str()).x;
        if(sz > maxWidthOutput) maxWidthOutput = sz;
    }

    ImGuiStyle& style = ImGui::GetStyle();

    extraspacing = widthName - maxWidthInput - maxWidthOutput;

    if(inputs.size() > 0 && outputs.size() > 0) {
        extraspacing -= 2.0f * ImGui::GetTextLineHeight() + 3.0f * style.ItemSpacing.x;
    } else if (inputs.size() > 0) {
        extraspacing -= ImGui::GetTextLineHeight() + 2.0f * style.ItemSpacing.x;
    }

    extraspacing = std::max(0.0f, extraspacing);

    dirty = false;
}

void Node::UpdateInputs()
{
    for(Pin &input: inputs) {
        input.ReadInputs();
    }
}

void Node::Draw(std::function<bool(const Pin *)> filter) {
    if(!enabled) return;

    if(dirty) {
        ComputeLayout();
    }

    ed::BeginNode(id);
    if(grayed) {
        ImGui::TextDisabled("%s", name.c_str());
    } else {
        ImGui::TextUnformatted(name.c_str());
    }
    ImGui::Spacing();
    ImVec2 hmin = ImGui::GetItemRectMin();
    ImVec2 hmax = ImGui::GetItemRectMax();


    if(inputs.size() > 0) {
        ImGui::BeginGroup();
        for(Pin &input:inputs) {
            input.Draw(filter);
        }
        ImGui::EndGroup();

        if(outputs.size() > 0) ImGui::SameLine();
    }

    ImGui::Dummy(ImVec2(extraspacing, 0));ImGui::SameLine();
    
    ImGui::BeginGroup();
    for(Pin &output:outputs) {
        ImGui::Dummy(ImVec2(maxWidthOutput - ImGui::CalcTextSize(output.displayname.c_str()).x,0));
        ImGui::SameLine();
        output.Draw(filter);
    }
    ImGui::EndGroup();
        
    ed::EndNode();
}

void Node::ToJSON(crude_json::value &out)
{
    out["typename"] = GetTypeName();
    out["id"] = std::to_string(id);
    out["enabled"] = enabled;
    out["pinidgen"] = std::to_string(pinidgen);
    
    if(placed) {
        ImVec2 pos = ed::GetNodePosition(id);

        if(pos.x == FLT_MAX) pos.x = 0;
        if(pos.y == FLT_MAX) pos.y = 0;

        out["xpos"] = pos.x;
        out["ypos"] = pos.y;
    }

    for(auto &n: inputs) {
        crude_json::value pin;
        pin["id"] = std::to_string((ptrdiff_t)n.id.AsPointer());
        pin["name"] = n.name;
        pin["displayname"] = n.displayname;
        pin["type"] = std::to_string((int)n.ptype);
        out["inputs"].push_back(pin);
    }
    for(auto &n: outputs) {
        crude_json::value pin;
        pin["id"] = std::to_string((ptrdiff_t)n.id.AsPointer());
        pin["name"] = n.name;
        pin["displayname"] = n.displayname;
        pin["type"] = std::to_string((int)n.ptype);
        if(n.context.deadzone != 0.0f)
            pin["deadzone"] = n.context.deadzone;
        if(n.context.saturation != 1.0f)
            pin["saturation"] = n.context.saturation;
        if(n.context.flip)
            pin["flip"] = true;
        if(n.context.rescale)
            pin["rescale"] = true;
        out["outputs"].push_back(pin);
    }
}

void Node::InitFromJSON(const crude_json::value &json)
{
    jsonTemplate = &json;

    id = std::stoi(json["id"].get<std::string>());
    pinidgen = std::stoi(json["pinidgen"].get<std::string>());
    enabled = json["enabled"].get<bool>();
    RegisterPins();
    initialized = true;

    jsonTemplate = NULL;
}

void Node::InitDefault(int i)
{
    id = i;
    pinidgen = (id << 16) + 1;
    RegisterPins();
    initialized = true;
}