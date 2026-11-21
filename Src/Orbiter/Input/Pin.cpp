#include "Pin.h"
#include "Icon.h"
#include <algorithm>
#include <imgui_node_editor.h>

Pin::Pin(std::string iname, std::string dname, enum Pin::type t, enum Pin::kind k, Node *p, ed::PinId i) {
    id = i;
    name = std::move(iname);
    displayname = std::move(dname);
    ptype = t;
    pkind = k;
    fVal = 0.0f;
    fValRaw = 0.0f;
    bVal = false;
    hVal = 0;
    parent = p;
}

bool Pin::operator==(const Pin &other) {
    return other.id == id;
}

Pin::~Pin() {
}

void Pin::ReadInputs()
{
    switch(ptype) {
        case Pin::type::Button:
            bVal = false;
            for(auto &p: inputs) {
                bVal |= p->bVal;
            }
            break;
        case Pin::type::HalfAxis:
            fVal = 0.0f;
            for(auto &p: inputs) {
                fVal += p->fVal;
            }
            fVal = std::clamp(fVal, 0.0f, 1.0f);
            break;
        case Pin::type::Axis:
            fVal = 0.0f;
            for(auto &p: inputs) {
                fVal += p->fVal;
            }
            fVal = std::clamp(fVal, -1.0f, 1.0f);
            break;
        case Pin::type::Hat:
            hVal = 0;
            // Hat values cannot be combined easily, just stop at the first entry set
            for(auto &p: inputs) {
                if(p->hVal) {
                    hVal = p->hVal;
                    break;
                }
            }
            break;
    }
}

//Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
static const ImU32 colBtn   = IM_COL32(230, 159,   0, 255);
static const ImU32 colAxis  = IM_COL32( 86, 180, 233, 255);
static const ImU32 colHAxis = IM_COL32(  0, 158, 115, 255);
static const ImU32 colHat   = IM_COL32(240, 228,  66, 255);
static const ImU32 colTrg   = IM_COL32(204, 121, 167, 255);
static const ImU32 colWhite = IM_COL32(255, 255, 255, 255);

ImColor GetIconColor(enum Pin::type type)
{
    switch (type)
    {
        case Pin::type::Button:   return colBtn;
        case Pin::type::HalfAxis: return colHAxis;
        case Pin::type::Axis:     return colAxis;
        case Pin::type::Hat:      return colHat;
    }
    assert(false);
    return colBtn;
};

void Pin::DrawIcon()
{
    ImColor color = GetIconColor(ptype);
    switch(ptype) {
        case Pin::type::Axis:
            Axis(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), fVal, color);
            break;
        case Pin::type::Button:
            Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), bVal?IconType::ButtonDown:IconType::ButtonUp, true, color, ImColor(32, 32, 32, 255));
            break;
        case Pin::type::Hat:
            Hat(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), hVal, color);
            break;
    }
}

void Pin::Draw(std::function<bool(const Pin *)> filter)
{
    float alpha = ImGui::GetStyle().Alpha;
    if(!filter(this)) {
        alpha = alpha * (48.0f / 255.0f);
    }
    ImGui::PushStyleVar(ImGuiStyleVar_Alpha, alpha);
    if(pkind == Pin::kind::Output) {
        ed::BeginPin(id, ed::PinKind::Output);
        ed::PinPivotAlignment(ImVec2(1.0f, 0.5f));
        ed::PinPivotSize(ImVec2(0, 0));
        ImGui::TextUnformatted(displayname.c_str());
        ImGui::SameLine();
        DrawIcon();
        ed::EndPin();
    } else {
        ed::BeginPin(id, ed::PinKind::Input);
        ed::PinPivotAlignment(ImVec2(0, 0.5f));
        ed::PinPivotSize(ImVec2(0, 0));
        DrawIcon();
        ImGui::SameLine();
        ImGui::TextUnformatted(displayname.c_str());
        ed::EndPin();
    }
    ImGui::PopStyleVar();
}
