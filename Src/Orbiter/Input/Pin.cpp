#include "Pin.h"
#include "Icon.h"
#include <algorithm>
#include <imgui_node_editor.h>

Pin::Pin(Pin &&from) noexcept {
    id = from.id;
    name = std::move(from.name);
    ptype = from.ptype;
    pkind = from.pkind;
    fVal = from.fVal;
    bVal = from.bVal;
    hVal = from.hVal;
    parent = from.parent;
    inputs = std::move(from.inputs);
    from.id = ed::PinId::Invalid;
}

Pin& Pin::operator=(Pin&&from) noexcept {
    id = from.id;
    name = std::move(from.name);
    ptype = from.ptype;
    pkind = from.pkind;
    fVal = from.fVal;
    bVal = from.bVal;
    hVal = from.hVal;
    parent = from.parent;
    inputs = std::move(from.inputs);
    from.id = ed::PinId::Invalid;
    return *this;
}

Pin::Pin(std::string n, enum Pin::type t, enum Pin::kind k, Node *p, ed::PinId i) {
    id = i;
    name = n;
    ptype = t;
    pkind = k;
    fVal = 0.0;
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
            hVal = false;
            for(auto &p: inputs) {
                hVal |= p->hVal;
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
};

void Pin::DrawIcon()
{
    ImColor color = GetIconColor(ptype);
    ed::BeginPin(id, ed::PinKind::Output);
	switch(ptype) {
		case Pin::type::Axis:
			Axis(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), fVal, color);
			break;
		case Pin::type::Button:
			Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), bVal?IconType::ButtonDown:IconType::ButtonUp, true, color, ImColor(32, 32, 32, 255));
			break;
		case Pin::type::Hat:
			Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), IconType::Grid, true, color, ImColor(32, 32, 32, 255));
			break;
	}
    ed::EndPin();
}

void Pin::Draw(std::function<bool(const Pin *)> filter)
{
    float alpha = ImGui::GetStyle().Alpha;
    if(!filter(this)) {
        alpha = alpha * (48.0f / 255.0f);
    }
    ImGui::PushStyleVar(ImGuiStyleVar_Alpha, alpha);
    if(pkind == Pin::kind::Output) {
        ImGui::TextUnformatted(name.c_str());
        ImGui::SameLine();
		DrawIcon();
    } else {
		DrawIcon();
        ImGui::SameLine();
        ImGui::TextUnformatted(name.c_str());
    }
    ImGui::PopStyleVar();
}