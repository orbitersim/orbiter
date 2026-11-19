#include "NodeJoystick.h"
#include "Icon.h"
#define SDL_DISABLE_OLD_NAMES
#include <SDL3/SDL.h>
#include "Joystick.h"

#include "implot.h"
#include <algorithm>


static float TransformAxisValue(Pin::Context *ctx, float f)
{
    float deadzone = ctx->deadzone;
    if(fabs(f) < deadzone) {
        f = 0.0f;
    } else if(f < 0.0) {
        f = std::max(-1.0f, (f + deadzone) / (ctx->saturation - deadzone));
    } else {
        f = std::min(1.0f, (f - deadzone) / (ctx->saturation - deadzone));
    }

    if(ctx->rescale) {
        f = (f + 1.0f) / 2.0f;
        if(f < 0.001f) f = 0.0f;
    }
    if(ctx->flip) f = -f;

    return f;
}

static void TransferFunctionEditor(const char *label, Pin::Context *ctx, float currentValue)
{
//    const ImVec4 deadzone_col(0.2f, 0.8f, 1.f, 1.f);
    const ImU32 deadzone_col = IM_COL32(51, 204, 255, 255);
    const ImU32 deadzone_fill = IM_COL32(51, 204, 255, 35);

    const ImU32 saturation_col = IM_COL32(255, 153, 51, 255);
    const ImU32 saturation_fill = IM_COL32(255, 153, 51, 35);

    // Make drag lines invisibles to limit clutter
    const ImVec4 invisible_col(0.f, 0.f, 0.f, 0.f);

    ctx->deadzone = std::clamp(ctx->deadzone, 0.0f, 0.99f);
    ctx->saturation = std::clamp(ctx->saturation, ctx->deadzone + 0.01f, 1.f);

    ImPlot::SetNextAxesLimits(-1.1, 1.1, -1.1, 1.1, ImGuiCond_Always);

    if (ImPlot::BeginPlot(label, ImVec2(320, 320), ImPlotFlags_NoLegend)) {
        ImPlot::SetupAxes("Input", "Output", ImPlotAxisFlags_Lock | ImPlotAxisFlags_NoTickLabels, ImPlotAxisFlags_Lock | ImPlotAxisFlags_NoTickLabels);

        // Plot "transfer" function
        constexpr int N = 101;
        float xs[N], ys[N];
        for (int i = 0; i < N; ++i) {
            float x = -1.f + 2.f * i / (N - 1);
            xs[i] = x;
            ys[i] = TransformAxisValue(ctx, x);
        }
        ImPlot::PlotLine("Response", xs, ys, N);

        ImDrawList* draw = ImPlot::GetPlotDrawList();

        ImPlot::PushPlotClipRect();

        // Draw deadzone
        ImVec2 dz_min = ImPlot::PlotToPixels(-ctx->deadzone, -1.1);
        ImVec2 dz_max = ImPlot::PlotToPixels(ctx->deadzone, 1.1);

        draw->AddRectFilled(ImVec2(dz_min.x+1, dz_max.y), ImVec2(dz_max.x+1, dz_min.y), deadzone_fill);

        // Negative saturation zone
        ImVec2 sat_left_min = ImPlot::PlotToPixels(-1.1, -1.0);
        ImVec2 sat_left_max = ImPlot::PlotToPixels(-ctx->saturation, 1.0);

        draw->AddRectFilled(ImVec2(sat_left_min.x, sat_left_max.y), ImVec2(sat_left_max.x, sat_left_min.y), saturation_fill);

        // Positive saturation zone
        ImVec2 sat_right_min = ImPlot::PlotToPixels(ctx->saturation, -1.0);
        ImVec2 sat_right_max = ImPlot::PlotToPixels(1.1, 1.0);

        draw->AddRectFilled(ImVec2(sat_right_min.x, sat_right_max.y), ImVec2(sat_right_max.x, sat_right_min.y), saturation_fill);

        ImPlot::PopPlotClipRect();

        // Draggable deadzone boundaries
        double dz_pos = ctx->deadzone;
        double dz_neg = -ctx->deadzone;

        if (ImPlot::DragLineX(0, &dz_pos, invisible_col, 8.f, ImPlotDragToolFlags_None)) {
            ctx->deadzone = std::clamp((float)std::abs(dz_pos), 0.f, ctx->saturation - 0.01f);
        }

        if (ImPlot::DragLineX(1, &dz_neg, invisible_col, 8.f, ImPlotDragToolFlags_None)) {
            ctx->deadzone = std::clamp((float)std::abs(dz_neg), 0.f, ctx->saturation - 0.01f);
        }

        // Draggable saturation boundaries
        double sat_pos = ctx->saturation;
        double sat_neg = -ctx->saturation;

        if (ImPlot::DragLineX(2, &sat_pos, invisible_col, 8.f, ImPlotDragToolFlags_None)) {
            ctx->saturation = std::clamp((float)std::abs(sat_pos), ctx->deadzone + 0.01f, 1.f);
        }

        if (ImPlot::DragLineX(3, &sat_neg, invisible_col, 8.f, ImPlotDragToolFlags_None)) {
            ctx->saturation = std::clamp((float)std::abs(sat_neg), ctx->deadzone + 0.01f, 1.f);
        }

        // Current value
        double cx = currentValue;
        double cy = TransformAxisValue(ctx, currentValue);

        ImPlot::SetNextMarkerStyle(ImPlotMarker_Circle, 3.f, ImVec4(1.f, 1.f, 1.f, 1.f));
        ImPlot::PlotScatter("Current", &cx, &cy, 1);

        // Legend
        ImPlot::PushPlotClipRect();

        const ImVec2 legend_pos = ImPlot::PlotToPixels(-0.92, 0.88);

        const auto LegendItem = [&](const char* text, ImU32 color, float y) {
            draw->AddCircleFilled(ImVec2(legend_pos.x, y), 4.f, color);
            draw->AddText(ImVec2(legend_pos.x + 10.f, y - ImGui::GetTextLineHeight() * 0.5f), ImGui::GetColorU32(ImGuiCol_Text), text);
        };

        LegendItem("Deadzone", deadzone_col, legend_pos.y);
        LegendItem("Saturation", saturation_col, legend_pos.y + ImGui::GetTextLineHeightWithSpacing());

        ImPlot::PopPlotClipRect();

        ImPlot::EndPlot();
    }
    ImGui::BeginGroup();
    ImGui::SetNextItemWidth(100.f);
    if(ImGui::DragFloat("Deadzone", &ctx->deadzone, 0.0f, 0.0f, 0.9f, "%0.02f")) {
        ctx->deadzone = std::clamp(ctx->deadzone, 0.f, ctx->saturation - 0.01f);
    }
    ImGui::SetNextItemWidth(100.f);
    if(ImGui::DragFloat("Saturation", &ctx->saturation, 0.0f, 0.0f, 0.9f, "%0.02f")) {
        ctx->saturation = std::clamp(ctx->saturation, ctx->deadzone + 0.01f, 1.f);
    }
    ImGui::EndGroup();
    ImGui::SameLine();
    ImGui::BeginGroup();
    ImGui::Checkbox("Flip axis", &ctx->flip);
    ImGui::Checkbox("Rescale", &ctx->rescale);
    ImGui::EndGroup();
}

NodeJoystick::NodeJoystick() : Node("Default controller")
{
}

NodeJoystick::~NodeJoystick()
{
}

void NodeJoystick::InitFromJoystick(Joystick *joy, int i)
{
	id = i;
	pinidgen = (id << 16) + 1;
	grayed = false;
    js = joy;

    const auto cbAxis = [this](Pin *pin) {
        TransferFunctionEditor("Axis transfer function", &pin->context, pin->fValRaw);
    };

    for(auto &btn: js->btnnames) {
        AddOutput(btn.c_str(), Pin::type::Button);
    }
    for(auto &axis: js->axesnames) {
        Pin *pin = AddOutput(axis.c_str(), Pin::type::Axis);
        pin->cbContextMenu = cbAxis;
    }
    for(size_t hat = 0; hat < js->state.hats.size(); hat++) {
        char hatname[32];
        sprintf(hatname, "Hat %zu", hat);
        AddOutput(hatname, Pin::type::Hat);
    }

    guid = js->guid;
	name = js->name;
}

void NodeJoystick::InitPins()
{
	assert(jsonTemplate);
	const auto cbAxis = [this](Pin *pin) {
        TransferFunctionEditor("Axis transfer function", &pin->context, pin->fValRaw);
    };

	if(jsonTemplate->contains("outputs")) {
		guid = (*jsonTemplate)["guid"].get<std::string>();
		name = (*jsonTemplate)["lastseenname"].get<std::string>();
		for(auto &o: (*jsonTemplate)["outputs"].get<crude_json::array>()) {
			std::string jsonname = o["name"].get<std::string>();
			enum Pin::type jsontype = (enum Pin::type)stoi(o["type"].get<std::string>());

			ed::PinId id = std::stoi(o["id"].get<std::string>());
			outputs.emplace_back(jsonname, jsontype, Pin::kind::Output, this, id);
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
			if(jsontype == Pin::type::Axis)
				ret->cbContextMenu = cbAxis;
		}
	}
}

void NodeJoystick::Update(bool dryrun)
{
    if(!js) return; // No joystick attached to this node

    js->UpdateState();
    auto normalize = [](int v) {
        return  v < 0 ? v / 32768.0f : v / 32767.0f;
    };
    const auto *state = js->GetState();
    size_t idx = 0;
    for(int i=0; i<state->btns.size(); i++) {
        outputs[idx++].bVal = state->btns[i];
    }

    for(int i=0; i<state->axis.size(); i++) {
        Pin &out = outputs[idx++];

        float f = normalize(state->axis[i]);
        out.fValRaw = f;
        out.fVal = TransformAxisValue(&out.context, f);
    }

    for(int i=0; i<state->hats.size(); i++) {
        outputs[idx++].hVal = state->hats[i];
    }
}

bool NodeJoystick::IsConnected()
{
    return js && js->IsConnected();
}

bool NodeJoystick::IsAttached()
{
    return js != NULL;
}

void NodeJoystick::Detach()
{
    if(!js) return;
	grayed = true;
    js = NULL;

    // Clear the outputs to prevent stale values
    for (Pin& pin : outputs) {
        switch (pin.ptype) {
        case Pin::type::Button:
            pin.bVal = false;
            break;
        case Pin::type::Axis:
        case Pin::type::HalfAxis:
            pin.fVal = 0.0f;
            break;
        case Pin::type::Hat:
            pin.hVal = SDL_HAT_CENTERED;
            break;
        }
    }
}

void NodeJoystick::Attach(Joystick *joy)
{
    assert(!js);
    js = joy;
	grayed = false;
	name = js->name;
}

bool NodeJoystick::CanAttach(Joystick *js) {
    if(js->guid != guid) return false;

    // Check that the attached joystick has the same mapping
    // in case two different controllers have the same GUID but different buttons
    const auto *state = js->GetState();
    size_t idx = 0;
//	if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size() + state->balls.size()) {
    if(outputs.size() != state->btns.size() + state->axis.size() + state->hats.size()) {
        return false;
    }

    for(int i=0; i<state->btns.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Button) {
            return false;
        }
    }
    for(int i=0; i<state->axis.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Axis) {
            return false;
        }
    }
    for(int i=0; i<state->hats.size(); i++) {
        if(outputs[idx++].ptype != Pin::type::Hat) {
            return false;
        }
    }
    return true;
}

void NodeJoystick::ToJSON(crude_json::value &out)
{
	Node::ToJSON(out);
	out["guid"] = guid;
	out["lastseenname"] = name;
}
