#define IMGUI_DEFINE_MATH_OPERATORS

#include "Graph.h"
#include "OrbiterAPI.h"
#include "NodeJoystick.h"
#include "Joystick.h"
#include "TimeData.h"
#include "NodeLegacyControls.h"

#include <chrono>
#include <cmath>
#include <imgui.h>
#include "implot.h"
#include <algorithm>
#include <array>
#include <optional>

extern TimeData td;

static ImVec2 ADIRotate(ImVec2 p, float angle)
{
    const float c = cosf(angle);
    const float s = sinf(angle);
    return ImVec2(p.x * c - p.y * s, p.x * s + p.y * c);
}

static ImVec2 ADITransform(ImVec2 p, ImVec2 center, float angle)
{
    const ImVec2 rp = ADIRotate(p, angle);
    return ImVec2(center.x + rp.x, center.y + rp.y);
}

static void ADIDrawCloud(ImDrawList* dl, ImVec2 center, float scale, float bank, ImU32 color)
{
    const ImVec2 offsets[] = {
        {-12.0f,  0.0f},
        {  0.0f, -5.0f},
        { 12.0f,  0.0f}
    };

    const float radii[] = { 9.0f, 13.0f, 9.0f };

    for (int i = 0; i < 3; ++i) {
        const ImVec2 offset(offsets[i].x * scale, offsets[i].y * scale);
        const ImVec2 p = ADIRotate(offset, bank);

        dl->AddCircleFilled(ImVec2(center.x + p.x, center.y + p.y), radii[i] * scale, color);
    }
}

// pitch/bank/yaw are in [-1;1], just treat them as radian for illustration purposes
static void DrawADI(ImVec2 size, float pitch, float bank, float yaw)
{
    ImVec2 cursorPos = ImGui::GetCursorScreenPos();
    ImDrawList *dl  = ImGui::GetWindowDrawList();
    ImVec2 center = cursorPos + size / 2.0f;
    float radius = size.x / 2.0f - 6.0f;

    // Radius plus bezel
    const float extradius = radius + 6.0f;
    const ImVec2 topleft(center.x - extradius, center.y - extradius);
    const ImVec2 bottomright(center.x + extradius, center.y + extradius);
    dl->PushClipRect(topleft, bottomright);

    bank = -bank;

    const ImU32 sky       = IM_COL32(45, 145, 205, 255);
    const ImU32 ground    = IM_COL32(135, 85, 48, 255);
    const ImU32 white     = IM_COL32(240, 240, 235, 255);
    const ImU32 bezel     = IM_COL32(35, 38, 42, 255);
    const ImU32 bezelEdge = IM_COL32(100, 105, 110, 255);

    // Background colour used to mask everything outside the instrument.
    const ImGuiStyle& style = ImGui::GetStyle();
    const ImU32 bgCol = ImColor(style.Colors[ImGuiCol_ChildBg]);

    // Pitch moves the horizon vertically.
    float horizon = sinf(pitch) * radius * 1.5f;

    // Yaw moves the scenery horizontally.
    // Positive yaw makes the scenery move left.
    float cloudOffset = -sinf(yaw) * radius * 1.5f;//0.65f;

    // -------------------------------------------------
    // Sky and ground
    // -------------------------------------------------

    const float extent = radius * 2.0f;

    ImVec2 skyPoints[] = {
        {-extent, -extent},
        { extent, -extent},
        { extent, horizon},
        {-extent, horizon}
    };

    ImVec2 groundPoints[] = {
        {-extent, horizon},
        { extent, horizon},
        { extent, extent},
        {-extent, extent}
    };

    for (ImVec2& p : skyPoints)
        p = ADITransform(p, center, bank);

    for (ImVec2& p : groundPoints)
        p = ADITransform(p, center, bank);

    dl->AddConvexPolyFilled(skyPoints, 4, sky);
    dl->AddConvexPolyFilled(groundPoints, 4, ground);

    // -------------------------------------------------
    // Clouds
    // -------------------------------------------------

    struct Cloud {
        float x, y, scale;
    };

    static constexpr Cloud clouds[] = {
        {-0.48f, -0.42f, 0.75f},
        { 0.05f, -0.62f, 0.90f},
        { 0.48f, -0.30f, 0.65f}
    };

    for (const Cloud& cloud : clouds)
    {
        float x = cloud.x * radius + cloudOffset;
        float y = cloud.y * radius + horizon;

        ImVec2 p = ADITransform(ImVec2(x, y), center, bank);

        ADIDrawCloud(dl, p, cloud.scale, bank, white);
    }

    // -------------------------------------------------
    // Circular mask
    // -------------------------------------------------

    // Hide everything outside the circular instrument.
    dl->AddCircle(center, radius * 1.5f, bgCol, 64, radius);

    // -------------------------------------------------
    // Horizon line
    // -------------------------------------------------

    if (fabsf(horizon) < radius) {
        float x = sqrtf(radius * radius - horizon * horizon);

        ImVec2 p1 = ADITransform(ImVec2(-x, horizon), center, bank);
        ImVec2 p2 = ADITransform(ImVec2( x, horizon), center, bank);

        dl->AddLine(p1, p2, white, 2.0f);
    }

    // -------------------------------------------------
    // Fixed spacecraft reference symbol
    // -------------------------------------------------

    float wing = radius * 0.28f;

    dl->AddLine(ImVec2(center.x - wing, center.y),
                ImVec2(center.x - radius * 0.06f, center.y),
                white, 4.0f);

    dl->AddLine(ImVec2(center.x + radius * 0.06f, center.y),
                ImVec2(center.x + wing, center.y),
                white, 4.0f);

    dl->AddCircle(center, radius * 0.055f, white, 16, 2.0f);

    // -------------------------------------------------
    // Instrument bezel
    // -------------------------------------------------

    dl->AddCircle(center, radius + 5.0f, bezel, 64);
    dl->AddCircle(center, radius + 5.0f, bezelEdge, 64, 2.0f);
    dl->AddCircle(center, radius, bezel, 64, 2.0f);

    dl->PopClipRect();

    ImGui::Dummy(ImVec2(2.0f*extradius, 2.0f*extradius));
}


static void DrawEngineIndicator(ImVec2 size, float val)
{
    ImDrawList* dl = ImGui::GetWindowDrawList();
    ImVec2 pos = ImGui::GetCursorScreenPos();

    float mainPower  = std::clamp( val, 0.0f, 1.0f);
    float retroPower = std::clamp(-val, 0.0f, 1.0f);

    const float width = size.x;
    const float height = size.y;

    const float left = pos.x;
    const float right = pos.x + width;
    const float top = pos.y;
    const float bottom = pos.y + height;

    const float mainHeight = height * 0.75f;
    const float retroHeight = height * 0.25f;
    const float centerY = top + mainHeight;

    const float columnWidth = width * 0.45f;
    const float columnLeft = pos.x + (width - columnWidth) * 0.5f;
    const float columnRight = columnLeft + columnWidth;

    const ImU32 background = IM_COL32(35, 38, 42, 255);
    const ImU32 green = IM_COL32(45, 190, 100, 255);
    const ImU32 red = IM_COL32(210, 65, 55, 255);
    const ImU32 tick = IM_COL32(175, 180, 185, 255);
    const ImU32 outline = IM_COL32(100, 105, 110, 255);

    // Background
    dl->AddRectFilled(ImVec2(columnLeft, top), ImVec2(columnRight, bottom), background);

    // Main engine: grows upwards from neutral
    const float mainFill = mainHeight * mainPower;

    if (mainFill > 0.0f) {
        dl->AddRectFilled(ImVec2(columnLeft, centerY - mainFill), ImVec2(columnRight, centerY), green);
    }

    // Retrothrusters: grows downwards from neutral
    const float retroFill = retroHeight * retroPower;

    if (retroFill > 0.0f) {
        dl->AddRectFilled(ImVec2(columnLeft, centerY), ImVec2(columnRight, centerY + retroFill), red);
    }

    // Tick marks
    constexpr int mainTicks = 12;
    constexpr int retroTicks = 4;

    for (int i = 0; i <= mainTicks; ++i) {
        float y = top + mainHeight * i / mainTicks;
        float tickWidth = (i % 3 == 0) ? width * 0.32f : width * 0.18f;

        dl->AddLine(ImVec2(pos.x + (width - tickWidth) * 0.5f, y), ImVec2(pos.x + (width + tickWidth) * 0.5f, y), tick, 1.0f);
    }

    for (int i = 1; i <= retroTicks; ++i) {
        float y = centerY + retroHeight * i / retroTicks;
        float tickWidth = (i % 2 == 0) ? width * 0.32f : width * 0.18f;

        dl->AddLine(ImVec2(pos.x + (width - tickWidth) * 0.5f, y), ImVec2(pos.x + (width + tickWidth) * 0.5f, y), tick, 1.0f);
    }

    // Outline
    dl->AddRect(ImVec2(columnLeft, top), ImVec2(columnRight, bottom), outline);

    // Stronger neutral reference line
    dl->AddLine(ImVec2(left, centerY), ImVec2(right, centerY), tick, 2.0f);

    // Legend
    const float legendX = columnRight + 12.0f;
    const float legendOffset = ImGui::GetTextLineHeight() * 0.5f;

    dl->AddText(ImVec2(legendX, top + mainHeight * 0.5f - legendOffset), green, "MAIN");
    dl->AddText(ImVec2(legendX, centerY + retroHeight * 0.5f - legendOffset), red, "RETRO");

    ImGui::Dummy(size);
}

struct Star {
    float x, y;
    float size;
    float brightness;
};

static void DrawStarfield(ImVec2 size, float yaw, float pitch)
{
    ImDrawList* dl = ImGui::GetWindowDrawList();
    ImVec2 pos = ImGui::GetCursorScreenPos();
    ImVec2 center = pos + size * 0.5f;

    float radius = std::min(size.x, size.y) * 0.5f - 5.0f;

    const ImU32 background = IM_COL32(5, 8, 19, 255);
    const ImU32 bezel = IM_COL32(65, 70, 85, 255);

    // Background
    dl->AddCircleFilled(center, radius, background, 64);

    static constexpr Star stars[] = {
        {-0.80f, -0.65f, 1.2f, 180},
        {-0.55f, -0.25f, 1.8f, 255},
        {-0.25f, -0.75f, 1.0f, 160},
        { 0.15f, -0.85f, 1.4f, 210},
        { 0.55f, -0.65f, 1.1f, 180},
        { 0.82f, -0.35f, 1.8f, 255},
        {-0.75f,  0.10f, 1.0f, 170},
        {-0.40f,  0.45f, 1.5f, 220},
        { 0.05f,  0.15f, 2.0f, 255},
        { 0.45f,  0.25f, 1.2f, 180},
        { 0.75f,  0.55f, 1.6f, 230},
        {-0.15f,  0.80f, 1.1f, 190},
        { 0.30f,  0.75f, 1.5f, 230},
        {-0.85f,  0.70f, 1.3f, 200},
        { 0.90f,  0.05f, 1.0f, 160},
    };

    const float shiftX = -yaw * radius * 0.65f;
    const float shiftY =  pitch * radius * 0.65f;

    auto wrap = [](float v, float extent) {
        return v - extent * floorf(v / extent);
    };

    dl->PushClipRect(pos, pos + size);

    for (const Star& star : stars)
    {
        // Wrap in a square viewport.
        float x = wrap(star.x * radius + shiftX + radius, 2.0f * radius);
        float y = wrap(star.y * radius + shiftY + radius, 2.0f * radius);

        ImVec2 p(
            center.x + x - radius,
            center.y + y - radius
        );

        // Keep stars inside the circular viewport.
        float dx = p.x - center.x;
        float dy = p.y - center.y;

        if (dx * dx + dy * dy <= radius * radius)
        {
            ImU32 color = IM_COL32(210, 220, 255, (int)star.brightness);
            dl->AddCircleFilled(p, star.size, color);
        }
    }

    dl->PopClipRect();

    // Fixed centre reference
    const ImU32 reference = IM_COL32(100, 190, 255, 220);

    dl->AddLine(
        ImVec2(center.x - 8, center.y),
        ImVec2(center.x + 8, center.y),
        reference, 1.5f
    );

    dl->AddLine(
        ImVec2(center.x, center.y - 8),
        ImVec2(center.x, center.y + 8),
        reference, 1.5f
    );

    // Bezel
    dl->AddCircle(center, radius, bezel, 64, 2.0f);

    ImGui::Dummy(size);
}
#define STATE_PITCH 0
#define STATE_BANK 1
#define STATE_YAW 2
#define STATE_MAINENGINE 3
#define STATE_CAMERA 4
#define STATE_ALT 5

void AxisAssignment::ToJSON(crude_json::value &out) {
    out["axis"]       = std::to_string(axis);
    out["joystick"]   = std::to_string(jsnode->id);
    out["deadzone"]   = deadzone;
    out["saturation"] = saturation;
    out["invert"]     = invert;
    out["rescale"]    = rescale;
}

bool AxisAssignment::FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId) {
    Node *node = nodeFromId(std::stoi(in["joystick"].get<std::string>()));
    jsnode     = dynamic_cast<NodeJoystick *>(node);
    if(jsnode) {
        axis       = std::stoi(in["axis"].get<std::string>());
        deadzone   = in["deadzone"].get<crude_json::number>();
        saturation = in["saturation"].get<crude_json::number>();
        invert     = in["invert"].get<bool>();
        rescale    = in["rescale"].get<bool>();
        return true;
    } else {
        return false;
    }
}

void HatAssignment::ToJSON(crude_json::value &out) {
    out["hat"] = std::to_string(hat);
    out["joystick"] = std::to_string(jsnode->id);
}

bool HatAssignment::FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId) {
    Node *node = nodeFromId(std::stoi(in["joystick"].get<std::string>()));
    jsnode     = dynamic_cast<NodeJoystick *>(node);
    if(jsnode) {
        hat = std::stoi(in["hat"].get<std::string>());
        return true;
    } else {
        return false;
    }
}


struct ButtonAssignment {
    NodeJoystick *jsnode;
    int button = 0;
};


void WizardConfig::ToJSON(crude_json::value &out) {
    if(pitch) {
        crude_json::value tmp;
        pitch->ToJSON(tmp);
        out["pitch"].push_back(tmp);
    }
    if(bank) {
        crude_json::value tmp;
        bank->ToJSON(tmp);
        out["bank"].push_back(tmp);
    }
    if(yaw) {
        crude_json::value tmp;
        yaw->ToJSON(tmp);
        out["yaw"].push_back(tmp);
    }
    if(thrusters) {
        crude_json::value tmp;
        thrusters->ToJSON(tmp);
        out["thrusters"].push_back(tmp);
    }
    if(camera) {
        crude_json::value tmp;
        camera->ToJSON(tmp);
        out["camera"].push_back(tmp);
    }
}

bool WizardConfig::FromJSON(crude_json::value &in, std::function<Node *(int)> nodeFromId) {
    if(in.contains("pitch")) {
        AxisAssignment tmp;
        if(tmp.FromJSON(in["pitch"], nodeFromId))
            pitch = tmp;
    }
    if(in.contains("bank")) {
        AxisAssignment tmp;
        if(tmp.FromJSON(in["bank"], nodeFromId))
            bank = tmp;
    }
    if(in.contains("yaw")) {
        AxisAssignment tmp;
        if(tmp.FromJSON(in["yaw"], nodeFromId))
            yaw = tmp;
    }
    if(in.contains("thrusters")) {
        AxisAssignment tmp;
        if(tmp.FromJSON(in["thrusters"], nodeFromId))
            thrusters = tmp;
    }
    if(in.contains("camera")) {
        HatAssignment tmp;
        if(tmp.FromJSON(in["camera"], nodeFromId))
            camera = tmp;
    }

    return true;
}

static float TransformAxisValue(AxisAssignment *ctx, float f)
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
    if(ctx->invert) f = -f;

    return f;
}


static void TransferFunctionEditor(ImVec2 size, const char *label, AxisAssignment *ctx, float currentValue)
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

    if (ImPlot::BeginPlot(label, size, ImPlotFlags_NoLegend)) {
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
}



#define DETECT_TIME_MS 250
#define DETECT_RANGE 10000
#define CONFIGURE_PITCH 0
#define CONFIGURE_BANK 1
#define CONFIGURE_YAW 2
#define CONFIGURE_ENGINE 3

struct AxisRange {
    int min;
    int max;
    AxisRange(int val) {
        min = val;
        max = val;
    }
    int Update(int val) {
        if(val>max) max = val;
        if(val<min) min = val;
        return max - min;
    }
};

struct Candidate {
    NodeJoystick *jsnode = NULL;
    int bestRange = 0;
    int axis;
    std::chrono::steady_clock::time_point when;
    bool found = false;

    void Update(NodeJoystick *js, int best, int ax) {
        if(found) return; // we already made our bed
        if(best < bestRange) return;
        if(best < DETECT_RANGE) return;

        // new better candidate
        if(jsnode != js || ax != axis) {
            when = std::chrono::steady_clock::now();
            jsnode = js;
            bestRange = best;
            axis = ax;
        } else {
            bestRange = best;
            auto now = std::chrono::steady_clock::now();
            auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now - when);

            if (elapsed.count() >= DETECT_TIME_MS && bestRange > DETECT_RANGE) {
                found = true;
            }
        }
    }
    bool Found() { return found; }
};

class AxisDetector {
    Candidate bestCandidate;
    std::unordered_map<NodeJoystick*, std::vector<AxisRange>> axisRanges;
public:
    AxisDetector(const std::vector<std::unique_ptr<Node>> &nodes) {
        Reset(nodes);
    }
    void Reset(const std::vector<std::unique_ptr<Node>> &nodes) {
        axisRanges.clear();
        bestCandidate = {};
        for (auto& node : nodes) {
            NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node.get());
            if (jsnode && jsnode->IsAttached()) {
                jsnode->enabled = true;
                Joystick *joy = jsnode->GetJoystick();
                assert(joy);
                auto jstate = joy->GetState();
                for(auto &val: jstate->axis) {
                    axisRanges[jsnode].emplace_back(val);
                }
            }
        }
    }

    std::optional<Candidate> Update() {
        for(auto &[jsnode, ranges] : axisRanges) {
            if (jsnode->IsAttached()) {
                Joystick *joy = jsnode->GetJoystick();
                assert(joy);
                auto jstate = joy->GetState();
                int idx = 0;
                for(auto val: jstate->axis) {
                    int idxAxis = idx++;
                    int range = ranges[idxAxis].Update(val);
                    bestCandidate.Update(jsnode, range, idxAxis);
                }
            }
        }

        if(bestCandidate.Found()) {
            return bestCandidate;
        }
        return std::nullopt;
    }
};

class ButtonDetector {
    std::vector<Node *> nodes;
    std::optional<ButtonAssignment> detected;
public:
    ButtonDetector(const std::vector<std::unique_ptr<Node>> &n) {
        Reset(n);
    }
    void Reset(const std::vector<std::unique_ptr<Node>> &n) {
        detected.reset();
        nodes.clear();
        for (auto& node : n) {
            NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node.get());
            if (jsnode && jsnode->IsAttached()) {
                nodes.push_back(jsnode);
            }
        }
    }

    std::optional<ButtonAssignment> Update() {
        if(detected) return detected;

        for (Node *node : nodes) {
            NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node);
            if (jsnode && jsnode->IsAttached()) {
                jsnode->enabled = true;
                Joystick *joy = jsnode->GetJoystick();
                assert(joy);
                auto jstate = joy->GetState();
                int idx = 0;
                for(auto val: jstate->btns) {
                    if(val) {
                        detected = {jsnode, idx};
                        return detected;
                    }
                    idx++;
                }
            }
        }

        return std::nullopt;
    }
};

class HatDetector {
    std::vector<Node *> nodes;
    std::optional<HatAssignment> detected;
public:
    HatDetector(const std::vector<std::unique_ptr<Node>> &n) {
        Reset(n);
    }
    void Reset(const std::vector<std::unique_ptr<Node>> &n) {
        detected.reset();
        nodes.clear();
        for (auto& node : n) {
            NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node.get());
            if (jsnode && jsnode->IsAttached()) {
                nodes.push_back(jsnode);
            }
        }
    }

    std::optional<HatAssignment> Update() {
        if(detected) return detected;

        for (Node *node : nodes) {
            NodeJoystick *jsnode = dynamic_cast<NodeJoystick*>(node);
            if (jsnode && jsnode->IsAttached()) {
                jsnode->enabled = true;
                Joystick *joy = jsnode->GetJoystick();
                assert(joy);
                auto jstate = joy->GetState();
                int idx = 0;
                for(auto val: jstate->hats) {
                    if(val != Pin::hat::CENTERED) {
                        detected = {jsnode, idx};
                        return detected;
                    }
                    idx++;
                }
            }
        }

        return std::nullopt;
    }
};

void ControllerGraph::DrawWizard()
{
    ImGui::Checkbox("Wizard mode", &wizardMode);
    static AxisDetector detector(nodes);
    static ButtonDetector btnDetector(nodes);
    static HatDetector hatDetector(nodes);
    static int state = 0;

    auto configureAxis = [this](ImVec2 size, const char *title, const char *axisDetectText, std::optional<AxisAssignment> &aa, int axis) {
        const ImGuiStyle& style = ImGui::GetStyle();
        float ctrlWidth = (size.x - style.ItemSpacing.x) / 2.0f;
        ImVec2 ctrlSize(ctrlWidth, ctrlWidth);

        ImGui::PushFont(NULL, style.FontSizeBase * 2.0f);
        ImGui::SeparatorText(title);
        ImGui::PopFont();
        if(aa) {
            if(ImGui::Button("Reset axis configuration")) {
                detector.Reset(nodes);
                aa.reset();
                return;
            }
            ImGui::SameLine();
            if(ImGui::Button("Invert axis")) {
                aa->invert = !aa->invert;
            }
            if(axis == CONFIGURE_ENGINE) {
                ImGui::SameLine();
                if(ImGui::Checkbox("Rescale axis", &aa->rescale)) {
                    if(aa->rescale) {
                        aa->deadzone = 0.0f;
                    }
                }
            }

            NodeJoystick *jsnode = aa->jsnode;
            Joystick *joy = jsnode->GetJoystick();
            auto *jstate = joy->GetState();
            auto normalize = [](int v) { return  v < 0 ? v / 32768.0f : v / 32767.0f; };
            float rawVal = normalize(jstate->axis[aa->axis]);
            float calibratedVal = TransformAxisValue(&*aa, rawVal);

            switch(axis) {
                case CONFIGURE_PITCH:
                    DrawADI(ctrlSize, calibratedVal, 0.0f, 0.0f);
                    break;
                case CONFIGURE_BANK:
                    DrawADI(ctrlSize, 0.0f, calibratedVal, 0.0f);
                    break;
                case CONFIGURE_YAW:
                    DrawADI(ctrlSize, 0.0f, 0.0f, calibratedVal);
                    break;
                case CONFIGURE_ENGINE:
                    DrawEngineIndicator(ctrlSize, calibratedVal);
                    break;
            }
            ImGui::SameLine();
            TransferFunctionEditor(ctrlSize, "Calibration", &*aa, rawVal);
        } else {
            ImGui::Dummy(size);
            const ImGuiStyle& style = ImGui::GetStyle();
            ImGui::PushFont(NULL, style.FontSizeBase * 2.0f);

            ImVec2 pos = ImGui::GetItemRectMin();
            ImVec2 textSize = ImGui::CalcTextSize(axisDetectText);

            ImGui::GetWindowDrawList()->AddText(pos + (size - textSize) * 0.5f, ImGui::GetColorU32(ImGuiCol_Text), axisDetectText);

            ImGui::PopFont();
            auto candidate = detector.Update();
            if(candidate) {
                constexpr float defaultDeadzone = 0.1f;
                constexpr float defaultSaturation = 1.0f;
                aa = { candidate->jsnode, candidate->axis, false, false, defaultDeadzone, defaultSaturation };
            }
        }
    };

    auto configureCamera = [this](ImVec2 size, const char *title, const char *axisDetectText, std::optional<HatAssignment> &ha) {
        const ImGuiStyle& style = ImGui::GetStyle();
        float ctrlWidth = (size.x - style.ItemSpacing.x) / 2.0f;
        ImVec2 ctrlSize(ctrlWidth, ctrlWidth);

        ImGui::PushFont(NULL, style.FontSizeBase * 2.0f);
        ImGui::SeparatorText(title);
        ImGui::PopFont();
        if(ha) {
            if(ImGui::Button("Reset camera configuration")) {
                hatDetector.Reset(nodes);
                ha.reset();
                return;
            }
//            ImGui::SameLine();
//            if(ImGui::Button("Invert axis")) {
//                ha->flip = !ha->flip;
//            }
            NodeJoystick *jsnode = ha->jsnode;
            Joystick *joy = jsnode->GetJoystick();
            auto *jstate = joy->GetState();
            Pin::hat hat = jstate->hats[ha->hat];

            static float yaw = 0.0f;
            static float pitch = 0.0f;
            if(hat & Pin::hat::DOWN)  pitch -= td.SysDT;
            if(hat & Pin::hat::UP)    pitch += td.SysDT;
            if(hat & Pin::hat::RIGHT) yaw += td.SysDT;
            if(hat & Pin::hat::LEFT)  yaw -= td.SysDT;
            DrawStarfield(ctrlSize, yaw, pitch);
        } else {
            ImGui::Dummy(size);
            const ImGuiStyle& style = ImGui::GetStyle();
            ImGui::PushFont(NULL, style.FontSizeBase * 2.0f);

            ImVec2 pos = ImGui::GetItemRectMin();
            ImVec2 textSize = ImGui::CalcTextSize(axisDetectText);

            ImGui::GetWindowDrawList()->AddText(pos + (size - textSize) * 0.5f, ImGui::GetColorU32(ImGuiCol_Text), axisDetectText);

            ImGui::PopFont();
            auto candidate = hatDetector.Update();
            if(candidate) {
                ha = { candidate->jsnode, candidate->hat, false };
            }
        }
    };

    switch(state) {
        case STATE_PITCH:
            configureAxis(ImVec2(600,300), "Pitch configuration", "Move your pitch control", wcfg.pitch, CONFIGURE_PITCH);
            break;
        case STATE_BANK:
            configureAxis(ImVec2(600,300), "Bank configuration", "Move your bank control", wcfg.bank, CONFIGURE_BANK);
            break;
        case STATE_YAW:
            configureAxis(ImVec2(600,300), "Yaw configuration", "Move your yaw control", wcfg.yaw, CONFIGURE_YAW);
            break;
        case STATE_MAINENGINE:
            configureAxis(ImVec2(600,300), "Engine throttle", "Move your thrust control", wcfg.thrusters, CONFIGURE_ENGINE);
            break;
        case STATE_CAMERA:
            configureCamera(ImVec2(600,300), "Camera configuration", "Touch the hat/dpad that controls the camera", wcfg.camera);
            break;
        case STATE_ALT:
        {
            ImGui::TextUnformatted("Alt button config");
            break;
        }
    }


    // Position navigation buttons at the bottom of the window
    const float buttonHeight = ImGui::GetFrameHeight();
    const float bottomMargin = ImGui::GetStyle().WindowPadding.y;
    float bottomY = ImGui::GetWindowHeight() - bottomMargin - buttonHeight;

    ImGui::SetCursorPosY(bottomY);


    const char *prev  = "Previous";
    const char *next  = "Next";
    const char *apply = "Confirm";

    float prevWidth  = ImGui::CalcTextSize(prev).x  + ImGui::GetStyle().FramePadding.x * 2;
    float nextWidth  = ImGui::CalcTextSize(next).x  + ImGui::GetStyle().FramePadding.x * 2;
    float applyWidth = ImGui::CalcTextSize(apply).x + ImGui::GetStyle().FramePadding.x * 2;

    float btnWidth = std::max(prevWidth, std::max(nextWidth, applyWidth)) * 2.0f;
    ImVec2 btnSize(btnWidth, ImGui::GetFrameHeight());

    float totalWidth = ImGui::GetStyle().ItemSpacing.x + btnWidth * 2.0;
    float startX = (ImGui::GetWindowWidth() - totalWidth) * 0.5f;

    ImGui::SetCursorPosX(startX);

    ImGui::BeginDisabled(state == 0);
    if(ImGui::Button(prev, btnSize)) {
        state--;
        detector.Reset(nodes);
        hatDetector.Reset(nodes);
    }
    ImGui::EndDisabled();

    ImGui::SameLine();

    if(state != STATE_ALT) {
        if(ImGui::Button(next, btnSize)) {
            state++;
            detector.Reset(nodes);
            hatDetector.Reset(nodes);
        }
    } else {
        if(ImGui::Button(apply, btnSize)) {
            CreateGraphFromWizard();
        }
    }
}

void ControllerGraph::CreateGraphFromWizard()
{
    // Start with a fresh node editor
    ed::DestroyEditor(m_Context);
    ed::Config config;
    config.SettingsFile = NULL;
    m_Context = ed::CreateEditor(&config);
    ed::SetCurrentEditor(m_Context);

    // Remove Nodes that are neither NodeJoysticks or unique nodes
    nodes.erase(
        std::remove_if(nodes.begin(), nodes.end(),
            [this](const std::unique_ptr<Node>& node) {
                if (dynamic_cast<NodeJoystick*>(node.get()))
                    return false;

                auto it = nodeFactory.types.find(node->GetTypeName());
                return it == nodeFactory.types.end() || !it->second.isSingleton;
            }),
        nodes.end()
    );

    // Reset position of remaining nodes
    for(auto &node: nodes) {
        node->placed = false;
    }

    // Recreate links
    links.clear();

    auto linkJoyPin = [this](std::optional<AxisAssignment> &aa, const char *inputPin) {
        if(aa) {
            NodeJoystick *jsnode = aa->jsnode;
            int axis = aa->axis;
            Pin *output = jsnode->PinFromAxis(axis);
            assert(output);
            output->context.deadzone   = aa->deadzone;
            output->context.saturation = aa->saturation;
            output->context.rescale    = aa->rescale;
            output->context.flip       = aa->invert;

            Node *n = GetNodeOfType("NodeLegacyControls");
            assert(n);
            NodeLegacyControls *legacy = dynamic_cast<NodeLegacyControls *>(n);
            assert(legacy);
            Pin *input = legacy->GetInputPinFromName(inputPin);
            assert(input);
            links.push_back({ ed::LinkId(linkidgen++), input->id, output->id });
        }
    };

    auto linkCameraPin = [this](std::optional<HatAssignment> &ha, const char *inputPin) {
        if(ha) {
            NodeJoystick *jsnode = ha->jsnode;
            int hat = ha->hat;
            Pin *output = jsnode->PinFromHat(hat);
            assert(output);

            Node *n = GetNodeOfType("NodeLegacyControls");
            assert(n);
            NodeLegacyControls *legacy = dynamic_cast<NodeLegacyControls *>(n);
            assert(legacy);
            Pin *input = legacy->GetInputPinFromName(inputPin);
            assert(input);
            links.push_back({ ed::LinkId(linkidgen++), input->id, output->id });
        }
    };


    linkJoyPin(wcfg.pitch,     "PitchVert");
    linkJoyPin(wcfg.bank,      "BankYaw");
    linkJoyPin(wcfg.yaw,       "YawLat");
    linkJoyPin(wcfg.thrusters, "MainEngine");
    linkCameraPin(wcfg.camera, "CamPanel");

    dirty = true;
    TopologicalSort();
}
