// Adapted fron from https://github.com/thedmd/imgui-node-editor/blob/master/examples/blueprints-example/utilities/drawing.cpp
// Added trigger and plus icons
/*
MIT License

Copyright (c) 2019 Michał Cichoń

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include "Icon.h"
#include <imgui_internal.h>
void DrawHat(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, Pin::hat hat, ImU32 color);
void DrawAxisGauge(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, float value, ImU32 color);
void DrawIcon(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, IconType type, bool filled, ImU32 color, ImU32 innerColor);

void Icon(const ImVec2& size, IconType type, bool filled, const ImVec4& color/* = ImVec4(1, 1, 1, 1)*/, const ImVec4& innerColor/* = ImVec4(0, 0, 0, 0)*/)
{
    if (ImGui::IsRectVisible(size))
    {
        auto cursorPos = ImGui::GetCursorScreenPos();
        auto drawList  = ImGui::GetWindowDrawList();
        DrawIcon(drawList, cursorPos, cursorPos + size, type, filled, ImColor(color), ImColor(innerColor));
    }

    ImGui::Dummy(size);
}


void Axis(const ImVec2& size, float value, const ImVec4& color) {

    if (ImGui::IsRectVisible(size))
    {
        auto cursorPos = ImGui::GetCursorScreenPos();
        auto drawList  = ImGui::GetWindowDrawList();
        DrawAxisGauge(drawList, cursorPos, cursorPos + size, value, ImColor(color));
    }

    ImGui::Dummy(size);
}

void Hat(const ImVec2& size, Pin::hat hat, const ImVec4& color) {

    if (ImGui::IsRectVisible(size))
    {
        auto cursorPos = ImGui::GetCursorScreenPos();
        auto drawList  = ImGui::GetWindowDrawList();
        DrawHat(drawList, cursorPos, cursorPos + size, hat, ImColor(color));
    }

    ImGui::Dummy(size);
}

void DrawIcon(ImDrawList* drawList, const ImVec2& a, const ImVec2& b, IconType type, bool filled, ImU32 color, ImU32 innerColor)
{
          auto rect           = ImRect(a, b);
          auto rect_x         = rect.Min.x;
          auto rect_y         = rect.Min.y;
          auto rect_w         = rect.Max.x - rect.Min.x;
          auto rect_h         = rect.Max.y - rect.Min.y;
          auto rect_center_x  = (rect.Min.x + rect.Max.x) * 0.5f;
          auto rect_center_y  = (rect.Min.y + rect.Max.y) * 0.5f;
          auto rect_center    = ImVec2(rect_center_x, rect_center_y);
    const auto outline_scale  = rect_w / 24.0f;
    const auto extra_segments = static_cast<int>(2 * outline_scale); // for full circle

    if (type == IconType::Flow)
    {
        const auto origin_scale = rect_w / 24.0f;

        const auto offset_x  = 1.0f * origin_scale;
        const auto offset_y  = 0.0f * origin_scale;
        const auto margin     = (filled ? 2.0f : 2.0f) * origin_scale;
        const auto rounding   = 0.1f * origin_scale;
        const auto tip_round  = 0.7f; // percentage of triangle edge (for tip)
        //const auto edge_round = 0.7f; // percentage of triangle edge (for corner)
        const auto canvas = ImRect(
            rect.Min.x + margin + offset_x,
            rect.Min.y + margin + offset_y,
            rect.Max.x - margin + offset_x,
            rect.Max.y - margin + offset_y);
        const auto canvas_x = canvas.Min.x;
        const auto canvas_y = canvas.Min.y;
        const auto canvas_w = canvas.Max.x - canvas.Min.x;
        const auto canvas_h = canvas.Max.y - canvas.Min.y;

        const auto left   = canvas_x + canvas_w            * 0.5f * 0.3f;
        const auto right  = canvas_x + canvas_w - canvas_w * 0.5f * 0.3f;
        const auto top    = canvas_y + canvas_h            * 0.5f * 0.2f;
        const auto bottom = canvas_y + canvas_h - canvas_h * 0.5f * 0.2f;
        const auto center_y = (top + bottom) * 0.5f;
        //const auto angle = AX_PI * 0.5f * 0.5f * 0.5f;

        const auto tip_top    = ImVec2(canvas_x + canvas_w * 0.5f, top);
        const auto tip_right  = ImVec2(right, center_y);
        const auto tip_bottom = ImVec2(canvas_x + canvas_w * 0.5f, bottom);

        drawList->PathLineTo(ImVec2(left, top) + ImVec2(0, rounding));
        drawList->PathBezierCubicCurveTo(
            ImVec2(left, top),
            ImVec2(left, top),
            ImVec2(left, top) + ImVec2(rounding, 0));
        drawList->PathLineTo(tip_top);
        drawList->PathLineTo(tip_top + (tip_right - tip_top) * tip_round);
        drawList->PathBezierCubicCurveTo(
            tip_right,
            tip_right,
            tip_bottom + (tip_right - tip_bottom) * tip_round);
        drawList->PathLineTo(tip_bottom);
        drawList->PathLineTo(ImVec2(left, bottom) + ImVec2(rounding, 0));
        drawList->PathBezierCubicCurveTo(
            ImVec2(left, bottom),
            ImVec2(left, bottom),
            ImVec2(left, bottom) - ImVec2(0, rounding));

        if (!filled)
        {
            if (innerColor & 0xFF000000)
                drawList->AddConvexPolyFilled(drawList->_Path.Data, drawList->_Path.Size, innerColor);

            drawList->PathStroke(color, true, 2.0f * outline_scale);
        }
        else
            drawList->PathFillConvex(color);
    }
    else
    {
        auto rect_offset = -static_cast<int>(rect_w * 0.25f * 0.25f);

        rect.Min.x    += rect_offset;
        rect.Max.x    += rect_offset;
        rect_x        += rect_offset;
        rect_center_x += rect_offset * 0.5f;
        rect_center.x += rect_offset * 0.5f;

        if (type == IconType::Circle)
        {
            const auto c = rect_center;

            if (!filled)
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;

                if (innerColor & 0xFF000000)
                    drawList->AddCircleFilled(c, r, innerColor, 12 + extra_segments);
                drawList->AddCircle(c, r, color, 12 + extra_segments, 2.0f * outline_scale);
            }
            else
            {
                drawList->AddCircleFilled(c, 0.5f * rect_w / 2.0f, color, 12 + extra_segments);
            }
        }

        if (type == IconType::Square)
        {
            if (filled)
            {
                const auto r  = 0.5f * rect_w / 2.0f;
                const auto p0 = rect_center - ImVec2(r, r);
                const auto p1 = rect_center + ImVec2(r, r);

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRectFilled(p0, p1, color, 0, ImDrawFlags_RoundCornersAll);
#else
                drawList->AddRectFilled(p0, p1, color, 0, 15);
#endif
            }
            else
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;
                const auto p0 = rect_center - ImVec2(r, r);
                const auto p1 = rect_center + ImVec2(r, r);

                if (innerColor & 0xFF000000)
                {
#if IMGUI_VERSION_NUM > 18101
                    drawList->AddRectFilled(p0, p1, innerColor, 0, ImDrawFlags_RoundCornersAll);
#else
                    drawList->AddRectFilled(p0, p1, innerColor, 0, 15);
#endif
                }

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRect(p0, p1, color, 0, ImDrawFlags_RoundCornersAll, 2.0f * outline_scale);
#else
                drawList->AddRect(p0, p1, color, 0, 15, 2.0f * outline_scale);
#endif
            }
        }

        if (type == IconType::Grid)
        {
            const auto r = 0.5f * rect_w / 2.0f;
            const auto w = ceilf(r / 3.0f);

            const auto baseTl = ImVec2(floorf(rect_center_x - w * 2.5f), floorf(rect_center_y - w * 2.5f));
            const auto baseBr = ImVec2(floorf(baseTl.x + w), floorf(baseTl.y + w));

            auto tl = baseTl;
            auto br = baseBr;
            for (int i = 0; i < 3; ++i)
            {
                tl.x = baseTl.x;
                br.x = baseBr.x;
                drawList->AddRectFilled(tl, br, color);
                tl.x += w * 2;
                br.x += w * 2;
                if (i != 1 || filled)
                    drawList->AddRectFilled(tl, br, color);
                tl.x += w * 2;
                br.x += w * 2;
                drawList->AddRectFilled(tl, br, color);

                tl.y += w * 2;
                br.y += w * 2;
            }
        }

        if (type == IconType::RoundSquare)
        {
            if (filled)
            {
                const auto r  = 0.5f * rect_w / 2.0f;
                const auto cr = r * 0.5f;
                const auto p0 = rect_center - ImVec2(r, r);
                const auto p1 = rect_center + ImVec2(r, r);

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRectFilled(p0, p1, color, cr, ImDrawFlags_RoundCornersAll);
#else
                drawList->AddRectFilled(p0, p1, color, cr, 15);
#endif
            }
            else
            {
                const auto r = 0.5f * rect_w / 2.0f - 0.5f;
                const auto cr = r * 0.5f;
                const auto p0 = rect_center - ImVec2(r, r);
                const auto p1 = rect_center + ImVec2(r, r);

                if (innerColor & 0xFF000000)
                {
#if IMGUI_VERSION_NUM > 18101
                    drawList->AddRectFilled(p0, p1, innerColor, cr, ImDrawFlags_RoundCornersAll);
#else
                    drawList->AddRectFilled(p0, p1, innerColor, cr, 15);
#endif
                }

#if IMGUI_VERSION_NUM > 18101
                drawList->AddRect(p0, p1, color, cr, ImDrawFlags_RoundCornersAll, 2.0f * outline_scale);
#else
                drawList->AddRect(p0, p1, color, cr, 15, 2.0f * outline_scale);
#endif
            }
        }
        else if (type == IconType::Diamond)
        {
            if (filled)
            {
                const auto r = 0.607f * rect_w / 2.0f;
                const auto c = rect_center;

                drawList->PathLineTo(c + ImVec2( 0, -r));
                drawList->PathLineTo(c + ImVec2( r,  0));
                drawList->PathLineTo(c + ImVec2( 0,  r));
                drawList->PathLineTo(c + ImVec2(-r,  0));
                drawList->PathFillConvex(color);
            }
            else
            {
                const auto r = 0.607f * rect_w / 2.0f - 0.5f;
                const auto c = rect_center;

                drawList->PathLineTo(c + ImVec2( 0, -r));
                drawList->PathLineTo(c + ImVec2( r,  0));
                drawList->PathLineTo(c + ImVec2( 0,  r));
                drawList->PathLineTo(c + ImVec2(-r,  0));

                if (innerColor & 0xFF000000)
                    drawList->AddConvexPolyFilled(drawList->_Path.Data, drawList->_Path.Size, innerColor);

                drawList->PathStroke(color, ImDrawFlags_Closed, 2.0f * outline_scale);
            }
        }
        else if (type == IconType::Trigger)
        {
            const auto r = 0.607f * rect_w / 2.0f - 0.5f;
            const auto c = rect_center;

            drawList->PathLineTo(c + ImVec2( -r,  r));
            drawList->PathLineTo(c + ImVec2(  0,  r));
            drawList->PathLineTo(c + ImVec2(  0, -r));
            drawList->PathLineTo(c + ImVec2(  r, -r));

            drawList->PathStroke(color, ImDrawFlags_None, 2.0f * outline_scale);
        }
        else if (type == IconType::Plus)
        {
            const auto r = 0.7f * rect_w / 2.0f;
            const float thickness = 1.0;
            const float margin = 4.0;

            drawList->AddCircleFilled(rect_center, r, color, 12 + extra_segments);

            ImVec2 tl{a.x + margin, a.y + rect_h/2.0f - thickness };
            ImVec2 br{b.x - margin, b.y - rect_h/2.0f + thickness };
            drawList->AddRectFilled(tl, br, innerColor);

            ImVec2 tlv{a.x + rect_w/2.0f - thickness , a.y + margin};
            ImVec2 brv{b.x - rect_w/2.0f + thickness , b.y - margin};
            drawList->AddRectFilled(tlv, brv, innerColor);
        }
        else if (type == IconType::ButtonUp || type == IconType::ButtonDown)
        {
            const bool pressed = type == IconType::ButtonDown;
            const float s = rect_w / 24.0f;
            const ImVec2 c = rect_center;

            const float rx = 9.0f * s;
            const float ry = 3.0f * s;

            const float baseY = c.y + 2.0f * s;
            const float capY = c.y + (pressed ? -1.0f : -5.0f) * s;
            const float capH = 5.0f * s;

            // Mounting base
            drawList->AddRectFilled(
                ImVec2(c.x - rx, baseY),
                ImVec2(c.x + rx, baseY + 5.0f * s),
                IM_COL32(90, 90, 90, 255));

            // Button stem
            drawList->AddRectFilled(
                ImVec2(c.x - 6.0f * s, capY),
                ImVec2(c.x + 6.0f * s, capY + capH),
                color);

            // Button top
            drawList->AddEllipseFilled(
                ImVec2(c.x, capY),
                ImVec2(6.0f * s, ry),
                color);
        }
    }
}


void DrawAxisGauge(ImDrawList* drawList, const ImVec2& a, const ImVec2& b,
                   float value, ImU32 color)
{
    value = ImClamp(value, -1.0f, 1.0f);

    const float cy = (a.y + b.y) * 0.5f;
    const float w = b.x - a.x;
    const float h = b.y - a.y;

    const float margin = w * 0.15f;
    const float left = a.x + margin;
    const float right = b.x - margin;
    const float center = (left + right) * 0.5f;

    // Track
    drawList->AddRectFilled(
        ImVec2(left, cy - h * 0.12f),
        ImVec2(right, cy + h * 0.12f),
        IM_COL32(70, 70, 70, 255),
        h * 0.12f);

    // Neutral position
    drawList->AddLine(
        ImVec2(center, cy - h * 0.25f),
        ImVec2(center, cy + h * 0.25f),
        IM_COL32(160, 160, 160, 255),
        1.0f);

    // Position indicator
    const float x = center + value * (right - left) * 0.5f;

    drawList->AddCircleFilled(
        ImVec2(x, cy),
        h * 0.22f,
        color);
}

void DrawHat(ImDrawList* drawList, const ImVec2& a, const ImVec2& b,
             Pin::hat hat, ImU32 color)
{
    const ImVec2 c = (a + b) * 0.5f;
    const float size = b.x - a.x;
    const float r = size * 0.32f;
    const float dotR = size * 0.10f;

    const ImU32 inactive = IM_COL32(85, 85, 85, 255);

    const float d = 0.70710678f;

    const ImVec2 dirs[] =
    {
        ImVec2( 0, -1),
        ImVec2( d, -d),
        ImVec2( 1,  0),
        ImVec2( d,  d),
        ImVec2( 0,  1),
        ImVec2(-d,  d),
        ImVec2(-1,  0),
        ImVec2(-d, -d)
    };

    const Pin::hat masks[] =
    {
        Pin::hat::UP,
        Pin::hat::RIGHTUP,
        Pin::hat::RIGHT,
        Pin::hat::RIGHTDOWN,
        Pin::hat::DOWN,
        Pin::hat::LEFTDOWN,
        Pin::hat::LEFT,
        Pin::hat::LEFTUP
    };

    for (int i = 0; i < 8; ++i)
    {
        const ImVec2 p = c + dirs[i] * r;
        drawList->AddCircleFilled(p, dotR, hat == masks[i] ? color : inactive);
    }

    drawList->AddCircleFilled(c, dotR, hat == Pin::hat::CENTERED ? color : inactive);
}
