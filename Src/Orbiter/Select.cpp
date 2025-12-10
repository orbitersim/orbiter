// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Select.h"
#include "imgui.h"
#include "imgui_extras.h"

Select::Select():ImGuiDialog("Select") {
    active = false;
    opened = false;
    title = "Selection";
    currententry = nullptr;
}

void Select::Open(const char *_title, Callbk submenu_cbk, Callbk enter_cbk, void *_userdata, int cntx, int cnty) {
    title = _title;

    cbSubmenu = submenu_cbk;
    cbEnter = enter_cbk;
    userdata = _userdata;

    rootmenu.clear();
    currententry = &rootmenu;

    submenu_cbk(this, 0, nullptr, _userdata);
    opened = true;
    active = true;
}

void Select::Append(const char *str, int flags) {
    SelectEntry e;
    e.m_Flags = flags;
    e.m_Text = str;

    currententry->emplace_back(e);
}

void Select::AppendSeparator() {
    SelectEntry e;
    e.m_Flags = ITEM_SEPARATOR;

    currententry->emplace_back(e);
}

void Select::DrawMenu(std::list<SelectEntry>& entries) {
    int i = 0;
    for (auto& e : entries) {
        if (e.m_Flags & ITEM_SEPARATOR) {
            ImGui::Separator();
        }
        else if (e.m_Flags & ITEM_SUBMENU) {
            if (e.m_SubEntries.size() == 0) {
                currententry = &e.m_SubEntries;
                cbSubmenu(this, i, const_cast<char*>(e.m_Text.c_str()), userdata);
            }
            if (ImGui::BeginMenu(e.m_Text.c_str(), e.m_SubEntries.size() != 0)) {
                DrawMenu(e.m_SubEntries);
                ImGui::EndMenu();
                if (ImGui::IsItemHovered(ImGuiHoveredFlags_RectOnly) && !(e.m_Flags & ITEM_NOHILIGHT)) {
                    if (ImGui::IsMouseReleased(0)) {
                        cbEnter(this, i, const_cast<char*>(e.m_Text.c_str()), userdata);
                        active = false;
                        ImGui::CloseCurrentPopup();
                    }
                }
            }
            i++;
        }
        else {
            if (ImGui::MenuItem(e.m_Text.c_str())) {
                cbEnter(this, i, const_cast<char*>(e.m_Text.c_str()), userdata);
                active = false;
            }
            i++;
        }
    }
}

void Select::OnDraw() {
    ImGuiIO& io = ImGui::GetIO();
	
    if (io.MouseDown[0] && opened) {
        return;
    }

	// Open the context menu inside the focused viewport
	// so it's not hidden by it in case it's opened from
	// a secondary viewport but its draw area fits inside
	// the main viewport
	ImGuiPlatformIO& pio = ImGui::GetPlatformIO();
	for (ImGuiViewport *viewport : pio.Viewports) {
		if(viewport->Flags & ImGuiViewportFlags_IsFocused) {
			ImGui::SetNextWindowViewport(viewport->ID);
			break;
		}
	}

    if (opened) {
        ImGui::OpenPopup(title.c_str());
        opened = false;
    }

    if (ImGui::BeginAnimatedPopup(title.c_str()))
    {
        ImGui::TextUnformatted(title.c_str());
        ImGui::Separator();
        DrawMenu(rootmenu);
        ImGui::EndAnimatedPopup();
    }
    else {
        active = false;
    }
}

InputBox::InputBox():ImGuiDialog("InputBox") {
    active = false;
    opened = false;
    title = "InputBox";
}

void InputBox::Open(const char *_title, char *_buf, int _vislen,
    Callbk cbk, void *_userdata, int cntx, int cnty)
{
    OpenEx(_title, _buf, _vislen, cbk, 0, _userdata, 0, cntx, cnty);
}

bool InputBox::OpenEx(const char *_title, char *_buf, int _vislen,
    Callbk enter_cbk, Callbk cancel_cbk, void *_userdata,
    int flags, int cntx, int cnty) {

    title = _title;

    cbEnter = enter_cbk;
    cbCancel = cancel_cbk;
    userdata = _userdata;

    opened = true;
    active = true;

    if (_buf)
        strcpy(inputbuf, _buf);
    else
        inputbuf[0] = '\0';

    return true;
}

void InputBox::OnDraw() {
    char buf[256];
    sprintf(buf, "%s###InputBox", title.c_str());

    bool firstTime = false;
    if (opened) {
        ImGui::OpenPopup(buf);
        opened = false;
        firstTime = true;
    }

    if (ImGui::BeginAnimatedPopup(buf))
    {
        ImGui::TextUnformatted(title.c_str());
        ImGui::Separator();
        ImGui::SetNextItemWidth(-FLT_MIN);
        if (firstTime)
            ImGui::SetKeyboardFocusHere();
        bool entered = ImGui::InputText("##InputText", inputbuf, IM_ARRAYSIZE(inputbuf), ImGuiInputTextFlags_EnterReturnsTrue);

        if (ImGui::Button("OK") || entered) {
            cbEnter(this, inputbuf, userdata);
            ImGui::CloseCurrentPopup();
            active = false;
        }
        ImGui::SameLine();
        if (ImGui::Button("Cancel")) {
            if (cbCancel)
                cbCancel(this, inputbuf, userdata);
            ImGui::CloseCurrentPopup();
            active = false;
        }
        ImGui::EndAnimatedPopup();
    }
    else {
        active = false;
    }
}
