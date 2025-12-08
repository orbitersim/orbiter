// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//                  ORBITER MODULE: ExtMFD
//                  Part of the ORBITER SDK
//            Copyright (C) 2006 Martin Schweiger
//                   All rights reserved
//
// ExtMFD.cpp
//
// Open multifunctional displays (MFD) in external windows
// ==============================================================

#define ORBITER_MODULE
#include "orbitersdk.h"
#include "imgui.h"
#include "imgui_extras.h"
#include <vector>
#include <string>
#include <iomanip>
#include <sstream>
#include "IconsFontAwesome6.h"

class ImGuiNote;
static std::vector<ImGuiNote *> g_notes;
static int g_cnt;

constexpr size_t MAX_SIZE = 1024;
class ImGuiNote: public ImGuiDialog {
	char buf[MAX_SIZE];
	float scale = 2.0f;

	// Dear ImGui will save the window's settings in imgui.ini, associated with the window's name
	// Since the file is global, we need unique names across scenarios -> we use the current date
	// A counter is still needed to prevent collisions when creating more than one note a second
	static std::string GetID() {
		std::time_t now = std::time(nullptr);
		struct tm tm = *std::localtime(&now);

		std::ostringstream oss;
		oss << std::put_time(&tm, "###Note%Y-%m-%d %H-%M-%S-") << g_cnt++;
		return oss.str();
	}

	// Private constructor used when loading a scenario file
	ImGuiNote(const char *name, const char *data, float s):ImGuiDialog(name) {
		g_notes.push_back(this);
		buf[MAX_SIZE - 1] = '\0';
		strncpy(buf, data, MAX_SIZE - 1);
		scale =s;
	}
public:
	ImGuiNote():ImGuiDialog(GetID().c_str()) {
		g_notes.push_back(this);
		buf[0] = '\0';
	}

	virtual ~ImGuiNote() {
		g_notes.erase(std::remove_if(g_notes.begin(), g_notes.end(),
                              [this](ImGuiNote *n) { return n == this; }));
	}

	void Display() override {
		ImGui::SetNextWindowSize(ImVec2(defaultSize.width, defaultSize.height), ImGuiCond_FirstUseEver);

		const ImVec4 bgColor = ImVec4(1.0f, 1.0f, 0.0f, 1.0f); // Yellow
		const ImVec4 txtColor = ImVec4(0.0f, 0.0f, 0.0f, 1.0f); // Black

        ImGui::PushStyleColor(ImGuiCol_WindowBg, bgColor);
        ImGui::PushStyleColor(ImGuiCol_FrameBg, bgColor);
        ImGui::PushStyleColor(ImGuiCol_TitleBg, bgColor);
        ImGui::PushStyleColor(ImGuiCol_TitleBgActive, bgColor);
        ImGui::PushStyleColor(ImGuiCol_TitleBgCollapsed, bgColor);
        ImGui::PushStyleColor(ImGuiCol_Text, txtColor);
        ImGui::PushStyleColor(ImGuiCol_InputTextCursor, txtColor);
        ImGui::PushStyleColor(ImGuiCol_ButtonHovered, bgColor);
        ImGui::PushStyleColor(ImGuiCol_ButtonActive, bgColor);
        ImGui::PushStyleColor(ImGuiCol_Border, bgColor);
				
		if(ImGui::Begin(name.c_str(), &active)) {
			ImGui::PushFont(ImGuiFont::MANUSCRIPT, scale);
			ImGui::InputTextMultiline("##note", buf, 1024, ImVec2(-FLT_MIN,-FLT_MIN), ImGuiInputTextFlags_NoHorizontalScroll);
			ImGui::PopFont();
			ImGui::PushStyleColor(ImGuiCol_PopupBg, bgColor);
			ImGui::PushStyleColor(ImGuiCol_FrameBgHovered, bgColor);
			ImGui::PushStyleColor(ImGuiCol_FrameBgActive, bgColor);
			ImGui::PushStyleColor(ImGuiCol_SliderGrab, txtColor);
			ImGui::PushStyleColor(ImGuiCol_SliderGrabActive, txtColor);

			if (ImGui::BeginPopupContextItem("Text scale"))
			{
				ImGui::SliderFloat("###Scale", &scale, 0.1, 10.0, "%0.1f");
				ImGui::EndPopup();
			}
			ImGui::PopStyleColor(5);
		}
		ImGui::End();

		ImGui::PopStyleColor(10);
		if (!active) OnClose();
	}

	void OnDraw() override {
		// Already handled via Display()
	}

	void OnClose() override {
		delete this;
	}

	// Serialization
	void Save(FILEHANDLE scn) {
		const char hex_lut[]={'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};
		oapiWriteLine(scn, "BEGIN_NOTE");
		oapiWriteItem_string(scn, "NAME", const_cast<char *>(name.c_str()));
		oapiWriteItem_float(scn, "SCALE", scale);

		size_t len = strlen(buf);
		// Lines cannot exceed 256 characters so we split the text
		for(size_t offset = 0; offset < len; offset += 64) {
			char out[256] = "DATA ";
			char *ptr = out + 5;
			size_t block_size = std::min(64u, len - offset);
			// Save in hexa to prevent problems with special characters and the like
			for(size_t i = 0; i < block_size; i++) {
				char c = buf[i + offset];
				*ptr++ = hex_lut[(c >> 4) & 0xf];
				*ptr++ = hex_lut[c & 0xf];
				*ptr++ = ' ';
			}
			oapiWriteLine(scn, out);
		}
		oapiWriteLine(scn, "END_NOTE");
	}

	static ImGuiNote *Load(FILEHANDLE scn) {
		// Note: the BEGIN_NOTE tag is already consummed in opcLoadState
		std::string name = "DEFAULT_NAME";

		char data[MAX_SIZE];
		data[MAX_SIZE-1] = '\0';
		char *data_in = data;
		char *line;
		double scale = 2.0;
		while (oapiReadScenario_nextline(scn, line)) {
			if (!strnicmp (line, "NAME = ", 7)) {
				name = line + 7;
			} else if (!strnicmp (line, "SCALE = ", 8)) {
				scale = atof(line + 8);
			} else if (!stricmp (line, "END_NOTE")) {
				break;
			} else if (!strnicmp (line, "DATA ", 5)) {
				char *hexstream = line + 5;
				char *next;
				do {
					char c = strtoul (hexstream, &next, 16);
					if(data_in - data < MAX_SIZE - 1) {
						*data_in++ = c;
					}
					hexstream = next;
				} while(*next);
			}
		}
		*data_in++ = '\0';

		return new ImGuiNote(name.c_str(), data, scale);
	}
};


// ==============================================================
// Global variables
// ==============================================================

static DWORD g_dwCmd;        // custom function identifier
static int g_dwMenuCmd;

// ==============================================================
// Local prototypes
// ==============================================================

void OpenDlgClbk (void *context);

// ==============================================================
// API interface
// ==============================================================

// ==============================================================
// This function is called when Orbiter starts or when the module
// is activated.

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	// To allow the user to open our new dialog box, we create
	// an entry in the "Custom Functions" list which is accessed
	// in Orbiter via Ctrl-F4.
	g_dwCmd = oapiRegisterCustomCmd ((char*)"Notes",
		(char*)"Opens an onscreen note",
		OpenDlgClbk, NULL);

	g_dwMenuCmd = oapiRegisterCustomMenuCmd ("Note", "MenuInfoBar/Notes.png", OpenDlgClbk, NULL);
}

// ==============================================================
// This function is called when Orbiter shuts down or when the
// module is deactivated

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (g_dwCmd);
	oapiUnregisterCustomMenuCmd (g_dwMenuCmd);
}


// ==============================================================
// Write some parameters to the scenario file

DLLCLBK void opcSaveState (FILEHANDLE scn)
{
	for(ImGuiNote *note: g_notes) {
		note->Save(scn);
	}
}

DLLCLBK void opcCloseRenderViewport()
{
	g_notes.clear();
}

// ==============================================================
// Read custom parameters from scenario

DLLCLBK void opcLoadState (FILEHANDLE scn)
{
	char *line;
	while (oapiReadScenario_nextline (scn, line)) {
		if (!stricmp (line, "BEGIN_NOTE")) {
			oapiOpenDialog(ImGuiNote::Load(scn));
		}
	}
}

// ==============================================================
// Open the dialog window

static void OpenDlgClbk (void *context)
{
	ImGuiNote *note = new ImGuiNote();
	oapiOpenDialog(note);
}
