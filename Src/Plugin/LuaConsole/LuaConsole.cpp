// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define ORBITER_MODULE
#include "Orbitersdk.h"
#include "LuaConsole.h"
#include <deque>
#include <string>
#include "imgui.h"
#include "imgui_extras.h"
#include "IconsFontAwesome6.h"
#include <sstream>
#include <process.h>

using std::min;
using std::max;
using namespace oapi;


// ==============================================================
class LuaConsoleDlg: public ImGuiDialog {
	const size_t MAX_HISTORY = 100;
	const size_t MAX_LINES = 10000;


	struct LineData {
		std::string text;
		LineType type;
		LineData(std::string &&s, LineType t):text(s), type(t) {}
	};
	std::deque<LineData> lines;
	std::deque<std::string> history;
	int idx_history;
	char cmd[4096];
	ImGuiInputTextFlags flags;
	char *cConsoleCmd;
public:
	LuaConsoleDlg(char *cmdbuf):ImGuiDialog("Lua Terminal"){
		cConsoleCmd = cmdbuf;
		cmd[0] = '\0';
		flags = ImGuiInputTextFlags_EnterReturnsTrue | ImGuiInputTextFlags_CtrlEnterForNewLine;
		idx_history = -1;
	}


	void AddLine(const char *str, LineType type = LineType::LUA_OUT) {
		std::stringstream ss(str);
		std::string line;

		if(str[0]=='\0')
			lines.emplace_back("", type);
		else while(std::getline(ss,line)) {
			lines.emplace_back(std::move(line), type);
		}

		while(lines.size() > MAX_LINES) {
			lines.pop_front();
		}
	}

	void ExecuteCommandBuffer() {
		history.push_back(cmd);
		AddLine(cmd, LineType::LUA_IN);
		strcpy(cConsoleCmd, cmd);
		cmd[0] = '\0';

		if(history.size() > MAX_HISTORY) {
			history.pop_front();
		}

		idx_history = history.size() - 1;
	}

	void HistoryClear() {
		history.clear();
		idx_history = 0;
	}

	void HistoryPrev() {
		if(history.size() == 0) return;
		if(idx_history >= 0) {
			strncpy(cmd, history[idx_history].c_str(), sizeof(cmd));
			idx_history--;
			if(idx_history < 0) idx_history = 0;
		}
	}

	void HistoryNext() {
		if(history.size() == 0) return;
		if(idx_history < history.size()) {
			strncpy(cmd, history[idx_history].c_str(), sizeof(cmd));
			idx_history++;
			if(idx_history >= history.size()) idx_history = history.size() - 1;
		}
	}
	void Clear() {
		lines.clear();
	}
	void Display() {
		ImGui::SetNextWindowSize(ImVec2(800,600));
		bool visible = ImGui::Begin("Lua Console", &active);
		if(ImGui::MenuButton(ICON_FA_TRASH, "Clear console")) {
			lines.clear();
		}
		if(visible) {
			OnDraw();
		}
		ImGui::End();
		if (!active) OnClose();
	}

	void DrawConsole() {
		ImGui::SetNextWindowSize(ImVec2(0,440));
		ImGui::BeginChild("##LuaConsole", ImVec2(0.0f, 0.0f), ImGuiChildFlags_ResizeY, ImGuiWindowFlags_AlwaysHorizontalScrollbar | ImGuiWindowFlags_AlwaysVerticalScrollbar);
			ImGui::PushFont(ImGuiFont::MONO);
			ImGuiListClipper clipper;
			clipper.Begin(lines.size());
			while (clipper.Step()) {
				for (int line_no = clipper.DisplayStart; line_no < clipper.DisplayEnd; line_no++) {
					LineType type = lines[line_no].type;

					switch(type) {
						case LineType::LUA_IN: 
							ImGui::Text("%%%s", lines[line_no].text.c_str());
							break;
						case LineType::LUA_OUT:
							ImGui::TextColored(ImVec4(0,1,0,1), " %s", lines[line_no].text.c_str());
							break;
						case LineType::LUA_OUT_ERROR:
							ImGui::TextColored(ImVec4(1,0,0,1), " %s", lines[line_no].text.c_str());
							break;
					}
				}
			}
			ImGui::PopFont();
			if (ImGui::GetScrollY() >= ImGui::GetScrollMaxY())
				ImGui::SetScrollHereY(1.0f);
		ImGui::EndChild();
	}
	
	void DrawInput() {
		ImGui::BeginChild("##LuaInput");
			ImGui::PushFont(ImGuiFont::MONO);
			if(ImGui::InputTextMultiline("##LuaPrompt", cmd, sizeof(cmd), ImVec2(0,0), flags)) {
				ImGui::SetKeyboardFocusHere(-1);
				ExecuteCommandBuffer();
			}
			ImGui::PopFont();

			ImGui::SetItemDefaultFocus(); // This does not seem to work...
			
			ImGui::SameLine();
			ImGui::BeginChild("##LuaTermCommands");
				if(ImGui::Button(ICON_FA_PLAY)) {
					ExecuteCommandBuffer();
				}
				ImGui::SetItemTooltip("Execute the command buffer");
				ImGui::SameLine();
				ImGui::CheckboxFlags("Single line", &flags, ImGuiInputTextFlags_CtrlEnterForNewLine);
				
				ImGui::SetItemTooltip("When checked, the command buffer is sent when pressing Enter\n"
									"Otherwise, you can enter multiple lines at once and execute\n"
									"them with Ctrl-Enter or the " ICON_FA_PLAY " button");

				ImGui::BeginGroupPanel("History", ImVec2(0,0));
					if(ImGui::Button(ICON_FA_ARROW_UP)) {
						HistoryPrev();
					}
					ImGui::SetItemTooltip("Previous command");

					ImGui::SameLine();
					if(ImGui::Button(ICON_FA_ARROW_DOWN)) {
						HistoryNext();
					}
					ImGui::SetItemTooltip("Next command");

					ImGui::SameLine();
					if(ImGui::Button(ICON_FA_TRASH)) {
						HistoryClear();
					}
					ImGui::SetItemTooltip("Clear history");
				ImGui::EndGroupPanel();
				
			ImGui::EndChild();
			
		ImGui::EndChild();
	}

	void OnDraw() {
		DrawConsole();
		DrawInput();
	}
};

// ==============================================================
// Global parameters

LuaConsole *g_Module = NULL;
ConsoleConfig *g_Config = NULL;

// ==============================================================
// class LuaConsole

LuaConsole::LuaConsole (HINSTANCE hDLL): Module (hDLL)
{
	hThread = NULL;
	interp = NULL;
	cConsoleCmd[0]=0;

	// Register a custom command for opening the console window
	dwCmd = oapiRegisterCustomCmd ((char*)"Lua console window",
		(char*)"Open a Lua script interpreter window.",
		OpenDlgClbk, this);

	hDlg = new LuaConsoleDlg(cConsoleCmd);
}

// ==============================================================

LuaConsole::~LuaConsole ()
{
	// Unregister the custom function in Orbiter
	oapiUnregisterCustomCmd (dwCmd);

	// Delete input buffer
	delete hDlg;
}

// ==============================================================

void LuaConsole::clbkSimulationStart (RenderMode mode)
{
	AddLine ("==== Orbiter Terminal (" LUA_RELEASE ") ====");
	AddLine ("Type 'help()' for help.");
	AddLine ("");
}

// ==============================================================

void LuaConsole::clbkSimulationEnd ()
{
	// Kill the interpreter thread
	if (interp) {
		if (hThread) {
			termInterp = true;
			interp->Terminate();
			interp->EndExec(); // give the thread opportunity to close
			if (WaitForSingleObject (hThread, 1000) != 0) {
				oapiWriteLog ((char*)"LuaConsole: timeout while waiting for interpreter thread");
				TerminateThread (hThread, 0);
			}
			CloseHandle (hThread);
			hThread = NULL;
		}
		delete interp;
		interp = NULL;
	}
}

// ==============================================================

void LuaConsole::clbkPreStep (double simt, double simdt, double mjd)
{
	if (interp) {
		if (interp->IsBusy() || cConsoleCmd[0] || interp->nJobs()) { // let the interpreter do some work
			interp->EndExec();        // orbiter hands over control
			// At this point the interpreter is performing one cycle
			interp->WaitExec();   // orbiter waits to get back control
		}
		interp->PostStep (simt, simdt, mjd);
	}
}

// ==============================================================

HWND LuaConsole::Open ()
{
	oapiOpenDialog(hDlg);

	// create the interpreter and execution thread
	if (!interp)
		interp = CreateInterpreter ();

	return NULL;
}

// ==============================================================

void LuaConsole::Close ()
{
	// Note that we keep the interpreter running when closing the
	// console window. Any active tasks (e.g. autopilots will continue
	// running in the background.

	oapiCloseDialog (hDlg);
}

void LuaConsole::OpenDlgClbk (void *context)
{
	LuaConsole *pConsole = (LuaConsole*)context;
	pConsole->Open();

//	static LuaConsoleDlg *dlg = new LuaConsoleDlg();
//	oapiOpenDialog(dlg);
}

void LuaConsole::AddLine(const char *str, LineType type)
{
	hDlg->AddLine(str, type);
}
void LuaConsole::Clear()
{
	hDlg->Clear();
}

// ==============================================================
// DLL entry and exit points

DLLCLBK void InitModule (HINSTANCE hDLL)
{
	// Create the console instance
	g_Module = new LuaConsole (hDLL);
	oapiRegisterModule (g_Module);
}

DLLCLBK void ExitModule (HINSTANCE hDLL)
{
	delete g_Config;
}

// ==============================================================

Interpreter *LuaConsole::CreateInterpreter ()
{
	unsigned int id;
	termInterp = false;
	interp = new ConsoleInterpreter (this);
	interp->Initialise();
	hThread = (HANDLE)_beginthreadex (NULL, 4096, &InterpreterThreadProc, this, 0, &id);
	return interp;
}
// Interpreter thread function
unsigned int WINAPI LuaConsole::InterpreterThreadProc (LPVOID context)
{
	int res;
	LuaConsole *console = (LuaConsole*)context;
	ConsoleInterpreter *interp = (ConsoleInterpreter*)console->interp;

	// interpreter loop
	for (;;) {
		interp->WaitExec(); // wait for execution permission
		if (console->termInterp) break; // close thread requested
		res = interp->RunChunk (console->cConsoleCmd, strlen (console->cConsoleCmd)); // run command from buffer
		if (interp->Status() == 1) break; // close thread requested
		console->cConsoleCmd[0] = '\0';    // free buffer
		interp->EndExec();        // return control
	}
	interp->EndExec();  // release mutex (is this necessary?)
	_endthreadex(0);
	return 0;
}
