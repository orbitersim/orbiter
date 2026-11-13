#include "Editor.h"

#include <imgui.h>
#include <imgui_node_editor.h>
#include "Icon.h"
#include "Pin.h"
#include "Node.h"
#include "Graph.h"
#include "Controller.h"

namespace ed = ax::NodeEditor;
ed::EditorContext* m_Context = nullptr;

static void OpenDlgClbk(void *context)
{
    InputEditor *dbg = (InputEditor *)context;
    oapiOpenDialog(dbg);
}


InputEditor::InputEditor():ImGuiDialog("Editor")
{
	ed::Config config;
	config.SettingsFile = "control.json";
	m_Context = ed::CreateEditor(&config);
	graph = InputController::GetGraph();

	m_cmd = oapiRegisterCustomCmd((char*)"Joystick configuration", (char*)"Advanced joystick configuration", OpenDlgClbk, this);
}

InputEditor::~InputEditor()
{
	ed::DestroyEditor(m_Context);
}

void InputEditor::OnDraw()
{
    auto& io = ImGui::GetIO();

    ImGui::Text("FPS: %.2f (%.2gms)", io.Framerate, io.Framerate ? 1000.0f / io.Framerate : 0.0f);

    ImGui::Separator();

    ed::SetCurrentEditor(m_Context);
    ed::Begin("My Editor", ImVec2(0.0, 0.0f));
	graph->Draw();

	ed::End();
    ed::SetCurrentEditor(nullptr);
}
