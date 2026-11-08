#include "Editor.h"

#include <imgui.h>
#include <imgui_node_editor.h>

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

	m_cmd = oapiRegisterCustomCmd((char*)"Editor", (char*)"This dialog allows to visualize active sound instances", OpenDlgClbk, this);

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
    int uniqueId = 1;
    // Start drawing nodes.
    ed::BeginNode(uniqueId++);
        ImGui::Text("Node A");
        ed::BeginPin(uniqueId++, ed::PinKind::Input);
            ImGui::Text("-> In");
        ed::EndPin();
        ImGui::SameLine();
        ed::BeginPin(uniqueId++, ed::PinKind::Output);
            ImGui::Text("Out ->");
        ed::EndPin();
    ed::EndNode();
    ed::End();
    ed::SetCurrentEditor(nullptr);
}
