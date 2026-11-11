#include "Editor.h"

#include <imgui.h>
#include <imgui_node_editor.h>
#include "Icon.h"
#include "Pin.h"
#include "Node.h"
#include "Graph.h"
#include "Graph.h"

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
//	m_Context = ed::CreateEditor();
	graph = new ControllerGraph();

	m_cmd = oapiRegisterCustomCmd((char*)"Joystick configuration", (char*)"Advanced joystick configuration", OpenDlgClbk, this);

	Node *n = new Node("test Looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong", 0);
	n->AddInput("a", Pin::type::Button);
	n->AddInput("ab", Pin::type::Button);
	n->AddInput("a", Pin::type::Button);
	n->AddInput("accc", Pin::type::Button);
	n->AddInput("3", Pin::type::Button);

	n->AddOutput("x", Pin::type::Button);
	n->AddOutput("yy", Pin::type::Button);
	n->AddOutput("x", Pin::type::Button);
	n->AddOutput("zzzzzz", Pin::type::Button);
	n->AddOutput("3", Pin::type::Button);

	graph->AddNode(n);

	Node *n2 = new Node("test", 1);
	n2->AddInput("xa", Pin::type::Button);
	n2->AddInput("xab", Pin::type::Button);
	n2->AddInput("xa", Pin::type::Button);
	n2->AddInput("xaccc", Pin::type::HalfAxis);
	n2->AddInput("x3", Pin::type::Button);

	n2->AddOutput("xx", Pin::type::Button);
	n2->AddOutput("xyy", Pin::type::Hat);
	n2->AddOutput("xx", Pin::type::Axis);
	n2->AddOutput("xzzzzzz", Pin::type::Button);
	n2->AddOutput("x3", Pin::type::Button);

	graph->AddNode(n2);

	Node *n3 = new Node("Inputs", 2);
	n3->AddInput("xa", Pin::type::Button);
	n3->AddInput("xab", Pin::type::Button);
	n3->AddInput("xa", Pin::type::Button);
	n3->AddInput("xaccc", Pin::type::Button);
	n3->AddInput("x3", Pin::type::Button);
	graph->AddNode(n3);

	Node *n3b = new Node("Inputs loooooooooooooooong", 3);
	n3b->AddInput("xa", Pin::type::Button);
	n3b->AddInput("xab", Pin::type::Button);
	n3b->AddInput("xa", Pin::type::Button);
	n3b->AddInput("xaccc", Pin::type::Button);
	n3b->AddInput("x3", Pin::type::Button);
	graph->AddNode(n3b);

	Node *n4 = new Node("Ouputs", 4);
	n4->AddOutput("xa", Pin::type::Button);
	n4->AddOutput("xab", Pin::type::Button);
	n4->AddOutput("xa", Pin::type::Button);
	n4->AddOutput("xaccc", Pin::type::Button);
	n4->AddOutput("x3", Pin::type::Button);
	graph->AddNode(n4);

	Node *n5 = new Node("Ouputs looooooooooooooong", 5);
	n5->AddOutput("xa", Pin::type::Button);
	n5->AddOutput("xab", Pin::type::Button);
	n5->AddOutput("xa", Pin::type::Button);
	n5->AddOutput("xaccc", Pin::type::Button);
	n5->AddOutput("x3", Pin::type::Button);
	graph->AddNode(n5);
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
