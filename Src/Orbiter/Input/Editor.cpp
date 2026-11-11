#include "Editor.h"

#include <imgui.h>
#include <imgui_node_editor.h>
#include "Icon.h"
#include "Pin.h"

namespace ed = ax::NodeEditor;
ed::EditorContext* m_Context = nullptr;

class Node {
	std::vector<Pin> inputs;
	std::vector<Pin> outputs;
	std::string name;
	float widthName = 0.0f;
	float maxWidthInput = 0.0f;
	float maxWidthOutput = 0.0f;
	int id;
	bool dirty;
	static inline int g_id = 0;
public:
	Node(const char *n) {
		dirty = true;
		name = n;
		id = g_id++;
	}
	void AddInput(const char *n, Pin::type type) {
		dirty = true;
		inputs.emplace_back(n, type, Pin::kind::Input, g_id++);
	}
	void AddOutput(const char *n, Pin::type type) {
		dirty = true;
		outputs.emplace_back(n, type, Pin::kind::Output, g_id++);
	}
	void ComputeLayout() {
		widthName = ImGui::CalcTextSize(name.c_str()).x;
		maxWidthInput = 0;
		for(auto &input: inputs) {
			float sz = ImGui::CalcTextSize(input.name.c_str()).x;
			if(sz > maxWidthInput) maxWidthInput = sz;
		}
		maxWidthOutput = 0;
		for(auto &output: outputs) {
			float sz = ImGui::CalcTextSize(output.name.c_str()).x;
			if(sz > maxWidthOutput) maxWidthOutput = sz;
		}
		dirty = false;
	}

	void Draw() {
		if(dirty) {
			ComputeLayout();
		}

		ed::BeginNode(id);
		ImGui::Text(name.c_str());
		ImGui::Spacing();
		ImVec2 hmin = ImGui::GetItemRectMin();
		ImVec2 hmax = ImGui::GetItemRectMax();


		if(inputs.size() > 0) {
			ImGui::BeginGroup();
			for(auto &input:inputs) {
				input.Draw();
			}
			ImGui::EndGroup();

			if(outputs.size() > 0) ImGui::SameLine();
		}

		float titlesz = ImGui::CalcTextSize(name.c_str()).x;
		if(titlesz>maxWidthOutput+maxWidthInput) {
			float extra = 0.0f;
			ImGuiStyle& style = ImGui::GetStyle();
			if(maxWidthOutput != 0.0 && maxWidthInput != 0.0f) {
				extra = titlesz - maxWidthOutput - maxWidthInput - 2.0f * ImGui::GetTextLineHeight() - 3.0f * style.ItemSpacing.x;
			} else {
				extra = titlesz - maxWidthOutput - maxWidthInput - 1.0f * ImGui::GetTextLineHeight() - 2.0f * style.ItemSpacing.x;
			}
			if(extra < 0.0f) extra = 0.0f;
			ImGui::Dummy(ImVec2(extra, 0));ImGui::SameLine();
		}
		ImGui::BeginGroup();
		for(auto &output:outputs) {
			ImGui::Dummy(ImVec2(maxWidthOutput - ImGui::CalcTextSize(output.name.c_str()).x,0));
			ImGui::SameLine();
			output.Draw();
		}
		ImGui::EndGroup();
		
		ed::EndNode();
	}
};

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

	m_cmd = oapiRegisterCustomCmd((char*)"Joystick configuration", (char*)"Advanced joystick configuration", OpenDlgClbk, this);

	

	Node *n = new Node("test Looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong");
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

	nodes.push_back(n);

	Node *n2 = new Node("test");
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

	nodes.push_back(n2);

	Node *n3 = new Node("Inputs");
	n3->AddInput("xa", Pin::type::Button);
	n3->AddInput("xab", Pin::type::Button);
	n3->AddInput("xa", Pin::type::Button);
	n3->AddInput("xaccc", Pin::type::Button);
	n3->AddInput("x3", Pin::type::Button);
	nodes.push_back(n3);

	Node *n3b = new Node("Inputs loooooooooooooooong");
	n3b->AddInput("xa", Pin::type::Button);
	n3b->AddInput("xab", Pin::type::Button);
	n3b->AddInput("xa", Pin::type::Button);
	n3b->AddInput("xaccc", Pin::type::Button);
	n3b->AddInput("x3", Pin::type::Button);
	nodes.push_back(n3b);

	Node *n4 = new Node("Ouputs");
	n4->AddOutput("xa", Pin::type::Button);
	n4->AddOutput("xab", Pin::type::Button);
	n4->AddOutput("xa", Pin::type::Button);
	n4->AddOutput("xaccc", Pin::type::Button);
	n4->AddOutput("x3", Pin::type::Button);
	nodes.push_back(n4);

	Node *n5 = new Node("Ouputs looooooooooooooong");
	n5->AddOutput("xa", Pin::type::Button);
	n5->AddOutput("xab", Pin::type::Button);
	n5->AddOutput("xa", Pin::type::Button);
	n5->AddOutput("xaccc", Pin::type::Button);
	n5->AddOutput("x3", Pin::type::Button);
	nodes.push_back(n5);
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
	for(Node *n: nodes) {
		n->Draw();
	}

	ed::End();
    ed::SetCurrentEditor(nullptr);
}
