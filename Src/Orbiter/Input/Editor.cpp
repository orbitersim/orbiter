#include "Editor.h"

#include <imgui.h>
#include <imgui_node_editor.h>
#include "Icon.h"

namespace ed = ax::NodeEditor;
ed::EditorContext* m_Context = nullptr;

class Pin {
public:
	std::string name;
	int id;
	static inline int g_id = 0;
	Pin(const char *n) {
		name = n;
		id = g_id++;
	}
};

class Node {
	std::vector<Pin> inputs;
	std::vector<Pin> outputs;
	std::string name;
	float widthName;
	float maxWidthInput;
	float maxWidthOutput;
	int id;
	bool dirty;
	static inline int g_id = 0;
public:
	Node(const char *n) {
		dirty = true;
		name = n;
		id = g_id++;
	}
	void AddInput(const char *n) {
		dirty = true;
		inputs.push_back(n);
	}
	void AddOutput(const char *n) {
		dirty = true;
		outputs.push_back(n);
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
				ed::BeginPin(input.id, ed::PinKind::Input);
				if(input.id & 1)
					Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), IconType::ButtonDown, true, ImColor(230, 159,   0, 255), ImColor(32, 32, 32, 255));
				else
					Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), IconType::ButtonUp, true, ImColor(230, 159,   0, 255), ImColor(32, 32, 32, 255));
				ed::EndPin();
					ImGui::SameLine();
					ImGui::TextUnformatted(input.name.c_str());
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
				ImGui::TextUnformatted(output.name.c_str());
				ImGui::SameLine();
			ed::BeginPin(output.id, ed::PinKind::Output);
				if(output.id & 1)
				Axis(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), -1.0f, ImColor(230, 159,   0, 255));
				else
				Axis(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), 1.0f, ImColor(230, 159,   0, 255));

				//Icon(ImVec2(ImGui::GetTextLineHeight(), ImGui::GetTextLineHeight()), IconType::Plus, true, ImColor(230, 159,   0, 255), ImColor(32, 32, 32, 255));
			ed::EndPin();
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
//	m_Context = ed::CreateEditor(&config);
	m_Context = ed::CreateEditor();

	m_cmd = oapiRegisterCustomCmd((char*)"Editor", (char*)"This dialog allows to visualize active sound instances", OpenDlgClbk, this);

	

	Node *n = new Node("test Looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong");
	n->AddInput("a");
	n->AddInput("ab");
	n->AddInput("a");
	n->AddInput("accc");
	n->AddInput("3");

	n->AddOutput("x");
	n->AddOutput("yy");
	n->AddOutput("x");
	n->AddOutput("zzzzzz");
	n->AddOutput("3");

	nodes.push_back(n);

	Node *n2 = new Node("test");
	n2->AddInput("xa");
	n2->AddInput("xab");
	n2->AddInput("xa");
	n2->AddInput("xaccc");
	n2->AddInput("x3");

	n2->AddOutput("xx");
	n2->AddOutput("xyy");
	n2->AddOutput("xx");
	n2->AddOutput("xzzzzzz");
	n2->AddOutput("x3");

	nodes.push_back(n2);

	Node *n3 = new Node("Inputs");
	n3->AddInput("xa");
	n3->AddInput("xab");
	n3->AddInput("xa");
	n3->AddInput("xaccc");
	n3->AddInput("x3");
	nodes.push_back(n3);

	Node *n3b = new Node("Inputs loooooooooooooooong");
	n3b->AddInput("xa");
	n3b->AddInput("xab");
	n3b->AddInput("xa");
	n3b->AddInput("xaccc");
	n3b->AddInput("x3");
	nodes.push_back(n3b);

	Node *n4 = new Node("Ouputs");
	n4->AddOutput("xa");
	n4->AddOutput("xab");
	n4->AddOutput("xa");
	n4->AddOutput("xaccc");
	n4->AddOutput("x3");
	nodes.push_back(n4);

	Node *n5 = new Node("Ouputs looooooooooooooong");
	n5->AddOutput("xa");
	n5->AddOutput("xab");
	n5->AddOutput("xa");
	n5->AddOutput("xaccc");
	n5->AddOutput("x3");
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
	/*
	Node n("tesvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv");
	n.AddInput("a");
	n.AddInput("ab");
	n.AddInput("a");
	n.AddInput("accc");
	n.AddInput("3");

	n.AddOutput("x");
	n.AddOutput("yy");
	n.AddOutput("x");
	n.AddOutput("zzzzzz");
	n.AddOutput("3");
	n.ComputeLayout();

	n.Draw();


	Node n2("test");
	n2.AddInput("xa");
	n2.AddInput("xab");
	n2.AddInput("xa");
	n2.AddInput("xaccc");
	n2.AddInput("x3");

	n2.AddOutput("xx");
	n2.AddOutput("xyy");
	n2.AddOutput("xx");
	n2.AddOutput("xzzzzzz");
	n2.AddOutput("x3");
	n2.ComputeLayout();

	n2.Draw();
	*/
	ed::End();
    ed::SetCurrentEditor(nullptr);
}
