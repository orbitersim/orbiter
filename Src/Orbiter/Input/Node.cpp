#include "Node.h"

Node::Node(const char *n, int i) {
	dirty = true;
	name = n;
	id = i;
	pinidgen = (id << 16) + 1;
}
void Node::AddInput(const char *n, Pin::type type) {
	dirty = true;
	inputs.emplace_back(n, type, Pin::kind::Input, pinidgen++);
}
void Node::AddOutput(const char *n, Pin::type type) {
	dirty = true;
	outputs.emplace_back(n, type, Pin::kind::Output, pinidgen++);
}
void Node::ComputeLayout() {
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

void Node::Draw() {
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

	// If the node name is long, we must add some padding before drawing the output nodes so that they line up properly on the right side
	// This is not pixel perfect but it does the job for now
	float titlesz = ImGui::CalcTextSize(name.c_str()).x;
	if(titlesz > maxWidthOutput + maxWidthInput) {
		float extra = 0.0f;
		ImGuiStyle& style = ImGui::GetStyle();
		if(maxWidthOutput != 0.0 && maxWidthInput != 0.0f) {
			extra = titlesz - maxWidthOutput - maxWidthInput - 2.0f * ImGui::GetTextLineHeight() - 3.0f * style.ItemSpacing.x;
		} else {
			extra = titlesz - maxWidthOutput - maxWidthInput - 1.0f * ImGui::GetTextLineHeight() - 2.0f * style.ItemSpacing.x;
		}
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
