#include "Node.h"

Node::Node(const char *n, int i) {
	dirty = true;
	name = n;
	id = i;
	pinidgen = (id << 16) + 1;
}
void Node::AddInput(const char *n, Pin::type type) {
	dirty = true;
	inputs.emplace_back(n, type, Pin::kind::Input, this, pinidgen++);
}
Pin *Node::AddOutput(const char *n, Pin::type type) {
	dirty = true;
	outputs.emplace_back(n, type, Pin::kind::Output, this, pinidgen++);
	return &outputs.back();
}
void Node::ComputeLayout() {
	widthName = ImGui::CalcTextSize(name.c_str()).x;
	maxWidthInput = 0;
	for(Pin &input: inputs) {
		float sz = ImGui::CalcTextSize(input.name.c_str()).x;
		if(sz > maxWidthInput) maxWidthInput = sz;
	}
	maxWidthOutput = 0;
	for(Pin &output: outputs) {
		float sz = ImGui::CalcTextSize(output.name.c_str()).x;
		if(sz > maxWidthOutput) maxWidthOutput = sz;
	}
	dirty = false;
}

void Node::UpdateInputs()
{
	for(Pin &input: inputs) {
		input.ReadInputs();
	}
}

void Node::Draw(std::function<bool(const Pin *)> filter) {
	if(dirty) {
		ComputeLayout();
	}

	ed::BeginNode(id);
	if(grayed) {
		ImGui::TextDisabled("%s", name.c_str());
	} else {
		ImGui::TextUnformatted(name.c_str());
	}
	ImGui::Spacing();
	ImVec2 hmin = ImGui::GetItemRectMin();
	ImVec2 hmax = ImGui::GetItemRectMax();


	if(inputs.size() > 0) {
		ImGui::BeginGroup();
		for(Pin &input:inputs) {
			input.Draw(filter);
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
	for(Pin &output:outputs) {
		ImGui::Dummy(ImVec2(maxWidthOutput - ImGui::CalcTextSize(output.name.c_str()).x,0));
		ImGui::SameLine();
		output.Draw(filter);
	}
	ImGui::EndGroup();
		
	ed::EndNode();
}

void Node::ToJSON(crude_json::value &out)
{
	out["typename"] = GetTypeName();
	out["id"] = std::to_string(id);
    ImVec2 pos = ed::GetNodePosition(id);

    if(pos.x == FLT_MAX) pos.x = 0;
    if(pos.y == FLT_MAX) pos.y = 0;

    out["xpos"] = pos.x;
    out["ypos"] = pos.y;

    for(auto &n: inputs) {
        crude_json::value pin;
        pin["id"] = std::to_string((ptrdiff_t)n.id.AsPointer());
        pin["name"] = n.name;
        pin["type"] = std::to_string((int)n.ptype);
        out["inputs"].push_back(pin);
    }
    for(auto &n: outputs) {
        crude_json::value pin;
        pin["id"] = std::to_string((ptrdiff_t)n.id.AsPointer());
        pin["name"] = n.name;
        pin["type"] = std::to_string((int)n.ptype);
        if(n.context.deadzone != 0.0f)
            pin["deadzone"] = n.context.deadzone;
        if(n.context.saturation != 1.0f)
            pin["saturation"] = n.context.saturation;
        if(n.context.flip)
            pin["flip"] = true;
        if(n.context.rescale)
            pin["rescale"] = true;
        out["outputs"].push_back(pin);
    }
}
