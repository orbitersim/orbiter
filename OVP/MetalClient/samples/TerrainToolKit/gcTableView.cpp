// ==================================================================
// Copyright (c) 2021-2026 Jarmo Nikkanen
// Licensed under the MIT License
// ==================================================================

#include "gcPropertyTree.h"
#include "imgui.h"
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <cmath>
#include <cfloat>


// ==================================================================================
//
static int InputTextResize(ImGuiInputTextCallbackData *data)
{
	if (data->EventFlag == ImGuiInputTextFlags_CallbackResize) {
		string *str = (string *)data->UserData;
		str->resize(data->BufTextLen);
		data->Buf = str->data();
	}
	return 0;
}


// ==================================================================================
//
gcPropertyTree::gcPropertyTree(WORD _idc, Callback pCall) : pCallback(pCall), pSelected(NULL), idc(_idc)
{
}


// ==================================================================================
//
gcPropertyTree::~gcPropertyTree()
{
	for (HPROP hp : Data)
	{
		if (hp->pSlider) delete hp->pSlider;
		delete hp;
	}
}


// ==================================================================================
//
void gcPropertyTree::Notify(HPROP hp, WORD id, Event ev)
{
	if (pCallback) pCallback(hp, id, ev);
}


// ==================================================================================
//
void gcPropertyTree::CopyToClipboard()
{
	if (!pSelected) return;
	ImGui::SetClipboardText(pSelected->val.c_str());
}


// ==================================================================================
//
float gcPropertyTree::LabelWidth(HPROP hPar, float indent)
{
	float w = 0.0f;
	for (HPROP hp : Data)
	{
		if (hp->bOn == false) continue;
		if (hp->parent != hPar) continue;
		float lw = indent + ImGui::CalcTextSize(hp->label.c_str()).x;
		if (hp->bChildren) {
			if (hp->val.size() == 0) lw = 0.0f;
			else lw += ImGui::GetTreeNodeToLabelSpacing();
			if (hp->bOpen) w = std::max(w, LabelWidth(hp, indent + ImGui::GetStyle().IndentSpacing));
		}
		w = std::max(w, lw);
	}
	return w;
}


// ==================================================================================
//
void gcPropertyTree::Draw()
{
	ImGuiStyle &style = ImGui::GetStyle();
	float wlbl = std::max(LabelWidth(NULL, 0.0f), ImGui::GetFontSize() * 4.0f) + style.ItemSpacing.x;

	ImVec2 p0 = ImGui::GetCursorScreenPos();
	float width = ImGui::GetContentRegionAvail().x;

	ImGuiTableFlags flags = ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersInnerV | ImGuiTableFlags_BordersOuter;
	if (ImGui::BeginTable("##properties", 2, flags)) {
		ImGui::TableSetupColumn("Label", ImGuiTableColumnFlags_WidthFixed, wlbl);
		ImGui::TableSetupColumn("Value", ImGuiTableColumnFlags_WidthStretch);
		DrawSection(NULL);
		ImGui::EndTable();
	}

	ImVec2 p1 = ImGui::GetCursorScreenPos();
	bool bHover = ImGui::IsWindowHovered(ImGuiHoveredFlags_ChildWindows) && ImGui::IsMouseHoveringRect(p0, ImVec2(p0.x + width, p1.y));

	if (pSelected && !bHover) pSelected = NULL;
	if (pSelected && ImGui::IsKeyChordPressed(ImGuiMod_Ctrl | ImGuiKey_C)) CopyToClipboard();
}


// ==================================================================================
//
void gcPropertyTree::DrawSection(HPROP hPar)
{
	for (size_t i = 0; i < Data.size(); i++)
	{
		HPROP hp = Data[i];
		if (hp->bOn == false) continue;
		if (hp->parent != hPar) continue;

		ImGui::TableNextRow();
		ImGui::TableSetColumnIndex(0);
		ImGui::PushID(hp);

		if (hp->bChildren) {
			ImGui::SetNextItemOpen(hp->bOpen, ImGuiCond_Always);
			bool bOpen = ImGui::TreeNodeEx(hp->label.c_str(), ImGuiTreeNodeFlags_SpanAvailWidth);
			hp->bOpen = bOpen;
			if (hp->val.size()) {
				ImGui::TableSetColumnIndex(1);
				ImGui::TextUnformatted(hp->val.c_str());
			}
			if (bOpen) {
				DrawSection(hp);
				ImGui::TreePop();
			}
			ImGui::PopID();
			continue;
		}

		if (hp->style == Style::TEXT) {
			if (ImGui::Selectable(hp->label.c_str(), pSelected == hp, ImGuiSelectableFlags_SpanAllColumns)) {
				pSelected = hp;
				Notify(pSelected, idc, Event::SELECTED);
			}
			if (ImGui::BeginPopupContextItem("##copy")) {
				pSelected = hp;
				if (ImGui::MenuItem("Copy value")) ImGui::SetClipboardText(hp->val.c_str());
				ImGui::EndPopup();
			}
			ImGui::TableSetColumnIndex(1);
			if (hp->color) ImGui::PushStyleColor(ImGuiCol_Text, hp->color | 0xFF000000);
			ImGui::TextUnformatted(hp->val.c_str());
			if (hp->color) ImGui::PopStyleColor();
		}
		else {
			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted(hp->label.c_str());
			ImGui::TableSetColumnIndex(1);
			ImGui::SetNextItemWidth(-FLT_MIN);
			DrawControl(hp);
		}

		ImGui::PopID();
	}
}


// ==================================================================================
//
void gcPropertyTree::DrawControl(HPROP hp)
{
	switch (hp->style) {

	case Style::TEXTBOX:
	{
		if (ImGui::InputText("##text", hp->val.data(), hp->val.capacity() + 1, ImGuiInputTextFlags_CallbackResize, InputTextResize, &hp->val))
			Notify(hp, hp->idc, Event::CHANGED);
		break;
	}

	case Style::COMBOBOX:
	{
		const char *preview = (hp->sel >= 0 && hp->sel < int(hp->items.size())) ? hp->items[hp->sel].c_str() : "";
		if (ImGui::BeginCombo("##combo", preview)) {
			for (int i = 0; i < int(hp->items.size()); i++) {
				bool bSel = (i == hp->sel);
				if (ImGui::Selectable(hp->items[i].c_str(), bSel) && !bSel) {
					hp->sel = i;
					Notify(hp, hp->idc, Event::CHANGED);
				}
				if (bSel) ImGui::SetItemDefaultFocus();
			}
			ImGui::EndCombo();
		}
		break;
	}

	case Style::SLIDER:
	{
		if (!hp->pSlider) break;
		char disp[32];
		snprintf(disp, sizeof(disp), "%.3g", GetSliderValue(hp));
		float pos = float(hp->pSlider->lin_pos);
		if (ImGui::SliderFloat("##slider", &pos, 0.0f, 1.0f, disp, ImGuiSliderFlags_NoInput)) {
			hp->pSlider->lin_pos = double(pos);
			Notify(hp, hp->idc, Event::CHANGED);
		}
		break;
	}

	default:
		break;
	}
}


// ==================================================================================
//
HPROP gcPropertyTree::SubSection(const string &lbl, HPROP parent)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->bChildren = true;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddEntry(const string &lbl, HPROP parent)
{
	gcProperty *td = new gcProperty();
	td->bOpen = td->bOn = true;
	td->bChildren = false;
	td->label = lbl;
	td->parent = parent;
	td->val = string("");
	td->pUser = NULL;
	td->pSlider = NULL;
	td->style = Style::TEXT;
	td->sel = -1;
	td->idc = 0;
	td->color = 0;
	if (parent) parent->bChildren = true;
	Data.push_back(td);
	return td;
}


// ==================================================================================
//
void gcPropertyTree::RemoveEntry(HPROP hEntry)
{
	if (!hEntry) return;

	vector<HPROP> children;
	for (HPROP hp : Data) if (hp->parent == hEntry) children.push_back(hp);
	for (HPROP hp : children) RemoveEntry(hp);

	Data.erase(std::remove(Data.begin(), Data.end(), hEntry), Data.end());
	if (pSelected == hEntry) pSelected = NULL;
	if (hEntry->pSlider) delete hEntry->pSlider;
	delete hEntry;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddEditControl(const string &lbl, WORD id, HPROP parent, const string &text, void *pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->style = Style::TEXTBOX;
	hProp->val = text;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddComboBox(const string &lbl, WORD id, HPROP parent, void* pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->style = Style::COMBOBOX;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::AddSlider(const string &lbl, WORD id, HPROP parent, void* pUser)
{
	HPROP hProp = AddEntry(lbl, parent);
	hProp->pSlider = new gcSlider();
	hProp->pSlider->scl = Scale::LINEAR;
	hProp->pSlider->fmin = 0.0;
	hProp->pSlider->fmax = 1.0;
	hProp->style = Style::SLIDER;
	hProp->idc = id;
	hProp->pUser = pUser;
	return hProp;
}


// ==================================================================================
//
HPROP gcPropertyTree::GetEntry(int idx)
{
	if (idx < 0 || idx >= int(Data.size())) return NULL;
	return Data[idx];
}


// ==================================================================================
//
void* gcPropertyTree::GetUserRef(HPROP hEntry)
{
	if (hEntry) return hEntry->pUser;
	return NULL;
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, int val)
{
	hEntry->val = std::to_string(val);
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, DWORD val, bool bHex)
{
	std::ostringstream oss;
	if (bHex) oss << std::hex;
	oss << val;
	hEntry->val = oss.str();
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, double val, int digits, Format st)
{
	string u;
	std::ostringstream oss;
	oss << std::setprecision(digits);
	oss << std::fixed;

	if (st == UNITS) {
		double x = fabs(val);
		if (x > 1e9) val /= 1e9, u = "G";
		else if (x > 1e6) val /= 1e6, u = "M";
		else if (x > 1e3) val /= 1e3, u = "k";
		if (x < 1e-6) val /= 1e-6, u = "µ";
		else if (x < 1e-3) val /= 1e-3, u = "m";
		oss << val << u;
	}

	if (st == LATITUDE) { if (val < 0) oss << fabs(val*DEG) << "°S"; else oss << val*DEG << "°N"; }
	if (st == LONGITUDE) { if (val < 0) oss << fabs(val*DEG) << "°W"; else oss << val*DEG << "°E"; }
	if (st == NORMAL) oss << val;

	hEntry->val = oss.str();
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, const string &val, DWORD clr)
{
	hEntry->val = val;
	hEntry->color = clr;
}


// ==================================================================================
//
void gcPropertyTree::SetValue(HPROP hEntry, const char *lbl, DWORD clr)
{
	hEntry->val = string(lbl);
	hEntry->color = clr;
}


// ==================================================================================
//
void gcPropertyTree::OpenEntry(HPROP hEntry, bool bOpen)
{
	hEntry->bOpen = bOpen;
}


// ==================================================================================
//
void gcPropertyTree::ShowEntry(HPROP hEntry, bool bShow)
{
	hEntry->bOn = bShow;
}


// ==================================================================================
//
void gcPropertyTree::SetSliderScale(HPROP hSlider, double fmin, double fmax, Scale scl)
{
	if (!hSlider) return;
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		pSl->fmin = fmin;
		pSl->fmax = fmax;
		pSl->lfmin = log(fmin);
		pSl->lfmax = log(fmax);
		pSl->scl = scl;
	}
}


// ==================================================================================
//
void gcPropertyTree::SetSliderValue(HPROP hSlider, double val)
{
	if (!hSlider) return;
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		if (pSl->scl == Scale::LOG) pSl->lin_pos = (log(val) - pSl->lfmin) / (pSl->lfmax - pSl->lfmin);
		double lin = (val - pSl->fmin) / (pSl->fmax - pSl->fmin);
		if (pSl->scl == Scale::LINEAR) pSl->lin_pos = lin;
		if (pSl->scl == Scale::SQRT) pSl->lin_pos = lin*lin;
		if (pSl->scl == Scale::SQUARE) pSl->lin_pos = sqrt(lin);
		pSl->lin_pos = std::clamp(pSl->lin_pos, 0.0, 1.0);
	}
}


// ==================================================================================
//
double gcPropertyTree::GetSliderValue(HPROP hSlider)
{
	if (!hSlider) return 0.0;
	gcSlider *pSl = hSlider->pSlider;
	if (pSl) {
		if (pSl->scl == Scale::LOG) return exp((pSl->lfmax - pSl->lfmin) * pSl->lin_pos + pSl->lfmin);
		if (pSl->scl == Scale::LINEAR) return (pSl->fmax - pSl->fmin) * pSl->lin_pos + pSl->fmin;
		if (pSl->scl == Scale::SQRT) return (pSl->fmax - pSl->fmin) * sqrt(pSl->lin_pos) + pSl->fmin;
		if (pSl->scl == Scale::SQUARE) return (pSl->fmax - pSl->fmin) * (pSl->lin_pos*pSl->lin_pos) + pSl->fmin;
	}
	return 0.0;
}


// ==================================================================================
//
string gcPropertyTree::GetTextBoxContent(HPROP hTextBox)
{
	if (!hTextBox || hTextBox->style != Style::TEXTBOX) return string();
	return string(hTextBox->val.c_str());
}


// ==================================================================================
//
void gcPropertyTree::SetTextBoxContent(HPROP hTextBox, string text)
{
	if (!hTextBox || hTextBox->style != Style::TEXTBOX) return;
	hTextBox->val = text;
}


// ==================================================================================
//
void gcPropertyTree::SetComboBoxSelection(HPROP hCombo, int idx)
{
	if (!hCombo || hCombo->style != Style::COMBOBOX) return;
	hCombo->sel = (idx >= 0 && idx < int(hCombo->items.size())) ? idx : -1;
}


// ==================================================================================
//
int	gcPropertyTree::GetComboBoxSelection(HPROP hCombo)
{
	if (!hCombo || hCombo->style != Style::COMBOBOX) return 0;
	return hCombo->sel;
}


// ==================================================================================
//
void gcPropertyTree::ClearComboBox(HPROP hCombo)
{
	if (!hCombo || hCombo->style != Style::COMBOBOX) return;
	hCombo->items.clear();
	hCombo->sel = -1;
}


// ==================================================================================
//
int gcPropertyTree::AddComboBoxItem(HPROP hCombo, const char *label)
{
	if (!hCombo || hCombo->style != Style::COMBOBOX) return -1;
	hCombo->items.push_back(string(label));
	return int(hCombo->items.size()) - 1;
}
