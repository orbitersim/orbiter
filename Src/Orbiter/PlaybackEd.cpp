// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "PlaybackEd.h"
#include "Camera.h"
#include "Log.h"
#include "imgui_extras.h"

using namespace std;

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern TimeData td;

static bool CompareEvents (const PlaybackEvent *first, const PlaybackEvent *second)
{
  return first->T0() < second->T0();
}

DlgPlaybackEditor::DlgPlaybackEditor() : ImGuiDialog("Playback Editor", {900,600}) {
	m_sysfname = nullptr;
	m_SelectedEvent = nullptr;
}

DlgPlaybackEditor::~DlgPlaybackEditor() {
	ClearEvents();
}

void DlgPlaybackEditor::OnDraw() {
	ImGuiWindowFlags window_flags = ImGuiWindowFlags_HorizontalScrollbar;

	ImGui::Text("Playback Editor - Simulation time : %f", td.SimT0);
	ImGui::BeginChild("ChildL", ImVec2(ImGui::GetContentRegionAvail().x/2.0, 0), ImGuiChildFlags_ResizeX,  window_flags);

    const ImGuiTableFlags flags = ImGuiTableFlags_SizingStretchProp|ImGuiTableFlags_ScrollY | ImGuiTableFlags_RowBg | ImGuiTableFlags_BordersOuter | ImGuiTableFlags_BordersV | ImGuiTableFlags_Resizable | ImGuiTableFlags_Reorderable | ImGuiTableFlags_Hideable;

    ImVec2 outer_size = ImVec2(0.0f, ImGui::GetContentRegionAvail().y - 100);
    if (ImGui::BeginTable("table_scrolly", 3, flags, outer_size))
    {
        ImGui::TableSetupScrollFreeze(0, 1); // Make top row always visible
        ImGui::TableSetupColumn("Timestamp", ImGuiTableColumnFlags_None, 0.5f);
        ImGui::TableSetupColumn("Event", ImGuiTableColumnFlags_None, 0.5f);
        ImGui::TableSetupColumn("Details", ImGuiTableColumnFlags_None, 2.0f);
        ImGui::TableHeadersRow();

		int i = 0;
		bool ts_drawn = false;
		if(m_Events.size() == 0) {
			ImGui::TableNextRow();
			ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImColor(1.0,0,0));
			ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg1, ImColor(1.0,0,0));
			ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImColor(1.0,0,0));
			ImGui::TableSetColumnIndex(0);
			ImGui::Text("%0.2f", td.SimT0);
			ImGui::TableSetColumnIndex(1);
			ImGui::TextUnformatted("NOW");
		}
		for(auto &e : m_Events) {
			i++;
			const bool item_is_selected = e == m_SelectedEvent;

			if(e->T0() > td.SimT0 && !ts_drawn) {
				ts_drawn = true;
				ImGui::TableNextRow();
				ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg0, ImColor(1.0,0,0));
				ImGui::TableSetBgColor(ImGuiTableBgTarget_RowBg1, ImColor(1.0,0,0));
				ImGui::TableSetBgColor(ImGuiTableBgTarget_CellBg, ImColor(1.0,0,0));
				ImGui::TableSetColumnIndex(0);
				ImGui::Text("%0.2f", td.SimT0);
				ImGui::TableSetColumnIndex(1);
				ImGui::TextUnformatted("NOW");
			}

			ImGui::TableNextRow();
			ImGui::TableSetColumnIndex(0);
			char label[32];
			sprintf(label, "%0.2f###line%d", e->T0(),i);
			ImGuiSelectableFlags selectable_flags = ImGuiSelectableFlags_SpanAllColumns;
			if (ImGui::Selectable(label, item_is_selected, selectable_flags))
			{
				m_SelectedEvent = e;
			}

			char tag[32];
			e->TagStr(tag);
			ImGui::TableSetColumnIndex(1);
			ImGui::Text("%s",tag);
			ImGui::TableSetColumnIndex(2);
			e->DrawPreview();
		}

        ImGui::EndTable();
    }

	ImGui::BeginGroupPanel("Insert event");
		if(ImGui::Button("NOTE")) {
			InsertEvent(new NoteEvent (td.SimT0, "New note"));
		}
		ImGui::SameLine();
		if(ImGui::Button("NOTEOFF")) {
			InsertEvent(new NoteoffEvent (td.SimT0));
		}
		ImGui::SameLine();
		if(ImGui::Button("NOTEPOS")) {
			InsertEvent(new NoteposEvent (td.SimT0, 0.1,0.1,0.9,0.9));
		}
		ImGui::SameLine();
		if(ImGui::Button("NOTECOL")) {
			InsertEvent(new NotecolEvent (td.SimT0, 1.0, 1.0, 1.0));
		}
		ImGui::SameLine();
		if(ImGui::Button("NOTESIZE")) {
			InsertEvent(new NotesizeEvent (td.SimT0, 1.0));
		}
		if(ImGui::Button("TACC")) {
			InsertEvent(new TaccEvent (td.SimT0, 1.0, 0.0));
		}
		ImGui::SameLine();
		if(ImGui::Button("CAMERA")) {
			InsertEvent(new CameraEvent (td.SimT0));
		}
		ImGui::SameLine();
		if(ImGui::Button("<Other>")) {
			InsertEvent(new GenericEvent (td.SimT0, "<TAG>", ""));
		}
	ImGui::EndGroupPanel();
	if(ImGui::Button("Delete")) {
		DeleteSelectedEvent();
	}
	ImGui::EndChild();
	ImGui::SameLine();
//	ImGui::BeginChild("ChildR", ImVec2(sz2, ImGui::GetContentRegionAvail().y), false, window_flags);
	ImGui::BeginChild("ChildR", ImVec2(0,0), 0, window_flags);
	ImGui::BeginChild("ChildRT", ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y - 40), true, window_flags);
	if(m_SelectedEvent) {
		m_SelectedEvent->DrawEdit();
		if(ImGui::Button("Apply")) {
			m_SelectedEvent->ApplyChanges();
			m_Events.sort(CompareEvents);
		}

	}
	ImGui::EndChild();
	ImGui::BeginChild("ChildRB", ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y), true, window_flags);
	if(ImGui::Button("Commit changes")) {
		SaveEventFile();
	}
	ImGui::SameLine();
	if(ImGui::Button("Discard uncommited changes")) {
		ClearEvents();
		ScanEventFile();
	}
		
	ImGui::EndChild();
	ImGui::EndChild();
}

void DlgPlaybackEditor::InsertEvent(PlaybackEvent *e) {
	m_Events.push_back(e);
	m_Events.sort(CompareEvents);
	m_SelectedEvent = e;
}

void DlgPlaybackEditor::DeleteSelectedEvent() {
	if(m_SelectedEvent==nullptr) return;
	PlaybackEvent *prev = nullptr;
	for(auto it = m_Events.begin(); it!=m_Events.end();++it) {
		if(*it==m_SelectedEvent) {
			m_Events.erase(it);
			if(prev) {
				m_SelectedEvent = prev;
			} else if(m_Events.size()>0) {
				m_SelectedEvent = m_Events.front();
			} else {
				m_SelectedEvent = nullptr;
			}
			return;
		}
		prev = *it;
	}
}

void DlgPlaybackEditor::Load(const char *ScnName) {
	char fname[1024];
	int i;
	for (i = strlen(ScnName)-1; i > 0; i--)
		if (ScnName[i-1] == '/' || ScnName[i-1] == '\\') break;
	sprintf (fname, "Flights/%s/system.dat", ScnName+i);

	if(m_sysfname) free(m_sysfname);
	m_sysfname = strdup(fname);
	ClearEvents();
	ScanEventFile();
}

void DlgPlaybackEditor::ScanEventFile ()
{
	char line[2048];
	ifstream ifs (m_sysfname);
	while (ifs.getline (line, 2048)) {
		PlaybackEvent *pe = PlaybackEvent::Create (line);
		if (pe) {
			m_Events.push_back(pe);
		}
	}
}

void DlgPlaybackEditor::SaveEventFile ()
{
	g_pOrbiter->FRecorder_SuspendPlayback();
	ofstream ofs (m_sysfname);
	for (auto &e: m_Events) {
		e->Write (ofs);
	}
	ofs.close();
	g_pOrbiter->FRecorder_RescanPlayback();
}

void DlgPlaybackEditor::ClearEvents() {
	for(auto &e : m_Events) {
		delete e;
	}
	m_Events.clear();
	m_SelectedEvent = nullptr;
}

// =========================================================

PlaybackEvent *PlaybackEvent::Create (char *event)
{
	char *s;
	double t;
	s = strtok (event, " \t\n");
	if (!s || sscanf (s, "%lf", &t) != 1) return NULL;
	s = strtok (NULL, " \t\n");
	if (!s) return NULL;
	if (!_stricmp (s, "TACC")) {
		double acc, delay;
		s = strtok (NULL, " \t\n");
		if (!s || sscanf (s, "%lf", &acc) != 1) return NULL;
		s = strtok (NULL, " \t\n");
		if (!s || sscanf (s, "%lf", &delay) != 1)
			delay = 0.0;
		TRACENEW; return new TaccEvent (t, acc, delay);
	} else if (!_stricmp (s, "CAMERA")) {
		int32_t pr;
		s = strtok (NULL, " \t\n");
		if (!s) return NULL;
		if (!_stricmp (s, "PRESET")) {
			s = strtok (NULL, " \t\n");
			if (!s || sscanf (s, "%d", &pr) != 1) return NULL;
			TRACENEW; return new CameraEvent (t, pr);
		} else if (!strcmp (s, "SET")) {
			TRACENEW; return new CameraEvent (t, s+4);
		}
	} else if (!_stricmp (s, "NOTE")) {
		TRACENEW; return new NoteEvent (t, strtok (NULL, "\n"));
	} else if (!_stricmp (s, "NOTEOFF")) {
		TRACENEW; return new NoteoffEvent (t);
	} else if (!_stricmp (s, "NOTEPOS")) {
		double x0, y0, x1, y1;
		int res = sscanf (s+8, "%lf %lf %lf %lf", &x0, &y0, &x1, &y1);
		if (res != 4) return NULL;
		else { TRACENEW; return new NoteposEvent (t, x0, y0, x1, y1); }
	} else if (!_stricmp (s, "NOTECOL")) {
		double r, g, b;
		int res = sscanf (s+8, "%lf %lf %lf", &r, &g, &b);
		if (res != 3) return NULL;
		else { TRACENEW; return new NotecolEvent (t, r, g, b); }
	} else if (!_stricmp (s, "NOTESIZE")) {
		double scale;
		int res = sscanf (s+9, "%lf", &scale);
		if (!res) return NULL;
		else { TRACENEW; return new NotesizeEvent (t, scale); }
	} else {
		TRACENEW; return new GenericEvent (t, s, strtok (NULL, "\n"));
	}
	return NULL;
}

PlaybackEvent::PlaybackEvent (double _t0)
{
	t0 = _t0;
	sprintf(m_tmp_t0, "%0.2f", t0);
}

void PlaybackEvent::TimeStr (char *str)
{
	sprintf (str, "%0.2f", t0);
}

void PlaybackEvent::WriteEvent (ofstream &ofs, const char *eventtype, const char *event)
{
	ofs << t0 << ' ' << eventtype;
	if (event) ofs << ' ' << event;
	ofs << endl;
}

void PlaybackEvent::DrawEdit() {
	ImGui::InputText("Event Time", m_tmp_t0, sizeof(m_tmp_t0), ImGuiInputTextFlags_CharsDecimal);
}

void PlaybackEvent::ApplyChanges() {
	t0 = atof(m_tmp_t0);
}
// =========================================================

GenericEvent::GenericEvent (double _t0, const char *_tag, const char *_content): PlaybackEvent (_t0)
{
	content = tag = 0;
	SetTag (_tag);
	SetContent (_content);
}

GenericEvent::~GenericEvent ()
{
	if (tag) delete []tag;
	if (content) delete []content;
}

void GenericEvent::SetTag (const char *_tag)
{
	if (tag) delete []tag;
	if (_tag) {
		tag = new char[strlen(_tag)+1]; TRACENEW
		strcpy (tag, _tag);
	} else tag = 0;
}

void GenericEvent::SetContent (const char *_content)
{
	if (content) delete []content;
	if (_content) {
		content = new char[strlen(_content)+1]; TRACENEW
		strcpy (content, _content);
	} else content = 0;
}

void GenericEvent::TagStr (char *str)
{
	strcpy (str, tag);
}

void GenericEvent::DescStr (char *str)
{
	if (content) strncpy (str, content, 100);
	else str[0] = '\0';
}

void GenericEvent::Write (ofstream &ofs)
{
	WriteEvent (ofs, tag, content);
}

void GenericEvent::DrawPreview() {
	if (content)
		ImGui::TextUnformatted(content);
	else
		ImGui::TextUnformatted("---");
}

void GenericEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
	char ltag[4096];
	char lcontent[4096];
	if(tag) strncpy(ltag, tag, 4095);
	if(content) strncpy(lcontent, content, 4095);
	ltag[4095]='\0';
	lcontent[4095]='\0';
	if(ImGui::InputText("Tag", ltag, sizeof(ltag))) {
		SetTag(ltag);
	}
	if(ImGui::InputText("Content", lcontent, sizeof(lcontent))) {
		SetContent(lcontent);
	}
}

// =========================================================

TaccEvent::TaccEvent (double _t0, double _tacc, float _delay): PlaybackEvent (_t0)
{
	tmp_tacc = tacc = _tacc;
	delay = _delay;
	sprintf(tmp_delay, "%f", delay);
}

void TaccEvent::TagStr (char *str)
{
	strcpy (str, "TACC");
}

void TaccEvent::DescStr (char *str)
{
	if (delay) {
		sprintf (str, "%0.2fx (delay %0.1f)", tacc, delay);
	} else {
		sprintf (str, "%0.2fx", tacc);
	}
}

void TaccEvent::Write (ofstream &ofs)
{
	char cbuf[256];
	if (delay) sprintf (cbuf, "%f %f", tacc, delay);
	else       sprintf (cbuf, "%f", tacc);
	WriteEvent (ofs, "TACC", cbuf);
}

void TaccEvent::DrawPreview() {
	if (delay) {
		ImGui::Text ("%0.2fx (delay %0.1f)", tacc, delay);
	} else {
		ImGui::Text ("%0.2fx", tacc);
	}
}

void TaccEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();

    ImGui::SliderFloat("Time Acceleration", &tmp_tacc, 0.1f, 10000.0f, "%.1f", ImGuiSliderFlags_Logarithmic);
	ImGui::InputText("Delay (s)", tmp_delay, sizeof(tmp_delay), ImGuiInputTextFlags_CharsDecimal);

}
void TaccEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
	tacc = tmp_tacc;
	delay = atof(tmp_delay);
}
// =========================================================

CameraEvent::CameraEvent (double _t0, int _preset): PlaybackEvent (_t0)
{
	if (_preset != -1) {
		SetPreset (_preset);
	} else {
		char cbuf[256];
		CameraMode *cm = g_camera->GetCMode();
		cm->Store (cbuf);
		SetInlineMode (cbuf);
	}
}

CameraEvent::CameraEvent (double _t0, char *_modestr): PlaybackEvent (_t0)
{
	SetInlineMode (_modestr);
}

void CameraEvent::SetPreset (int _preset, bool editmode)
{
	if(!editmode) {
		sprintf (modestr, "PRESET %d", _preset);
	}
	sprintf (m_tmp_modestr, "PRESET %d", _preset);
}

void CameraEvent::SetInlineMode (char *mode, bool editmode)
{
	if(!editmode) {
		strcpy (modestr, "SET ");
		strcat (modestr, mode);
	}
	strcpy (m_tmp_modestr, "SET ");
	strcat (m_tmp_modestr, mode);
}

void CameraEvent::TagStr (char *str)
{
	strcpy (str, "CAMERA");
}

void CameraEvent::DescStr (char *str)
{
	strcpy (str, modestr);
}

void CameraEvent::Write (ofstream &ofs)
{
	WriteEvent (ofs, "CAMERA", modestr);
}

void CameraEvent::DrawPreview() {
	ImGui::TextUnformatted (modestr);
}

void CameraEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
	ImGui::Text("Camera configuration: ");
	ImGui::SameLine();
	ImGui::TextUnformatted(m_tmp_modestr);

	char descr[256];
	int np = g_camera->nPreset();
	if(np) {
		ImGui::Separator();
		ImGui::TextUnformatted("Presets:");
		for(int i=0;i<np;i++) {
			CameraMode *cm = g_camera->GetPreset (i);
			cm->GetDescr (descr, 256);
			char cbuf[280];
			sprintf(cbuf, "%d (%s)", i, descr);
			if(ImGui::Button(cbuf)) {
				SetPreset(i, true);
			}
		}
	}
	ImGui::Separator();
	ImGui::Text("Current view: ");
	CameraMode *cm = g_camera->GetCMode();
	char cbuf[256];
	cm->Store (cbuf);
	ImGui::SameLine();
	if(ImGui::Button(cbuf)) {
		SetInlineMode (cbuf, true);
	}
}
void CameraEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();

	strcpy(modestr, m_tmp_modestr);
}

// =========================================================

NoteEvent::NoteEvent (double _t0, const char *_note): PlaybackEvent (_t0)
{
	note = strdup(_note);
	strncpy(m_tmp_note, note, sizeof(m_tmp_note));
}

NoteEvent::~NoteEvent ()
{
	if (note) free(note);
}

void NoteEvent::TagStr (char *str)
{
	strcpy (str, "NOTE");
}

void NoteEvent::DescStr (char *str)
{
	if (note) strncpy (str, note, 100);
	else str[0] = '\0';
}

void NoteEvent::Write (ofstream &ofs)
{
	WriteEvent (ofs, "NOTE", note ? note : "");
}

void NoteEvent::DrawPreview() {
	if (note) {
		ImGui::TextUnformatted (note);
	} else {
		ImGui::TextUnformatted ("---");
	}
}

void NoteEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
	ImGuiInputTextFlags flags = ImGuiInputTextFlags_AllowTabInput;
	ImGui::InputTextMultiline("##NoteEvent", m_tmp_note, IM_ARRAYSIZE(m_tmp_note), ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().y - 30), flags);
}
void NoteEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
	if(note) free(note);
	note = strdup(m_tmp_note);
}

// =========================================================

NoteposEvent::NoteposEvent (double _t0, double _x0, double _y0, double _x1, double _y1): PlaybackEvent (_t0)
{
	x0 = _x0;
	y0 = _y0;
	x1 = _x1;
	y1 = _y1;
}

void NoteposEvent::TagStr (char *str)
{
	strcpy (str, "NOTEPOS");
}

void NoteposEvent::DescStr (char *str)
{
	sprintf (str, "%g %g %g %g", x0, y1, x1, y1);
}

void NoteposEvent::Write (ofstream &ofs)
{
	char cbuf[256];
	sprintf (cbuf, "%g %g %g %g", x0, y0, x1, y1);
	WriteEvent (ofs, "NOTEPOS", cbuf);
}

void NoteposEvent::DrawPreview() {
	ImGui::Text ("%g %g %g %g", x0, y0, x1, y1);
}

void NoteposEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();

	ImVec2 virtual_screen_size(ImGui::GetContentRegionAvail().x, ImGui::GetContentRegionAvail().x * 3.0f/4.0f);
	ImGui::InvisibleButton("##empty", virtual_screen_size);
	ImDrawList* draw_list = ImGui::GetWindowDrawList();
	const ImVec2 p0 = ImGui::GetItemRectMin();
	const ImVec2 p1 = ImGui::GetItemRectMax();
	draw_list->AddRectFilled(p0, p1, IM_COL32(90, 90, 120, 255));

	ImVec2 wpos1 = ImVec2(m_offsetPos.x+p0.x+x0 * virtual_screen_size.x,m_offsetPos.y+ p0.y+y0 * virtual_screen_size.y);
	ImVec2 wpos2 = ImVec2(m_offsetPos.x+p0.x+m_offsetSize.x+x1 * virtual_screen_size.x,m_offsetPos.y+ p0.y+m_offsetSize.y+y1 * virtual_screen_size.y);

	ImGui::PushClipRect(p0, p1, true);
	draw_list->AddRectFilled(wpos1, wpos2, IM_COL32(70, 70, 100, 255));
	ImVec2 tri1 = ImVec2(wpos2.x - 10, wpos2.y);
	ImVec2 tri2 = ImVec2(wpos2.x, wpos2.y - 10);
	draw_list->AddRect(wpos1, wpos2, IM_COL32(20, 20, 40, 255));
	draw_list->AddTriangleFilled(wpos2, tri1, tri2, IM_COL32(20, 20, 40, 255));
	ImGui::PopClipRect();

	float mx = ImGui::GetIO().MousePos.x;
	float my = ImGui::GetIO().MousePos.y;
	
	if(wpos1.x<mx && mx <wpos2.x && wpos1.y<my && my<wpos2.y) {

		if( wpos2.x - 10 < mx && wpos2.y - 10 < my) {
			if(ImGui::IsMouseClicked(ImGuiMouseButton_Left))
				m_state = 1;
			else
				ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNWSE);
		} else {
			if(ImGui::IsMouseClicked(ImGuiMouseButton_Left))
				m_state = 2;
			else
				ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
	}
	if(ImGui::IsMouseReleased(ImGuiMouseButton_Left)) m_state = 0;

	if(m_state == 1) {
		ImGui::SetMouseCursor(ImGuiMouseCursor_ResizeNWSE);
		if(ImGui::IsMouseDragging(ImGuiMouseButton_Left)) {
			m_offsetSize.x += ImGui::GetIO().MouseDelta.x;
			m_offsetSize.y += ImGui::GetIO().MouseDelta.y;
		}
	}

	if(m_state == 2) {
		ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		if(ImGui::IsMouseDragging(ImGuiMouseButton_Left)) {
			m_offsetPos.x += ImGui::GetIO().MouseDelta.x;
			m_offsetPos.y += ImGui::GetIO().MouseDelta.y;
		}
	}

	m_tmp_x0 = (wpos1.x-p0.x)/virtual_screen_size.x;
	m_tmp_y0 = (wpos1.y-p0.y)/virtual_screen_size.y;
	m_tmp_x1 = (wpos2.x-p0.x)/virtual_screen_size.x;
	m_tmp_y1 = (wpos2.y-p0.y)/virtual_screen_size.y;
}
void NoteposEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
	x0 = m_tmp_x0;
	y0 = m_tmp_y0;
	x1 = m_tmp_x1;
	y1 = m_tmp_y1;

	m_offsetPos = {0,0};
	m_offsetSize = {0,0};
	m_state = 0;
}

// =========================================================

NotecolEvent::NotecolEvent (double _t0, double _r, double _g, double _b): PlaybackEvent (_t0)
{
	r = _r;
	g = _g;
	b = _b;
	m_tmp[0] = (float)r;
	m_tmp[1] = (float)g;
	m_tmp[2] = (float)b;
}

void NotecolEvent::TagStr (char *str)
{
	strcpy (str, "NOTECOL");
}

void NotecolEvent::DescStr (char *str)
{
	sprintf (str, "%g %g %g", r, g, b);
}

void NotecolEvent::Write (ofstream &ofs)
{
	char cbuf[256];
	sprintf (cbuf, "%g %g %g", r, g, b);
	WriteEvent (ofs, "NOTECOL", cbuf);
}

void NotecolEvent::DrawPreview() {
    ImVec4 col = { (float)r,(float)g,(float)b,1.0 };
	ImGui::TextColored(col, "COLOR");
}

void NotecolEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
	ImGui::Text("Note Color");
	ImGui::ColorPicker3("Color", m_tmp, ImGuiColorEditFlags_NoInputs|ImGuiColorEditFlags_NoLabel);
}
void NotecolEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
	r = m_tmp[0];
	g = m_tmp[1];
	b = m_tmp[2];
}

// =========================================================

NotesizeEvent::NotesizeEvent (double _t0, double _size): PlaybackEvent (_t0)
{
	size = _size;
	sprintf(m_tmp_size,"%f",size);
}

void NotesizeEvent::TagStr (char *str)
{
	strcpy (str, "NOTESIZE");
}

void NotesizeEvent::DescStr (char *str)
{
	sprintf (str, "%g", size);
}

void NotesizeEvent::Write (ofstream &ofs)
{
	char cbuf[256];
	sprintf (cbuf, "%g", size);
	WriteEvent (ofs, "NOTESIZE", cbuf);
}

void NotesizeEvent::DrawPreview() {
	ImGui::Text ("%g", size);
}

void NotesizeEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
	ImGui::InputText("Note Size", m_tmp_size, sizeof(m_tmp_size), ImGuiInputTextFlags_CharsDecimal);
}
void NotesizeEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
	size = atof(m_tmp_size);
}

// =========================================================

NoteoffEvent::NoteoffEvent (double _t0): PlaybackEvent (_t0)
{
}

void NoteoffEvent::TagStr (char *str)
{
	strcpy (str, "NOTEOFF");
}

void NoteoffEvent::DescStr (char *str)
{
	str[0] = '\0';
}

void NoteoffEvent::Write (ofstream &ofs)
{
	WriteEvent (ofs, "NOTEOFF", "");
}

void NoteoffEvent::DrawPreview() {
	ImGui::Text ("---", size);
}

void NoteoffEvent::DrawEdit() {
	PlaybackEvent::DrawEdit();
}
void NoteoffEvent::ApplyChanges() {
	PlaybackEvent::ApplyChanges();
}
