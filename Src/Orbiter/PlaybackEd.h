// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __PLAYBACKED_H
#define __PLAYBACKED_H

#include "Orbiter.h"
#include "imgui.h"

class PlaybackEditor;

// =========================================================
class PlaybackEvent;
class DlgPlaybackEditor : public ImGuiDialog {
public:
	DlgPlaybackEditor();
	~DlgPlaybackEditor();
	void OnDraw() override;

	void Load(const char *scenario);
	void ScanEventFile();
	void SaveEventFile();
	char *m_sysfname;         // system event file name
	std::list<PlaybackEvent *> m_Events;
	PlaybackEvent *m_SelectedEvent;

	void ClearEvents();
	void InsertEvent(PlaybackEvent *);
	void DeleteSelectedEvent();
};

class PlaybackEvent {
public:
	static PlaybackEvent *Create (char *event);
	PlaybackEvent (double _t0);
	virtual ~PlaybackEvent () {}
	inline double T0() const { return t0; }
	void TimeStr (char *str);
	virtual void TagStr (char *str) = 0;
	virtual void DescStr (char *str) { str[0] = '\0'; }
	virtual void Write (std::ofstream &ofs) = 0;
	void WriteEvent (std::ofstream &ofs, const char *eventtype, const char *event);
	virtual void DrawPreview() = 0;
	virtual void DrawEdit();
	virtual void ApplyChanges();
	char m_tmp_t0[32];
protected:
	double t0;         // event start time
};

// =========================================================

class GenericEvent: public PlaybackEvent {
public:
	GenericEvent (double _t0, const char *_tag, const char *_content);
	~GenericEvent ();
	void SetTag (const char *_tag);
	void SetContent (const char *_content);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;

private:
	char *tag;
	char *content;
};

// =========================================================

class TaccEvent: public PlaybackEvent {
public:
	TaccEvent (double _t0, double _tacc, float _delay = 0.0);
	inline double Tacc() const { return tacc; }
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	double tacc;      // time acceleration factor
	double delay;     // delay time
	float tmp_tacc;
	char tmp_delay[20];
};

// =========================================================

class CameraEvent: public PlaybackEvent {
public:
	CameraEvent (double _t0, int _preset = -1);
	CameraEvent (double _t0, char *_modestr);
	void SetPreset (int _preset, bool editmode = false);
	void SetInlineMode (char *mode, bool editmode = false);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	char modestr[256];
	char m_tmp_modestr[256];
};

// =========================================================

class NoteEvent: public PlaybackEvent {
public:
	NoteEvent (double _t0, const char *_note);
	~NoteEvent ();
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	char *note;
	char m_tmp_note[2048];
};

// =========================================================

class NoteposEvent: public PlaybackEvent {
public:
	NoteposEvent (double _t0, double _x0, double _y0, double _x1, double _y1);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	double x0, y0, x1, y1;
	ImVec2 m_offsetPos = {0,0};
	ImVec2 m_offsetSize = {0,0};
	int m_state = 0;
	double m_tmp_x0;
	double m_tmp_y0;
	double m_tmp_x1;
	double m_tmp_y1;
};

// =========================================================

class NotecolEvent: public PlaybackEvent {
public:
	NotecolEvent (double _t0, double _r, double _g, double _b);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	double r, g, b;
	float m_tmp[3];
};

// =========================================================

class NotesizeEvent: public PlaybackEvent {
public:
	NotesizeEvent (double _t0, double _size);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	double size;
	char m_tmp_size[20];
};

// =========================================================

class NoteoffEvent: public PlaybackEvent {
public:
	NoteoffEvent (double _t0);
	void TagStr (char *str) override;
	void DescStr (char *str) override;
	void Write (std::ofstream &ofs) override;
	void DrawPreview() override;
	void DrawEdit() override;
	void ApplyChanges() override;

private:
	double size;
	char m_tmp_size[20];
};

#endif // !__PLAYBACKED_H
