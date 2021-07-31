// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __PLAYBACKED_H
#define __PLAYBACKED_H

#include "Orbiter.h"

class PlaybackEditor;

// =========================================================

class PlaybackEvent {
public:
	static PlaybackEvent *Create (PlaybackEditor *editor, char *event);
	PlaybackEvent (PlaybackEditor *_editor, double _t0);
	virtual ~PlaybackEvent () {}
	inline double T0() const { return t0; }
	void TimeStr (char *str);
	virtual void TagStr (char *str) = 0;
	virtual void DescStr (char *str) { str[0] = '\0'; }
	virtual void Write (std::ofstream &ofs) = 0;
	void WriteEvent (std::ofstream &ofs, char *eventtype, char *event);
	virtual void EditEvent (PlaybackEditor *editor);
	virtual void CommitEdit ();
	virtual BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	PlaybackEditor *editor;
	double t0;         // event start time
};

// =========================================================

class GenericEvent: public PlaybackEvent {
public:
	GenericEvent (PlaybackEditor *_editor, double _t0, char *_tag, char *_content);
	~GenericEvent ();
	void SetTag (char *_tag);
	void SetContent (char *_content);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	char *tag;
	char *content;
};

// =========================================================

class TaccEvent: public PlaybackEvent {
public:
	TaccEvent (PlaybackEditor *_editor, double _t0, double _tacc, double _delay = 0.0);
	inline double Tacc() const { return tacc; }
	void SetTacc (double _tacc);
	void SetDelay (double _delay);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	double tacc;      // time acceleration factor
	double delay;     // delay time
};

// =========================================================

class CameraEvent: public PlaybackEvent {
public:
	CameraEvent (PlaybackEditor *_editor, double _t0, DWORD _preset = (DWORD)-1);
	CameraEvent (PlaybackEditor *_editor, double _t0, char *_modestr);
	inline DWORD Preset() const { return preset; }
	void SetPreset (DWORD _preset);
	void SetInlineMode (char *mode);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	void ScanPresets (HWND hTab);
	void AddCurrentView (HWND hTab);
	DWORD preset;
	char modestr[256];
};

// =========================================================

class NoteEvent: public PlaybackEvent {
public:
	NoteEvent (PlaybackEditor *_editor, double _t0, char *_note);
	~NoteEvent ();
	void SetNote (char *_note);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	char *note;
};

// =========================================================

class NoteposEvent: public PlaybackEvent {
public:
	NoteposEvent (PlaybackEditor *_editor, double _t0, double _x0, double _y0, double _x1, double _y1);
	void SetPos (double _x0, double _y0, double _x1, double _y1);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	double x0, y0, x1, y1;
};

// =========================================================

class NotecolEvent: public PlaybackEvent {
public:
	NotecolEvent (PlaybackEditor *_editor, double _t0, double _r, double _g, double _b);
	void SetCol (double _r, double _g, double _b);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	double r, g, b;
};

// =========================================================

class NotesizeEvent: public PlaybackEvent {
public:
	NotesizeEvent (PlaybackEditor *_editor, double _t0, double _size);
	void SetSize (double _size);
	void TagStr (char *str);
	void DescStr (char *str);
	void Write (std::ofstream &ofs);
	void EditEvent (PlaybackEditor *editor);
	void CommitEdit ();
	BOOL MsgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static INT_PTR CALLBACK EditProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:
	double size;
};

// =========================================================
// =========================================================

class PlaybackEditor {
public:
	PlaybackEditor (Orbiter *ob, const char *ScnName);
	~PlaybackEditor ();
	BOOL DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);
	HWND OpenEditTab (PlaybackEvent *event, int resid, DLGPROC tabproc);
	HWND EditTab () const { return hEdit; }
	void SortEvent (PlaybackEvent *e);

private:
	HWND OpenDialog ();
	void CloseDialog ();
	void ScanEventFile ();
	void SaveEventFile ();
	void RefreshEventList (PlaybackEvent *emark = 0);
	int  InsertTMarker ();
	void RefreshTMarker ();
	void CreateInsertEventList (HWND hDlg);
	void InsertEvent ();
	void DeleteEvent ();
	void CommitEdit ();
	void RegisterEdit (HWND hEdit);
	Orbiter *orbiter;
	char *sysfname;         // system event file name
	HWND hDlg;              // editor dialog handle
	HWND hEdit;             // currently displayed edit tab
	UINT timer;             // timer identifier
	int  tmarkidx;          // list index of "current time" marker
	struct Event {
		PlaybackEvent *e;
		struct Event *next;
	} *Efirst, *Elast;      // linked list of events
};

#endif // !__PLAYBACKED_H