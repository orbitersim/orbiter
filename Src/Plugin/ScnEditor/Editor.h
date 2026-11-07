// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//              ORBITER MODULE: Scenario Editor
//                  Part of the ORBITER SDK
//
// Editor.h
//
// Interface definition for the ScnEditor dialog and its pages.
// ==============================================================

#ifndef __SCNEDITOR_H
#define __SCNEDITOR_H

#include "OrbiterAPI.h"
#include "VesselAPI.h"
#include "ScnEditorAPI.h"
#include "Convert.h"
#include <memory>
#include <string>
#include <vector>

class ScnEditorPage;

enum {
	PG_VESSEL,
	PG_NEW,
	PG_SAVE,
	PG_EDIT,
	PG_ELEMENTS,
	PG_STATEVEC,
	PG_LANDED,
	PG_ORIENTATION,
	PG_ANGVEL,
	PG_PROPELLANT,
	PG_DOCKING,
	PG_DATE,
	PG_COUNT
};

// ==============================================================
// class ScnEditor
// ==============================================================

class ScnEditor: public ImGuiDialog, public ScnEditorInterface {
public:
	ScnEditor ();
	~ScnEditor ();
	void Open ();
	void Close ();
	void Display () override;

	void SwitchPage (int page);
	int AddPage (ScnEditorPage *page);
	void DelCustomPages ();
	bool CreateVessel (const char *name, const char *classname);
	void VesselDeleted (OBJHANDLE hV);
	void *LoadVesselLibrary (const VESSEL *vessel);
	void FreeVesselLibrary ();

	bool AddFuncButton (const EditorFuncSpec *spec) override;
	bool AddPageButton (const EditorPageSpec *spec) override;

	OBJHANDLE hVessel;

protected:
	void OnDraw () override {}
	void OnClose () override;

private:
	void ShowPage (int page);

	std::vector<std::unique_ptr<ScnEditorPage>> pages;
	int cur;
	int pending;
	void *hEdLib;
	DWORD dwCmd;
	int dwMenuCmd;
};

// ==============================================================
// class ScnEditorPage: base class for editor pages
// ==============================================================

class ScnEditorPage {
public:
	ScnEditorPage (ScnEditor *editor): ed (editor) {}
	virtual ~ScnEditorPage () {}
	virtual void Init () {}
	virtual void Draw () = 0;
	virtual void Footer () = 0;
	virtual const char *HelpTopic () { return "/ScnEditor.htm"; }
	virtual void OpenHelp ();
	VESSEL *Vessel () { return oapiIsVessel (ed->hVessel) ? oapiGetVesselInterface (ed->hVessel) : 0; }

protected:
	ScnEditor *ed;
};

// ==============================================================

class EditorPage_Vessel: public ScnEditorPage {
public:
	EditorPage_Vessel (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	void VesselDeleted (OBJHANDLE hV);

protected:
	void SelectVessel (OBJHANDLE hV);
	void VesselSelected ();
	void EditVessel ();
	void DeleteVessel ();
	bool CanDelete (OBJHANDLE hV);

private:
	OBJHANDLE sel;
	bool track;
	bool deleteError;
};

// ==============================================================

class EditorPage_New: public ScnEditorPage {
public:
	struct Node {
		std::string name;
		std::string path;
		bool dir;
		std::vector<Node> children;
	};

	EditorPage_New (ScnEditor *editor);
	~EditorPage_New ();
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/NewVessel.htm"; }

protected:
	void ScanConfigDir (const char *dir, const std::string &prefix, std::vector<Node> &list);
	void DrawTree (const std::vector<Node> &list);
	void SelectType (const std::string &path);
	bool CreateVessel ();
	void ReleaseImage ();

private:
	char name[256];
	bool camera;
	bool focus;
	std::vector<Node> tree;
	std::string seltype;
	SURFHANDLE img;
	int imgw, imgh;
	bool create;
};

// ==============================================================

class EditorPage_Save: public ScnEditorPage {
public:
	EditorPage_Save (ScnEditor *editor);
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/SaveScenario.htm"; }

protected:
	bool SaveScenario ();

private:
	char fname[256];
	char title[256];
	char text[4096];
};

// ==============================================================

class EditorPage_Edit: public ScnEditorPage {
public:
	EditorPage_Edit (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/EditVessel.htm"; }
	bool AddFuncButton (const EditorFuncSpec *spec);
	bool AddPageButton (const EditorPageSpec *spec);
	void Reset () { hVessel = 0; }

private:
	struct CustomButton {
		std::string label;
		CustomButtonFunc func;
		int page;
	};
	OBJHANDLE hVessel;
	std::vector<CustomButton> custom;
};

// ==============================================================

class EditorPage_Date: public ScnEditorPage {
public:
	EditorPage_Date (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Date.htm"; }

protected:
	void Apply ();
	void Refresh ();
	void UpdateDateTime ();
	void SetUT (struct tm *new_date, bool reset_ut = false);
	void SetMJD (double new_mjd);
	void OnChangeDateTime ();

private:
	double mjd;
	struct tm date;
	int ut[6];
	double vmjd, vjd, vjc, vepoch;
	int propOrbital;
	int propSuborbital;
};

// ==============================================================

class EditorPage_Elements: public ScnEditorPage {
public:
	EditorPage_Elements (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Elements.htm"; }

protected:
	void Apply ();
	void Refresh ();
	double Scale (int i) const;
	double &Element (int i);

private:
	ELEMENTS el;
	ORBITPARAM prm;
	double elmjd;
	OBJHANDLE hRef;
	int frm;
	int epc;
	int unit[6];
	double val[6];
	int prec;
	bool valid;
};

// ==============================================================

class EditorPage_Statevec: public ScnEditorPage {
public:
	EditorPage_Statevec (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Statevec.htm"; }

protected:
	void Refresh (OBJHANDLE hV = NULL);
	void Apply ();

private:
	OBJHANDLE hRef;
	int frm;
	int crd;
	double val[6];
	OBJHANDLE hCopy;
};

// ==============================================================

class EditorPage_Landed: public ScnEditorPage {
public:
	EditorPage_Landed (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Location.htm"; }

protected:
	void SelectCBody (OBJHANDLE hBody);
	void SelectBase (OBJHANDLE hBase);
	void SetBasePosition ();
	void Refresh (OBJHANDLE hV = NULL);
	void Apply ();

private:
	OBJHANDLE hRef;
	OBJHANDLE hBase;
	int pad;
	double lng, lat, hdg;
	OBJHANDLE hCopy;
};

// ==============================================================

class EditorPage_Orientation: public ScnEditorPage {
public:
	EditorPage_Orientation (ScnEditor *editor);
	void Init () override { Refresh (); }
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Orientation.htm"; }

protected:
	void Refresh ();
	void Apply ();
	void Rotate (int axis, double da);

private:
	double arot[3];
};

// ==============================================================

class EditorPage_AngularVel: public ScnEditorPage {
public:
	EditorPage_AngularVel (ScnEditor *editor);
	void Init () override { Refresh (); }
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/AngularVel.htm"; }

protected:
	void Refresh ();
	void Apply ();
	void Killrot ();

private:
	double avel[3];
};

// ==============================================================

class EditorPage_Propellant: public ScnEditorPage {
public:
	EditorPage_Propellant (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Propellant.htm"; }

protected:
	void Refresh ();
	void Apply ();
	void SetLevel (double lvl, bool setall = false);

private:
	int tank;
	int ntank;
	double level;
	double mass;
	double maxmass;
	int gauge;
	bool massEdited;
};

// ==============================================================

class EditorPage_Docking: public ScnEditorPage {
public:
	EditorPage_Docking (ScnEditor *editor);
	void Init () override;
	void Draw () override;
	void Footer () override;
	const char *HelpTopic () override { return "/Docking.htm"; }

protected:
	int DockNo ();
	void SetTargetDock (int dock);
	void SetIDSChannel (double freq, int dch);
	void Dock ();
	void Undock ();
	void Refresh ();

private:
	int dock;
	OBJHANDLE hTarget;
	int tgtdock;
	int mode;
	double freq;
	bool freqEditing;
	std::string errmsg;
};

// ==============================================================

class EditorPage_Custom: public ScnEditorPage {
public:
	EditorPage_Custom (ScnEditor *editor, const EditorPageSpec *spec);
	void Draw () override;
	void Footer () override;
	void OpenHelp () override;

private:
	CustomPageFunc func;
	HELPCONTEXT *help;
};

#endif // !__SCNEDITOR_H
