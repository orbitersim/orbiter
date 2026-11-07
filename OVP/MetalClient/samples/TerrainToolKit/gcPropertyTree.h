#pragma once

#include <vector>
#include <string>
#include <functional>
#include "OrbiterAPI.h"

using namespace std;

typedef struct gcProperty * HPROP;


class gcPropertyTree
{
public:

	enum Style { TEXT, TEXTBOX, COMBOBOX, SLIDER };
	enum Scale { LINEAR, LOG, SQRT, SQUARE };
	enum Format { NORMAL, LATITUDE, LONGITUDE, UNITS };
	enum Event { CHANGED, SELECTED };

	typedef struct {
		Scale scl;
		double fmin, fmax, lin_pos;
		double lfmin, lfmax;
	} gcSlider;

	typedef std::function<void(HPROP hEntry, WORD id, Event ev)> Callback;

	//		--------------------------------------------------------------------------

			gcPropertyTree(WORD idc, Callback pCall);
			~gcPropertyTree();

	//		--------------------------------------------------------------------------

	void*	GetUserRef(HPROP hEntry);
	HPROP	GetEntry(int idx);
	HPROP	GetSelected() const { return pSelected; }
	void	OpenEntry(HPROP hEntry, bool bOpen = true);
	void	ShowEntry(HPROP hEntry, bool bShow = true);
	void	RemoveEntry(HPROP hEntry);

	//		--------------------------------------------------------------------------

	HPROP	AddEditControl(const string &lbl, WORD id, HPROP parent = NULL, const string &text = "", void *pUser = NULL);
	HPROP	AddComboBox(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);
	HPROP	AddSlider(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);

	//		--------------------------------------------------------------------------

	HPROP	AddEntry(const string &lbl, HPROP hParent = NULL);
	HPROP   SubSection(const string &lbl, HPROP hParent = NULL);
	void	SetValue(HPROP hEntry, int val);
	void	SetValue(HPROP hEntry, DWORD val, bool bHex = false);
	void	SetValue(HPROP hEntry, double val, int digits = 6, Format st = Format::NORMAL);
	void	SetValue(HPROP hEntry, const string &val, DWORD clr = 0);
	void	SetValue(HPROP hEntry, const char *lbl, DWORD clr = 0);

	//		--------------------------------------------------------------------------

	void	SetSliderScale(HPROP hSlider, double fmin, double fmax, Scale scl);
	void	SetSliderValue(HPROP hSlider, double val);
	double  GetSliderValue(HPROP hSlider);

	//		--------------------------------------------------------------------------

	string	GetTextBoxContent(HPROP hTextBox);
	void	SetTextBoxContent(HPROP hTextBox, string text);

	//		--------------------------------------------------------------------------

	void	SetComboBoxSelection(HPROP hCombo, int idx);
	int		GetComboBoxSelection(HPROP hCombo);
	void	ClearComboBox(HPROP hCombo);
	int		AddComboBoxItem(HPROP hCombo, const char *label);

	//		--------------------------------------------------------------------------

	void	Draw();

private:

	void	DrawSection(HPROP hPar);
	void	DrawControl(HPROP hp);
	float	LabelWidth(HPROP hPar, float indent);
	void	CopyToClipboard();
	void	Notify(HPROP hp, WORD id, Event ev);

	Callback pCallback;
	HPROP pSelected;
	WORD idc;
	std::vector<HPROP> Data;
};


typedef struct gcProperty
{
	string label;
	string val;
	gcProperty *parent;
	bool bOpen;
	bool bChildren;
	bool bOn;
	void* pUser;
	gcPropertyTree::gcSlider *pSlider;
	gcPropertyTree::Style style;
	vector<string> items;
	int sel;
	WORD idc;
	DWORD color;
} gcProperty;
