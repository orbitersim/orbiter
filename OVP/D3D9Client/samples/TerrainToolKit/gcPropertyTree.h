#pragma once

#include <vector>
#include <string>
#include <windows.h>
#include <gcCoreAPI.h>
#include <gcGUI.h>


using namespace std;

void gcPropertyTreeInitialize(HINSTANCE hInst);
void gcPropertyTreeRelease(HINSTANCE hInst);

#define GCGUI_MSG_SELECTED	0x0001


typedef struct gcProperty * HPROP;


class gcPropertyTree
{
	DWORD alloc_id;

public:

	enum Style { TEXT, TEXTBOX, COMBOBOX, SLIDER };
	enum Scale { LINEAR, LOG, SQRT, SQUARE };
	enum Format { NORMAL, LATITUDE, LONGITUDE, UNITS };

	typedef struct {
		Scale scl;
		double fmin, fmax, lin_pos;
		double lfmin, lfmax;
	} gcSlider;

	//		--------------------------------------------------------------------------

			gcPropertyTree(gcGUIApp *_pApp, HWND hWnd, WORD idc, DLGPROC pCall, HFONT hFnt, HINSTANCE hInst);
			~gcPropertyTree();

	//		--------------------------------------------------------------------------

	HWND	GetHWND() const;
	HWND	GetControl(HPROP hEntry);
	void*	GetUserRef(HPROP hEntry);
	HPROP	GetEntry(int idx);
	HPROP	GetEntry(HWND hCtrl);
	void	OpenEntry(HPROP hEntry, bool bOpen = true);
	void	ShowEntry(HPROP hEntry, bool bShow = true);

	//		--------------------------------------------------------------------------

	HPROP	AddEditControl(const string &lbl, WORD id, HPROP parent = NULL, const string &text = "", void *pUser = NULL);
	HPROP	AddComboBox(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);
	HPROP	AddSlider(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);
	LRESULT SendCtrlMessage(HPROP hCtrl, UINT uMsg, WPARAM wParam, LPARAM lParam);

	//		--------------------------------------------------------------------------

	HPROP	AddEntry(const string &lbl, HPROP hParent = NULL);
	HPROP   SubSection(const string &lbl, HPROP hParent = NULL);
	void	SetValue(HPROP hEntry, int val);
	void	SetValue(HPROP hEntry, DWORD val, bool bHex = false);
	void	SetValue(HPROP hEntry, double val, int digits = 6, Format st = Format::NORMAL);
	void	SetValue(HPROP hEntry, const string &val);
	void	SetValue(HPROP hEntry, const char *lbl);

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

	void	Update();
	void	Paint(HDC hDC);
	LRESULT WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

private:

	int		PaintSection(HDC hDC, HPROP hPar, int ident, int wlbl, int y, int lvl);
	void	PaintIcon(int x, int y, int id);
	int		GetSubsentionLength(HPROP hP);
	void	CopyToClipboard();
	void	CloseTree(HPROP hp);
	HWND	CreateEditControl(WORD id, bool bReadOnly = true);
	HWND	CreateComboBox(WORD id);
	HWND	CreateSlider(WORD id);
	bool	HasMoved(HPROP hP, int x, int y);

	gcCore2 *pCore;
	gcGUIApp *pApp;
	DLGPROC pCallback;
	HWND  hWnd, hDlg;
	HDC	hSr, hBM;
	HFONT hFont;
	HPEN hPen;
	HBRUSH hBr0, hBr1, hBr2, hBr4;
	HBRUSH hBrTit[3];
	HBITMAP hBuf;
	HBITMAP	hIcons;
	HINSTANCE hInst;
	HPROP pSelected, pDown;
	int wlbl, hlbl, wmrg, tmrg, bmrg, len;
	WORD idc;
	std::vector<HPROP> Data;
	char buffer[256];
	bool bOdd;
};


typedef struct gcProperty
{
	string label;
	string val;
	gcProperty *parent;
	bool bOpen;
	bool bVisible;
	bool bChildren;
	bool bOn;
	HWND hCtrl;
	void* pUser;
	gcPropertyTree::gcSlider *pSlider;
	RECT rect;
	gcPropertyTree::Style style;
	WORD idc;
	int oldx, oldy;
} gcProperty;