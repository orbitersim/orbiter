#pragma once

#include <vector>
#include <string>
// windows.h left out: the Win32 types come from OrbiterPlatform.h
#include <gcCoreAPI.h>
#include <gcGUI.h>


using namespace std;

void gcPropertyTreeInitialize(void *hInst);
void gcPropertyTreeRelease(void *hInst);

#define GCGUI_MSG_SELECTED	0x0001


typedef struct gcProperty * HPROP;

// not upstream: the WM_COMMAND (id, notification code, entry) the tree sent to its DLGPROC; codes are RESN_*, TB_THUMBTRACK or GCGUI_MSG_SELECTED
typedef void (*GCPROPCLBK)(QWidget *hDlg, WORD id, WORD code, HPROP hEntry);

class QEvent;


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

			gcPropertyTree(gcGUIApp *_pApp, QWidget *hWnd, WORD idc, GCPROPCLBK pCall, QFont *hFnt, void *hInst);
			~gcPropertyTree();

	//		--------------------------------------------------------------------------

	QWidget* GetHWND() const;
	QWidget* GetControl(HPROP hEntry);
	void*	GetUserRef(HPROP hEntry);
	HPROP	GetEntry(int idx);
	HPROP	GetEntry(QWidget *hCtrl);
	void	OpenEntry(HPROP hEntry, bool bOpen = true);
	void	ShowEntry(HPROP hEntry, bool bShow = true);

	//		--------------------------------------------------------------------------

	HPROP	AddEditControl(const string &lbl, WORD id, HPROP parent = NULL, const string &text = "", void *pUser = NULL);
	HPROP	AddComboBox(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);
	HPROP	AddSlider(const string &lbl, WORD id, HPROP parent = NULL, void* pUser = NULL);
	// SendCtrlMessage left out: raw window messages to the child controls; nothing calls it, the typed functions below are its uses

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

	void	Update();
	void	Paint(QPainter *hDC);
	bool	WndProc(QWidget *hWnd, QEvent *e);
	void	CtrlNotify(QWidget *hCtrl, WORD code);

private:

	int		PaintSection(QPainter *hDC, HPROP hPar, int ident, int wlbl, int y, int lvl);
	void	PaintIcon(int x, int y, int id);
	int		GetSubsentionLength(HPROP hP);
	void	CopyToClipboard();
	void	CloseTree(HPROP hp);
	QWidget* CreateEditControl(WORD id, bool bReadOnly = true);
	QWidget* CreateComboBox(WORD id);
	QWidget* CreateSlider(WORD id);
	bool	HasMoved(HPROP hP, int x, int y);

	gcCore2 *pCore;
	gcGUIApp *pApp;
	GCPROPCLBK pCallback;
	QWidget *hWnd, *hDlg;
	QPainter *hBM;		// memory DC on hBuf, during Paint (hSr, the icon source DC, left out: icons are drawn from their image)
	QFont *hFont;
	QPen *hPen;
	QBrush *hBr0, *hBr1, *hBr2, *hBr4;
	QBrush *hBrTit[3];
	QImage *hBuf;
	QImage *hIcons;
	void *hInst;
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
	QWidget *hCtrl;
	void* pUser;
	gcPropertyTree::gcSlider *pSlider;
	RECT rect;
	gcPropertyTree::Style style;
	WORD idc;
	int oldx, oldy;
	DWORD color;
} gcProperty;
