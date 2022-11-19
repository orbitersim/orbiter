// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Custom dialog control classes

#ifndef __CUSTOMCONTROLS_H
#define __CUSTOMCONTROLS_H

class CustomCtrl {
public:
	CustomCtrl ();
	CustomCtrl (HWND hCtrl);
	void SetHwnd (HWND hCtrl);
	HWND HWnd() const { return hWnd; }
	virtual LRESULT WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static void RegisterClass(HINSTANCE hInstance);

protected:
	HWND hWnd;
	HWND hParent;

private:
	static LRESULT CALLBACK s_WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
};

class GenericCtrl : public CustomCtrl {
public:
	GenericCtrl();
	GenericCtrl(HWND hCtrl);
};

class SplitterCtrl: public CustomCtrl {
public:
	enum PaneId { PANE_NONE=0, PANE1=1, PANE2=2 };
	SplitterCtrl ();
	SplitterCtrl (HWND hCtrl);
	void SetHwnd (HWND hCtrl, HWND hPane1, HWND hPane2);
	void SetStaticPane (PaneId which, int width=0);
	int GetPaneWidth (PaneId which);
	LRESULT WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

protected:
	BOOL OnSize (HWND hWnd, WPARAM wParam, int w, int h);
	BOOL OnLButtonDown (HWND hWnd, LONG modifier, short x, short y);
	BOOL OnLButtonUp (HWND hWnd, LONG modifier, short x, short y);
	BOOL OnMouseMove (HWND hWnd, short x, short y);
	void Refresh ();

private:
	HWND hPane[2];
	int paneW[2];
	int splitterW;
	int totalW;
	PaneId staticPane;
	double widthRatio;
	bool isPushing;
	short mouseX, mouseY;
	HCURSOR m_hCursor;
};

#endif // !__CUSTOMCONTROLS_H