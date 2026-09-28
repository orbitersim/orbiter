// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Custom dialog control classes

#ifndef __CUSTOMCONTROLS_H
#define __CUSTOMCONTROLS_H

#ifndef __linux__
class CustomCtrl {
#else // __linux__
#include "OrbiterPlatform.h"
#include <QObject>
#include <QWidget>

class QMouseEvent;

// attaches to an OrbiterDlgCtrl widget and receives its events (window procedure counterpart)
class CustomCtrl: public QObject {
#endif // __linux__
public:
	CustomCtrl ();
#ifndef __linux__
	CustomCtrl (HWND hCtrl);
	void SetHwnd (HWND hCtrl);
	HWND HWnd() const { return hWnd; }
	virtual LRESULT WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	static void RegisterClass(HINSTANCE hInstance);
#else // __linux__
	CustomCtrl (QWidget *hCtrl);
	void SetHwnd (QWidget *hCtrl);
	QWidget *HWnd() const { return hWnd; }
	virtual bool WndProc (QWidget *hWnd, QEvent *event); // true: event handled
	static void RegisterClass(void *hInstance);
#endif // __linux__

protected:
#ifndef __linux__
	HWND hWnd;
	HWND hParent;
#else // __linux__
	QWidget *hWnd = nullptr;
	QWidget *hParent = nullptr;
#endif // __linux__

private:
#ifndef __linux__
	static LRESULT CALLBACK s_WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
	bool eventFilter (QObject *obj, QEvent *event) override;
#endif // __linux__
};

class GenericCtrl : public CustomCtrl {
public:
	GenericCtrl();
#ifndef __linux__
	GenericCtrl(HWND hCtrl);
#else // __linux__
	GenericCtrl(QWidget *hCtrl);
#endif // __linux__
};

class SplitterCtrl: public CustomCtrl {
public:
	enum PaneId { PANE_NONE=0, PANE1=1, PANE2=2 };
	SplitterCtrl ();
#ifndef __linux__
	SplitterCtrl (HWND hCtrl);
	void SetHwnd (HWND hCtrl, HWND hPane1, HWND hPane2);
#else // __linux__
	SplitterCtrl (QWidget *hCtrl);
	void SetHwnd (QWidget *hCtrl, QWidget *hPane1, QWidget *hPane2);
#endif // __linux__
	void SetStaticPane (PaneId which, int width=0);
	int GetPaneWidth (PaneId which);
#ifndef __linux__
	LRESULT WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#else // __linux__
	bool WndProc (QWidget *hWnd, QEvent *event) override;
#endif // __linux__

protected:
#ifndef __linux__
	BOOL OnSize (HWND hWnd, WPARAM wParam, int w, int h);
	BOOL OnLButtonDown (HWND hWnd, LONG modifier, short x, short y);
	BOOL OnLButtonUp (HWND hWnd, LONG modifier, short x, short y);
	BOOL OnMouseMove (HWND hWnd, short x, short y);
#else // __linux__
	BOOL OnSize (QWidget *hWnd, int w, int h);
	BOOL OnLButtonDown (QWidget *hWnd, Qt::KeyboardModifiers modifier, short x, short y);
	BOOL OnLButtonUp (QWidget *hWnd, Qt::KeyboardModifiers modifier, short x, short y);
	BOOL OnMouseMove (QWidget *hWnd, short x, short y);
#endif // __linux__
	void Refresh ();

private:
#ifndef __linux__
	HWND hPane[2];
#else // __linux__
	QWidget *hPane[2];
#endif // __linux__
	int paneW[2];
	int splitterW;
	int totalW;
	PaneId staticPane;
	double widthRatio;
	bool isPushing;
	short mouseX, mouseY;
#ifndef __linux__
	HCURSOR m_hCursor;
#endif // !__linux__
};

#endif // !__CUSTOMCONTROLS_H
