// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
//                     ORBITER SOFTWARE DEVELOPMENT KIT
// ScnEditorAPI.h
// Scenario editor plugin interface
// ======================================================================

#ifndef __SCNEDITORAPI_H
#define __SCNEDITORAPI_H

#include "OrbiterAPI.h"
#ifdef __linux__
#include <QWidget>
#include <QVariant>
#endif // __linux__

#ifndef __linux__
#define WM_SCNEDITOR WM_USER
#else // __linux__
// WM_SCNEDITOR (WM_USER) left out: no window messages, the requests go through ScnEditorMsg below
#endif // __linux__

#define SE_ADDFUNCBUTTON 0x01
#define SE_ADDPAGEBUTTON 0x02
#define SE_GETVESSEL     0x04

typedef void (*CustomButtonFunc)(OBJHANDLE);

typedef struct {
	char btnlabel[32];
	CustomButtonFunc func;
} EditorFuncSpec;

#ifdef __linux__
// TabProc: called once when the page is built, with the edited vessel (OBJHANDLE) as context (WM_INITDIALOG lParam)
#endif // __linux__
typedef struct {
	char btnlabel[32];
#ifndef __linux__
	HINSTANCE hDLL;
#else // __linux__
	void *hDLL;
#endif // __linux__
	WORD ResId;
#ifndef __linux__
	DLGPROC TabProc;
#else // __linux__
	DLGINIT TabProc;
#endif // __linux__
} EditorPageSpec;
#ifdef __linux__

// not upstream: SendMessage (hWnd, WM_SCNEDITOR, wParam, lParam) counterpart for the page passed to secInit or a custom page
typedef INT_PTR (*SCNEDITORMSG)(QWidget *hWnd, WPARAM wParam, LPARAM lParam);
inline INT_PTR ScnEditorMsg (QWidget *hWnd, WPARAM wParam, LPARAM lParam)
{
	SCNEDITORMSG msgproc = (SCNEDITORMSG)hWnd->property ("ScnEditorMsg").value<void*>();
	return (msgproc ? msgproc (hWnd, wParam, lParam) : 0);
}
#endif // __linux__

#endif // !__SCNEDITORAPI_H
