// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// In-session options dialog
// ======================================================================

/************************************************************************
 * \file DlgOptions.h
 * \brief Implementation of the in-session options dialog.
 */

#ifndef __DLGOPTIONS_H
#define __DLGOPTIONS_H

#include "DialogWin.h"
#include "OptionsPages.h"
#include "CustomControls.h"
#include <windows.h>
#include <commctrl.h>
#include <winuser.h>

class OptionsPage;

/************************************************************************
 * \brief Options frame dialog.
 * 
 * This dialog acts as the frame for the individual options pages.
 */
class DlgOptions : public DialogWin, public OptionsPageContainer {
public:
	/**
	 * \brief Contructor for Options dialog object.
	 * \param hInstance application instance handle
	 * \param hParent parent window handle
	 * \param context pointer to context data
	 */
	DlgOptions(HINSTANCE hInstance, HWND hParent, void* context);

	/**
	 * \brief Destructor for Options dialog object.
	 */
	~DlgOptions();

	/**
	 * \brief Disables per-timestep updates.
	 */
	bool UpdateContinuously() const { return false; }

	void Update();

	BOOL OnInitDialog(HWND hDlg, WPARAM wParam, LPARAM lParam);

	BOOL OnCommand(HWND hDlg, WORD ctrlId, WORD notification, HWND hCtrl);

	BOOL OnSize(HWND hDlg, WPARAM wParam, int w, int h);

	BOOL OnVScroll(HWND hDlg, WORD request, WORD curpos, HWND hControl);

	BOOL OnNotify(HWND hDlg, int idCtrl, LPNMHDR pnmh);

protected:
	void SetSize(HWND hDlg);
};

#endif // !__DLGOPTIONS_H