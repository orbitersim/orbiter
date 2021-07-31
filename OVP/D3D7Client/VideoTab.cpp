// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//   ORBITER VISUALISATION PROJECT (OVP)
//   D3D7 Client module
// ==============================================================

// ==============================================================
// VideoTab.cpp
// Class VideoTab (implementation)
// Manages the user selections in the "Video" tab of the Orbiter
// Launchpad dialog.
// ==============================================================

// Note: must include D3D7Client.h *first* to fix warnings on VS 2003+
#include "D3D7Client.h"
#include "VideoTab.h"
#include "resource.h"
#include "resource_video.h"
#include <stdio.h>

using namespace oapi;

// ==============================================================
// Constructor

VideoTab::VideoTab (D3D7Client *gc, HINSTANCE _hInst, HINSTANCE _hOrbiterInst, HWND hVideoTab)
{
	gclient      = gc;
	hInst        = _hInst;
	hOrbiterInst = _hOrbiterInst;
	hTab         = hVideoTab;
	Initialise (gclient->CurrentDevice());
}

// ==============================================================
// Dialog message handler

BOOL VideoTab::WndProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDC_VID_DEVICE:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				DWORD idx = SendDlgItemMessage (hWnd, IDC_VID_DEVICE, CB_GETCURSEL, 0, 0);
				D3D7Enum_DeviceInfo *dev = gclient->PickDevice (idx);
				if (idx) {
					gclient->SelectDevice (dev);
					SelectDevice (dev);
				}
				return TRUE;
			}
			break;
		case IDC_VID_MODE:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				DWORD idx;
				idx = SendDlgItemMessage (hWnd, IDC_VID_MODE, CB_GETCURSEL, 0, 0);
				SelectMode (gclient->CurrentDevice(), idx);
				return TRUE;
			}
			break;
		case IDC_VID_BPP:
			if (HIWORD(wParam) == CBN_SELCHANGE) {
				DWORD idx;
				idx = SendDlgItemMessage (hWnd, IDC_VID_BPP, CB_GETCURSEL, 0, 0);
				SelectBPP (gclient->CurrentDevice(), idx);
				return TRUE;
			}
			break;
		case IDC_VID_PAGEFLIP:
			if (HIWORD(wParam) == BN_CLICKED) {
				SelectPageflip();
				return TRUE;
			}
			break;
		case IDC_VID_FULL:
			if (HIWORD(wParam) == BN_CLICKED) {
				SelectDispmode (gclient->CurrentDevice(), FALSE);
				return TRUE;
			}
			break;
		case IDC_VID_WINDOW:
			if (HIWORD(wParam) == BN_CLICKED) {
				SelectDispmode (gclient->CurrentDevice(), TRUE);
				return TRUE;
			}
			break;
		case IDC_VID_WIDTH:
			if (HIWORD(wParam) == EN_CHANGE) {
				SelectWidth ();
				return TRUE;
			}
			break;
		case IDC_VID_HEIGHT:
			if (HIWORD(wParam) == EN_CHANGE) {
				SelectHeight ();
				return TRUE;
			}
			break;
		case IDC_VID_ASPECT:
			if (HIWORD(wParam) == BN_CLICKED) {
				SelectFixedAspect ();
				SelectWidth ();
				return TRUE;
			}
			break;
		case IDC_VID_4X3:
		case IDC_VID_16X10:
		case IDC_VID_16X9:
			if (HIWORD(wParam) == BN_CLICKED) {
				aspect_idx = LOWORD(wParam)-IDC_VID_4X3;
				SelectWidth ();
				return TRUE;
			}
			break;
		case IDC_VID_INFO:
			DialogBox (hInst, MAKEINTRESOURCE(IDD_DIALOG1), hTab, AboutDlgProc);
			return TRUE;
		}
		break;
	}
	return FALSE;
}

// ==============================================================
// Initialise the Launchpad "video" tab

void VideoTab::Initialise (D3D7Enum_DeviceInfo *dev)
{
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();
	
	D3D7Enum_DeviceInfo *devlist;
	char cbuf[20];
	DWORD i, ndev, idx;
	D3D7Enum_GetDevices (&devlist, &ndev);

	SendDlgItemMessage (hTab, IDC_VID_DEVICE, CB_RESETCONTENT, 0, 0);
	for (i = 0; i < ndev; i++) {
		SendMessage (GetDlgItem (hTab, IDC_VID_DEVICE), CB_ADDSTRING, 0,
			(LPARAM)(devlist[i].strDesc));
	}
	if (SendDlgItemMessage (hTab, IDC_VID_DEVICE, CB_SETCURSEL, data->deviceidx, 0) == CB_ERR) {
		if ((idx = SendDlgItemMessage (hTab, IDC_VID_DEVICE, CB_FINDSTRINGEXACT, -1, (LPARAM)dev->strDesc)) == CB_ERR)
			idx = 0;
		SendDlgItemMessage (hTab, IDC_VID_DEVICE, CB_SETCURSEL, idx, 0);
	}
	SendDlgItemMessage (hTab, IDC_VID_ENUM, BM_SETCHECK, data->forceenum ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VID_STENCIL, BM_SETCHECK, data->trystencil ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VID_VSYNC, BM_SETCHECK, data->novsync ? BST_CHECKED : BST_UNCHECKED, 0);
	SendDlgItemMessage (hTab, IDC_VID_PAGEFLIP, BM_SETCHECK, data->pageflip ? BST_UNCHECKED : BST_CHECKED, 0);

	SetWindowText (GetDlgItem (hTab, IDC_VID_WIDTH), _itoa (data->winw, cbuf, 10));
	SetWindowText (GetDlgItem (hTab, IDC_VID_HEIGHT), _itoa (data->winh, cbuf, 10));

	if (data->winw == (4*data->winh)/3 || data->winh == (3*data->winw)/4)
		aspect_idx = 1;
	else if (data->winw == (16*data->winh)/10 || data->winh == (10*data->winw)/16)
		aspect_idx = 2;
	else if (data->winw == (16*data->winh)/9 || data->winh == (9*data->winw)/16)
		aspect_idx = 3;
	else
		aspect_idx = 0;
	SendDlgItemMessage (hTab, IDC_VID_ASPECT, BM_SETCHECK, aspect_idx ? BST_CHECKED : BST_UNCHECKED, 0);
	if (aspect_idx) aspect_idx--;
	SendDlgItemMessage (hTab, IDC_VID_4X3+aspect_idx, BM_SETCHECK, BST_CHECKED, 0);

	SelectDevice (dev);
	SelectDispmode (dev, data->fullscreen ? FALSE:TRUE);

	ShowWindow (GetDlgItem (hTab, IDC_VID_INFO), SW_SHOW);
}

// ==============================================================
// Respond to user device selection

void VideoTab::SelectDevice (D3D7Enum_DeviceInfo *dev)
{
	DWORD i, j;
	char cbuf[256];
	DDSURFACEDESC2 &cmode = dev->ddsdFullscreenMode;
	DWORD nres = 0, *wres = new DWORD[dev->dwNumModes], *hres = new DWORD[dev->dwNumModes];
	DWORD nbpp = 0, *bpp = new DWORD[dev->dwNumModes];

	SendDlgItemMessage (hTab, IDC_VID_MODE, CB_RESETCONTENT, 0, 0);
	SendDlgItemMessage (hTab, IDC_VID_BPP, CB_RESETCONTENT, 0, 0);

	for (i = 0; i < dev->dwNumModes; i++) {
		DDSURFACEDESC2 *ddsd = dev->pddsdModes+i;
		DWORD w = ddsd->dwWidth, h = ddsd->dwHeight;
		for (j = 0; j < nres; j++) if (wres[j] == w && hres[j] == h) break;
		if (j == nres) wres[nres] = w, hres[nres] = h, nres++;
		DWORD bc = ddsd->ddpfPixelFormat.dwRGBBitCount;
		for (j = 0; j < nbpp; j++) if (bpp[j] == bc) break;
		if (j == nbpp) bpp[nbpp++] = bc;
	}
	for (i = 0; i < nres; i++) {
		sprintf (cbuf, "%d x %d", wres[i], hres[i]);
		SendDlgItemMessage (hTab, IDC_VID_MODE, CB_ADDSTRING, 0, (LPARAM)cbuf);
		SendDlgItemMessage (hTab, IDC_VID_MODE, CB_SETITEMDATA, i, (LPARAM)(hres[i]<<16 | wres[i]));
		if (wres[i] == cmode.dwWidth && hres[i] == cmode.dwHeight)
			SendDlgItemMessage (hTab, IDC_VID_MODE, CB_SETCURSEL, i, 0);
	}
	for (i = 0; i < nbpp; i++) {
		sprintf (cbuf, "%d", bpp[i]);
		SendDlgItemMessage (hTab, IDC_VID_BPP, CB_ADDSTRING, 0, (LPARAM)cbuf);
		SendDlgItemMessage (hTab, IDC_VID_BPP, CB_SETITEMDATA, i, (LPARAM)(bpp[i]));
		if (bpp[i] == cmode.ddpfPixelFormat.dwRGBBitCount)
			SendDlgItemMessage (hTab, IDC_VID_BPP, CB_SETCURSEL, i, 0);
	}
	for (i = 0; i < 2; i++)
		EnableWindow (GetDlgItem (hTab, IDC_VID_FULL+i), TRUE);
	SendDlgItemMessage (hTab, dev->bWindowed ? IDC_VID_WINDOW:IDC_VID_FULL, BM_CLICK, 0, 0);
	for (i = 0; i < 2; i++)
		EnableWindow (GetDlgItem (hTab, IDC_VID_FULL+i), dev->bDesktopCompatible);
	delete []wres;
	delete []hres;
	delete []bpp;
}

// ==============================================================
// Respond to user selection of fullscreen/window mode

void VideoTab::SelectDispmode (D3D7Enum_DeviceInfo *dev, BOOL bWindow)
{
	DWORD i;
	for (i = 0; i < 6; i++)
		EnableWindow (GetDlgItem (hTab, IDC_VID_STATIC5+i), !bWindow);
	for (i = 0; i < 9; i++)
		EnableWindow (GetDlgItem (hTab, IDC_VID_STATIC7+i), bWindow);
	if (!bWindow) {
		if (SendDlgItemMessage (hTab, IDC_VID_PAGEFLIP, BM_GETCHECK, 0, 0) == BST_CHECKED)
			EnableWindow (GetDlgItem (hTab, IDC_VID_VSYNC), FALSE);
	} else {
		if (SendDlgItemMessage (hTab, IDC_VID_ASPECT, BM_GETCHECK, 0, 0) != BST_CHECKED) {
			for (i = 0; i < 3; i++)
				EnableWindow (GetDlgItem (hTab, IDC_VID_4X3+i), FALSE);
		}
	}

	dev->bWindowed = bWindow;
}

// ==============================================================
// Respond to user selection of fullscreen resolution

void VideoTab::SelectMode (D3D7Enum_DeviceInfo *dev, DWORD idx)
{
	DWORD i, data, w, h, mode, bpp, ibpp, usebpp;
	data = SendDlgItemMessage (hTab, IDC_VID_MODE, CB_GETITEMDATA, idx, 0);
	w    = data & 0xFFFF;
	h    = data >> 16;
	// check that this resolution is compatible with the current bpp setting
	idx  = SendDlgItemMessage (hTab, IDC_VID_BPP, CB_GETCURSEL, 0, 0);
	bpp  = SendDlgItemMessage (hTab, IDC_VID_BPP, CB_GETITEMDATA, idx, 0);
	for (i = mode = usebpp = 0; i < dev->dwNumModes; i++) {
		DDSURFACEDESC2 *ddsd = dev->pddsdModes+i;
		if (ddsd->dwWidth != w || ddsd->dwHeight != h) continue;
		ibpp = ddsd->ddpfPixelFormat.dwRGBBitCount;
		if (ibpp == bpp)   { usebpp = ibpp; mode = i; break; } // found match
		if (ibpp > usebpp) { usebpp = ibpp; mode = i; } // best match so far
	}
	dev->dwCurrentMode = mode;
	dev->ddsdFullscreenMode = dev->pddsdModes[dev->dwCurrentMode];
	// if a bpp change was required, notify the bpp control
	if (bpp != usebpp) {
		char cbuf[20];
		SendDlgItemMessage (hTab, IDC_VID_BPP, CB_SELECTSTRING, -1,
			(LPARAM)_itoa (usebpp, cbuf, 10));
	}
}

// ==============================================================
// Respond to user selection of fullscreen colour depth

void VideoTab::SelectBPP (D3D7Enum_DeviceInfo *dev, DWORD idx)
{
	DWORD data, w, h, mode, bpp;
	bpp  = SendDlgItemMessage (hTab, IDC_VID_BPP, CB_GETITEMDATA, idx, 0);
	// check that this bitdepth is compatible with the current resolution
	idx  = SendDlgItemMessage (hTab, IDC_VID_MODE, CB_GETCURSEL, 0, 0);
	data = SendDlgItemMessage (hTab, IDC_VID_MODE, CB_GETITEMDATA, idx, 0);
	w    = data & 0xFFFF;
	h    = data >> 16;
	for (mode = 0; mode < dev->dwNumModes; mode++) {
		DDSURFACEDESC2 *ddsd = dev->pddsdModes+mode;
		if (ddsd->ddpfPixelFormat.dwRGBBitCount == bpp &&
			ddsd->dwWidth == w &&
			ddsd->dwHeight == h) {
				dev->dwCurrentMode = mode;
				dev->ddsdFullscreenMode = dev->pddsdModes[mode];
				return;
		}
	}

	// If we get here, the selected screen resolution isn't supported
	// at this bit depth (shouldn't happen)
	bpp = dev->pddsdModes[dev->dwCurrentMode].ddpfPixelFormat.dwRGBBitCount;
	char cbuf[20];
	SendDlgItemMessage (hTab, IDC_VID_BPP, CB_SELECTSTRING, -1,
		(LPARAM)_itoa (bpp, cbuf, 10));

	// If we get here, then we've screwed up big time
	// and leave quietly
}

// =======================================================================

void VideoTab::SelectPageflip ()
{
	bool disable_pageflip = (SendDlgItemMessage (hTab, IDC_VID_PAGEFLIP, BM_GETCHECK, 0, 0) == BST_CHECKED);
	EnableWindow (GetDlgItem (hTab, IDC_VID_VSYNC), disable_pageflip ? FALSE:TRUE);
}

// ==============================================================
// Respond to user selection of render window width

static int aspect_wfac[4] = {4,16,16};
static int aspect_hfac[4] = {3,10,9};

void VideoTab::SelectWidth ()
{
	if (SendDlgItemMessage (hTab, IDC_VID_ASPECT, BM_GETCHECK, 0, 0) == BST_CHECKED) {
		char cbuf[128];
		int w, h, wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		GetWindowText (GetDlgItem (hTab, IDC_VID_WIDTH),  cbuf, 127); w = atoi(cbuf);
		GetWindowText (GetDlgItem (hTab, IDC_VID_HEIGHT), cbuf, 127); h = atoi(cbuf);
		if (w != (wfac*h)/hfac) {
			h = (hfac*w)/wfac;
			SetWindowText (GetDlgItem (hTab, IDC_VID_HEIGHT), itoa (h, cbuf, 10));
		}
	}
}

// ==============================================================
// Respond to user selection of render window height

void VideoTab::SelectHeight ()
{
	if (SendDlgItemMessage (hTab, IDC_VID_ASPECT, BM_GETCHECK, 0, 0) == BST_CHECKED) {
		char cbuf[128];
		int w, h, wfac = aspect_wfac[aspect_idx], hfac = aspect_hfac[aspect_idx];
		GetWindowText (GetDlgItem (hTab, IDC_VID_WIDTH),  cbuf, 127); w = atoi(cbuf);
		GetWindowText (GetDlgItem (hTab, IDC_VID_HEIGHT), cbuf, 127); h = atoi(cbuf);
		if (h != (hfac*w)/wfac) {
			w = (wfac*h)/hfac;
			SetWindowText (GetDlgItem (hTab, IDC_VID_WIDTH), itoa (w, cbuf, 10));
		}
	}
}

// =======================================================================
// Respond to user selection of fixed aspect ratio on/off

void VideoTab::SelectFixedAspect ()
{
	bool fixed_aspect = (SendDlgItemMessage (hTab, IDC_VID_ASPECT, BM_GETCHECK, 0, 0) == BST_CHECKED);
	for (int i = 0; i < 3; i++)
		EnableWindow (GetDlgItem (hTab, IDC_VID_4X3+i), fixed_aspect ? TRUE:FALSE);
}

// ==============================================================
// copy dialog state back to parameter structure

void VideoTab::UpdateConfigData ()
{
	char cbuf[128];
	DWORD i, dat, w, h, bpp, ndev, nmod;
	GraphicsClient::VIDEODATA *data = gclient->GetVideoData();

	D3D7Enum_DeviceInfo *devlist, *dev;
	D3D7Enum_GetDevices (&devlist, &ndev);

	// device parameters
	i   = SendDlgItemMessage (hTab, IDC_VID_DEVICE, CB_GETCURSEL, 0, 0);
	if (i >= ndev) i = 0; // should not happen
	dev = devlist+i;
	data->deviceidx = i;
	i   = SendDlgItemMessage (hTab, IDC_VID_MODE, CB_GETCURSEL, 0, 0);
	dat = SendDlgItemMessage (hTab, IDC_VID_MODE, CB_GETITEMDATA, i, 0);
	w   = dat & 0xFFFF;
	h   = dat >> 16;
	i   = SendDlgItemMessage (hTab, IDC_VID_BPP, CB_GETCURSEL, 0, 0);
	bpp = SendDlgItemMessage (hTab, IDC_VID_BPP, CB_GETITEMDATA, i, 0);
	nmod = dev->dwNumModes;
	data->modeidx = 0; // in case there is a problem
	for (i = 0; i < nmod; i++) {
		if (dev->pddsdModes[i].dwWidth == w && dev->pddsdModes[i].dwHeight == h &&
			dev->pddsdModes[i].ddpfPixelFormat.dwRGBBitCount == bpp) {
			data->modeidx = i;
			break;
		}
	}

	data->fullscreen = (SendDlgItemMessage (hTab, IDC_VID_FULL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	data->novsync    = (SendDlgItemMessage (hTab, IDC_VID_VSYNC, BM_GETCHECK, 0, 0) == BST_CHECKED);
	data->pageflip   = (SendDlgItemMessage (hTab, IDC_VID_PAGEFLIP, BM_GETCHECK, 0, 0) != BST_CHECKED);
	data->trystencil = (SendDlgItemMessage (hTab, IDC_VID_STENCIL, BM_GETCHECK, 0, 0) == BST_CHECKED);
	data->forceenum  = (SendDlgItemMessage (hTab, IDC_VID_ENUM, BM_GETCHECK, 0, 0) == BST_CHECKED);
	GetWindowText (GetDlgItem (hTab, IDC_VID_WIDTH),  cbuf, 127); data->winw = atoi(cbuf);
	GetWindowText (GetDlgItem (hTab, IDC_VID_HEIGHT), cbuf, 127); data->winh = atoi(cbuf);
}

// ==============================================================

INT_PTR CALLBACK VideoTab::AboutDlgProc (HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG: {
		char cbuf[1024];
		LoadString ((HINSTANCE)GetWindowLongPtr (hWnd, GWLP_HINSTANCE), IDS_STRING1, cbuf, 1024);
		SetWindowText (GetDlgItem (hWnd, IDC_EDIT1), cbuf);
		} return TRUE;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDOK:
		case IDCANCEL:
			EndDialog (hWnd, 0);
			break;
		}
	}
	return FALSE;
}
