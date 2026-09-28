// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __DLGCTRLLOCAL_H
#define __DLGCTRLLOCAL_H

#ifdef __linux__
#include <QColor>

// pen and brush colours the controls draw with
#endif // __linux__
typedef struct {
#ifndef __linux__
	HPEN hPen1, hPen2;
	HBRUSH hBrush1, hBrush2;
#else // __linux__
	QColor hPen1, hPen2;
	QColor hBrush1, hBrush2;
#endif // __linux__
} GDIRES;

#ifndef __linux__
void RegisterPropertyList (HINSTANCE hInst);
void UnregisterPropertyList (HINSTANCE hInst);
#else // __linux__
void RegisterPropertyList (void *hInst);
void UnregisterPropertyList (void *hInst);
#endif // __linux__

#endif // !__DLGCTRLLOCAL_H
