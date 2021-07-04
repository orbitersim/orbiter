// ==============================================================
//                 ORBITER MODULE: ShuttleA
//                  Part of the ORBITER SDK
//          Copyright (C) 2001-2016 Martin Schweiger
//                   All rights reserved
//
// paneltext.h
// Text output into panel elements
// ==============================================================

#ifndef __PANELTEXT_H
#define __PANELTEXT_H

#include "Orbitersdk.h"

#ifndef __PANELTEXT_CPP
extern int small_font_xpos[256];
extern int small_font_width[256];
extern int small_font_ypos[2];
extern int small_font_height;
#endif

const DWORD ALIGN_RIGHT = 0x0001;

void BltStr (SURFHANDLE tgt, SURFHANDLE src, int x, int y, const char *newstr, char *oldstr, DWORD style = 0);

#endif // !__PANELTEXT_H