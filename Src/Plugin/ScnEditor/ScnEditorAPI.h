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

#define WM_SCNEDITOR WM_USER

#define SE_ADDFUNCBUTTON 0x01
#define SE_ADDPAGEBUTTON 0x02
#define SE_GETVESSEL     0x04

typedef void (*CustomButtonFunc)(OBJHANDLE);

typedef struct {
	char btnlabel[32];
	CustomButtonFunc func;
} EditorFuncSpec;

typedef struct {
	char btnlabel[32];
	HINSTANCE hDLL;
	WORD ResId;
	DLGPROC TabProc;
} EditorPageSpec;

#endif // !__SCNEDITORAPI_H