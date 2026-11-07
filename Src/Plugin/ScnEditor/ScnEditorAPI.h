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

/**
 * Vessel modules can add buttons to the scenario editor's vessel page.
 * The editor loads the module named by the EditorModule entry of the vessel
 * class config file and calls its exported function
 *
 *   DLLCLBK void secInit (ScnEditorInterface *editor, OBJHANDLE hVessel);
 *
 * which registers the buttons with AddFuncButton or AddPageButton.
 * A function button calls func when clicked. A page button opens a page
 * whose content is drawn with ImGui by func, once per frame, while the page
 * is visible. The editor draws the page frame and the "Done" button.
 * If help is set, the editor's help button opens that help context on
 * this page.
 */

typedef void (*CustomButtonFunc)(OBJHANDLE hVessel);
typedef void (*CustomPageFunc)(OBJHANDLE hVessel);

typedef struct {
	char btnlabel[32];
	CustomButtonFunc func;
} EditorFuncSpec;

typedef struct {
	char btnlabel[32];
	CustomPageFunc func;
	HELPCONTEXT *help;
} EditorPageSpec;

class ScnEditorInterface {
public:
	virtual bool AddFuncButton (const EditorFuncSpec *spec) = 0;
	virtual bool AddPageButton (const EditorPageSpec *spec) = 0;
};

typedef void (*SecInitFunc)(ScnEditorInterface *editor, OBJHANDLE hVessel);

#endif // !__SCNEDITORAPI_H
