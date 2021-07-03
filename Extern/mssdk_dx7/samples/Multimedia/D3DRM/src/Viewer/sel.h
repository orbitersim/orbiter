/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: sel.h
 *
 ***************************************************************************/

void ShowBoxes(int);
int ToggleBoxes(void);
LPDIRECT3DRMFRAME3 SelectedFrame(void);
LPDIRECT3DRMVISUAL SelectedVisual(void);
LPDIRECT3DRMLIGHT SelectedLight(void);
void DeselectVisual(void);
void SelectVisual(LPDIRECT3DRMVISUAL visual, LPDIRECT3DRMFRAME3 frame);
void FindAndSelectVisual(LPDIRECT3DRMVIEWPORT2 view, int x, int y);
void CutVisual(void);
void CopyVisual(void);
void PasteVisual(LPDIRECT3DRMFRAME3 scene);
void DeleteVisual(void);
void ClearClipboard(void);
