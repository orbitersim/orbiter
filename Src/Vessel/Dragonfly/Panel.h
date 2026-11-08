// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __PANELS_H_
#define __PANELS_H_

#include "orbitersdk.h"
#include "instruments.h"

typedef struct
{ int x;
  int y;
  char text[20];
} TEXT_LIST;
typedef struct
{ int x;
  int y;
  int lngth;
  int dir;
  char text[20];
} CTEXT_LIST;

typedef struct
{ int x;
  int y;
} SCREW_LIST;

typedef struct
{ int x;
  int y;
  int num;
} BORDER_LIST;

class Panel
{public:
   int Wdth,Hght;		//Width & Height of the panel;
   int ATT_mode;
   SURFHANDLE surf;		//surface of background;
   TEXT_LIST* Text_list;
   SCREW_LIST* Screw_list;
   CTEXT_LIST* CText_list;
   BORDER_LIST* Border_list;
   instrument_list *instruments;
   int text_num,screw_num,ctext_num,border_num;
   int neighbours[4];
   int idx;				//index of panel
   VESSEL *v;
   Panel();
   ~Panel();
   SURFHANDLE MakeYourBackground();
   void NowPutTextOnBackground(oapi::Sketchpad *skp);
   void NowPutScrews(oapi::Sketchpad *skp);
   void NowPutCText(oapi::Sketchpad *skp);
   void NowPutBorders(oapi::Sketchpad *skp);
   void AddInstrument(instrument* new_inst);
   void RegisterYourInstruments();
   void Paint(int index);	//all the instruments;
   void Refresh(int index);	//for instruments who need constant refresh;
   void LBD(int index,int x,int y);
   void RBD(int index,int x,int y);
   void BU(int index);
   void Load (FILEHANDLE scn);
   void Save (FILEHANDLE scn);
};

void PANEL_InitResources();
void PANEL_ReleaseResources();

void PanelArc(oapi::Sketchpad *skp, int l, int t, int r, int b, int xs, int ys, int xe, int ye);
void PanelPie(oapi::Sketchpad *skp, int l, int t, int r, int b, int xs, int ys, int xe, int ye);

extern oapi::Font *hFNT_Panel;
extern oapi::Brush *hBRUSH_Black,*hBRUSH_Red,*hBRUSH_Green,*hBRUSH_Background,*hBRUSH_LBkg;
extern oapi::Brush *hBRUSH_TotalBlack,*hBRUSH_FYellow,*hBRUSH_Gray,*hBRUSH_White;
extern oapi::Pen *hPEN_White,*hPEN_Gray,*hPEN_Black,*hPEN_Cyan,*hPEN_BYellow,*hPEN_LGray,*hPEN_TotalBlack;
extern SURFHANDLE hClockSRF;
extern SURFHANDLE hSwitchSRF;
extern SURFHANDLE hHgaugeSRF;
extern SURFHANDLE hEgaugeSRF;
extern SURFHANDLE hRotarySRF;
extern SURFHANDLE hTbSRF;
extern SURFHANDLE hCbSRF;
extern SURFHANDLE hSliderSRF;
extern SURFHANDLE hCwSRF;
extern SURFHANDLE hMFDSRF;
extern SURFHANDLE hDockBSRF;
extern SURFHANDLE hDockSW1SRF;
extern SURFHANDLE hDockDlSRF;
extern SURFHANDLE hDockSW2SRF;
extern SURFHANDLE hNavSRF;
extern SURFHANDLE hRadarSRF;
extern SURFHANDLE hRadBkSRF;
extern SURFHANDLE hRadSrfSRF;
extern SURFHANDLE hFuelSRF;
extern SURFHANDLE hVrotSRF;
extern SURFHANDLE hVrotBkSRF;
extern SURFHANDLE hADIBorder;
extern SURFHANDLE hFront_Panel_SRF[7];

#endif
