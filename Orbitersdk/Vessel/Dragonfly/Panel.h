
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
   HBITMAP hBitmap;		//handle to background bitmap; 
   HINSTANCE hModule;	//handle to program instance // ??need this to load resource bitmaps.. ugh.. windows is idiot!!!
   HDC hDC,hDC2;
   HDC hDC3;
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
   void MakeYourBackground();
   void NowPutTextOnBackground();
   void NowPutScrews();
   void NowPutCText();
   void NowPutBorders();
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

void PANEL_InitGDIResources(HINSTANCE hModule);
void PANEL_ReleaseGDIResources();
void PANEL_DLLAtach();


#endif