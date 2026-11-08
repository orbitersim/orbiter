// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "panel.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "orbitersdk.h"

oapi::Font *hFNT_Panel;
oapi::Brush *hBRUSH_Black,*hBRUSH_Red,*hBRUSH_Green,*hBRUSH_Background,*hBRUSH_LBkg;
oapi::Brush *hBRUSH_TotalBlack,*hBRUSH_FYellow,*hBRUSH_Gray,*hBRUSH_White;
oapi::Pen *hPEN_White,*hPEN_Gray,*hPEN_Black,*hPEN_Cyan,*hPEN_BYellow,*hPEN_LGray,*hPEN_TotalBlack;
SURFHANDLE hClockSRF;
SURFHANDLE hSwitchSRF;
SURFHANDLE hHgaugeSRF;
SURFHANDLE hEgaugeSRF;
SURFHANDLE hRotarySRF;
SURFHANDLE hTbSRF;
SURFHANDLE hCbSRF;
SURFHANDLE hSliderSRF;
SURFHANDLE hCwSRF;
SURFHANDLE hMFDSRF;
SURFHANDLE hDockBSRF;
SURFHANDLE hDockSW1SRF;
SURFHANDLE hDockDlSRF;
SURFHANDLE hDockSW2SRF;
SURFHANDLE hNavSRF;
SURFHANDLE hRadarSRF;
SURFHANDLE hRadBkSRF;
SURFHANDLE hRadSrfSRF;
SURFHANDLE hFuelSRF;
SURFHANDLE hVrotSRF;
SURFHANDLE hVrotBkSRF;
SURFHANDLE hADIBorder;
SURFHANDLE hFront_Panel_SRF[7];
static int Panel_Resources_Users = 0;

static void ArcPoints(int l, int t, int r, int b, int xs, int ys, int xe, int ye, oapi::IVECTOR2 *pt, int n)
{
	double cx = 0.5*(l+r), cy = 0.5*(t+b);
	double rx = 0.5*(r-l), ry = 0.5*(b-t);
	double a0 = atan2(-(ys-cy)/ry, (xs-cx)/rx);
	double a1 = atan2(-(ye-cy)/ry, (xe-cx)/rx);
	if (a1 <= a0) a1 += 2.0*PI;
	for (int i = 0; i < n; i++) {
		double a = a0 + (a1-a0)*i/(n-1);
		pt[i].x = (long)floor(cx + rx*cos(a) + 0.5);
		pt[i].y = (long)floor(cy - ry*sin(a) + 0.5);
	}
}

void PanelArc(oapi::Sketchpad *skp, int l, int t, int r, int b, int xs, int ys, int xe, int ye)
{
	oapi::IVECTOR2 pt[65];
	ArcPoints(l, t, r, b, xs, ys, xe, ye, pt, 65);
	skp->Polyline(pt, 65);
}

void PanelPie(oapi::Sketchpad *skp, int l, int t, int r, int b, int xs, int ys, int xe, int ye)
{
	oapi::IVECTOR2 pt[66];
	pt[0].x = (l+r)/2;
	pt[0].y = (t+b)/2;
	ArcPoints(l, t, r, b, xs, ys, xe, ye, pt+1, 65);
	skp->Polygon(pt, 66);
}

Panel::Panel()
{
	instruments=NULL;
	screw_num = 0;
};

Panel::~Panel()
{
  instrument_list *runner=instruments;
  instrument_list *gone;
  while (runner){ gone=runner;
				  runner=runner->next;
				  delete gone->instance;
				  delete gone;
  };
};
SURFHANDLE Panel::MakeYourBackground()
{
	SURFHANDLE bg=oapiCreateSurface(Wdth,Hght);
	oapi::Sketchpad *skp=oapiGetSketchpad(bg);
	skp->SetPen(hPEN_TotalBlack);
	skp->SetBrush(hBRUSH_Background);
	skp->Rectangle(0,0,Wdth,Hght);
	Panel::NowPutScrews(skp);
	Panel::NowPutTextOnBackground(skp);
	Panel::NowPutCText(skp);
	Panel::NowPutBorders(skp);
	oapiReleaseSketchpad(skp);
	return bg;
}
void Panel::NowPutScrews(oapi::Sketchpad *skp)
{int di=5*sqrt(2.0)/2;
 skp->SetPen(hPEN_White);
 skp->SetBrush(hBRUSH_Background);
 int x,y;
	for (int i=0;i<screw_num;i++)
	{  x=Screw_list[i].x;
	   y=Screw_list[i].y;
	   skp->Ellipse(x-5,y-5,x+5,y+5);
	   skp->MoveTo(x-di,y-di);skp->LineTo(x+di,y+di);
	}
};
void Panel::NowPutTextOnBackground(oapi::Sketchpad *skp)
{  skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);
   skp->SetTextColor(RGB(255,100,100));
   skp->SetTextAlign(oapi::Sketchpad::CENTER);
   skp->SetFont(hFNT_Panel);
   for (int i=0;i<text_num;i++)
	skp->Text(Text_list[i].x,Text_list[i].y,Text_list[i].text,strlen(Text_list[i].text));
}
void Panel::NowPutCText(oapi::Sketchpad *skp)
{
  skp->SetTextColor(RGB(255,100,100));
  skp->SetPen(hPEN_Cyan);
  skp->SetTextAlign(oapi::Sketchpad::CENTER);
  skp->SetBackgroundColor(RGB(145,48,48));
  skp->SetBackgroundMode(oapi::Sketchpad::BK_OPAQUE);
  skp->SetFont(hFNT_Panel);
  for (int i=0;i<ctext_num;i++) {
  skp->MoveTo(CText_list[i].x+CText_list[i].lngth/2, CText_list[i].y+CText_list[i].dir*7+5);skp->LineTo(CText_list[i].x+CText_list[i].lngth/2,CText_list[i].y+5);
  skp->LineTo(CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+5);skp->LineTo(CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+CText_list[i].dir*7+5);

  skp->Text(CText_list[i].x,CText_list[i].y,CText_list[i].text,strlen(CText_list[i].text));
  }
};

void Panel::NowPutBorders(oapi::Sketchpad *skp)
{ int px,py,nr;
 skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);
 skp->SetPen(hPEN_Black);
 for (int i=0;i<border_num;i++){
 px=Border_list[i].x;py=Border_list[i].y;
 nr=Border_list[i].num;

 PanelArc(skp,px-5,py-5,px+6,py+6,px,py-5,px-5,py);
 skp->MoveTo(px-5,py);skp->LineTo(px-5,py+40);py+=40;
 PanelArc(skp,px-5,py-5,px+6,py+6,px-5,py,px,py+5);
 skp->MoveTo(px,py+5);skp->LineTo(px+45*nr+5,py+5);px+=45*nr+5;
 PanelArc(skp,px-5,py-5,px+6,py+6,px,py+5,px+5,py);
 skp->MoveTo(px+5,py);skp->LineTo(px+5,py-40);py-=40;
 PanelArc(skp,px-5,py-5,px+6,py+6,px+5,py,px,py-5);
 skp->MoveTo(px,py-5);skp->LineTo(px-45*nr-5,py-5);
 }
}
void Panel::AddInstrument(instrument *new_inst)
{ instrument_list *new_item=new instrument_list;
  new_item->instance=new_inst;
  new_item->next=instruments;
  instruments=new_item;
};
void Panel::RegisterYourInstruments()
{  instrument_list *runner=instruments;
  int index=0;
  while (runner)
  { if (runner->instance)
			runner->instance->RegisterMe(index++); //now register all the inst. in the list
    runner=runner->next;
  }
};
void Panel::Paint(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->PaintMe();

}
void Panel::Refresh(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->RefreshMe();

}
void Panel::LBD(int index,int x,int y)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->LBD( x, y);

}
void Panel::RBD(int index,int x,int y)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->RBD( x, y);

}
void Panel::BU(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->BU();

}

void Panel::Save(FILEHANDLE scn)
{ instrument_list *runner=instruments;
  char cbuf[80];
  int int_sv[10];
  int index=0;
 //save the switches
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==33))
		    int_sv[index++]=((Switch*)runner->instance)->pos;
 		    runner=runner->next;
			//every 5 switches save
			if (index==5) {sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	                       oapiWriteScenario_string (scn, (char*)"    SW ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, (char*)"    SW ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  SW END ", cbuf);
  //save the rotary
  runner=instruments;
  index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==34))
		    int_sv[index++]=((Rotary*)runner->instance)->set;
 		    runner=runner->next;
			//every 5 switches save
			if (index==5) {sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	                       oapiWriteScenario_string (scn, (char*)"    ROT ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, (char*)"    ROT ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  ROT END ", cbuf);
  runner=instruments;
  //index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==44))//ADI ball saving
			{
			ADI* adi_p=(ADI*)runner->instance;
			sprintf(cbuf,"%0.4f %0.4f %0.4f %i %i %0.4f %0.4f %0.4f",adi_p->now.x,adi_p->now.y,adi_p->now.z,
															    adi_p->orbital_ecliptic,adi_p->function_mode,
																adi_p->reference.x,adi_p->reference.y,adi_p->reference.z);
			oapiWriteScenario_string (scn, (char*)"    ADI ", cbuf);
			};//end of if
    runner=runner->next;
  };//end of while
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  PAN END ", cbuf);
}
void Panel::Load(FILEHANDLE scn)
{ instrument_list *runner=instruments;
 char *line;
 int int_sv[10];
 //int index=0;
 //read the switches
 oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"SW END",6))
		{ sscanf (line,"    SW %i %i %i %i %i", &int_sv[0],&int_sv[1],&int_sv[2],&int_sv[3],&int_sv[4]);
	      for (int i=0;i<5;i++)
		  {   while ((runner) && (runner->instance->type!=33)) runner=runner->next;
			   ((Switch*)runner->instance)->pos=int_sv[i];
			   if (int_sv[i+1]==99) break;
			   runner=runner->next;
		  }
 oapiReadScenario_nextline (scn, line);
 }
//read the rotaries
 runner=instruments;
  oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"ROT END",6))
		{ sscanf (line,"    ROT %i %i %i %i %i", &int_sv[0],&int_sv[1],&int_sv[2],&int_sv[3],&int_sv[4]);
	      for (int i=0;i<5;i++)
		  {   while ((runner) && (runner->instance->type!=34)) runner=runner->next;
			   ((Rotary*)runner->instance)->set=int_sv[i];
			   if (int_sv[i+1]==99) break;
			   runner=runner->next;
		  }
 oapiReadScenario_nextline (scn, line);
 }
//read the extra instruments info
runner=instruments;
  oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"PAN END",6))	//until we clear this panel
	{ if (!strncmp(line,"ADI",3)) //we have an adi to load
		{ while ((runner) && (runner->instance->type!=44)) runner=runner->next;//go to ADI
			ADI*  adi_p=(ADI*)runner->instance;
         sscanf(line,"    ADI %lf %lf %lf %i %i %lf %lf %lf",&(adi_p->now.x),&(adi_p->now.y),&(adi_p->now.z),
													   &(adi_p->orbital_ecliptic),&(adi_p->function_mode),
														&(adi_p->reference.x),&(adi_p->reference.y),&(adi_p->reference.z));
		};//end of adi found !
	oapiReadScenario_nextline (scn, line);
	};//end of while
};

static SURFHANDLE LoadBmp (const char *name)
{
	char fname[256];
	snprintf (fname, sizeof(fname), "Dragonfly/Bitmaps/%s.bmp", name);
	return oapiLoadSurfaceEx (fname, OAPISURFACE_TEXTURE);
}

void PANEL_InitResources()
{ if (Panel_Resources_Users++) return;
  hPEN_White=oapiCreatePen(1,1,RGB(250,250,250));
  hPEN_Gray=oapiCreatePen(1,1,RGB(100,100,100));
  hPEN_Black=oapiCreatePen(1,1,RGB(15,15,15));
  hPEN_Cyan=oapiCreatePen(1,1,RGB(255,100,100));
  hPEN_BYellow=oapiCreatePen(1,1,RGB(0,250,0));
  hPEN_LGray=oapiCreatePen(1,1,RGB(145,49,49));
  hPEN_TotalBlack=oapiCreatePen(1,1,RGB(0,0,0));

  hBRUSH_Black=oapiCreateBrush(RGB(15,15,15));
  hBRUSH_TotalBlack=oapiCreateBrush(RGB(0,0,0));
  hBRUSH_FYellow=oapiCreateBrush(RGB(0,50,0));
  hBRUSH_Red=oapiCreateBrush(RGB(49,74,41));
  hBRUSH_Green=oapiCreateBrush(RGB(0,255,0));
  hBRUSH_White=oapiCreateBrush(RGB(255,255,255));
  hBRUSH_Background=oapiCreateBrush(RGB(145,48,48));
  hBRUSH_LBkg=oapiCreateBrush(RGB(40,54,59));
  hBRUSH_Gray=oapiCreateBrush(RGB(145,49,49));
  hFNT_Panel=oapiCreateFont(12,true,"Arial");
  hClockSRF=LoadBmp("Fonts");
  hSwitchSRF=LoadBmp("switch");
  hHgaugeSRF=LoadBmp("gauge");
  hEgaugeSRF=LoadBmp("egauge");
  hRotarySRF=LoadBmp("rotary");
  hTbSRF=LoadBmp("tb");
  hCbSRF=LoadBmp("cb");
  hSliderSRF=LoadBmp("SL");
  hCwSRF=LoadBmp("cw");
  hMFDSRF=LoadBmp("MFD");
  hDockBSRF=LoadBmp("dock");
  hDockSW1SRF=LoadBmp("Switch2");
  hDockDlSRF=LoadBmp("Dial1");
  hDockSW2SRF=LoadBmp("Switch1");
  hNavSRF=LoadBmp("nav");
  hRadarSRF=LoadBmp("dragon");
  hRadBkSRF=LoadBmp("radar2");
  hRadSrfSRF=LoadBmp("rad2");
  hFuelSRF=LoadBmp("fuel");
  hVrotSRF=LoadBmp("vrot");
  hVrotBkSRF=LoadBmp("vrotbk");
  hADIBorder=LoadBmp("ADIborder");
  oapiSetSurfaceColourKey(hADIBorder,0xFF000000);
  hFront_Panel_SRF[1]=LoadBmp("front");
  hFront_Panel_SRF[2]=LoadBmp("Up");
  hFront_Panel_SRF[3]=LoadBmp("left");
  hFront_Panel_SRF[4]=LoadBmp("grill");
  hFront_Panel_SRF[5]=LoadBmp("right");
  hFront_Panel_SRF[6]=LoadBmp("warn");
 }


void PANEL_ReleaseResources()
{  if (!Panel_Resources_Users || --Panel_Resources_Users) return;
   oapiReleasePen(hPEN_White);
   oapiReleasePen(hPEN_Gray);
   oapiReleasePen(hPEN_Black);
   oapiReleasePen(hPEN_Cyan);
   oapiReleasePen(hPEN_BYellow);
   oapiReleasePen(hPEN_LGray);
   oapiReleasePen(hPEN_TotalBlack);

   oapiReleaseFont(hFNT_Panel);
   oapiReleaseBrush(hBRUSH_Black);
   oapiReleaseBrush(hBRUSH_TotalBlack);
   oapiReleaseBrush(hBRUSH_Red);
   oapiReleaseBrush(hBRUSH_Green);
   oapiReleaseBrush(hBRUSH_White);
   oapiReleaseBrush(hBRUSH_Background);
   oapiReleaseBrush(hBRUSH_LBkg);
   oapiReleaseBrush(hBRUSH_FYellow);
   oapiReleaseBrush(hBRUSH_Gray);

   oapiDestroySurface(hClockSRF);
   oapiDestroySurface(hSwitchSRF);
   oapiDestroySurface(hHgaugeSRF);
   oapiDestroySurface(hEgaugeSRF);
   oapiDestroySurface(hRotarySRF);
   oapiDestroySurface(hTbSRF);
   oapiDestroySurface(hCbSRF);
   oapiDestroySurface(hSliderSRF);
   oapiDestroySurface(hCwSRF);
   oapiDestroySurface(hMFDSRF);
   oapiDestroySurface(hDockBSRF);
   oapiDestroySurface(hDockSW1SRF);
   oapiDestroySurface(hDockDlSRF);
   oapiDestroySurface(hDockSW2SRF);
   oapiDestroySurface(hNavSRF);
   oapiDestroySurface(hRadarSRF);
   oapiDestroySurface(hRadBkSRF);
   oapiDestroySurface(hRadSrfSRF);
   oapiDestroySurface(hFuelSRF);
   oapiDestroySurface(hVrotSRF);
   oapiDestroySurface(hVrotBkSRF);
   oapiDestroySurface(hADIBorder);
   oapiDestroySurface(hFront_Panel_SRF[1]);
   oapiDestroySurface(hFront_Panel_SRF[2]);
   oapiDestroySurface(hFront_Panel_SRF[3]);
   oapiDestroySurface(hFront_Panel_SRF[4]);
   oapiDestroySurface(hFront_Panel_SRF[5]);
   oapiDestroySurface(hFront_Panel_SRF[6]);
}
