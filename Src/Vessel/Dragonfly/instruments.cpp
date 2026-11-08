// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "instruments.h"
#include "vectors.h"        
#include "panel.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "orbitersdk.h"
#include "dragonfly.h"

using std::min;
using std::max;



instrument::instrument(int x, int y,Panel* i_parent)
{ ScrX=x;ScrY=y;parent=i_parent;
  parent->AddInstrument(this);
type=30; //void instrument
};



TB::TB(int x, int y, float *i_SRC,Panel *i_parent):instrument(x,y,i_parent)
{ SRC=i_SRC;
  last_d=2; //we sure need a redraw :-)
type=31;//TB
}
void TB::RegisterMe(int index)
{
oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+20,ScrY+20),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_NONE);
idx=index;

}
void TB::PaintMe()
{
if (*SRC)
{oapiBlt(parent->surf,hTbSRF,0,0,20,0,20,20);
last_d=1;}
else {
 oapiBlt(parent->surf,hTbSRF,0,0,0,0,20,20);
 last_d=0;
}

}

void TB::RefreshMe()
{ if (*SRC) {if (last_d==0) PaintMe();}
  else { if (last_d==1) PaintMe();}
}

Switch::Switch(int x, int y, int i_pos,int i_num_pos,int i_sp, int *i_SRC,Panel *i_parent):instrument(x,y,i_parent)
{ pos=i_pos;num_pos=i_num_pos;
  spring=i_sp;if (num_pos<3) spring=0; //can't spring on 2 pos. only 3 
  SRC=i_SRC;type=33; //switch
};

void Switch::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+50,ScrY+50),PANEL_REDRAW_MOUSE,15,PANEL_MAP_NONE);
 idx=index;
};
void Switch::PaintMe()
{ 
oapiBlt(parent->surf,hSwitchSRF,0,0,50+pos*50,0,50,50);
};

void Switch::LBD(int x, int y)
{ if (pos<1)
		{pos+=4-num_pos;
	     oapiTriggerPanelRedrawArea(parent->idx,idx);
		 *SRC=pos;

				};
};
void Switch::RBD(int x, int y)
{ if (pos>-1) {pos-=4-num_pos;
		       oapiTriggerPanelRedrawArea(parent->idx,idx);
			   *SRC=pos;

				};
};
void Switch::BU()
{ 
	if (spring) {pos=0;
				*SRC=0;
	oapiTriggerPanelRedrawArea(parent->idx,idx);}
};
CB::CB(int x,int y,int i_pos,int *i_SRC, Panel *i_Panel):Switch(x,y,i_pos,2,0,i_SRC,i_Panel)
{}
void CB::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+50,ScrY+50),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_DOWN,PANEL_MAP_NONE);
 idx=index;
}
void CB::PaintMe()

{
oapiBlt(parent->surf,hCbSRF,0,0,pos*50,0,50,50);
};
void CB::LBD(int x,int y)
{ if (pos) pos=0;
  else pos=1;
  oapiTriggerPanelRedrawArea(parent->idx,idx);
  *SRC=pos;
};
void CB::RBD(int x,int y)
{ if (pos) pos=0;
  else pos=1;
  oapiTriggerPanelRedrawArea(parent->idx,idx);
  *SRC=pos;
};
void CB::RefreshMe()
{ if (*SRC!=pos) 
		{pos=*SRC;
		 oapiTriggerPanelRedrawArea(parent->idx,idx);
			}
};

Slider::Slider(int x,int y, int *i_SRC, Panel *i_parent):instrument(x,y,i_parent)
{SRC=i_SRC;
 type=29;//Slider
 if (*SRC) am_i=0;
 else am_i=42;
}
void Slider::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+34,ScrY+20),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_NONE);
 idx=index;
};
void Slider::PaintMe()
{ //copy the main surface
oapiBlt(parent->surf,hSliderSRF,0,0,0,0,34,20);
//then the sliding thingie
oapiBlt(parent->surf,hSliderSRF,7,5,35+(int)(am_i/2),4,22,10);
};
void Slider::RefreshMe()
{ if ((*SRC)&&(am_i>0))  {
					am_i--;
					oapiTriggerPanelRedrawArea(parent->idx,idx);
						}
if (!(*SRC)&&(am_i<42)) {
					am_i++;
					oapiTriggerPanelRedrawArea(parent->idx,idx);
						}
};
 	
	//------------------------------------ SFSWITCH ------------------------------------------
SFSwitch::SFSwitch(int x, int y, int i_pos,int i_num_pos,int i_safed,Panel *i_parent):Switch(x,y,i_pos,i_num_pos,0,NULL,i_parent)
{ safed=i_safed;
 temps=oapiCreateSurface(50,61);
type=36;//SFSwitch
};
void SFSwitch::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY-4,ScrX+50,ScrY+44),PANEL_REDRAW_MOUSE,PANEL_MOUSE_DOWN,PANEL_MAP_CURRENT);
 idx=index;
};
void SFSwitch::PaintMe()
{ int lng=24/sqrt(3.0);

oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->SetBrush(hBRUSH_Background);skp->SetPen(NULL);
skp->Rectangle(0,0,51,50);
skp->SetPen(hPEN_Black);
skp->Rectangle(5-5,4,5,4+40);skp->Rectangle(5+40,4,5+45,4+40);
skp->SetBrush(hBRUSH_LBkg);
skp->Rectangle(5,4,5+40,4+40);
skp->MoveTo(5+20-lng/2,4+8);skp->LineTo(5+20+lng/2,4+8);
skp->LineTo(5+20+lng,4+20);skp->LineTo(5+20+lng/2,4+32);
skp->LineTo(5+20-lng/2,4+32);skp->LineTo(5+20-lng,4+20);
skp->LineTo(5+20-lng/2,4+8);
skp->Ellipse(5+10,4+10,5+31,4+31);
skp->SetBrush(hBRUSH_FYellow);
skp->Ellipse(5+12,4+12,5+29,4+29);
skp->SetPen(NULL);
skp->SetBrush(hBRUSH_LBkg);
skp->Rectangle(5+20-4*pos,4+20,5+20+4*pos,4+20-7*pos);
skp->Ellipse(5+17,4+17,5+24,4+24);
skp->SetPen(hPEN_Black);
skp->MoveTo(5+20-4*pos,4+20);skp->LineTo(5+20-4*pos,4+20-7*pos);
skp->MoveTo(5+20+4*pos,4+20);skp->LineTo(5+20+4*pos,4+20-7*pos);
PanelArc(skp,5+20-4,4+20-4,5+25,4+25,5+20-4*pos,4+20,5+20+4*pos,4+20);

skp->Rectangle(5+12,4+15-7*pos,5+29,4+25-7*pos-(3-num_pos)*2);
if (num_pos==3) skp->Rectangle(5+12,4+18-9*pos,5+29,4+23-9*pos);
oapiReleaseSketchpad(skp);

oapiBlt(temps,parent->surf,0,0,0,0,50,48);
skp=oapiGetSketchpad(parent->surf);
skp->SetPen(hPEN_Black);
skp->SetBrush(hBRUSH_Red);
if (safed)
{
skp->Rectangle(05,0,45,59);
oapiReleaseSketchpad(skp);

oapiBlt(parent->surf,temps,10,15,10,15,30,15);
oapiBlt(parent->surf,temps,10,40,10,40,30,8);
skp=oapiGetSketchpad(parent->surf);
skp->SetPen(hPEN_Black);
skp->MoveTo(10,15);skp->LineTo(40,15);
skp->LineTo(40,30);skp->LineTo(10,30);skp->LineTo(10,15);
skp->MoveTo(10,48);skp->LineTo(10,40);
skp->LineTo(40,40);skp->LineTo(40,48);
oapiReleaseSketchpad(skp);
}   
else
{  skp->Rectangle(5,0,45,25);
   oapiReleaseSketchpad(skp);
   oapiBlt(parent->surf,temps,10,15,10,15,30,15);
   oapiBlt(parent->surf,temps,10,0,10,0,30,4);

}                                             

}

void SFSwitch::LBD(int x, int y)
{ if ((safed)&& (y<25)) {safed=0;
oapiTriggerPanelRedrawArea(parent->idx,idx);}
else{
	if ((!safed)&&(pos<1))
		{pos+=4-num_pos;
	     oapiTriggerPanelRedrawArea(parent->idx,idx);
				};
}
  
};

void SFSwitch::RBD(int x, int y)
{ if (!(safed)&& (y<25)) {safed=1;
oapiTriggerPanelRedrawArea(parent->idx,idx);}
else {
	 if ((!safed) && (pos>-1)) {pos-=4-num_pos;
		       oapiTriggerPanelRedrawArea(parent->idx,idx);
				};
	};
};
 SFSwitch::~SFSwitch()
{ 
  oapiDestroySurface(temps);
};

//------------------------------------ EGAUGE ------------------------------------------ 
 EGauge::EGauge(int x, int y, float *i_SRC,const char i_unit[5],int i_min,int i_max, float i_scale,Panel* i_parent):instrument(x,y,i_parent)
 {MinV=i_max;MaxV=i_min;scale=i_scale;SRC=i_SRC;
  strcpy(unit,i_unit);
  temps=oapiCreateSurface(40,40);
  lastdraw=-5*3.1415/8;
type=37;//Egauge
 };

void screwdraw(oapi::Sketchpad *skp,int x, int y)
{int di=5*sqrt(2.0)/2;
 skp->SetPen(hPEN_White);
 skp->SetBrush(hBRUSH_Background);
 skp->Ellipse(x-5,y-5,x+5,y+5);
 skp->MoveTo(x-di,y-di);skp->LineTo(x+di,y+di);
 }

void EGauge::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+100,ScrY+100),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 idx=index;
};

 void EGauge::PaintMe()
 {float Pi=3.1415;
  char intst[16];
  float ang=-Pi/4+Pi/8;
  int i;
  oapi::IVECTOR2 TR[3];
 oapiBlt(parent->surf,hEgaugeSRF,0,0,0,0,100,100); //copy the backgorund

oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->SetBrush(hBRUSH_Gray);
skp->SetPen(hPEN_LGray);			// some of the circles
for (i=0;i<16;i++) {
skp->MoveTo(50+cos(ang)*20,50-sin(ang)*20);// now put the small scales
skp->LineTo(50+cos(ang)*24,50-sin(ang)*24);
ang+=Pi/12;
};ang=-Pi/4+Pi/8;
for (i=0;i<6;i++) {
skp->MoveTo(50+cos(ang)*20,50-sin(ang)*20); //now the big scales
skp->LineTo(50+cos(ang)*28,50-sin(ang)*28);
snprintf(intst,sizeof(intst),"%d.",(int)(i*(MaxV-MinV)/5+MinV)); //convert number to char
skp->SetFont(hFNT_Panel);
skp->SetTextAlign(oapi::Sketchpad::CENTER);skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
skp->SetTextColor(RGB(140,49,49));
skp->Text(50+cos(ang)*35,45-sin(ang)*35,intst,strlen(intst)); // and scale numbers
ang+=Pi/4;
};
skp->SetPen(NULL);
skp->Ellipse(30,30,70,70);
skp->SetBrush(hBRUSH_Black);
skp->Ellipse(40,40,59,59);	// the rest of the circles
TR[0].x=50;TR[0].y=45;		// a black triangle on the bottom
TR[1].x=20;TR[1].y=75;
TR[2].x=80;TR[2].y=75;
skp->Polygon(TR,3);
skp->Text(50,70,unit,strlen(unit));
oapiReleaseSketchpad(skp);
oapiBlt(temps,parent->surf,0,0,30,30,40,40); //save this onto a back-surf

 };


void EGauge::RefreshMe()
{
 oapiBlt(parent->surf,temps,30,30,0,0,40,40);//clean the surface 
oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);	// then get a DC to draw new pointer
float ang;
float Pi=3.1415;
//check for need to redraw ?
ang= -(((*SRC/scale)-MinV)/(MaxV-MinV)* 5*Pi/4)+5*Pi/8;          // get turn angle for the indicator
if (ang>5*Pi/8) ang=5*Pi/8;	// if too large
if (ang<-5*Pi/8) ang = -5*Pi/8;  // or too small
if (fabsf(ang-lastdraw)>0.3) ang=lastdraw+(ang>lastdraw?0.3:-0.3)+(ang-lastdraw)/100;//slowly move to the value 
lastdraw=ang;

oapi::IVECTOR2 S[3],TR[3];
TR[0].x=0;TR[0].y=-20;
TR[1].x=5;TR[1].y=0;
TR[2].x=-6;TR[2].y=0;
for (int i=0;i<3;i++) // rotate the pointer by 'ang'
{
   S[i].x=50 + TR[i].x*cos(ang)-TR[i].y*sin(ang);
   S[i].y=50 + TR[i].x*sin(ang)+TR[i].y*cos(ang);
}
skp->SetBrush(hBRUSH_Black);
skp->SetPen(NULL);

skp->Polygon(S,3);// then the pointer
oapiReleaseSketchpad(skp);
 };
//----------------------------------- HGAUGE ------------------------------------------
HGauge::HGauge(int x,int y, int i_w,int i_h,float* i_SRC1,float* i_SRC2,const char i_unit[15],int i_min,int i_max, float i_scale,int i_fig,int i_lin,Panel *parent):instrument(x,y,parent)
{ Hght=i_h;Wdth=i_w;SRC1=i_SRC1;SRC2=i_SRC2;
  MaxV=i_max;MinV=i_min;scale=i_scale;NrFig=i_fig;NrLin=i_lin;
  strcpy(unit,i_unit);
  temps=oapiCreateSurface(31,190);lastdraw1=0;lastdraw2=0;
type=38;//Hgauge
};
void HGauge::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+85,ScrY+190),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 idx=index;
};
void HGauge::PaintMe()
{
 char intst[16];


 oapiBlt(parent->surf,hHgaugeSRF,0,0,0,0,85,190); //copy the backgorund

 oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
   //Put the texts where they're supposed to be*/
 skp->SetFont(hFNT_Panel);
 skp->SetTextAlign(oapi::Sketchpad::CENTER);  skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
 skp->SetTextColor(RGB(140,49,49));
 skp->Text((int)85/2,15,unit,strlen(unit)); // the unit of msrm.
 int k,j,i=0;
 do  {
      snprintf(intst,sizeof(intst),"%d.",(int)(i*(MaxV-MinV)/NrFig+MinV)); //convert number to char
      k=(int)(190-(40+(190-85)*i/NrFig));;          // calculate the y coord.
      skp->SetPen(hPEN_Gray);
      if (i<NrFig) for(j=1;j<NrLin+1;j++) { // then the scale (three lines between the numbers)
			skp->MoveTo(23,k-(int)(190-85)*j/(NrFig*NrLin));
			skp->LineTo(85-23,k-(int)(190-85)*j/(NrFig*NrLin));
			};
      skp->Text((int)85/2,k-5,intst,strlen(intst));// write the numbers
      i++; 
      } while (i<NrFig+1);
 
 
 oapiReleaseSketchpad(skp);
  
 oapiBlt(temps,parent->surf,0,0,12,0,9,190);	//save the two empty stripes
 oapiBlt(temps,parent->surf,10,0,85-22,0,9,190);

 //draw the arrows
 skp=oapiGetSketchpad(temps);
 skp->SetBrush(hBRUSH_White);
 skp->SetPen(hPEN_TotalBlack);
 skp->Rectangle(19,0,31,21);
 oapi::IVECTOR2 arrow1[3],arrow2[3];
 arrow1[0].x=21;arrow1[0].y=1; arrow2[0].x=28;arrow2[0].y=11;
 arrow1[1].x=21;arrow1[1].y=9;arrow2[1].x=28;arrow2[1].y=19;
 arrow1[2].x=29;arrow1[2].y=5;arrow2[2].x=20;arrow2[2].y=15;
 skp->SetBrush(hBRUSH_Black);skp->SetPen(NULL);
 skp->Polygon(arrow1,3);
 skp->Polygon(arrow2,3);
 oapiReleaseSketchpad(skp);



};
void HGauge::RefreshMe()
{int num;
//get the naked stripes,
oapiBlt(parent->surf,temps,12,0,0,0,9,190);
oapiBlt(parent->surf,temps,85-22,0,10,0,9,190);

  num=(190-45)- (*SRC1/scale-MinV)/(MaxV-MinV) * (190-85); 
  if (num<20) num=20; //get the value scaled
  if (abs(lastdraw1-num)>10) num=lastdraw1+(lastdraw1>num?-10:10)+(num-lastdraw1)/5;   
  lastdraw1=num;
  oapiBlt(parent->surf,temps,12,num,20,1,8,10,0xFFFFFF);
 
  num=(190-45)- (*SRC2/scale-MinV)/(MaxV-MinV) * (190-85); if (num<20) num=20;
  if (abs(lastdraw2-num)>10) num=lastdraw2+(lastdraw2>num?-10:10)+(num-lastdraw2)/5;
  lastdraw2=num;
  oapiBlt(parent->surf,temps,85-21,num,21,10,8,10,0xFFFFFF);
    
      };
//---------------------------- ROTARY --------------------------------------------

Rotary::Rotary(int x,int y,const char i_name[15],const char i_names[7][12], int i_set, int i_poz,Panel* parent):instrument(x,y,parent)
{
	 strcpy(screentext,i_name); 
	 for (int i=0;i<7;i++)  strcpy(names[i],i_names[i]);
	 set=i_set;poznr=i_poz;
	 draw[0].x=-11;draw[0].y=40;
	 draw[1].x= 11;draw[1].y=40;
	 draw[2].x= 12;draw[2].y= 0;
	 draw[3].x=  6;draw[3].y=-40;
	 draw[4].x= -6;draw[4].y=-40;
	 draw[5].x=-12;draw[5].y=0;
	 type=34;//rotary
};
void Rotary::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+161,ScrY+141),PANEL_REDRAW_MOUSE,PANEL_MOUSE_DOWN,PANEL_MAP_CURRENT);
 idx=index;
};
void Rotary::PaintMe()
{
  float ang;
float Pi=3.1415;
oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->SetPen(NULL);
skp->SetBrush(hBRUSH_Background);
skp->Rectangle(0,0,0+162,0+142); 
skp->SetFont(hFNT_Panel);
skp->SetTextAlign(oapi::Sketchpad::CENTER);
skp->SetTextColor(RGB(255,100,100));	// text color for powered/ unpowered
skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
skp->Text(85,10,screentext,strlen(screentext));	// name of rotary
ang = (int)poznr/2; ang =Pi/2-ang*Pi/6;				// calculate the starting angle depending on poznr
for (int i=0; i<poznr;i++)					// now put all texts arround the circle
{ skp->Text(85-cos(ang)*70,90-sin(ang)*70,names[i],strlen(names[i]));
  ang+=Pi/6;
}

 oapiReleaseSketchpad(skp);
 oapiBlt(parent->surf,hRotarySRF,36,41,400+(set-(int)(poznr/2))*100,0,100,100); //copy the backgorund
};
void Rotary::LBD(int x,int y)
{if (set>0)
	{ set--;
    oapiTriggerPanelRedrawArea(parent->idx,idx);
	}
}; 
void Rotary::RBD(int x,int y)
{ if (set<poznr-1)
	{ set++;
	oapiTriggerPanelRedrawArea(parent->idx,idx);
	}
}; 

void RotElement::RegisterMe(int index)
{
  oapiRegisterPanelArea(index,_R(0,0,1,1),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE);
 idx=index;

};
void RotElement::RefreshMe()
{ if (lastdraw!=*set) 
		{lastdraw=*set;
		 *SRC=TRG[lastdraw];
		};		
};

/*
class DigClock:public instrument
{ public:
 char* SRC;
 DigClock(int x,int y,char* i_SRC,Panel* i_parent);
 void RegisterMe(int index);
 void PaintMe();
 void RefreshMe();
private:
 SURFHANDLE temps;
};
*/
DigClock::DigClock(int x, int y, char *i_SRC,int i_lng,Panel *i_parent):instrument(x,y,i_parent)
{SRC=i_SRC;
 len=i_lng;
 init=false;
 local[0] = '\0';
  //strcpy(local,SRC);
 type=40;//digclock
 local_srf=oapiCreateSurface(2+len*22,44);
powered=0;
};
DigClock::~DigClock()
{oapiDestroySurface(local_srf);
};
void DigClock::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+2+len*22,ScrY+44),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE);
 idx=index;
};

void DigClock::PaintMe()
{
oapi::Sketchpad *skp=oapiGetSketchpad(local_srf);
skp->SetPen(hPEN_White);
skp->MoveTo(0,43);skp->LineTo(len*22+1,43);
skp->LineTo(len*22+1,0);
skp->SetPen(hPEN_Gray);
skp->LineTo(0,0);skp->LineTo(0,43);

oapiReleaseSketchpad(skp);

for (int i=0;i<len;i++)
{	oapiBlt(local_srf,hClockSRF,22*i+1,1,220,0,22,42);//all blanc
local[i]=0;};
oapiBlt(parent->surf,local_srf,0,0,0,0,2+len*22,44);

}
void DigClock::RefreshMe()
{ SRC[len]=0;
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
{powered=1;
if (strcmp(local,SRC)) {
	for (int i=0;i<len;i++)
			if (local[i]!=SRC[i])	{
				local[i]=SRC[i];
				if ((local[i]>47)&&local[i]<58)  oapiBlt(local_srf,hClockSRF,22*i+1,1,22*(local[i]-48),0,22,42);
			    else 
					if (local[i]==' ' ) oapiBlt(local_srf,hClockSRF,22*i+1,1,0,0,22,42); //space must be 0
					else  
						if ((local[i]==':')||(local[i]=='.'))
							oapiBlt(local_srf,hClockSRF,22*i+1,1,242,0,22,42); //:
						else 
							oapiBlt(local_srf,hClockSRF,22*i+1,1,220,0,22,42);//rest
//					sprintf(oapiDebugString(),"%s %i ",local,local[0]);
									}
oapiBlt(parent->surf,local_srf,0,0,0,0,2+len*22,44);
}
}
else
{if (powered) PaintMe();
powered=0;
};
};
  

bounds::bounds(float *i_SRC,float i_max,float i_min)
{SRC=i_SRC;max=i_max;min=i_min;next=NULL;
};
bool bounds::test()
{ if ((*SRC>max)||(*SRC<min)) return true;
 return false;
};

CW::CW(int x,int y,const char *i_text,Panel *i_parent):instrument(x,y,i_parent)
{b_list.next=NULL;last=&b_list;
 strcpy(text,i_text);alarm=1;
 temps=oapiCreateSurface(100,31);
//
oapiBlt(temps,hCwSRF,0,0,0,0,100,31);
   //Put the texts where they're supposed to be*/
 oapi::Sketchpad *skp=oapiGetSketchpad(temps);
 skp->SetFont(hFNT_Panel);
 skp->SetTextAlign(oapi::Sketchpad::CENTER);  skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
 skp->SetTextColor(RGB(255,100,100));
 skp->Text(25,10,text,strlen(text)); 
 skp->SetTextColor(RGB(149,48,48));
 skp->Text(75,10,text,strlen(text)); 
oapiReleaseSketchpad(skp);

};
CW::~CW()
{oapiDestroySurface(temps);
};
void CW::Watch(bounds *new_b)
{ last->next=new_b;
  last=new_b;
};
void CW::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+50,ScrY+31),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE);
 idx=index;
}
void CW::PaintMe()
{ 
oapiBlt(parent->surf,temps,0,0,alarm*50,0,50,31);

 }
void CW::RefreshMe()
{int tripped=1;
 bounds *runner=b_list.next;
 while (runner) 
	{ if (runner->test()) tripped=0;
       runner=runner->next;
	};

 if ((tripped!=alarm)) {alarm=tripped;  oapiTriggerPanelRedrawArea(parent->idx,idx);};
};

inst_MFD::inst_MFD(int x, int y,int i_type, Panel *i_parent):instrument(x,y,i_parent)
{mfdspecs.pos.left=x+55;mfdspecs.pos.top=y+20;
 mfdspecs.pos.right=x+300;mfdspecs.pos.bottom=y+265;
 mfdspecs.nbt_left=6;mfdspecs.nbt_right=6;
 mfdspecs.bt_yofs=41; mfdspecs.bt_ydist=34;
 type=i_type; //MFD - user specified
}
void inst_MFD::RegisterMe(int index)
{idx=index;
oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+350,ScrY+296),PANEL_REDRAW_INIT,PANEL_MOUSE_LBDOWN);
oapiRegisterMFD(type,mfdspecs);
};
void inst_MFD::PaintMe()
{
oapiBlt(parent->surf,hMFDSRF,0,0,0,0,350,296);
oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->SetFont(hFNT_Panel);
skp->SetTextColor (RGB(20, 20, 20));
skp->SetTextAlign (oapi::Sketchpad::CENTER);
skp->SetBackgroundMode (oapi::Sketchpad::BK_TRANSPARENT);

const char *label;
	for (int bt = 0; bt < 12; bt++) {
		if ((label = oapiMFDButtonLabel (type, bt)))
			{if (bt<6) skp->Text (31, 58+34*bt, label, strlen(label));
			else skp->Text (322, 58+34*(bt-6), label, strlen(label));}
		else break;
	}
oapiReleaseSketchpad(skp);

};


void inst_MFD::LBD(int x, int y)
{
	y-=55;

	if ((y>213)&&(y<227)) {
		if ((x>107)&&(x<147)) {oapiToggleMFD_on(type);return;}
		if ((x>156)&&(x<196)) {oapiSendMFDKey(type,OAPI_KEY_F1);return;};
		if ((x>205)&&(x<245)) {oapiSendMFDKey(type,OAPI_KEY_GRAVE);return;};
	}

	if (y%34 <18) {
		int bt=(int)(y/34);//which button
		if ((bt>=0)&&(bt<=6)) {
			if ((x>19)&&(x<47)) {
				oapiProcessMFDButton(type,bt,PANEL_MOUSE_LBDOWN); return;
			}
			bt+=6;
			if ((x>309)&&(x<337)) {
				oapiProcessMFDButton(type,bt,PANEL_MOUSE_LBDOWN);return;
			}
		}
	}
}

Bitmap::Bitmap(int x, int y, int w, int h, int index, Panel *i_parent):instrument(x,y,i_parent)
{ wdht=w; hght=h; number=index;}
void Bitmap::RegisterMe(int index)
{idx=index;
oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+wdht,ScrY+hght),PANEL_REDRAW_INIT,PANEL_MOUSE_IGNORE);
};
void Bitmap::PaintMe()
{ 	 oapiBlt(parent->surf,hFront_Panel_SRF[number],0,0,0,0,wdht,hght);
};

SSTRUCT_ITEM::SSTRUCT_ITEM(OBJHANDLE i_vs)
{ vs=i_vs;
  port=0;//initial port is 0;
  VESSEL *vessel=oapiGetVesselInterface(vs);//get a vessel interface
  maxports=(vessel)->DockCount();//so that ve can get number of docking ports
next=NULL;
};
bool SSTRUCT_ITEM::Add(SSTRUCT_ITEM *newitem)
{SSTRUCT_ITEM *runner;
runner=next;
if (vs==newitem->vs) return false; //same ship, no adding
if (runner) //if there are other items in the list
{
if (runner->vs==newitem->vs) return false; //is the ship anywhere in the list
while (runner->next){ runner=runner->next; //goto last item in list;					   	
					  if (runner->vs==newitem->vs) return false;
					};
runner->next=newitem;//ship was not found, add it, then return true
return true;
}
else next=newitem;
return true;
};

SSTRUCT_ITEM::~SSTRUCT_ITEM()
{ if (next)
   delete next;
};



Docker::Docker(int x, int y, Panel *i_parent):instrument(x,y,i_parent)
{dockflap=0;
cgmode=0;                 // 0=auto, 1=manual
cgswitch=0;               // -1=left, 0=center, 1=right
sensormode=0;             // docking sensor mode (local/remote)
portnr=0;             // activep port
cgofs=0;               // longitudinal offset of CG when attached to other object
vs = 0;
}
void Docker::RegisterMe(int index)
{idx=index;
oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+202,ScrY+179),PANEL_REDRAW_ALWAYS,15);
};
void Docker::PaintMe()
{ 	
oapiBlt(parent->surf,hDockBSRF,0,0,0,0,202,179);
cgofs=((Dragonfly*)(parent->v))->cgofs;
SURFHANDLE temps=oapiCreateSurface(202,179);
oapi::Sketchpad *skp=oapiGetSketchpad(temps);
skp->SetBrush(hBRUSH_TotalBlack);
skp->SetPen(hPEN_TotalBlack);
skp->Rectangle(0,0,202,179);
skp->SetTextColor(RGB(0,255,0));
skp->SetTextAlign(oapi::Sketchpad::CENTER);
skp->SetFont(hFNT_Panel);
skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
char text[300];
sprintf(text, "%0.1f m", cgofs);
skp->Text (140, 95, text, strlen (text));
int loc = 102+min ((int)(cgofs*3.784), 74);
skp->SetPen (hPEN_BYellow);
skp->MoveTo (loc, 93); skp->LineTo (loc-3, 86); skp->LineTo (loc+3, 86); skp->LineTo (loc, 93);

VESSEL *vessel;
int docked_port=0;
if (sensormode) 
	{vessel=oapiGetVesselInterface(vs->vs);
	  strcpy(text,vessel->GetName());
	 skp->Text(130,137,text,strlen(text));
	 sprintf(text,"PORT %i",vs->port);
	 skp->Text(130,147,text,strlen(text));
	 if (vessel->GetDockStatus(vessel->GetDockHandle(vs->port)))
		{docked_port=1;
		strcpy(text,"ENG");
		skp->SetTextColor (0);
		skp->SetBackgroundColor (RGB(255,255,0));
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
        skp->Text(160,147,text,strlen(text));
		}
	}
else {strcpy(text,"LOCAL");
      skp->Text(130,137,text,strlen(text));
	 sprintf(text,"PORT %i",portnr);
	 skp->Text(130,147,text,strlen(text));
	 if (parent->v->GetDockStatus(parent->v->GetDockHandle(portnr)))
		{docked_port=1;
		strcpy(text,"ENG");
		skp->SetTextColor (0);
		skp->SetBackgroundColor (RGB(255,255,0));
		skp->SetBackgroundMode (oapi::Sketchpad::BK_OPAQUE);
        skp->Text(160,147,text,strlen(text));
		}
	 }

oapiReleaseSketchpad(skp);
if (dockflap){
		if (docked_port)
					oapiBlt(parent->surf,hDockSW1SRF,128,11,74,0,66,46);//a button you can push}
		else   
 					oapiBlt(parent->surf,hDockSW1SRF,128,11,146,0,66,46);//a button you can push
	}
if (cgswitch)
	oapiBlt(parent->surf,hDockSW2SRF,18,98,(cgswitch+1)*27,0,27,16);
  
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
     oapiBlt(parent->surf,temps,0,0,0,0,202,179,0x000000);
oapiDestroySurface(temps);
if (cgmode)
 oapiBlt(parent->surf,hDockDlSRF,21,73,21,0,21,21);
};
int Docker::BuildShipList(SSTRUCT_ITEM *ship)
{OBJHANDLE newship;
 DOCKHANDLE newdock;
 SSTRUCT_ITEM *newitem;
 VESSEL *vessel;
 VECTOR3 pos,dir,rot;
if (list.Add(ship)) //are we there ?
	{ //this is a new ship, we add all docked ships
	vessel=oapiGetVesselInterface(ship->vs);//get a vessel interface
		for (int i=0;i<ship->maxports;i++)
		{ 	newdock=vessel->GetDockHandle(i);//and a handle to that port
		    vessel->GetDockParams(newdock,pos,dir,rot);
		    newship=vessel->GetDockStatus(newdock);
			if (newship) //we are docked to something?
				{newitem=new SSTRUCT_ITEM(newship); 
				 if (!BuildShipList(newitem)) delete newitem;//not new!
				};
		}
	return true; //the ship was added, and all ports scanned
	}
return false;//ship exists;
};
void Docker::LBD(int x,int y)
{
struct { OBJHANDLE hObj; int dock; } dockspec;
VESSEL *vessel;
	if ((dockflap) && ( x>166 && x<186 && y>14 && y<51)) {dockflap=0; return;}
	if (x>128 && x<166 && y>14 && y<51)
	{ if (dockflap) {
					if (sensormode)
					{vessel=oapiGetVesselInterface(vs->vs);//get a vessel interface
					 vessel->Undock(vs->port);
					 return;
					 }
					else
					{parent->v->Undock(portnr);
					 return;
					};
					}
	  else {dockflap=1; return;
			};
	};

	if (x >11 && y>132 && x<50 && y<160) //change vessel
	{ //need to change vessel for active port :-?
			if (!sensormode)	//we are local ,need to get remote
		{		if (list.next) {delete list.next;//delete old list if present
								list.next=NULL;};
				 vs=new SSTRUCT_ITEM((parent->v->GetHandle()));//we are the root,make an item
				 BuildShipList(vs);//now add all docked ships,
				 vs=list.next;//this is us again

				 if (vs->next) {vs=vs->next;
								sensormode=1;//we have becomed remote!
								dockspec.hObj=vs->vs;
								dockspec.dock=vs->port;
								oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);//change the vessel
								return;						  
							
				 }
				 else return;//no change, just return, there is no docked ship
				}
		else {	//we are already remote, need to go to next ship in the list
			   if (vs->next) //is there an next one?
								{vs=vs->next;
								dockspec.hObj=vs->vs;
								dockspec.dock=vs->port;
								oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);//change the vessel
						        return;
			   }
			   else { //the last ship in the list, then we go local
					 sensormode=0; //local,
					 dockspec.hObj=parent->v->GetHandle();
					 dockspec.dock=0;
					 oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);//change the vessel
					 return;
					}			
		}//end of sensormode
	}//end of mouse click

	if (x >52 && y>132 && x<91 && y<160) {//
		if(sensormode)	{//remote ship ++port
			vs->port++;
			if(vs->port>=vs->maxports) vs->port=0;
			dockspec.hObj=vs->vs;
			dockspec.dock=vs->port;
			oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);
			return;
		}
		else {
			dockspec.hObj=parent->v->GetHandle();
			if (++portnr>=parent->v->DockCount()) portnr=0;
			dockspec.dock=portnr;
			oapiBroadcastMFDMessage (MFD_DOCKING, 0, (void*)&dockspec);//change the vessel
			return;
			}
										}
	if (x>21 && x<42 && y>73 && y<94) 
	{ cgmode++; if (cgmode>1) cgmode=0;
	  ((Dragonfly*)(parent->v))->SetCGMode (cgmode);
	  return;
	};
	if (x>13 && x<32 && y>98 && y<110) cgswitch=-1;
    if (x>33 && x<51 && y>98 && y<110) cgswitch=1;
};

void Docker::BU()
{cgswitch=0;
oapiTriggerPanelRedrawArea(parent->idx,idx); 
};
void Docker::RefreshMe()
{if (cgswitch)
	{	((Dragonfly*)(parent->v))->MoveCGOfs (cgswitch);
         cgofs=((Dragonfly*)(parent->v))->cgofs;
		  oapiTriggerPanelRedrawArea(parent->idx,idx);  
}
};

NAVFRQ::NAVFRQ(int x, int y, Panel *i_parent):instrument(x,y, i_parent)
{nav=1;//1 is by default

frq1=0;
frq2=0;
timer=-1 ;
frswitch=0;
step=1;
};

void NAVFRQ::RegisterMe(int index)
{idx=index;
oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+201,ScrY+63),PANEL_REDRAW_INIT,15);
};
void NAVFRQ::PaintMe()
{oapiBlt(parent->surf,hNavSRF,0,0,0,0,201,63);
if (nav==2) oapiBlt(parent->surf,hDockDlSRF,21,14,21,0,21,21);
if (frswitch)
	oapiBlt(parent->surf,hDockSW2SRF,18,39,(frswitch+1)*27,0,27,16);
if (*(((Dragonfly*)parent->v)->DC_power)>0)  {
SURFHANDLE temps=oapiCreateSurface(101,26);
oapi::Sketchpad *skp=oapiGetSketchpad(temps);
skp->SetBrush(hBRUSH_TotalBlack);
skp->SetPen(hPEN_TotalBlack);
skp->Rectangle(0,0,100,25);

skp->SetTextColor(RGB(0,255,0));
skp->SetTextAlign(oapi::Sketchpad::CENTER);
skp->SetFont(hFNT_Panel);
skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
char text[64];
sprintf(text, "NAV1: %5.2f MHz", 108.0+(0.05*frq1));
skp->Text (10, 2, text, strlen (text));
sprintf(text, "NAV2: %5.2f MHz", 108.0+(0.05*frq2));
skp->Text (10, 13, text, strlen (text));
oapiReleaseSketchpad(skp);
oapiBlt(parent->surf,temps,80,20,0,0,80,25);
oapiDestroySurface(temps);
}
};
void NAVFRQ::RefreshMe()
{
if (frswitch)
{ if (timer<0){ if (nav==1)frq1+=frswitch*step;
			   else frq2+=frswitch*step;
			   if (frq1>1000) frq1=640 - (4294967296-frq1);
			   if  (frq1>639) frq1-=639.0;
			   if (frq2>1000) frq2=640 - (4294967296-frq2);
			   if  (frq2>639) frq2-=639.0;
			   step++;
			     parent->v->SetNavRecv (0, frq1);
				 parent->v->SetNavRecv (1, frq2);
			   timer=0.15;			
				};
  timer-=oapiGetSysStep();	
  oapiTriggerPanelRedrawArea(parent->idx,idx); 

}
//sprintf(oapiDebugString(),"%i %i",frq1,frq2);
}
void NAVFRQ::LBD(int x, int y)
{ if (x>21 && x<42 && y>14 && y<35) 
		{nav++ ;if (nav>2) nav=1;	oapiTriggerPanelRedrawArea(parent->idx,idx); };
 	if (x>13 && x<32 && y>38 && y<50) frswitch=-1;
    if (x>33 && x<51 && y>38 && y<50) frswitch=1;
};

void NAVFRQ::BU()
{frswitch=0;
timer=-1;step=1;
oapiTriggerPanelRedrawArea(parent->idx,idx); 
};

ADI::ADI(int x,int y, Panel *i_parent):instrument(x,y,i_parent)
{
type= 44; //ADI ball
init=0;
ref_handle=0;
over_rate=0;
tex_w=tex_h=0;
radius=10;
int i;
float trad;
float Pi=acos(-1.0);
int indx;
function_mode=0;//GDC;
orbital_ecliptic=-1; //orbital GDC;
reference.x=0.0;reference.y=0.0;reference.z=0.0;//ecliptic is also first reference
now.x=0.37;now.y=-1.2;now.z=0.5;
float norm;
//we define the ADI sphere to be rendered by OpenGL
//we use a 6 by 12 sphere, with 84 vertices and 72 triangles
//bottom point
tri=0;//number of triangle strips
for (i=0;i<16;i++)
  for (int j=0;j<16;j++)
		{ trad=cos(-Pi/2+Pi/15*i)*radius;
		  indx=i*16+j;
		 sphere_vert[indx][0]=cos(2*acos(-1.0)/16*j)*trad;
		 sphere_vert[indx][1]=sin(2*acos(-1.0)/16*j)*trad;
		 sphere_vert[indx][2]=sin(-Pi/2+Pi/15*i)*radius;
		 sphere_tex[indx][0]=j/16.0;
		 sphere_tex[indx][1]=i/15.0;
		 norm=sqrt(pow(sphere_vert[indx][0],2)+
				   pow(sphere_vert[indx][1],2)+
				   pow(sphere_vert[indx][2],2));
		 sphere_norm[indx][0]=sphere_vert[indx][0]/norm;
		 sphere_norm[indx][1]=sphere_vert[indx][1]/norm;
		 sphere_norm[indx][2]=sphere_vert[indx][2]/norm;
		 if (i>0) {
			 sphere_index[tri++]=indx;
			 sphere_index[tri++]=indx-16;
			// sphere_index[tri++]=indx-1;
			 //sphere_index[tri++]=indx;
			 //sphere_index[tri++]=indx-12;
			 //sphere_index[tri++]=indx-13;
							};
		};

		 
};
static bool LoadADITexture(const char *fname, std::vector<unsigned char> &rgb, int &w, int &h)
{
	FILE *file=fopen(fname,"rb");
	if (!file) return false;
	std::vector<unsigned char> d;
	unsigned char buf[65536];
	size_t n;
	while ((n=fread(buf,1,sizeof(buf),file))>0) d.insert(d.end(),buf,buf+n);
	fclose(file);
	if (d.size()<54 || d[0]!='B' || d[1]!='M') return false;
	auto u16=[&](size_t o) { return (unsigned)(d[o] | (d[o+1]<<8)); };
	auto u32=[&](size_t o) { return (uint32_t)d[o] | ((uint32_t)d[o+1]<<8) | ((uint32_t)d[o+2]<<16) | ((uint32_t)d[o+3]<<24); };
	size_t off=u32(10);
	int bw=(int32_t)u32(18), bh=(int32_t)u32(22);
	if (u16(28)!=24 || bw<=0 || bh==0) return false;
	bool topdown=(bh<0);
	if (topdown) bh=-bh;
	size_t stride=((size_t)bw*3+3)&~(size_t)3;
	if (off+stride*bh>d.size()) return false;
	rgb.resize((size_t)bw*bh*3);
	for (int y=0;y<bh;y++) {
		const unsigned char *src=&d[off+stride*(topdown?bh-1-y:y)];
		unsigned char *dst=&rgb[(size_t)y*bw*3];
		for (int x=0;x<bw;x++) {
			dst[3*x]=src[3*x+2];
			dst[3*x+1]=src[3*x+1];
			dst[3*x+2]=src[3*x];
		}
	}
	w=bw; h=bh;
	return true;
}

void ADI::InitGL()
{
if (!LoadADITexture("Textures/adi.dds",tex,tex_w,tex_h)) {
	tex.clear();
	tex_w=tex_h=0;
}
frame.assign(130*130,0);
depth.assign(130*130,0.0f);
SetView();
init=1;
};

ADI::~ADI()
{
};

void ADI::SetView()
{
	double cz=cos(-now.z),sz=sin(-now.z);
	double cy=cos(now.y),sy=sin(now.y);
	double cx=cos(now.x),sx=sin(now.x);
	double Ry[3][3]={{cz,0,sz},{0,1,0},{-sz,0,cz}};
	double Rx[3][3]={{1,0,0},{0,cy,-sy},{0,sy,cy}};
	double Rz[3][3]={{cx,-sx,0},{sx,cx,0},{0,0,1}};
	double L[3][3]={{1,0,0},{0,0,1},{0,-1,0}};
	double A[3][3],B[3][3];
	int i,j,k;
	for (i=0;i<3;i++) for (j=0;j<3;j++) {
		A[i][j]=0; for (k=0;k<3;k++) A[i][j]+=L[i][k]*Ry[k][j];
	}
	for (i=0;i<3;i++) for (j=0;j<3;j++) {
		B[i][j]=0; for (k=0;k<3;k++) B[i][j]+=A[i][k]*Rx[k][j];
	}
	for (i=0;i<3;i++) for (j=0;j<3;j++) {
		rot[i][j]=0; for (k=0;k<3;k++) rot[i][j]+=B[i][k]*Rz[k][j];
	}
}

void ADI::RenderBall()
{
	const int W=130;
	const double f=1.0/tan(22.5*PI/180.0);
	double Ll=sqrt(10.0*10.0+20.0*20.0+10.0*10.0);
	const double Lx=10.0/Ll,Ly=20.0/Ll,Lz=10.0/Ll;
	struct PV { double x,y,iw,r,g,b,s,t; } pv[256];
	for (int k=0;k<256;k++) {
		const float *p=sphere_vert[k],*nm=sphere_norm[k];
		double ex=rot[0][0]*p[0]+rot[0][1]*p[1]+rot[0][2]*p[2];
		double ey=rot[1][0]*p[0]+rot[1][1]*p[1]+rot[1][2]*p[2];
		double ez=rot[2][0]*p[0]+rot[2][1]*p[1]+rot[2][2]*p[2]-35.0;
		double nx=rot[0][0]*nm[0]+rot[0][1]*nm[1]+rot[0][2]*nm[2];
		double ny=rot[1][0]*nm[0]+rot[1][1]*nm[1]+rot[1][2]*nm[2];
		double nz=rot[2][0]*nm[0]+rot[2][1]*nm[1]+rot[2][2]*nm[2];
		double w=-ez;
		pv[k].x=(f*ex/w+1.0)*80.0-15.0;
		pv[k].y=160.0-(f*ey/w+1.0)*80.0-15.0;
		pv[k].iw=1.0/w;
		double dif=nx*Lx+ny*Ly+nz*Lz;
		double spc=0.0;
		if (dif>0.0) spc=0.09; else dif=0.0;
		pv[k].r=min(1.0,0.2+0.8*dif+spc);
		pv[k].g=pv[k].b=min(1.0,0.16+0.16*dif+spc);
		pv[k].s=sphere_tex[k][0];
		pv[k].t=sphere_tex[k][1];
	}
	std::fill(frame.begin(),frame.end(),0);
	std::fill(depth.begin(),depth.end(),0.0f);
	bool textured=(tex_w>0);
	for (int k=0;k+2<tri;k++) {
		const PV &a=pv[sphere_index[k]],&b=pv[sphere_index[k+1]],&c=pv[sphere_index[k+2]];
		double area=(b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x);
		if (area==0.0) continue;
		int x0=max(0,(int)floor(min(a.x,min(b.x,c.x))));
		int x1=min(W-1,(int)ceil(max(a.x,max(b.x,c.x))));
		int y0=max(0,(int)floor(min(a.y,min(b.y,c.y))));
		int y1=min(W-1,(int)ceil(max(a.y,max(b.y,c.y))));
		for (int py=y0;py<=y1;py++) {
			double qy=py+0.5;
			for (int px=x0;px<=x1;px++) {
				double qx=px+0.5;
				double l0=((c.x-b.x)*(qy-b.y)-(c.y-b.y)*(qx-b.x))/area;
				double l1=((a.x-c.x)*(qy-c.y)-(a.y-c.y)*(qx-c.x))/area;
				double l2=1.0-l0-l1;
				if (l0<0.0 || l1<0.0 || l2<0.0) continue;
				double iw=l0*a.iw+l1*b.iw+l2*c.iw;
				int idx=py*W+px;
				if (iw<=depth[idx]) continue;
				depth[idx]=(float)iw;
				double q0=l0*a.iw/iw,q1=l1*b.iw/iw,q2=l2*c.iw/iw;
				double r=q0*a.r+q1*b.r+q2*c.r;
				double g=q0*a.g+q1*b.g+q2*c.g;
				double bl=q0*a.b+q1*b.b+q2*c.b;
				if (textured) {
					double u=(q0*a.s+q1*b.s+q2*c.s)*tex_w-0.5;
					double v=(q0*a.t+q1*b.t+q2*c.t)*tex_h-0.5;
					double fu=floor(u),fv=floor(v);
					double au=u-fu,av=v-fv;
					int u0=((int)fu%tex_w+tex_w)%tex_w,u1=(u0+1)%tex_w;
					int v0=((int)fv%tex_h+tex_h)%tex_h,v1=(v0+1)%tex_h;
					const unsigned char *t00=&tex[((size_t)v0*tex_w+u0)*3],*t10=&tex[((size_t)v0*tex_w+u1)*3];
					const unsigned char *t01=&tex[((size_t)v1*tex_w+u0)*3],*t11=&tex[((size_t)v1*tex_w+u1)*3];
					double tc[3];
					for (int ch=0;ch<3;ch++)
						tc[ch]=((1-au)*(1-av)*t00[ch]+au*(1-av)*t10[ch]+(1-au)*av*t01[ch]+au*av*t11[ch])/255.0;
					r*=tc[0]; g*=tc[1]; bl*=tc[2];
				}
				frame[idx]=RGB((int)(r*255.0+0.5),(int)(g*255.0+0.5),(int)(bl*255.0+0.5));
			}
		}
	}
}

void ADI::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+141,ScrY+141),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 idx=index;
};
void ADI::SetOrbital()//this is quite heavy. API could use a way to get orientation w.r.t. orbit
{OBJHANDLE planet=parent->v->GetGravityRef();
VECTOR3 gpos,pos,vel,vel2;
vector3 Vpos,Vvel,Vnorm;
 parent->v->GetGlobalPos(gpos);
 parent->v->GetRelativePos(planet,pos);
 parent->v->GetRelativeVel(planet,vel);
 Vpos=_vector3(pos.x,pos.y,pos.z);
 Vnorm=_vector3(vel.x,vel.y,vel.z);
 Vnorm=Vpos*Vnorm;//this is normal on orbital plane;
 Vnorm.selfnormalize();
 
 vel.x+=gpos.x;vel.y+=gpos.y;vel.z+=gpos.z;
 parent->v->Global2Local(vel,vel2);
 Vvel=_vector3(vel2.x,vel2.y,vel2.z);//this is V vector in local frame
 Vvel.selfnormalize();
 Vnorm.x+=gpos.x;Vnorm.y+=gpos.y;Vnorm.z+=gpos.z;
 parent->v->Global2Local(_V(Vnorm.x,Vnorm.y,Vnorm.z),vel2);

 Vnorm=_vector3(vel2.x,vel2.y,vel2.z);//and this is N vector in local frame
 Vnorm.selfnormalize();

 vector3 local_up=Vnorm;
 vector3 local_front=Vnorm*_vector3(0,0,1);
 
 Vnorm.z=0;Vnorm.selfnormalize();//all we need from here is roll now
 float Pi=acos(-1.0);
 // ***********************
 float roll=Pi-atan2(Vnorm.x,Vnorm.y);
 // ************************
 float pitch=-(Pi/2-local_up.angle(_vector3(0,0,1)));
 //*************************
 
 float heading=Vvel.angle(local_front);
 vector3 local_head=local_front*Vvel;
 local_head.selfnormalize();//need to bring heading to quadtrant. ugly...
 if (local_head.z*local_up.z>0) heading=-heading;
 target.x=heading+Pi;
 target.y=pitch;
 target.z=roll;
};
 


void ADI::SetEcliptic()
{	float Pi=acos(-1.0);
    VESSELSTATUS vstat;parent->v->GetStatus(vstat);
	vstat.arot.z+=acos(-1.0)/2; //roll is referenced to horizontal ?!!?
	vstat.arot.x+=Pi/2;
	vstat.arot.x=2*Pi-vstat.arot.x;
	target.x=vstat.arot.x;
	target.y=vstat.arot.y;
	target.z=vstat.arot.z;

};
void ADI::SetReference()
{	float Pi=acos(-1.0);
	VESSELSTATUS vstat;parent->v->GetStatus(vstat);
	vstat.arot.z+=acos(-1.0)/2; //roll is referenced to horizontal ?!!?
	vstat.arot.x+=Pi/2;
	vstat.arot.x=2*Pi-vstat.arot.x;
	vstat.arot.x-=reference.x;vstat.arot.y-=reference.y;vstat.arot.z-=reference.z;
	target.x=vstat.arot.x;//*cos(vstat.arot.z)+sin(vstat.arot.z)*vstat.arot.y;
	target.y=vstat.arot.y;//*cos(vstat.arot.z)-sin(vstat.arot.z)*vstat.arot.x;
	target.z=vstat.arot.z;

};
void ADI::SetEquatorial()
{ float Pi=acos(-1.0);
  float pitch=parent->v->GetPitch();
  double heading;
  oapiGetHeading(parent->v->GetHandle(),&heading);
  heading+=Pi/2;
  heading=2*Pi-heading;
  //heading+=Pi;
  double bank=-(parent->v->GetBank());
  target.x=heading;
  target.y=pitch;
  target.z=bank;





}
void ADI::GetReference()
{	float Pi=acos(-1.0);
	VESSELSTATUS vstat;parent->v->GetStatus(vstat);
	vstat.arot.z+=acos(-1.0)/2;
	//vstat.arot.x+=Pi/2;
	vstat.arot.x=2*Pi-vstat.arot.x;
	reference.x=vstat.arot.x;
	reference.y=vstat.arot.y;
	reference.z=vstat.arot.z;

}
void ADI::MoveBall()
{   float delta;
	float Pi=acos(-1.0);
    over_rate=0.0;
    delta=target.z-now.z;
	if (delta>0.05) {if (delta>Pi) {now.z+=2*Pi;MoveBall();
									return;}
						now.z+=0.05;over_rate=1.;}
	else if (delta<-0.05){ if (delta<-Pi) {now.z-=2*Pi;MoveBall();
								return;}
						now.z-=0.05;over_rate=1.;}
	else now.z+=delta;

	delta=target.y-now.y;
	if (delta>0.05) {if (delta>Pi) {now.y+=2*Pi;MoveBall();
									return;}
					now.y+=0.05;over_rate=1.;}
	else if (delta<-0.05) {if (delta<-Pi) {now.y-=2*Pi;MoveBall();
								return;}
						now.y-=0.05;over_rate=1.;}
	else now.y+=delta;

	delta=target.x-now.x;
	if (delta>0.05) {if (delta>Pi) {now.x+=2*Pi;MoveBall();
									return;}
						now.x+=0.05;over_rate=1.;}
	else if (delta<-0.05) {if (delta<-Pi) {now.x-=2*Pi;MoveBall();
								return;}
						now.x-=0.05;over_rate=1.;}
	else now.x+=delta;

	SetView();

}
void ADI::PaintMe()
{
	if (!init) InitGL();
	if (ref_handle) {GetReference();ref_handle=0;};
	switch (function_mode)
	{case 0:if (orbital_ecliptic>0)
				SetEcliptic();
			else SetOrbital();
	        break;
	 case -1:
		    SetEquatorial();
		     break;
	 case 1:
			SetReference();
			break;
	};
	if(*((Dragonfly*)parent->v)->DC_power>0){ MoveBall();}
	else {over_rate=1;};
	RenderBall();

oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->QuickPen(0);
skp->SetPen(NULL);
skp->QuickBrush(0xFF000000);
skp->Rectangle(5,5,136,136);
for (int y=0;y<130;y++) {
	const DWORD *row=&frame[y*130];
	int x=0;
	while (x<130) {
		DWORD col=row[x];
		int xe=x+1;
		while (xe<130 && row[xe]==col) xe++;
		if (col) {
			skp->QuickBrush(col|0xFF000000);
			skp->Rectangle(5+x,5+y,5+xe+1,5+y+2);
		}
		x=xe;
	}
}
oapiReleaseSketchpad(skp);

oapiBlt(parent->surf,hADIBorder,0,0,0,0,140,140,0x0);

}
void ADI::RefreshMe()
{
PaintMe();
}

Radar::Radar(int x,int y, Panel *i_parent):instrument(x,y,i_parent)
{
range=150; //meters;
vessel_index=0;    //no target
list_index=0;	//1- go to next vessel , 0 - stay here
phase=0;
new_range=1;
last_antena_yaw=-1;
powered=0;
radar_background=oapiCreateSurface(102,102);
};
Radar::~Radar()
{oapiDestroySurface(radar_background);
};
void Radar::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+350,ScrY+193),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_DOWN,PANEL_MAP_CURRENT);
 idx=index;
};
void Radar::PaintMe()
{
oapiBlt(parent->surf,hRadSrfSRF,0,0,0,0,350,193);

char label[5];
oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
skp->SetFont(hFNT_Panel);
skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
skp->SetTextColor(RGB(0,0,0));
skp->SetTextAlign(oapi::Sketchpad::CENTER);

strcpy(label,"R+");skp->Text(31,58,label,strlen(label));
strcpy(label,"R-");skp->Text(31,92,label,strlen(label));
strcpy(label,"VS");skp->Text(31,129,label,strlen(label));
strcpy(label,"AT1");skp->Text(322,58,label,strlen(label));
strcpy(label,"AT2");skp->Text(322,92,label,strlen(label));

oapiReleaseSketchpad(skp);
  

}

void Radar::RefreshMe()
{
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
{powered=1;
	oapi::Sketchpad *skp;
	int num_ob=oapiGetVesselCount();
	OBJHANDLE object_us=parent->v->GetHandle();
	OBJHANDLE object;
	OBJHANDLE obj_last;
	VECTOR3 dist;
	VECTOR3 pos;
	if (new_range){new_range=0;
		skp=oapiGetSketchpad(parent->surf);
		char label[16];
		sprintf(label,"R:%im",range);
		skp->SetBrush(hBRUSH_TotalBlack);
		skp->SetPen(hPEN_TotalBlack);
		skp->Rectangle(220,20,270,31);
		skp->SetFont(hFNT_Panel);
		skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
		skp->SetTextColor(RGB(0,255,0));
		skp->SetTextAlign(oapi::Sketchpad::CENTER);
		skp->Text(255,20,label,strlen(label));
		oapiReleaseSketchpad(skp);
	};	//draw a new range 
	if (((Dragonfly*)parent->v)->UY_pos!=last_antena_yaw) {
		skp=oapiGetSketchpad(radar_background);
		skp->SetBrush(hBRUSH_TotalBlack);
		skp->SetPen(NULL);

		skp->Rectangle(0,0,101,101);
		last_antena_yaw=((Dragonfly*)parent->v)->UY_pos;
		float yaw=150.0 - ((Dragonfly*)parent->v)->UY_pos*300+15.0;
		yaw=yaw/180.0*acos(-1.0);
		float yaw1=yaw - 30.0/180.0 *acos(-1.0);
		float py1=cos(yaw)*45;
		float  px1=sin(yaw)*45;

		float py2=cos(yaw1)*45;
		float  px2=sin(yaw1)*45;

		skp->SetBrush(hBRUSH_FYellow);
		PanelPie(skp,0,0,100,100,50+px1,50-py1,50+px2,50-py2);
		oapiReleaseSketchpad(skp);
		oapiBlt(radar_background,hRadBkSRF,0,0,0,0,100,100,0x0);
	};//new yawed background


	oapiBlt(parent->surf,radar_background,60,50,0,0,100,100); //blt the radar backstuff
	oapiBlt(parent->surf,hRadBkSRF,190,50,0,0,100,100); //blt the radar stuff
	phase+=oapiGetSysStep()*16;if (phase>4) phase=0; //shift phase by 1/frame

int mod;
int k=1;	//vessel index in the radar list
float line;
for (int i=0;i<num_ob;i++)
{  object=oapiGetVesselByIndex(i);		// goto all objects
   oapiGetRelativePos(object_us,object,&dist); // then find a distance vector to ir
	   if (((line=_vector3(dist.x,dist.y,dist.z).mod())<range)&&(line>0.1)) {//anything within 150meters
			oapiGetGlobalPos(object,&dist);
		    parent->v->Global2Local(dist,pos);//now we have a position w.r.t ship
			
			mod=line/range*100;//number of pixles away
			mod=mod/5+phase; //
			mod = mod % 4; //one of 3 phases
			if ((list_index)&&(k-1==vessel_index))//we need a change and we are there
					{list_index=0; vessel_index=k;
					((Dragonfly*)(parent->v))->Dock_target_object=object;
					};
			k++;//next one
			oapiBlt(parent->surf,hRadarSRF,110-5+pos.x/range*50,100-5-pos.z/range*50,64-10*mod,1,10,10,0x000000);
			oapiBlt(parent->surf,hRadarSRF,240-5+pos.z/range*50,100-5-pos.y/range*50,64-10*mod,1,10,10,0x000000);
			if (object==((Dragonfly*)(parent->v))->Dock_target_object)
				{	oapiBlt(parent->surf,hRadarSRF,110-5+pos.x/range*50,100-5-pos.z/range*50,35,12,10,10,0x0);
					oapiBlt(parent->surf,hRadarSRF,240-5+pos.z/range*50,100-5-pos.y/range*50,35,12,10,10,0x0);
				}//end of docking object	
				
	   }//end of if

	   } //end of for
if (list_index) {//if we are here, then sorry, end of list,
				vessel_index=0;//back to the start next time
			};
}
else 
{if (powered) PaintMe();
 powered=0;
};

};

void Radar::LBD(int x, int y)
{
y-=55;
 int bt = -1; // MS 210831: initialised this, assuming that no button is hit if the y condition is not met.
if (y%34 <18)  bt=(int)(y/34);//which button
if ((bt>=0)&&(bt<=2)) {
	if ((x>19)&&(x<47)) 
		{	if (bt==0) {range+=50.0;new_range=1; if (range>500) range=500;return;}
			if (bt==1) {range-=50.0;new_range=1; if (range<50) range=50;return;}
			if (bt==2) {list_index=1;}
		}
	if ((x>309)&&(x<337)) 
		{	
		}
	}
}

FuelMeter::FuelMeter(int x, int y ,Panel *i_parent):instrument(x,y,i_parent)
{
old_fuel=1;powered=0;
};

void FuelMeter::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+81,ScrY+250),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 idx=index;old_fuel=1;powered=0;
};
void FuelMeter::PaintMe()
{ oapiBlt(parent->surf,hFuelSRF,0,0,0,0,81,250);
};
void FuelMeter::RefreshMe()
{
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
{powered=1;
if ((old_fuel>0)||(parent->v->GetPropellantFlowrate (((Dragonfly*)(parent->v))->ph_main))) {
	double fuel= ((Dragonfly*)(parent->v))->GetPropellantMass(((Dragonfly*)(parent->v))->ph_main);
	fuel = fuel / 4e3; //percentage of qty left;
//	sprintf(oapiDebugString(),"%f %f",fuel,old_fuel);

	oapi::Sketchpad *skp=oapiGetSketchpad(parent->surf);
	skp->SetFont(hFNT_Panel);
	skp->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT); 
	skp->SetTextColor(RGB(0,255,0));
	skp->SetTextAlign(oapi::Sketchpad::CENTER);
	skp->SetPen(NULL);
	skp->SetBrush(hBRUSH_Red);

	skp->Rectangle(22,35,44,45);
	skp->Rectangle(22,56,44,66);


	char cbuf[32];
	int i;

	i = (int)(143.0*fuel);

	skp->Rectangle (23, 84, 39, 228-i);
	skp->SetBrush(hBRUSH_Green);
	skp->Rectangle (23, 228-i, 39, 228);
	skp->SetBrush(hBRUSH_Green);
    sprintf (cbuf, "%3.1f", 100.0*fuel);
	skp->Text (32, 35, cbuf, strlen(cbuf));
	sprintf (cbuf, "%3.2f",old_fuel=parent->v->GetPropellantFlowrate (((Dragonfly*)(parent->v))->ph_main));
	skp->Text (32, 56, cbuf, strlen(cbuf));
	oapiReleaseSketchpad(skp);
	}
}
else
{ if (powered) PaintMe();
  powered=0;

};
}

VROT::VROT(int x, int y, Panel *i_parent):instrument(x,y,i_parent)
{powered=0;
};
void VROT::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+300,ScrY+110),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 powered=0;
};
void VROT::PaintMe()
{oapiBlt(parent->surf,hVrotBkSRF,0,0,0,0,300,110);
};
void VROT::RefreshMe()
{
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
{powered=1;
VESSELSTATUS vs;
parent->v->GetStatus(vs);
int i;
i=100*vs.vrot.x;
if (i>6) i=6;if (i<-6) i=-6;
oapiBlt(parent->surf,hVrotSRF,36,23,403-i*67,3,67,38);
i=100*vs.vrot.y;
if (i>6) i=6;if (i<-6) i=-6;
oapiBlt(parent->surf,hVrotSRF,121,23,403-i*67,3,67,38);
i=-100*vs.vrot.z;
if (i>6) i=6;if (i<-6) i=-6;
oapiBlt(parent->surf,hVrotSRF,202,23,403-i*67,3,67,38);
}
else 
{ if (powered) PaintMe();
 powered=0;
};

};
