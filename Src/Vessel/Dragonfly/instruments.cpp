// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "instruments.h"
#include < GL\gl.h >                                
#include < GL\glu.h >
#include "vectors.h"        
#include "panel.cpp"
#include "math.h"
#include "resource.h"
#include "orbitersdk.h"
#include "dragonfly.h"

using std::min;



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
{ /*
	HDC hDC=oapiGetDC(parent->surf);
 SelectObject(hDC,hPEN_Black);
 SelectObject(hDC,hBRUSH_Background);
 Rectangle(hDC,0,0,21,21);
 SelectObject(hDC,hPEN_Gray);
 MoveTo(hDC,17,3);LineTo(hDC,3,3);
 LineTo(hDC,3,17);
 SelectObject(hDC, hPEN_White);
 LineTo(hDC,17,17);LineTo(hDC,17,3);
 SelectObject(hDC,hPEN_NULL);SetBkColor(hDC,RGB(255,0,0));
 //if ((*SRC) || (!powered())) 
 last_d=0;
 if (*SRC)
				{ last_d=1;
					SelectObject(hDC,hBRUSH_Red);
					Rectangle(hDC,4,4,17,17);
					SelectObject(hDC,hBRUSH_StrpWht); 
				}  
 Rectangle(hDC,4,4,17,17);
 oapiReleaseDC(parent->surf,hDC);
*/
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
 hTEMPDC=oapiGetDC(temps);
type=36;//SFSwitch
};
void SFSwitch::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY-4,ScrX+50,ScrY+44),PANEL_REDRAW_MOUSE,PANEL_MOUSE_DOWN,PANEL_MAP_CURRENT);
 idx=index;
};
void SFSwitch::PaintMe()
{ int lng=24/sqrt(3.0);

HDC hDC=oapiGetDC(parent->surf);
SelectObject(hDC,hBRUSH_Background);SelectObject(hDC,hPEN_NULL);
Rectangle(hDC,0,0,51,50);
SelectObject(hDC,hPEN_Black);
Rectangle(hDC,5-5,4,5,4+40);Rectangle(hDC,5+40,4,5+45,4+40);
SelectObject(hDC,hBRUSH_LBkg);
Rectangle(hDC,5,4,5+40,4+40);
MoveTo(hDC,5+20-lng/2,4+8);LineTo(hDC,5+20+lng/2,4+8);
LineTo(hDC,5+20+lng,4+20);LineTo(hDC,5+20+lng/2,4+32);
LineTo(hDC,5+20-lng/2,4+32);LineTo(hDC,5+20-lng,4+20);
LineTo(hDC,5+20-lng/2,4+8);
Ellipse(hDC,5+10,4+10,5+31,4+31);
SelectObject(hDC,hBRUSH_FYellow);
Ellipse(hDC,5+12,4+12,5+29,4+29);
SelectObject(hDC,hPEN_NULL);
SelectObject(hDC,hBRUSH_LBkg);
Rectangle(hDC,5+20-4*pos,4+20,5+20+4*pos,4+20-7*pos);
Ellipse(hDC,5+17,4+17,5+24,4+24);
SelectObject(hDC,hPEN_Black);
MoveTo(hDC,5+20-4*pos,4+20);LineTo(hDC,5+20-4*pos,4+20-7*pos);
MoveTo(hDC,5+20+4*pos,4+20);LineTo(hDC,5+20+4*pos,4+20-7*pos);
Arc(hDC,5+20-4,4+20-4,5+25,4+25,5+20-4*pos,4+20,5+20+4*pos,4+20);

Rectangle(hDC,5+12,4+15-7*pos,5+29,4+25-7*pos-(3-num_pos)*2);
if (num_pos==3) Rectangle(hDC,5+12,4+18-9*pos,5+29,4+23-9*pos);


BitBlt(hTEMPDC,0,0,51,50,hDC,0,0,SRCCOPY);
SelectObject(hDC,hBRUSH_Red);
if (safed)
{
Rectangle(hDC,05,0,45,59);

BitBlt(hDC,10,15,30,15,hTEMPDC,10,15,SRCCOPY);
BitBlt(hDC,10,40,30,10,hTEMPDC,10,40,SRCCOPY);
MoveTo(hDC,10,15);LineTo(hDC,40,15);
LineTo(hDC,40,30);LineTo(hDC,10,30);LineTo(hDC,10,15);
MoveTo(hDC,10,48);LineTo(hDC,10,40);
LineTo(hDC,40,40);LineTo(hDC,40,48);
}   
else
{  Rectangle(hDC,5,0,45,25);
   BitBlt(hDC,10,15,30,15,hTEMPDC,10,15,SRCCOPY);
   BitBlt(hDC,10,0,30,4,hTEMPDC,10,0,SRCCOPY);

}                                             

oapiReleaseDC(parent->surf,hDC);                                       

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
  oapiReleaseDC(temps,hTEMPDC);
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

void screwdraw(HDC h,int x, int y)
{int di=5*sqrt(2.0)/2;
 SelectObject(h,hPEN_White);
 SelectObject(h,hBRUSH_Background);
 Ellipse(h,x-5,y-5,x+5,y+5);
 MoveTo(h,x-di,y-di);LineTo(h,x+di,y+di);
 }

void EGauge::RegisterMe(int index)
{oapiRegisterPanelArea(index,_R(ScrX,ScrY,ScrX+100,ScrY+100),PANEL_REDRAW_ALWAYS,PANEL_MOUSE_IGNORE,PANEL_MAP_CURRENT);
 idx=index;
};

 void EGauge::PaintMe()
 {float Pi=3.1415;
  char intst[10];
  float ang=-Pi/4+Pi/8;
  int i;
  POINT TR[3];
 oapiBlt(parent->surf,hEgaugeSRF,0,0,0,0,100,100); //copy the backgorund

HDC hDC=oapiGetDC(parent->surf);
SelectObject(hDC,hBRUSH_Gray);
SelectObject(hDC,hPEN_LGray);			// some of the circles
for (i=0;i<16;i++) {
MoveTo(hDC,50+cos(ang)*20,50-sin(ang)*20);// now put the small scales
LineTo(hDC,50+cos(ang)*24,50-sin(ang)*24);
ang+=Pi/12;
};ang=-Pi/4+Pi/8;
for (i=0;i<6;i++) {
MoveTo(hDC,50+cos(ang)*20,50-sin(ang)*20); //now the big scales
LineTo(hDC,50+cos(ang)*28,50-sin(ang)*28);
gcvt((int)(i*(MaxV-MinV)/5+MinV) ,6,intst); //convert number to char
SelectObject(hDC,hFNT_Panel);
SetTextAlign(hDC,TA_CENTER);SetBkMode(hDC,TRANSPARENT); 
SetTextColor(hDC,RGB(140,49,49));
TextOut(hDC,50+cos(ang)*35,45-sin(ang)*35,intst,sizeof(char)*strlen(intst)); // and scale numbers
ang+=Pi/4;
};
SelectObject(hDC,hPEN_NULL);
Ellipse(hDC,30,30,70,70);
SelectObject(hDC,hBRUSH_Black);
Ellipse(hDC,40,40,59,59);	// the rest of the circles
TR[0].x=50;TR[0].y=45;		// a black triangle on the bottom
TR[1].x=20;TR[1].y=75;
TR[2].x=80;TR[2].y=75;
Polygon(hDC,TR,3);
TextOut(hDC,50,70,unit,sizeof(char)*strlen(unit));
oapiReleaseDC(parent->surf,hDC);
oapiBlt(temps,parent->surf,0,0,30,30,40,40); //save this onto a back-surf

 };


void EGauge::RefreshMe()
{
 oapiBlt(parent->surf,temps,30,30,0,0,40,40);//clean the surface 
HDC hDC=oapiGetDC(parent->surf);				// then get a DC to draw new pointer
float ang;
float Pi=3.1415;
//check for need to redraw ?
ang= -(((*SRC/scale)-MinV)/(MaxV-MinV)* 5*Pi/4)+5*Pi/8;          // get turn angle for the indicator
if (ang>5*Pi/8) ang=5*Pi/8;	// if too large
if (ang<-5*Pi/8) ang = -5*Pi/8;  // or too small
if (abs(ang-lastdraw)>0.3) ang=lastdraw+(ang>lastdraw?0.3:-0.3)+(ang-lastdraw)/100;//slowly move to the value 
lastdraw=ang;

POINT S[3],TR[3];
TR[0].x=0;TR[0].y=-20;
TR[1].x=5;TR[1].y=0;
TR[2].x=-6;TR[2].y=0;
for (int i=0;i<3;i++) // rotate the pointer by 'ang'
{
   S[i].x=50 + TR[i].x*cos(ang)-TR[i].y*sin(ang);
   S[i].y=50 + TR[i].x*sin(ang)+TR[i].y*cos(ang);
}
SelectObject(hDC,hBRUSH_Black);
SelectObject(hDC,hPEN_NULL);

Polygon(hDC,S,3);// then the pointer
oapiReleaseDC(parent->surf,hDC);
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
 char intst[10];


 oapiBlt(parent->surf,hHgaugeSRF,0,0,0,0,85,190); //copy the backgorund

 HDC hDC=oapiGetDC(parent->surf);
   //Put the texts where they're supposed to be*/
 SelectObject(hDC,hFNT_Panel);
 SetTextAlign(hDC,TA_CENTER);  SetBkMode(hDC,TRANSPARENT); 
 SetTextColor(hDC,RGB(140,49,49));
 TextOut(hDC,(int)85/2,15,unit,sizeof(char)*strlen(unit)); // the unit of msrm.
 int k,j,i=0;//SetBkMode(hDC,OPAQUE);SetBkColor(hDC,RGB(0,0,0));
 do  {
      gcvt((int)(i*(MaxV-MinV)/NrFig+MinV) ,6,intst); //convert number to char
      k=(int)(190-(40+(190-85)*i/NrFig));;          // calculate the y coord.
      SelectObject(hDC,hPEN_Gray);
      if (i<NrFig) for(j=1;j<NrLin+1;j++) { // then the scale (three lines between the numbers)
			MoveTo(hDC,23,k-(int)(190-85)*j/(NrFig*NrLin));
			LineTo(hDC,85-23,k-(int)(190-85)*j/(NrFig*NrLin));
			};
      TextOut(hDC,(int)85/2,k-5,intst,sizeof(char)*strlen(intst));// write the numbers
      i++; 
      } while (i<NrFig+1);
 
 
 oapiReleaseDC(parent->surf,hDC);
  
 oapiBlt(temps,parent->surf,0,0,12,0,9,190);	//save the two empty stripes
 oapiBlt(temps,parent->surf,10,0,85-22,0,9,190);

 //draw the arrows
 hDC=oapiGetDC(temps);
 SelectObject(hDC,hBRUSH_White);
 Rectangle(hDC,19,0,31,21);
 POINT arrow1[3],arrow2[3];
 arrow1[0].x=21;arrow1[0].y=1; arrow2[0].x=28;arrow2[0].y=11;
 arrow1[1].x=21;arrow1[1].y=9;arrow2[1].x=28;arrow2[1].y=19;
 arrow1[2].x=29;arrow1[2].y=5;arrow2[2].x=20;arrow2[2].y=15;
 SelectObject(hDC,hBRUSH_Black);SelectObject(hDC,hPEN_NULL);
 Polygon(hDC,arrow1,3);
 Polygon(hDC,arrow2,3);
 oapiReleaseDC(temps,hDC);



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
{// POINT td[6];
//  POINT E1,E2;
  float ang;
float Pi=3.1415;
HDC hDC=oapiGetDC(parent->surf);
SelectObject(hDC,hPEN_NULL);
SelectObject(hDC,hBRUSH_Background);
Rectangle(hDC,0,0,0+162,0+142); 
SelectObject(hDC,hFNT_Panel);
SetTextAlign(hDC,TA_CENTER);
SetTextColor(hDC,RGB(255,100,100));	// text color for powered/ unpowered
SetBkMode(hDC,TRANSPARENT); 
TextOut(hDC,85,10,screentext,sizeof(char)*strlen(screentext));	// name of rotary
ang = (int)poznr/2; ang =Pi/2-ang*Pi/6;				// calculate the starting angle depending on poznr
for (int i=0; i<poznr;i++)					// now put all texts arround the circle
{ TextOut(hDC,85-cos(ang)*70,90-sin(ang)*70,names[i],sizeof(char)*strlen(names[i]));
  ang+=Pi/6;
}

 oapiReleaseDC(parent->surf,hDC);
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
 //temps= oapiCreateSurface (LoadBitmap(parent->hModule,MAKEINTRESOURCE(IDB_BITMAP1)));
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
HDC hDC=oapiGetDC(local_srf);
SelectObject(hDC,hPEN_White);
MoveTo(hDC,0,43);LineTo(hDC,len*22+1,43);
LineTo(hDC,len*22+1,0);
SelectObject(hDC,hPEN_Gray);
LineTo(hDC,0,0);LineTo(hDC,0,43);

oapiReleaseDC(local_srf,hDC);

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
HDC hDC2=oapiGetDC(temps);
HDC hDC=oapiGetDC(hCwSRF);
BitBlt(hDC2,0,0,100,31,hDC,0,0,SRCCOPY);
   //Put the texts where they're supposed to be*/
 SelectObject(hDC2,hFNT_Panel);
 SetTextAlign(hDC2,TA_CENTER);  SetBkMode(hDC2,TRANSPARENT); 
 SetTextColor(hDC2,RGB(255,100,100));
 TextOut(hDC2,25,10,text,sizeof(char)*strlen(text)); 
 SetTextColor(hDC2,RGB(149,48,48));
 TextOut(hDC2,75,10,text,sizeof(char)*strlen(text)); 
 oapiReleaseDC(hCwSRF,hDC);
oapiReleaseDC(temps,hDC2);

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
//oapiBlt(parent->surf,hMFDSRF,0,0,0,0,310,296); 
HDC hDC=oapiGetDC(parent->surf);
HDC hDC2=oapiGetDC(hMFDSRF);
BitBlt(hDC,0,0,350,296,hDC2,0,0,SRCCOPY);
SelectObject(hDC, hFNT_Panel);
SetTextColor (hDC, RGB(20, 20, 20));
SetTextAlign (hDC, TA_CENTER);
SetBkMode (hDC, TRANSPARENT);

const char *label;
	for (int bt = 0; bt < 12; bt++) {
		if (label = oapiMFDButtonLabel (type, bt))
			{if (bt<6) TextOut (hDC, 31, 58+34*bt, label, strlen(label));
			else TextOut (hDC, 322, 58+34*(bt-6), label, strlen(label));}
		else break;
	}
oapiReleaseDC(parent->surf,hDC);
oapiReleaseDC(hMFDSRF,hDC2);

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
HDC hDC=oapiGetDC(temps);
SelectObject(hDC, hBRUSH_TotalBlack);
Rectangle(hDC,0,0,202,179);
SetTextColor(hDC,RGB(0,255,0));
SetTextAlign(hDC,TA_CENTER);
SelectObject(hDC,hFNT_Panel);
SetBkMode(hDC,TRANSPARENT); 
char text[30];
sprintf(text, "%0.1f m", cgofs);
TextOut (hDC, 140, 95, text, strlen (text));
int loc = 102+min ((int)(cgofs*3.784), 74);
SelectObject (hDC, hPEN_BYellow);
MoveToEx (hDC, loc, 93, NULL); LineTo (hDC, loc-3, 86); LineTo (hDC, loc+3, 86); LineTo (hDC, loc, 93);

VESSEL *vessel;
int docked_port=0;
if (sensormode) 
	{vessel=oapiGetVesselInterface(vs->vs);
	  strcpy(text,vessel->GetName());
	 TextOut(hDC,130,137,text,strlen(text));
	 sprintf(text,"PORT %i",vs->port);
	 TextOut(hDC,130,147,text,strlen(text));
	 if (vessel->GetDockStatus(vessel->GetDockHandle(vs->port)))
		{docked_port=1;
		strcpy(text,"ENG");
		SetTextColor (hDC, 0);
		SetBkColor (hDC, RGB(255,255,0));
		SetBkMode (hDC, OPAQUE);
        TextOut(hDC,160,147,text,strlen(text));
		}
	}
else {strcpy(text,"LOCAL");
      TextOut(hDC,130,137,text,strlen(text));
	 sprintf(text,"PORT %i",portnr);
	 TextOut(hDC,130,147,text,strlen(text));
	 if (parent->v->GetDockStatus(parent->v->GetDockHandle(portnr)))
		{docked_port=1;
		strcpy(text,"ENG");
		SetTextColor (hDC, 0);
		SetBkColor (hDC, RGB(255,255,0));
		SetBkMode (hDC, OPAQUE);
        TextOut(hDC,160,147,text,strlen(text));
		}
	 }

oapiReleaseDC(temps,hDC);
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
HDC hDC=oapiGetDC(temps);
SelectObject(hDC, hBRUSH_TotalBlack);
Rectangle(hDC,0,0,100,25);

SetTextColor(hDC,RGB(0,255,0));
SetTextAlign(hDC,TA_CENTER);
SelectObject(hDC,hFNT_Panel);
SetBkMode(hDC,TRANSPARENT); 
char text[30];
sprintf(text, "NAV1: %5.2f MHz", 108.0+(0.05*frq1));
TextOut (hDC, 10, 2, text, strlen (text));
sprintf(text, "NAV2: %5.2f MHz", 108.0+(0.05*frq2));
TextOut (hDC, 10, 13, text, strlen (text));
oapiReleaseDC(temps,hDC);
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
void ADI::InitGL()
{

GLuint      PixelFormat;  
BITMAPINFOHEADER BIH;
int iSize=sizeof(BITMAPINFOHEADER);
BIH.biSize=iSize;
BIH.biWidth=160;				//size of the sphere is 160x160
BIH.biHeight=160;
BIH.biPlanes=1;
BIH.biBitCount=16;//default is 16.
BIH.biCompression=BI_RGB;
BIH.biSizeImage=0;
void* m_pBits;
hDC2=CreateCompatibleDC(NULL);//we make a new DC and DIbitmap for OpenGL to draw onto
static  PIXELFORMATDESCRIPTOR pfd2;
DescribePixelFormat(hDC2,1,sizeof(PIXELFORMATDESCRIPTOR),&pfd2);//just get a random pixel format.. 
BIH.biBitCount=pfd2.cColorBits;//to get the current bit depth.. !?
hBMP=CreateDIBSection(hDC2,(BITMAPINFO*)&BIH,DIB_RGB_COLORS,&m_pBits,NULL,0);
hBMP_old=(HBITMAP)SelectObject(hDC2,hBMP);

static  PIXELFORMATDESCRIPTOR pfd={                             // pfd Tells Windows How We Want Things To Be
															   
        sizeof(PIXELFORMATDESCRIPTOR),                              // Size Of This Pixel Format Descriptor
        1,                                                          // Version Number
		PFD_DRAW_TO_BITMAP |                                        // Format Must Support Bitmap Rendering
        PFD_SUPPORT_OPENGL |									
		PFD_SUPPORT_GDI,											// Format Must Support OpenGL,                                           
		0,//        PFD_TYPE_RGBA,                                              // Request An RGBA Format
        16,															// Select Our Color Depth
        0, 0, 0, 0, 0, 0,                                           // Color Bits Ignored
        0,//1,                                                          // No Alpha Buffer
        0,                                                          // Shift Bit Ignored
        0,                                                          // No Accumulation Buffer
        0, 0, 0, 0,                                                 // Accumulation Bits Ignored
        0,//16,                                                         // 16Bit Z-Buffer (Depth Buffer)  
        0,                                                          // No Stencil Buffer
        0,                                                          // No Auxiliary Buffer
        0,//PFD_MAIN_PLANE,                                             // Main Drawing Layer
        0,                                                          // Reserved
        0, 0, 0                                                     // Layer Masks Ignored
    };
pfd.cColorBits=pfd2.cColorBits;//same color depth needed.
DWORD code;
code=GetLastError();

PixelFormat=ChoosePixelFormat(hDC2,&pfd);// now pretend we want a new format
int ret;
ret=SetPixelFormat(hDC2,PixelFormat,&pfd);
code=GetLastError();
hRC=wglCreateContext(hDC2);
ret=wglMakeCurrent(hDC2,hRC);					//all standard OpenGL init so far

//We load the texture
int texture_index=LoadOGLBitmap("Textures\\adi.dds");
if (texture_index>0) glEnable(GL_TEXTURE_2D);


glShadeModel(GL_SMOOTH);                        // Enable Smooth Shading
glClearColor(0.0f, 0.0f, 0.0f, 0.0f);           // Panel Background color
glClearDepth(1.0f);                             // Depth Buffer Setup
glEnable(GL_DEPTH_TEST);                        // Enables Depth Testing
glDepthFunc(GL_LESS);                           // The Type Of Depth Testing To Do
glViewport(0,0,160,160);                       // Reset The Current Viewport
glMatrixMode(GL_PROJECTION);                        // Select The Projection Matrix
glLoadIdentity();                                   // Reset The Projection Matrix
gluPerspective(45.0f,1.0,1.0f,1000.0f);      
glMatrixMode(GL_MODELVIEW);                         // Select The Modelview Matrix          
glLoadIdentity();                                   // Reset The Projection Matrix

//some ambiental setup
GLfloat light_position[]={10.0,20.0,10.0,0.0};
GLfloat light_diffuse[]={1.0,0.2,0.2,1.0};
GLfloat light_ambient[]={0.8,0.6,0.6,1.0};
GLfloat mat_specular[]={0.3,0.3,0.3,1.0};
GLfloat mat_shin[]={5.0};
glMaterialfv(GL_FRONT,GL_SPECULAR,mat_specular);
glEnable(GL_LIGHTING);
glLightfv(GL_LIGHT0,GL_DIFFUSE,light_diffuse);
glLightfv(GL_LIGHT0,GL_POSITION,light_position);
glLightfv(GL_LIGHT0,GL_AMBIENT,light_ambient);
glLightfv(GL_LIGHT0,GL_SPECULAR,mat_specular);
glEnable(GL_LIGHT0);


//defining our geometry and composing a display list;
glEnableClientState(GL_NORMAL_ARRAY);
if (texture_index>0) glEnableClientState(GL_TEXTURE_COORD_ARRAY);
glEnableClientState(GL_VERTEX_ARRAY);
glVertexPointer(3,GL_FLOAT,0,sphere_vert);
glNormalPointer(GL_FLOAT,0,sphere_norm);
if (texture_index>0) glTexCoordPointer(2,GL_FLOAT,0,sphere_tex);
list_name=glGenLists(1);
glNewList(list_name,GL_COMPILE);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);             // Clear The Screen And The Depth Buffer        
	glColor3f(1.0,1.0,1.0);   
    glDrawElements(GL_TRIANGLE_STRIP,tri,GL_UNSIGNED_INT,sphere_index);
glEndList();

init=1;		//that's it. If we made it so far, we can use OpenGL
};

ADI::~ADI()
{
wglMakeCurrent(NULL,NULL);	//standard OpenGL release
wglDeleteContext(hRC);
hRC=NULL;
SelectObject(hDC2,hBMP_old);//remember to delete DC and bitmap memory we created
DeleteObject(hBMP);
DeleteDC(hDC2);

};
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

	glLoadIdentity(); 
	gluLookAt(0.0,-35.0,0.0, 0.0,0.0,0.0,0.0,0.0,1.0);
	glRotatef(-now.z/Pi*180.0,0.0,1.0,0.0);
	glRotatef(now.y/Pi*180.0,1.0,0.0,0.0);
	glRotatef(now.x/Pi*180.0,0.0,0.0,1.0);

}
void ADI::PaintMe()
{
	if (!init) InitGL();
/*	float temp;
	float sign;
	float Pi=acos(-1);
	vector3 me;
	vector3 up;
	VESSELSTATUS vstat;parent->v->GetStatus(vstat);
	vstat.arot.z+=acos(-1)/2;
	me.z=35*sin(vstat.arot.y);
	me.x=35*cos(vstat.arot.y);
	me.y=me.x*cos(vstat.arot.x);
	me.x=me.x*sin(vstat.arot.x);
	
	up.z=cos(vstat.arot.z);//project the up onto the me plane
//	up.z*=cos(vstat.arot.y);//then further project this onto the z axis
	//temp=sqrt(1-up.z*up.z);//extract the other projection
	temp=sin(vstat.arot.z);
	up.x=temp*cos(vstat.arot.x);//project this onto the axis
	up.y=temp*sin(vstat.arot.x);
	//problem is this type of projection will not lead to a unit vector;
    temp=sqrt(up.z*up.z+up.x*up.x+up.y*up.y);
	sprintf(oapiDebugString(),"%0.4f %0.4f %0.4f %0.4f %0.4f %0.4f %0.4f",vstat.arot.x,vstat.arot.y,vstat.arot.z,up.x,up.y,up.z,temp);
	gluLookAt(me.x,me.y,me.z, 0.0,0.0,0.0,up.x,up.y,up.z);
	*/
  
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
	glCallList(list_name);	//render
	glFlush();
	glFinish();

HDC hDC=oapiGetDC(parent->surf);

BitBlt(hDC,5,5,130,130,hDC2,15,15,SRCCOPY);//then we bitblt onto the panel. wish there
oapiReleaseDC(parent->surf,hDC);		// was a faster way ...

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
HDC hDC=oapiGetDC(parent->surf);
HDC hDC2=oapiGetDC(hRadSrfSRF);
BitBlt(hDC,0,0,350,296,hDC2,0,0,SRCCOPY);

char label[5];
SelectObject(hDC, hFNT_Panel);
SetBkMode(hDC,TRANSPARENT); 
SetTextColor(hDC,RGB(0,0,0));
SetTextAlign(hDC,TA_CENTER);

strcpy(label,"R+");TextOut(hDC,31,58,label,strlen(label));
strcpy(label,"R-");TextOut(hDC,31,92,label,strlen(label));
strcpy(label,"VS");TextOut(hDC,31,129,label,strlen(label));
strcpy(label,"AT1");TextOut(hDC,322,58,label,strlen(label));
strcpy(label,"AT2");TextOut(hDC,322,92,label,strlen(label));

oapiReleaseDC(parent->surf,hDC);
oapiReleaseDC(hRadSrfSRF,hDC2);
  

}

void Radar::RefreshMe()
{
if (*(((Dragonfly*)parent->v)->DC_power)>0) 
{powered=1;
	HDC hDC;
	int num_ob=oapiGetVesselCount();
	OBJHANDLE object_us=parent->v->GetHandle();
	OBJHANDLE object;
	OBJHANDLE obj_last;
	VECTOR3 dist;
	VECTOR3 pos;
	if (new_range){new_range=0;
		hDC=oapiGetDC(parent->surf);
		char label[10];
		sprintf(label,"R:%im",range);
		SelectObject(hDC,hBRUSH_TotalBlack);
		Rectangle(hDC,220,20,270,31);
		SelectObject(hDC, hFNT_Panel);
		SetBkMode(hDC,TRANSPARENT); 
		SetTextColor(hDC,RGB(0,255,0));
		SetTextAlign(hDC,TA_CENTER);
		TextOut(hDC,255,20,label,strlen(label));
		oapiReleaseDC(parent->surf,hDC);
	};	//draw a new range 
	if (((Dragonfly*)parent->v)->UY_pos!=last_antena_yaw) {
		hDC=oapiGetDC(radar_background);
		SelectObject(hDC,hBRUSH_TotalBlack);
		SelectObject(hDC,hPEN_NULL);

		Rectangle(hDC,0,0,101,101);
		last_antena_yaw=((Dragonfly*)parent->v)->UY_pos;
		float yaw=150.0 - ((Dragonfly*)parent->v)->UY_pos*300+15.0;
		yaw=yaw/180.0*acos(-1.0);
		float yaw1=yaw - 30.0/180.0 *acos(-1.0);
		float py1=cos(yaw)*45;
		float  px1=sin(yaw)*45;

		float py2=cos(yaw1)*45;
		float  px2=sin(yaw1)*45;

		SelectObject(hDC,hBRUSH_FYellow);
		Pie(hDC,0,0,100,100,50+px1,50-py1,50+px2,50-py2);
		oapiReleaseDC(radar_background,hDC);
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

	HDC hDC=oapiGetDC(parent->surf);
	SelectObject(hDC, hFNT_Panel);
	SetBkMode(hDC,TRANSPARENT); 
	SetTextColor(hDC,RGB(0,255,0));
	SetTextAlign(hDC,TA_CENTER);
	SelectObject(hDC,hPEN_NULL);
	SelectObject(hDC,hBRUSH_Red);

	Rectangle(hDC,22,35,44,45);
	Rectangle(hDC,22,56,44,66);


	char cbuf[10];
	int i;

	i = (int)(143.0*fuel);

	Rectangle (hDC, 23, 84, 39, 228-i);
	SelectObject(hDC,hBRUSH_Green);
	Rectangle (hDC, 23, 228-i, 39, 228);
	SelectObject(hDC,hBRUSH_Green);
    sprintf (cbuf, "%3.1f", 100.0*fuel);
	TextOut (hDC, 32, 35, cbuf, strlen(cbuf));
	sprintf (cbuf, "%3.2f",old_fuel=parent->v->GetPropellantFlowrate (((Dragonfly*)(parent->v))->ph_main));
	TextOut (hDC, 32, 56, cbuf, strlen(cbuf));
	oapiReleaseDC(parent->surf,hDC);
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
