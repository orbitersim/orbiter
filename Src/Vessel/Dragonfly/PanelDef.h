		
#include "panel.h"
#include "hsystems.h"
#include <string.h>

const int NumPanels=2;
//Panel PanelList[NumPanels];
float Main_Eng;
float Hover_Eng;
float Tk[3];
float Flow;
char char_p[10];
int trig[3];
Tank Tk1(_vector3(0,0,0),14); //14 liter tank at center
double Lsim;
/*
//--------------------PANEL 0 RESOURCES ---------------------------
BORDER_LIST BD1[2]={{100,200,3},{100,250,5}};
CTEXT_LIST CT1[2]={{167,180,150,1," DO NOTHING "},{212,300,250,-1," NEITHER THESE "}};
TEXT_LIST  TL1[4]= {{160,130,"FUEL QTY"},
					{545,260,"MN        HOV"},
{545,280,"ENG STS"},{545,500,"HIT CTRL+ ->"}};
						
RotElement *RotEl;
int SelTk;
char names[7][12]={"TK1","TK2","TK3"};
//----------------- PANEL 1 RESOURCES ----------------------------
TEXT_LIST TL2[32]=	{{267,15,"HOURS"},{307,15,"MIN"},{352,15,"SEC"},
					{307,25,"--TENS--"},{307,85,"--UNITS--"},{172,120,"NORMAL"},
					{217,120,"CSM"},{262,120,"POWER"},{307,120,"LAMP TEST"},
					{120,150,"BOOST"},{172,190,"ACM"},{217,190,"CM"},
					{352,190,"RESET"},{352,120,"START"},{172,210,"1"},
					{227,210,"2"},{305,210,"1    AUTO    2"},{405,210,"1    AUTO    2"},
					{472,200,"O2 PRESS"},{472,210,"TANK 1"},{560,210,"1    AUTO    2"},
					{690,210,"1    AUTO    2"},{205,280,"OFF"},{350,280,"- ON -"},
					{472,280,"SURGE TANK"},{625,280,"- ON -"},{505,30,"PRESSURE"},
					{680,30,"QUANTITY"},{447,40,"1    H2    2"},{447+85,40,"1    O2    2"},
{447+170,40,"1    H2    2"},{447+255,40,"1    O2    2"}};
BORDER_LIST BD2[3]={{150,140,5},{240,40,3},	{150,230,13}};
CTEXT_LIST CT2[8]={{200,10,350,1," MISSION TIMER "},
					{240,110,200,1," CAUTION/WARNING "},
					{205,200,110,1,"CABIN FAN"},
					{307,200,80,1,"H2 HEATERS"},
					{405,200,90,1,"O2 HEATERS"},
					{560,200,110,1,"H2 FANS"},
					{690,200,110,1,"O2 FANS"},
					{575,20,340,1," CYROGENIC TANKS "}};
char names2[7][12]={"1","2","A","B","C","D"};
//-------------------- INIT PANEL ----------------------------------
void InitPanel(int i)
{ 
  //Switch* sw1;
  //SFSwitch* sfw1;
	switch (i)
{
   case 0:
	   PanelList[i].Wdth=800;PanelList[i].Hght=400;PanelList[i].idx=i;
	   PanelList[i].Text_list=TL1;PanelList[i].text_num=4  ;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   PanelList[i].CText_list=CT1;PanelList[i].ctext_num=2;
	   PanelList[i].Border_list=BD1;PanelList[i].border_num=2;
	   PanelList[i].neighbours[0]=-1;PanelList[i].neighbours[1]=1; //left / right
	   PanelList[i].neighbours[2]=-1;PanelList[i].neighbours[3]=-1; //up / down
	   /*new Switch(145,200,-1,2,&PanelList[i]);
//	   sfw1=new SFSwitch(100,200,-1,2,1,&PanelList[i]);						//create will register it automatically
	   new Switch(145,200,-1,2,&PanelList[i]);
	   new Switch(190,200,-1,2,&PanelList[i]);

	   new Switch(100,250,1,2,&PanelList[i]);
	   new Switch(145,250,1,3,&PanelList[i]);
	   new Switch(190,250,-1,3,&PanelList[i]);
	   new Switch(235,250,-1,2,&PanelList[i]);
	   new Switch(280,250,-1,2,&PanelList[i]);

	   
	   new HGauge(500,10,90,250,&Main_Eng,&Hover_Eng,"Kg",0,100,1,5,4,&PanelList[i]);

	   RotEl=new RotElement(&(new EGauge(110,10,&Tk[0],"%",0,100,1,&PanelList[i]))->SRC,&(new Rotary(300,1,"FUEL SELECTOR",names,0,3,&PanelList[i]))->set,&PanelList[i]);
       RotEl->TRG[0]=&Tk[0];RotEl->TRG[1]=&Tk[1];RotEl->TRG[2]=&Tk[2];
	   SelTk=0;
	   Tk[0]=100;
	   Tk[1]=70;
	   Tk[2]=40;
	   break;
   case 1:
	   Tk1.FillTank(14,730000,170,2,2500,6000,150);
	   Tk1.open=1;Lsim=0;
	   PanelList[i].Wdth=800;PanelList[i].Hght=400;PanelList[i].idx=i;
	   PanelList[i].Text_list=TL2;PanelList[i].text_num=32;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   PanelList[i].CText_list=CT2;PanelList[i].ctext_num=8;
	   PanelList[i].Border_list=BD2;PanelList[i].border_num=3;
	   PanelList[i].neighbours[0]=0;PanelList[i].neighbours[1]=-1; //left / right
	   PanelList[i].neighbours[2]=-1;PanelList[i].neighbours[3]=-1; //up / down
	   trig[0]=0;
	   new Switch(240,40,0,3,1,&trig[0],&PanelList[i]);
	   new Switch(285,40,0,3,1,&trig[1],&PanelList[i]);
	   new Switch(330,40,0,3,1,&trig[2],&PanelList[i]);

	   new Switch(150,140,1,2,0,&Tk1.open_handle,&PanelList[i]);
	   new TB(100,140,&Tk1.pz,&PanelList[i]);
	  /* new Switch(195,140,-1,2,&PanelList[i]);
	   new Switch(240,140,0,3,&PanelList[i]);
	   new Switch(285,140,0,3,&PanelList[i]);
	   new Switch(330,140,0,3,&PanelList[i]);

	   new Switch(150,230,-1,2,&PanelList[i]);
	   new Switch(205,230,-1,2,&PanelList[i]);
	   new Switch(260,230,0,3,&PanelList[i]);
	   new Switch(305,230,0,3,&PanelList[i]);
	   new Switch(350,230,0,3,&PanelList[i]);
	   new Switch(405,230,0,3,&PanelList[i]);
	   new Switch(450,230,-1,2,&PanelList[i]);
	   new Switch(505,230,0,3,&PanelList[i]);
	   new Switch(570,230,0,3,&PanelList[i]);
	   new Switch(635,230,0,3,&PanelList[i]);
	   new Switch(700,230,0,3,&PanelList[i]);
	 //  new Rotary(0,150,"RCS INDICATORS",names2,3,6,&PanelList[i]);
	   strcpy(char_p,"00:00:00");
	   new DigClock(20,20,char_p,8,&PanelList[i]);
     new HGauge(405,50,85,140,&Flow,&Tk1.Press,"Prs",0,10000,1,2,4,&PanelList[i]);
	 new HGauge(490,50,85,140,&Tk1.Temp,&Hover_Eng,"Tmp",0,300,1,2,4,&PanelList[i]);
	 new HGauge(575,50,85,140,&Tk1.mass,&Hover_Eng,"Kg",0,1000,1000,2,4,&PanelList[i]);
	 new HGauge(660,50,85,140,&Flow,&Hover_Eng,"Flow",0,100,1,2,4,&PanelList[i]);



	 break;
}
};

void InitAllPanels(HINSTANCE hModule)
{ for (int i=0;i<NumPanels;i++)
	{PanelList[i].hModule=hModule;
	 InitPanel(i);
	};	
}
*/