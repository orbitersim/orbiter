#include "internal.h"
#include <stdio.h>
#include "orbitersdk.h"
#include "dragonfly.h"
ShipInternal::ShipInternal()
{Dk[0]=NULL;
};
ShipInternal::~ShipInternal()
{
};

void ShipInternal::MakePanels(VESSEL *vessel)
{

//Panel F1
int i=0;
static CTEXT_LIST CTF[2]={//{630,520,120,1,"RADAR DIST"},{830,520,120,1,"RADAR CLSR"},
{725,580,210,1,"DOPPLER CM/SEC"},
{108,560,150,1,"RCS MODE"}};//2

static TEXT_LIST TLF[15]={{65,570,"LIN"},{65,630,"DISB"},{30,600,"ROT"},//3
{132,570,"-NORM-"},{110,630,"VERN"},{190,600,"PL"},//6
{870,600,"RAD SGN"},{155,630,"RATE"},{222,570,"KILROT"},//9
{735,320,"REF"},{785,350,"GDC"},{735,380,"HOR"},//12
{875,320,"MARK"},{825,320,"ECL"},{825,380,"ORB"}//15

};

       PanelList[i].Wdth=1024;PanelList[i].Hght=650;PanelList[i].idx=i;
	   PanelList[i].ATT_mode=0x0011; //bottom
	   PanelList[i].v=vessel;
	   PanelList[i].Text_list=TLF;
	   PanelList[i].text_num=15;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   PanelList[i].CText_list=CTF;
	   PanelList[i].ctext_num=2;
	   //PanelList[i].Border_list=BD2;
	   PanelList[i].border_num=0;
	 	  
	   PanelList[i].neighbours[0]=1;PanelList[i].neighbours[1]=4; //left / right
	   PanelList[i].neighbours[2]=3;PanelList[i].neighbours[3]=-1; //up / down
	   MFDS[0]=new inst_MFD(10,220,0,&PanelList[i]);
	   Dk[0]=new Docker(360,320,&PanelList[i]);
	   new NAVFRQ(360,258,&PanelList[i]); 
	   //MFDS[1]=new inst_MFD(600,321,1,&PanelList[i]);
	   new FuelMeter(939,325,&PanelList[i]);
		ADI* n_adi;
	   n_adi=new ADI(570,240,&PanelList[i]);
	    new Switch(715,330,0,3,0,&(n_adi->function_mode),&PanelList[i]);
	    new Switch(805,330,-1,2,0,&(n_adi->orbital_ecliptic),&PanelList[i]);
		new Switch(855,330,0,3,1,&(n_adi->ref_handle),&PanelList[i]);
		new TB(715,240,&(n_adi->over_rate),&PanelList[i]);
	   new Radar(570,391,&PanelList[i]);     
	   new VROT(260,530,&PanelList[i]);
//	   new DigClock(570,530,((Dragonfly*)(vessel))->Dock_dist,5,&PanelList[i]);  
//	   new DigClock(770,530,((Dragonfly*)(vessel))->Dock_vel,5,&PanelList[i]);  

	   new DigClock(620,590,((Dragonfly*)(vessel))->Dock_x_vel,3,&PanelList[i]);  
	   new DigClock(690,590,((Dragonfly*)(vessel))->Dock_y_vel,3,&PanelList[i]);  
	   new DigClock(760,590,((Dragonfly*)(vessel))->Dock_z_vel,3,&PanelList[i]);  
		new TB(860,610,&((Dragonfly*)(vessel))->signal_flag,&PanelList[i]);
	   Nav_mode_switch=new Switch(40,580,0,3,0,&((Dragonfly*)(vessel))->NAV_handle,&PanelList[i]);
	   Vern_mode_switch=new Switch(85,580,1,2,0,&((Dragonfly*)(vessel))->VERN_handle,&PanelList[i]);
	   Intr_mode_switch=new Switch(130,580,1,3,0,&((Dragonfly*)(vessel))->INTR_handle,&PanelList[i]);
	   new CB(200,580,1,&((Dragonfly*)(vessel))->Kill_rot,&PanelList[i]);
	   cw[9]=new CW(800,270,"ELECTR",&PanelList[i]);   
	    cw[9]->Watch(new bounds(&BT[0]->power,5200000,1000000));
	    cw[9]->Watch(new bounds(&DC[0]->Volts,30,27));
		cw[9]->Watch(new bounds(&DC[1]->Volts,30,27));
		cw[9]->Watch(new bounds(&AC[0]->Volts,38,35));
		cw[9]->Watch(new bounds(&AC[0]->Amperes,100,-1));
		 cw[9]->Watch(new bounds(&Tanks[3]->Press,1e11,200));
		cw[9]->Watch(new bounds(&Tanks[4]->Press,1e11,200));
		cw[9]->Watch(new bounds(&Tanks[5]->Press,1e11,200));
		cw[9]->Watch(new bounds(&Tanks[0]->Press,1e11,200));
		cw[9]->Watch(new bounds(&Tanks[1]->Press,1e11,200));
		cw[9]->Watch(new bounds(&Tanks[2]->Press,1e11,200));
		cw[9]->Watch(new bounds(&FC[0]->Temp,350,270));
		cw[9]->Watch(new bounds(&FC[1]->Temp,350,270));
		cw[9]->Watch(new bounds(&FC[0]->Amperes,200,20));
		cw[9]->Watch(new bounds(((float*)(&FC[1]->H2_flow)),0.35,0));
		cw[9]->Watch(new bounds(((float*)(&FC[1]->O2_flow)),1.75,0));
		cw[9]->Watch(new bounds(((float*)(&FC[0]->H2_flow)),0.35,0));
		cw[9]->Watch(new bounds(((float*)(&FC[0]->O2_flow)),1.75,0));
	    cw[10]=new CW(850,270,"ECLSS",&PanelList[i]);   
        cw[10]->Watch(new bounds(((float*)(&Man[3]->OV[0].mass)),1e11,0.055));
		//new Bitmap(600,230,110,80,4,&PanelList[i]);
		new Bitmap(0,0,1024,258,1,&PanelList[i]);	   
	   
	   
//Panel L1
i=1;
static CTEXT_LIST CT0[17]={{85,30,150,1,"CRYO 02 MAN"},{60,310,100,-1,"FC1       FC2"},//2
{240,30,100,1,"FC1"},{240,110,100,1,"FC2"},//4
{240,190,100,1,"BATTERY"},{395,30,150,1,"CRYO H2 MAN"}, //6
{370,310,100,-1,"FC1       FC2"},{85,330,150,1,"TK HEATERS"}, //8
{590,240,150,1,"POWER ROUTING"},{85,490,150,-1,"TK1    TK2    TK3"},//10
{590,320,150,-1,"DC1     DC2     AC1"},{822,350,100,1,"H20 WST TK"}, //12
{1005,90,250,1,"MN CB"},{65,520,110,1,"CYRO ISOL V"}, //14
{80,590,140,1,"OVB DUMP"},{252,590,180,1,"OVB VNT"}, //16
{445,580,150,1,"BUS SNS TRP"} //17
} ;
static TEXT_LIST TL0[67]={{35,60,"OPEN"},{35,120,"CLOSE"},{35,140,"OPEN"},//3
{35,200,"CLOSE"},{85,130,"--XFEED--"},{85,210,"REAC VLV"},//6
{35,220,"OPEN"},{35,280,"CLOSE"},{240, 40,"START   PURGE"},//9
{240,100,"STOP    NORM "},{240,120,"START   PURGE"},{240,180,"STOP    NORM "},//12
{240,200,"FC2      LOAD"},{240,260,"FC1      NORM"},{345,60,"OPEN"}, //15
{345,120,"CLOSE"},{345,140,"OPEN"},{345,200,"CLOSE"},//18
{395,130,"--XFEED--"},{395,210,"REAC VLV"},{345,220,"OPEN"},//21
{345,280,"CLOSE"},{85,340,"-CYRO 02-"},{85,400,"OFF"},//24
{85,410,"-CYRO H2-"},{85,420,"ON"},{85,480,"OFF"},//27
{220,550,"O2    H2"},{310,550,"O2    H2"},{400,550,"O2    H2"},//30
{220,350,"PRESS"},{310,350,"TEMP"},{400,350,"QTY"},	//33
{565,250,"-FC2-"},{635,250,"DC2"},{565,310,"-FC1-"},//36
{635,310,"DC1"},{670,285,"BAT"},//38
{645,350,"FUEL PH"},{735,350,"FC STACK TEMP"},//40
{645,550,"FC1   FC2"},{735,550,"FC1   FC2"},//42
{825,40,"BAT LOAD"}, //43
{905,150,"BT LD"},{955,150,"BT 1"}, //45
{1005,150,"MN 1"},{1055,150,"MN 2"}, //47
{1105,150,"AC 1"} ,{25,530,"O2"},//49
{115,530,"H2"},{25,600,"O2"},//51
{115,600,"H2"},{175,600,"O2"},//53
{220,600,"H2"},{295,600,"-H20-"},//55
{395,590,"DC1"},{445,590,"DC2"},//57
{495,590,"AC1"},{445,600,"-RESET/TEST-"},//59
{445,660,"-TRIP-"},{445,670,"AUTO TRIP"},//61
{445,680,"-ON-"},{205,670,"-CLOSE-"},//63
{545,590,"HTRS"},{835,250,"HTRS"},{880,250,"FAN"},//66
{595,600,"FAN"}//67
};

  static char names[7][12]={"FC1","FC2","BAT","DC1","DC2","AC1"};
  static char cy_names[7][12]={"TK1","TK2","TK3"};
EGauge *eg1,*eg2;
HGauge *hg1,*hg2,*hg3;
Rotary *rot1;
Switch *sw1;

	   PanelList[i].Wdth=1300;PanelList[i].Hght=768;PanelList[i].idx=i;
	   PanelList[i].ATT_mode=0x0011; //left
	   PanelList[i].v=vessel;
	   PanelList[i].Text_list=TL0;PanelList[i].text_num=67;
	   PanelList[i].CText_list=CT0;PanelList[i].ctext_num=17;;
	   PanelList[i].border_num=0;
	   PanelList[i].neighbours[0]=-1;PanelList[i].neighbours[1]=0; //left / right
	   PanelList[i].neighbours[2]=3;PanelList[i].neighbours[3]=-1; //up / down

	   //02 cyro / manifold switches
	   new TB(25,40,&Tanks[0]->pz,&PanelList[i]);
	   new TB(75,40,&Tanks[1]->pz,&PanelList[i]);
	   new TB(125,40,&Tanks[2]->pz,&PanelList[i]);
	   new Switch( 10,70,1,2,0,&Tanks[0]->open_handle ,&PanelList[i]);
	   new Switch( 60,70,1,2,0,&Tanks[1]->open_handle ,&PanelList[i]);
	   new Switch(110,70,-1,2,0,&Tanks[2]->open_handle ,&PanelList[i]);
	   Tanks[2]->open=0;
       new Switch( 10,150,1,2,0,&Man[0]->X[0].open_handle ,&PanelList[i]);
	   new Switch( 60,150,1,2,0,&Man[0]->X[1].open_handle ,&PanelList[i]);
	   //new Switch(110,150,-1,2,0,&Man[0]->X[2].open_handle ,&PanelList[i]);
       Man[0]->X[2].open=1;
	   new Switch( 10,230,1,2,0,&Man[0]->OV[0].open_handle ,&PanelList[i]);
	   new Switch( 60,230,1,2,0,&Man[0]->OV[1].open_handle ,&PanelList[i]);
	   new TB(25,290,&(Man[0]->OV[0].pz),&PanelList[i]);
	   new TB(75,290,&(Man[0]->OV[1].pz),&PanelList[i]);
	   Man[0]->OV[2].open=0;

	   //fuel cell / battery management
	   new TB(165,65,&FC[0]->running,&PanelList[i]);
	   new TB(295,65,&FC[0]->running,&PanelList[i]);
	   new TB(165,145,&FC[1]->running,&PanelList[i]);
	   new TB(295,145,&FC[1]->running,&PanelList[i]);
	   new TB(295,225,&BT[0]->loading,&PanelList[i]);

	   new Switch( 190,50,0,3,1,&FC[0]->start_handle ,&PanelList[i]);
	   new Switch( 240,50,-1,2,0,&FC[0]->purge_handle ,&PanelList[i]);	   
	   new Switch( 190,130,0,3,1,&FC[1]->start_handle ,&PanelList[i]);
	   new Switch( 240,130,-1,2,0,&FC[1]->purge_handle ,&PanelList[i]);
	   new Switch(190,210,-1,2,0,&Sock[0]->socket_handle,&PanelList[i]);
	   new Switch(235,210,-1,2,0,&BT[0]->load_handle,&PanelList[i]);

		//Cyro H2 manifold
	   new TB(335,40,&Tanks[3]->pz,&PanelList[i]);
	   new TB(385,40,&Tanks[4]->pz,&PanelList[i]);
	   new TB(435,40,&Tanks[5]->pz,&PanelList[i]);
	   new Switch(320,70,1,2,0,&Tanks[3]->open_handle ,&PanelList[i]);
	   new Switch(370,70,1,2,0,&Tanks[4]->open_handle ,&PanelList[i]);
	   new Switch(420,70,-1,2,0,&Tanks[5]->open_handle ,&PanelList[i]);
	   Tanks[5]->open=0;
       new Switch(320,150,1,2,0,&Man[1]->X[0].open_handle ,&PanelList[i]);
	   new Switch(370,150,1,2,0,&Man[1]->X[1].open_handle ,&PanelList[i]);
	 //  new Switch(420,150,-1,2,0,&Man[1]->X[2].open_handle ,&PanelList[i]);
       Man[1]->X[2].open=1;
	   new Switch(320,230,1,2,0,&Man[1]->OV[0].open_handle ,&PanelList[i]);
	   new Switch(370,230,1,2,0,&Man[1]->OV[1].open_handle ,&PanelList[i]);
	   new TB(335,290,&Man[1]->OV[0].pz,&PanelList[i]);
	   new TB(385,290,&Man[1]->OV[1].pz,&PanelList[i]);
	   Man[1]->OV[2].open=0;

	   //cyro heaters
	   new Switch(10,350,0,3,0,&HT[0]->start_handle,&PanelList[i]);
	   new Switch(55,350,0,3,0,&HT[1]->start_handle,&PanelList[i]);
	   new Switch(100,350,0,3,0,&HT[2]->start_handle,&PanelList[i]);
       
	   new Switch(10,430,0,3,0,&HT[3]->start_handle,&PanelList[i]);
	   new Switch(55,430,0,3,0,&HT[4]->start_handle,&PanelList[i]);
	   new Switch(100,430,0,3,0,&HT[5]->start_handle,&PanelList[i]);
		//ECLLS piping and overboard dump
	   new Switch(10,540,-1,2,0,&Man[0]->OV[2].open_handle,&PanelList[i]);
       new Switch(100,540,-1,2,0,&Man[1]->OV[2].open_handle,&PanelList[i]);
       

		//high-volume overbd dump
	   new Switch(10,610,0,3,1,&Valves[2]->open_handle,&PanelList[i]);
       new Switch(100,610,0,3,1,&Valves[3]->open_handle,&PanelList[i]);
	   //2 safe pressure vent
	   new Switch(160,610,1,2,0,&Valves[5]->open_handle,&PanelList[i]);
	   new Switch(205,610,1,2,0,&Valves[6]->open_handle,&PanelList[i]);
       
		//TB indicators for overb dump       
	   new Slider(18,680,&Valves[2]->open,&PanelList[i]);
	   new Slider(108,680,&Valves[3]->open,&PanelList[i]);
	   new TB(55,680,&Valves[2]->pz,&PanelList[i]);
	   new TB(145,680,&Valves[3]->pz,&PanelList[i]);
	   new TB(185,680,&Valves[5]->pz,&PanelList[i]);
	   new TB(220,680,&Valves[6]->pz,&PanelList[i]);
	   //h20 waste vent
	   new Switch(250,610,1,2,0,&Valves[0]->open_handle,&PanelList[i]);
	   new Switch(295,610,1,2,0,&Valves[1]->open_handle,&PanelList[i]);
	   new TB(275,670,&Valves[0]->pz,&PanelList[i]);
	   new TB(310,670,&Valves[1]->pz,&PanelList[i]);
		//Bus SNS management
       new Switch(370,610,0,3,1,&DC[0]->reset_handle ,&PanelList[i]);
	   new Switch(420,610,0,3,1,&DC[1]->reset_handle ,&PanelList[i]);
       new Switch(470,610,0,3,1,&AC[0]->reset_handle, &PanelList[i]);
	   new Switch(520,610,0,3,1,&DC[2]->reset_handle, &PanelList[i]);
	    new Switch(570,610,0,3,1,&DC[3]->reset_handle, &PanelList[i]);
       
	   new Switch(370,690,1,2,0,&DC[0]->atrip_handle, &PanelList[i]);
	   new Switch(420,690,1,2,0,&DC[1]->atrip_handle, &PanelList[i]);
       new Switch(470,690,1,2,0,&AC[0]->atrip_handle, &PanelList[i]);
       new Switch(520,690,1,2,0,&DC[2]->atrip_handle, &PanelList[i]);
		new Switch(570,690,1,2,0,&DC[3]->atrip_handle, &PanelList[i]);
	   hg1=new HGauge(180,360,85,190,&Tanks[0]->Press,&Tanks[3]->Press,"kPa",0,2500,1,2,4,&PanelList[i]);
	   hg2=new HGauge(270,360,85,190,&Tanks[0]->Temp,&Tanks[3]->Temp,"K",0,300,1,2,4,&PanelList[i]);
	   hg3=new HGauge(360,360,85,190,&Tanks[0]->mass,&Tanks[3]->mass,"Kg",0,150,1000,2,4,&PanelList[i]);
       
	   new HGauge(600,360,85,190,&FC[0]->clogg,&FC[1]->clogg,"dPh",0,2,1,2,4,&PanelList[i]);
  	   new HGauge(690,360,85,190,&FC[0]->Temp,&FC[1]->Temp,"K",0,400,1,2,4,&PanelList[i]);
       
	   new HGauge(780,360,85,190,&Tanks[6]->Press,&Tanks[6]->Press,"kPa",0,900,1,2,4,&PanelList[i]);
	   
	  

	   new CB(880,100,1,&BT[0]->load_cb,&PanelList[i]);
	   new CB(930,100,1,&BT[0]->c_breaker,&PanelList[i]);
	   new CB(980,100,1,&DC[0]->c_breaker,&PanelList[i]);
	   new CB(1030,100,1,&DC[1]->c_breaker,&PanelList[i]);
	   new CB(1080,100,1,&AC[0]->c_breaker,&PanelList[i]);
	   
	   new CB(880,180,1,&DC[2]->c_breaker,&PanelList[i]);
	   new CB(930,180,1,&DC[3]->c_breaker,&PanelList[i]);
	/*   new CB(980,180,1,&DC[0]->c_breaker,&PanelList[i]);
	   new CB(1030,180,1,&DC[1]->c_breaker,&PanelList[i]);
	   new CB(1080,180,1,&AC[0]->c_breaker,&PanelList[i]);

	   
	   new CB(880,260,1,&BT[0]->load_cb,&PanelList[i]);
	   new CB(930,260,1,&BT[0]->c_breaker,&PanelList[i]);
	   new CB(980,260,1,&DC[0]->c_breaker,&PanelList[i]);
	   new CB(1030,260,1,&DC[1]->c_breaker,&PanelList[i]);
	   new CB(1080,260,1,&AC[0]->c_breaker,&PanelList[i]);
	

	   new CB(880,320,1,&BT[0]->load_cb,&PanelList[i]);
	   new CB(930,320,1,&BT[0]->c_breaker,&PanelList[i]);
	   new CB(980,320,1,&DC[0]->c_breaker,&PanelList[i]);
	   new CB(1030,320,1,&DC[1]->c_breaker,&PanelList[i]);
	   new CB(1080,320,1,&AC[0]->c_breaker,&PanelList[i]);
*/
		cw[0]=new CW(880,500,"BAT",&PanelList[i]);   
	    cw[0]->Watch(new bounds(&BT[0]->power,5200000,1000000));
		cw[1]=new CW(930,500,"DC V",&PanelList[i]);   
	    cw[1]->Watch(new bounds(&DC[0]->Volts,30,27));
		cw[1]->Watch(new bounds(&DC[1]->Volts,30,27));
		cw[2]=new CW(980,500,"AC V",&PanelList[i]);   
	    cw[2]->Watch(new bounds(&AC[0]->Volts,38,35));
        cw[3]=new CW(1030,500,"AC OVR",&PanelList[i]);   
	    cw[3]->Watch(new bounds(&AC[0]->Amperes,100,-1));
		cw[4]=new CW(880,535,"H2 PRS",&PanelList[i]); 
        cw[4]->Watch(new bounds(&Tanks[3]->Press,1e11,200));
		cw[4]->Watch(new bounds(&Tanks[4]->Press,1e11,200));
		cw[4]->Watch(new bounds(&Tanks[5]->Press,1e11,200));
		cw[5]=new CW(930,535,"O2 PRS",&PanelList[i]); 
        cw[5]->Watch(new bounds(&Tanks[0]->Press,1e11,200));
		cw[5]->Watch(new bounds(&Tanks[1]->Press,1e11,200));
		cw[5]->Watch(new bounds(&Tanks[2]->Press,1e11,200));
		cw[6]=new CW(980,535,"FC TMP",&PanelList[i]); 
        cw[6]->Watch(new bounds(&FC[0]->Temp,350,270));
		cw[6]->Watch(new bounds(&FC[1]->Temp,350,270));
		cw[7]=new CW(1030,535,"FC1 LD",&PanelList[i]); 
        cw[7]->Watch(new bounds(&FC[0]->Amperes,200,20));
	//	cw=new CW(1080,535,"FC2 L",&PanelList[i]); 
    //    cw->Watch(new bounds(&FC[1]->Amperes,1000,20));
		cw[8]=new CW(1080,535,"FC FLW",&PanelList[i]); 
        cw[8]->Watch(new bounds(((float*)(&FC[1]->H2_flow)),0.35,0));
		cw[8]->Watch(new bounds(((float*)(&FC[1]->O2_flow)),1.75,0));
		cw[8]->Watch(new bounds(((float*)(&FC[0]->H2_flow)),0.35,0));
		cw[8]->Watch(new bounds(((float*)(&FC[0]->O2_flow)),1.75,0));
		
    
		

		rot1=new Rotary(450,380,"CYRO SEL",cy_names,0,3,&PanelList[i]);
	   RotElement *RotEl;
	   RotEl=new RotElement(&hg1->SRC1,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[0]->Press;RotEl->TRG[1]=&Tanks[1]->Press;
	   RotEl->TRG[2]=&Tanks[2]->Press;
	   RotEl=new RotElement(&hg1->SRC2,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[3]->Press;RotEl->TRG[1]=&Tanks[4]->Press;
	   RotEl->TRG[2]=&Tanks[5]->Press;
	   RotEl=new RotElement(&hg2->SRC1,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[0]->Temp;RotEl->TRG[1]=&Tanks[1]->Temp;
	   RotEl->TRG[2]=&Tanks[2]->Temp;
	   RotEl=new RotElement(&hg2->SRC2,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[3]->Temp;RotEl->TRG[1]=&Tanks[4]->Temp;
	   RotEl->TRG[2]=&Tanks[5]->Temp;
	   RotEl=new RotElement(&hg3->SRC1,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[0]->mass;RotEl->TRG[1]=&Tanks[1]->mass;
	   RotEl->TRG[2]=&Tanks[2]->mass;
	   RotEl=new RotElement(&hg3->SRC2,&rot1->set,&PanelList[i]);
	   RotEl->TRG[0]=&Tanks[3]->mass;RotEl->TRG[1]=&Tanks[4]->mass;
	   RotEl->TRG[2]=&Tanks[5]->mass;

	   eg1=new EGauge(500,30,&FC[0]->Amperes ,"Amps",0,300,1,&PanelList[i]);
	   eg2=new EGauge(500,130,&FC[0]->Volts ,"V",0,40,1,&PanelList[i]);
       new HGauge(780,50,85,190,&BT[0]->power,&BT[0]->power,"%",0,100,BT[0]->max_power/100,2,4,&PanelList[i]);
      
	   rot1=new Rotary(610,50,"BUS SEL",names,0,6,&PanelList[i]);
	   RotEl=new RotElement(&eg1->SRC,&rot1->set,&PanelList[i]);
       RotEl->TRG[0]=&FC[0]->Amperes;RotEl->TRG[1]=&FC[1]->Amperes;
	   RotEl->TRG[2]=&BT[0]->Amperes;RotEl->TRG[3]=&DC[0]->Amperes;
	   RotEl->TRG[4]=&DC[1]->Amperes;RotEl->TRG[5]=&AC[0]->Amperes;

       RotEl=new RotElement(&eg2->SRC,&rot1->set,&PanelList[i]);
       RotEl->TRG[0]=&FC[0]->Volts;RotEl->TRG[1]=&FC[1]->Volts;
	   RotEl->TRG[2]=&BT[0]->Volts;RotEl->TRG[3]=&DC[0]->Volts;
	   RotEl->TRG[4]=&DC[1]->Volts;RotEl->TRG[5]=&AC[0]->Volts;

		//finnally the power routing 
	   new Switch(520,260,-1,3,0,&Sock[1]->socket_handle ,&PanelList[i]);
	   new Switch(565,260,-1,3,0,&Sock[2]->socket_handle ,&PanelList[i]);
	   new Switch(610,260,-1,3,0,&Sock[3]->socket_handle ,&PanelList[i]);

	   new Switch(810,260,-1,3,0,&Sock[4]->socket_handle ,&PanelList[i]);
	   new Switch(855,260,-1,3,0,&Sock[5]->socket_handle ,&PanelList[i]);

       hg1=new HGauge(780,570,85,190,&FC[0]->O2_flow,&FC[3]->H2_flow,"gr/s",0,15,1,2,4,&PanelList[i]);
	   rot1=new Rotary(870,570,"FC SEL",names,0,2,&PanelList[i]);
	   
	   RotEl=new RotElement(&hg1->SRC1,&rot1->set,&PanelList[i]);	   
	   RotEl->TRG[0]=&FC[0]->O2_flow;RotEl->TRG[1]=&FC[1]->O2_flow;
	   RotEl=new RotElement(&hg1->SRC2,&rot1->set,&PanelList[i]);	   
	   RotEl->TRG[0]=&FC[0]->H2_flow;RotEl->TRG[1]=&FC[1]->H2_flow;
	   new Bitmap(1127,0,173,768,3,&PanelList[i]);
//Panel L1	  
i=2;
 PanelList[i].Wdth=1024;PanelList[i].Hght=500;PanelList[i].idx=i;
 PanelList[i].v=vessel;	
 //PanelList[i].Text_list=TL0;
	   PanelList[i].text_num=0;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   //PanelList[i].CText_list=CT0;
	   PanelList[i].ctext_num=0;
	   //PanelList[i].Border_list=BD2;
	   PanelList[i].border_num=0;
	   PanelList[i].neighbours[0]=0;PanelList[i].neighbours[1]=-1; //left / right
	   PanelList[i].neighbours[2]=3;PanelList[i].neighbours[3]=4; //up / down

	   //Panel O1
i=3;
static CTEXT_LIST CT3[7]={{232,50,445,1,"ECLSS PRESS"},{115,270,210,1,"N2 PRESS"},
{55,470,100,1,"N2 OVB DMP"},{415,270,330,1,"02 PRESS"},{297,380,100,1,"CAB FAN"},
{910,270,190,1,"TIMER"}
};//6
static TEXT_LIST TL3[39]={{52,60,"N2"},{142,60,"O2 PP"},{232,60,"N2 PP"},//3
{322,60,"CO2 PP"},{35,280,"N2 TK1"},{35,380,"N2 TK2"} ,//6
{80,280,"ISOL VLV"},{35,340,"CLOSE"},{35,440,"CLOSE"},//9
{145,380,"XFEED"},{145,280,"N2 SPLY"},{55,480,"-OPEN-"},//12
{55,540,"-CLOSE-"},{80,380,"ISOL VLV"},{275,280,"O2 R1"},//15
{325,280,"BLR"},{375,280,"O2 R2"},{455,280,"02 SPLY"},//18
{505,280,"LiOH"},{555,280,"HUM SP"},{275,390,"1"},//21
{320,390,"2"},{295,450,"-OFF-"},{412,60,"TOTAL P"},//24
{512,60,"N2 FLW O2"},{702,60,"AMB TEMP"},{792,60,"AMB DELTA P"},//27
{882,60,"FAN DELTA P"},{800,350,"START"},{845,350,"UP"},//30
{890,350,"HOURS"},{935,350,"MINS"},{980,350,"SECS"},//33
{740,370,"STOP"},{800,410,"TEST"},{845,410,"DOWN"},//36
{775,280,"GMT"},{775,340,"TIMER"},//38
{612,60,"R1  TEMP  R2"}
};

       PanelList[i].Wdth=1024;PanelList[i].Hght=768;PanelList[i].idx=i;
       PanelList[i].ATT_mode=0x0022; //top
       PanelList[i].v=vessel;	   
	   PanelList[i].Text_list=TL3;
	   PanelList[i].text_num=39;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   PanelList[i].CText_list=CT3;
	   PanelList[i].ctext_num=7;
	   //PanelList[i].Border_list=BD2;
	   PanelList[i].border_num=0;
       
	   
	   PanelList[i].neighbours[0]=1;PanelList[i].neighbours[1]=4; //left / right
	   PanelList[i].neighbours[2]=-1;PanelList[i].neighbours[3]=0; //up / down

	   new HGauge(10,70,85,190,&Tanks[7]->Press,&Tanks[8]->Press,"N2",0,900,1,2,4,&PanelList[i]);
	   new HGauge(100,70,85,190,&Tanks[10]->Press,&Tanks[13]->Press,"kPa",0,40,1,2,4,&PanelList[i]);
	   new HGauge(190,70,85,190,&Tanks[11]->Press,&Tanks[14]->Press,"kPa",0,100,1,2,4,&PanelList[i]);
	   new HGauge(280,70,85,190,&Tanks[12]->Press,&Tanks[15]->Press,"kPa",0,20,1,2,4,&PanelList[i]); 
	   new HGauge(370,70,85,190,&Cabin_press,&Dock_press,"kPa",0,200,1,2,4,&PanelList[i]);   
 	   new HGauge(470,70,85,190,&Man[2]->OV[0].mass ,&Man[3]->OV[0].mass,"gr/h",0,1500,0.0002777,2,4,&PanelList[i]);   
	   
	   new HGauge(570,70,85,190,&Valves[13]->Temp,&Valves[23]->Temp,"K",0,300,1,2,4,&PanelList[i]);
	   new HGauge(660,70,85,190,&Cabin_temp,&Dock_temp,"C",-10,50,1,6,4,&PanelList[i]);
       new HGauge(750,70,85,190,&Cabin_dp,&Dock_dp,"kPa/s",-1,1,1,2,4,&PanelList[i]);	   
	   new HGauge(840,70,85,190,&Fan_dp,&Fan_dp,"kPa",0,30,1,2,4,&PanelList[i]);	   
	   
	   //N2 flow from tanks to rooms
	   new Switch(10,290,0,3,1,&Tanks[7]->open_handle, &PanelList[i]);
	   new Switch(55,290,1,2,0,&Valves[7]->open_handle,&PanelList[i]);
       new Slider(18,350,&Tanks[7]->open,&PanelList[i]);
	   new TB(55,350,&Tanks[7]->pz,&PanelList[i]);
	   
	   new Switch(10,390,0,3,1,&Tanks[8]->open_handle, &PanelList[i]);
	   new Switch(55,390,1,2,0,&Valves[8]->open_handle,&PanelList[i]);
	   new Slider(18,450,&Tanks[8]->open,&PanelList[i]);
	   new TB(55,450,&Tanks[8]->pz,&PanelList[i]);
		//xfeed ??
	   new Switch(120,390,-1,2,0,&Man[2]->X[0].open_handle,&PanelList[i]);
	   new Switch(120,290,1,2,0,&Man[2]->OV[0].open_handle, &PanelList[i]);
	   //ovb n2 dump   
	   new Switch(10,490,0,3,1,&Valves[9]->open_handle,&PanelList[i]);
	   new Switch(60,490,0,3,1,&Valves[10]->open_handle,&PanelList[i]);
	    new Slider(18,550,&Valves[9]->open,&PanelList[i]);
		new Slider(68,550,&Valves[10]->open,&PanelList[i]);
	   //02 flow from cyro + cooling + LiOH refresh
      	   
	   new Switch(250,290,1,2,0,&Valves[13]->open_handle, &PanelList[i]); //regulated O2
       new Switch(300,290,1,2,0,&Valves[22]->open_handle, &PanelList[i]); //boiler
	   new Switch(350,290,1,2,0,&Valves[23]->open_handle, &PanelList[i]); //second reg
	   new Switch(430,290,1,2,0,&Man[3]->OV[0].open_handle,&PanelList[i]); //main 02
	   new Switch(480,290,1,2,0,&Tanks[16]->open_handle,&PanelList[i]);	//refresher
	   new Switch(530,290,1,2,0,&Tanks[16]->open_handle,&PanelList[i]);	//refresher
	   new TB(265,350,&Valves[13]->pz,&PanelList[i]);
	   new TB(315,350,&Valves[22]->pz,&PanelList[i]);
	   new TB(365,350,&Valves[23]->pz,&PanelList[i]);
	   new TB(445,350,&Man[3]->OV[0].pz,&PanelList[i]);
	   new TB(495,350,&Tanks[16]->pz,&PanelList[i]);

	   //fans switches
       new Switch(250,400,0,3,1,&Fans[0]->start_handle, &PanelList[i]); 
       new Switch(295,400,0,3,1,&Fans[1]->start_handle, &PanelList[i]); 
	   new Slider(258,460,&Fans[0]->on,&PanelList[i]);
	   new Slider(303,460,&Fans[1]->on,&PanelList[i]);
	   //
		//N2 Heaters?
       new DigClock(820,290,Clk->time,8,&PanelList[i]);
	   new Switch(750,290,1,2,0,&mjd_d,&PanelList[i]);
	   new Switch(775,360,0,3,0,&Clk->h_stop,&PanelList[i]);
	   new Switch(820,360,1,2,0,&Clk->direction,&PanelList[i]);
	   new Switch(865,360,0,3,1,&Clk->h_hour,&PanelList[i]);
	   new Switch(910,360,0,3,1,&Clk->h_min,&PanelList[i]);
	   new Switch(955,360,0,3,1,&Clk->h_sec,&PanelList[i]);
	   new Bitmap(855,450,110,80,4,&PanelList[i]);
	   new Bitmap(0,568,1024,200,2,&PanelList[i]);
	   //Panel R1
i=4;
static CTEXT_LIST CTR[4]={{305,20,95,1,"ANT 1"},{405,20,95,1,"ANT 2"},
{510,20,100,1,"ANT 1"},{510,140,100,1,"ANT 2"}
};
static TEXT_LIST TLR[14]={{230,340,"LATCH"},{230,400,"RELEASE"},{285,30," Y//R"},//
{330,30,"P//U"},{385,30,"Y//R"},{430,30,"P//U"},
{285,90,"Y//L"},{330,90,"P//D"},{385,90,"Y//L"}, //9
{430,90,"P//D"},{285,110,"CNT"},{285,170,"AQ&TRK"},//12
{385,110,"CNT"},{385,170,"AQ&TRK"}//14
};//6


	   PanelList[i].Wdth=1024;PanelList[i].Hght=768;PanelList[i].idx=i;
	   PanelList[i].ATT_mode=0x0011;//right
	   PanelList[i].v=vessel;
	   PanelList[i].Text_list=TLR;
	   PanelList[i].text_num=14;
	   //PanelList[i].Screw_list=SC1;PanelList[i].screw_num=4;
	   PanelList[i].CText_list=CTR;
	   PanelList[i].ctext_num=4;
	   //PanelList[i].Border_list=BD2;
	   PanelList[i].border_num=0; 
	   PanelList[i].neighbours[0]=0;PanelList[i].neighbours[1]=-1; //left / right
	   PanelList[i].neighbours[2]=3;PanelList[i].neighbours[3]=-1; //up / down
	   
	   new Switch (205,350,0,3,0,&((Dragonfly*)(vessel))->latch_handle,&PanelList[i]);
	   new TB(220,410,&((Dragonfly*)(vessel))->dock_latched,&PanelList[i]);
	   
	   new Switch(260,40,0,3,1,&((Dragonfly*)(vessel))->UY_handle,&PanelList[i]);
	   new Switch(305,40,0,3,1,&((Dragonfly*)(vessel))->UP_handle,&PanelList[i]);
	   new Switch(260,120,0,3,0,&((Dragonfly*)(vessel))->UAnt_handle,&PanelList[i]);
	   
	   new Switch(360,40,0,3,1,&((Dragonfly*)(vessel))->LY_handle,&PanelList[i]);
	   new Switch(405,40,0,3,1,&((Dragonfly*)(vessel))->LP_handle,&PanelList[i]);
	   new Switch(360,120,0,3,0,&((Dragonfly*)(vessel))->LAnt_handle,&PanelList[i]);

       new EGauge(460,40,&((Dragonfly*)(vessel))->UY_pos ,"YAW",0,300,0.003333,&PanelList[i]);
	   new EGauge(560,40,&((Dragonfly*)(vessel))->UP_pos ,"PITCH",0,75,0.013333,&PanelList[i]);
	   new EGauge(660,40,&((Dragonfly*)(vessel))->UAnt_SStr ,"SGN",0,100,0.01,&PanelList[i]);
	   new EGauge(460,150,&((Dragonfly*)(vessel))->LY_pos ,"YAW",0,300,0.003333,&PanelList[i]);
	   new EGauge(560,150,&((Dragonfly*)(vessel))->LP_pos ,"PITCH",0,75,0.013333,&PanelList[i]);
       new EGauge(660,150,&((Dragonfly*)(vessel))->LAnt_SStr ,"SGN",0,100,0.01,&PanelList[i]);
	   new Bitmap(851,0,173,768,3,&PanelList[i]);
	//   new Bitmap(200,250,100,70,6,&PanelList[i]);
	//   new Bitmap(0,0,173,768,5,&PanelList[i]);
	//   new Bitmap(400,330,110,80,4,&PanelList[i]);

//	   n_adi=new ADI(500,350,&PanelList[i]);

}
void ShipInternal::Init(VESSEL *vessel)
{  //3 X 02 cyro tanks
  parent=vessel;
  H_systems.AddSystem(Tanks[0]=new Tank(_vector3(0,0,0),4));
                      Tanks[0]->FillTank(O2_SPECIFICC,130000,170,O2_MMASS,0,600,150);
  H_systems.AddSystem(Tanks[1]=new Tank(_vector3(0,0,0),4));
                      Tanks[1]->FillTank(O2_SPECIFICC,130000,170,O2_MMASS,0,600,150);
  H_systems.AddSystem(Tanks[2]=new Tank(_vector3(0,0,0),4));
                      Tanks[2]->FillTank(O2_SPECIFICC,130000,170,O2_MMASS,0,600,150); 
  //cyro 02 manifold
  H_systems.AddSystem(Man[0]=new Manifold(Tanks[0],Tanks[1],Tanks[2],150));

  //3 x H2 cyro tanks
  H_systems.AddSystem(Tanks[3]=new Tank(_vector3(0,0,0),10));
                      Tanks[3]->FillTank(H2_SPECIFICC,70000,70,H2_MMASS,0,600,150);
  H_systems.AddSystem(Tanks[4]=new Tank(_vector3(0,0,0),10));
                      Tanks[4]->FillTank(H2_SPECIFICC,70000,70,H2_MMASS,0,600,150);
  H_systems.AddSystem(Tanks[5]=new Tank(_vector3(0,0,0),10));
                      Tanks[5]->FillTank(H2_SPECIFICC,70000,70,H2_MMASS,0,600,150);
  H_systems.AddSystem(Man[1]=new Manifold(Tanks[3],Tanks[4],Tanks[5],150));
  //H20 waste tank for FC1/2
  H_systems.AddSystem(Tanks[6]=new Tank(_vector3(0,0,0),8));
					  Tanks[6]->FillTank(H2O_SPECIFICC,10,70,H2O_MMASS,250,600,150);

  //2 vent overpressure-valves for H20 waste tank
  H_systems.AddSystem(Valves[0]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(0.0,-1.0,0.0),
	                10,0.5,1,2,150,Tanks[6]));
  H_systems.AddSystem(Valves[1]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(0.0,1.0,0.0),
	                10,0.5,1,2,150,Tanks[6]));
  //2 ovb dump valves
   H_systems.AddSystem(Valves[2]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,0,2,450,&Man[0]->OV[2]));
   H_systems.AddSystem(Valves[3]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,0,2,450,&Man[1]->OV[2]));
   //2 pressure regulators + 2 vent valves  for pressure-safe cyro tanks
   H_systems.AddSystem(Valves[4]=new PValve(1,5,1500,1450,350,&Man[0]->OV[2]));
   H_systems.AddSystem(Valves[5]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,1,2,150,Valves[4]));
   H_systems.AddSystem(Valves[24]=new PValve(1,5,2500,2450,350,&Man[1]->OV[2]));
   H_systems.AddSystem(Valves[6]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,1,2,150,Valves[24]));

  // 2 x Fuel cells
  E_systems.AddSystem(FC[0]=new FCell(_vector3(0,0,0),&Man[0]->OV[0],&Man[1]->OV[0],(VentValve*)Valves[0],Tanks[6],10));                      
  E_systems.AddSystem(FC[1]=new FCell(_vector3(0,0,0),&Man[0]->OV[1],&Man[1]->OV[1],(VentValve*)Valves[0],Tanks[6],10));                      
  // 1 x 30min backup battery
  E_systems.AddSystem(BT[0]=new Battery(FC[0],5184000));
  // 2 x DC busses , 1 back-up + AC buss
  E_systems.AddSystem(DC[0]=new DCbus(FC[0]));
  E_systems.AddSystem(DC[1]=new DCbus(FC[0]));
  E_systems.AddSystem(AC[0]=new ACbus(DC[0]));
  //link Dragonfly's power source to the AC1 and DC1
	((Dragonfly*)vessel)->AC_power=&AC[0]->Volts;
	((Dragonfly*)vessel)->DC_power=&DC[0]->Volts;
  E_systems.AddSystem(DC[2]=new DCbus(DC[0])); //main heater bus
  E_systems.AddSystem(DC[3]=new DCbus(DC[0])); //fan bus
  // all the socks we need
  E_systems.AddSystem(Sock[0]=new Socket(BT[0],FC[0],FC[0],FC[1]));
  E_systems.AddSystem(Sock[1]=new Socket(DC[0],FC[0],BT[0],FC[1]));
  E_systems.AddSystem(Sock[2]=new Socket(DC[1],FC[0],BT[0],FC[1]));
  E_systems.AddSystem(Sock[3]=new Socket(AC[0],DC[0],BT[0],DC[1]));
  E_systems.AddSystem(Sock[4]=new Socket(DC[2],DC[0],BT[0],DC[1]));
  E_systems.AddSystem(Sock[5]=new Socket(DC[3],DC[0],BT[0],DC[1]));
  // cyro heaters  
  E_systems.AddSystem(HT[0]=new Heater(Tanks[0],&Tanks[0]->Press,470,450, 120,15,DC[2]));
  E_systems.AddSystem(HT[1]=new Heater(Tanks[1],&Tanks[1]->Press,470,450,120,15,DC[2]));
  E_systems.AddSystem(HT[2]=new Heater(Tanks[2],&Tanks[2]->Press,470,450,120,15,DC[2]));
  E_systems.AddSystem(HT[3]=new Heater(Tanks[3],&Tanks[3]->Press,470,450,120,15,DC[2]));
  E_systems.AddSystem(HT[4]=new Heater(Tanks[4],&Tanks[4]->Press,470,450,120,15,DC[2]));
  E_systems.AddSystem(HT[5]=new Heater(Tanks[5],&Tanks[5]->Press,470,450,120,15,DC[2]));
  E_systems.AddSystem(Clk=new Clock());

  //N2 pressure supply
 H_systems.AddSystem(Tanks[7]=new Tank(_vector3(0,0,0),3));
					  Tanks[7]->FillTank(14,20000,288,N2_MMASS,50,600,150);
 H_systems.AddSystem(Tanks[8]=new Tank(_vector3(0,0,0),3));
					  Tanks[8]->FillTank(14,20000,288,N2_MMASS,50,600,150);
  //a pressure regulator for each tank
 H_systems.AddSystem(Valves[7]=new PValve(1,5,78.3,70,120,Tanks[7]));
 H_systems.AddSystem(Valves[8]=new PValve(1,5,78.3,70,120,Tanks[8]));
 H_systems.AddSystem(Valves[13]=new PValve(1,5,290.0,280.0,120,&Man[0]->OV[2])); //reducing O2 press so we can boil it
 H_systems.AddSystem(Valves[22]=new Boiler(1,5,120,Valves[13],295.0,O2_BOILING,DC[0]));//heating it to ~22 deg
H_systems.AddSystem(Valves[23]=new PValve(1,5,23.0,20.0,120,Valves[22])); //then finnally PP02 ~23kPA
 

 //ovb dump for N2
 H_systems.AddSystem(Valves[9]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,0,2,150,Tanks[7]));
 H_systems.AddSystem(Valves[10]=new VentValve(vessel,_vector3(3.5,0.0,0.0),_vector3(-1.0,0.0,0.0),
	                10,0.5,0,2,150,Tanks[8]));
 //a simple manifold for N2
 H_systems.AddSystem(Man[2]=new Manifold(Valves[7],Valves[8],Valves[8],150));
  Man[2]->X[2].open=0;Man[2]->OV[2].open=0;Man[2]->X[1].open=1;Man[2]->X[0].open=0;
  Man[2]->OV[1].open=0;Man[2]->OV[0].open=1;
 //CO2 colector for LiOH
  H_systems.AddSystem(Tanks[12]=new Tank(_vector3(0,0,0),30));
					  Tanks[12]->FillTank(5,4800,295,CO2_MMASS,0,600,150);
  H_systems.AddSystem(Tanks[16]=new Room(_vector3(0,0,0),5,Tanks[12]));
					  Tanks[16]->FillTank(5,2100,295,CO2_MMASS,2,600,150);
  
  //and a manifold for O2 circular;       Regulate cyro 02 + refreshed 02 +cooled O2
 H_systems.AddSystem(Man[3]=new Manifold(Valves[23],Tanks[16],Tanks[16],150));
 Man[3]->X[2].open=0;Man[3]->X[1].open=1;Man[3]->X[0].open=1;
 Man[3]->OV[2].open=0;Man[3]->OV[1].open=0;Man[3]->OV[0].open=1;
 //no cooling for now :-(
 
 //cabin O2+N2+C02 air
 H_systems.AddSystem(Tanks[10]=new Room(_vector3(0,0,0),30,&Man[3]->OV[0])); //02 source is Pvalve from cyro02 manifold OV2
					  Tanks[10]->FillTank(O2_SPECIFICC,9100,295,O2_MMASS,0,600,150);
 H_systems.AddSystem(Tanks[11]=new Room(_vector3(0,0,0),30,&Man[2]->OV[0])); //Man[2] is N2 
					  Tanks[11]->FillTank(14,27000,295,N2_MMASS,0,600,150);
 
  //docking bay atm 
  //first the docking port
 H_systems.AddSystem(Valves[19]=new Valve(0,2,600,Tanks[10]));
 Valves[19]->open=0;
 H_systems.AddSystem(Valves[20]=new Valve(0,2,600,Tanks[11]));
 H_systems.AddSystem(Valves[21]=new Valve(0,2,600,Tanks[12]));
 H_systems.AddSystem(Tanks[13]=new Room(_vector3(0,0,0),30,Valves[19]));
					  Tanks[13]->FillTank(O2_SPECIFICC,9100,295,O2_MMASS,0,600,150);
 H_systems.AddSystem(Tanks[14]=new Room(_vector3(0,0,0),30,Valves[20]));
					  Tanks[14]->FillTank(14,27000,295,N2_MMASS,0,600,150);
 H_systems.AddSystem(Tanks[15]=new Room(_vector3(0,0,0),30,Valves[21]));
					  Tanks[15]->FillTank(14,4800,295,CO2_MMASS,0,600,150);
			//		  Tanks[10]->open=0;Tanks[11]->open=0;Tanks[12]->open=0;
 //circle is complete, 

					  
					  // the docking port can vent all out
 H_systems.AddSystem(Valves[16]=new VentValve(vessel,_vector3(0.0,0.0,3.2),_vector3(0.0,0.0,1.0),
	                10,2,0,5,550,Tanks[13]));
 H_systems.AddSystem(Valves[17]=new VentValve(vessel,_vector3(0.0,0.0,3.2),_vector3(0.0,0.0,1.0),
	                10,2,0,5,550,Tanks[14]));
 H_systems.AddSystem(Valves[18]=new VentValve(vessel,_vector3(0.0,0.0,3.2),_vector3(0.0,0.0,1.0),
	                10,2,0,5,550,Tanks[15]));
 
 E_systems.AddSystem(Fans[0]=new Fan(Tanks[12],Tanks[16],-20.0,7,DC[3]));
 E_systems.AddSystem(Fans[1]=new Fan(Tanks[12],Tanks[16],-20.0,7,DC[3]));
 
  DC[0]->PLOAD(70);
  //DC[1]->PLOAD(80);
  AC[0]->PLOAD(30);

  mjd_d=1;
};





void ShipInternal::Refresh(double dt)
{ 
  H_systems.Refresh(dt);
  E_systems.Refresh(dt);
  if (mjd_d==1) {
  double mjd=oapiGetSimMJD();
  mjd=mjd-(int)mjd;
  if (mjd<0) mjd+=1;
  mjd*=3600*24;
  int hh=(int)(mjd/3600);
  if (hh*3600>mjd) hh--;
  if (hh>23) while (hh>23) hh-=24;
  int mm=((mjd-hh*3600)/60);
  int ss=(mjd-hh*3600-mm*60);
  sprintf(Clk->time,"%2i:%2i:%2i",hh,mm,ss);
  };
//2 crew members use 0.075 grams of O2/sec
  float Lungs=Tanks[10]->Flow(0.075,dt);
  Tanks[12]->PutMass(Lungs*dt,Tanks[10]->Temp);
	//cabin related stuff  
  float temp_press;
  Cabin_temp=(Tanks[10]->Temp+Tanks[12]->Temp+Tanks[11]->Temp)/3-273.3;
  temp_press=(Tanks[10]->Press+Tanks[12]->Press+Tanks[11]->Press);
  Cabin_dp=(Cabin_press-temp_press)/dt;Cabin_press=temp_press;
	//docking hatch related 
  Dock_temp=(Tanks[13]->Temp+Tanks[14]->Temp+Tanks[15]->Temp)/3-273.3;
  temp_press=(Tanks[13]->Press+Tanks[14]->Press+Tanks[15]->Press);
  Dock_dp=(Dock_press-temp_press)/dt;Dock_press=temp_press;

  Fan_dp=Tanks[16]->Press-Tanks[12]->Press;
//  oapi


};

void ShipInternal::Load(FILEHANDLE scn, void *def_vs)
{  char *line;
   oapiReadScenario_nextline (scn, line);
   if (!strncmp(line,"INTERNAL: v1.0.0B",17))
   {  oapiReadScenario_nextline (scn, line);
	 H_systems.Load(scn);
	  oapiReadScenario_nextline (scn, line);
     E_systems.Load(scn);
	  oapiReadScenario_nextline (scn, line);
	 PanelList[0].Load(scn);
	  PanelList[1].Load(scn);
	   PanelList[2].Load(scn);
	    PanelList[3].Load(scn);
		   PanelList[4].Load(scn);
   }				
   else
	   parent->ParseScenarioLineEx (line, def_vs);
};

void ShipInternal::Save(FILEHANDLE scn)
{	
	char cbuf[50];
	strcpy(cbuf,"v1.0.0B");
   	oapiWriteScenario_string (scn, "INTERNAL:", cbuf);
	cbuf[0]=0;
		oapiWriteScenario_string (scn, "  HYDRAULICS:", cbuf);
	H_systems.Save(scn);
		oapiWriteScenario_string (scn, "  ELECTRICAL:", cbuf);
	E_systems.Save(scn);
	 	oapiWriteScenario_string (scn, "  PANEL     :", cbuf);
	PanelList[0].Save(scn);
		PanelList[1].Save(scn);
			PanelList[2].Save(scn);
				PanelList[3].Save(scn);
				  PanelList[4].Save(scn);
	oapiWriteScenario_string (scn, " END INTERNAL", cbuf);
};