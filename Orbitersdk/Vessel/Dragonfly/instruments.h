
#ifndef __INSTRUMENTS_H_
#define __INSTRUMENTS_H_

#include <stdlib.h>
#include <windows.h>
#include "vectors.h"
#include "orbitersdk.h"


class Panel;

class instrument		// basic instrument template
{  public:
	instrument(int x, int y,Panel *i_parent);
	virtual ~instrument(){};
	int type;							//instrument registration number for run-time ID
	virtual void RegisterMe(int index){};//register instrument within Orbiter's api
	virtual void PaintMe(){};		//repaint the instrument
	virtual void RefreshMe(){};	//basic refresh()
	virtual void LBD(int x,int y) {};
	virtual void RBD(int x, int y) {};
	virtual void BU() {};


	int ScrX;
    int ScrY;			//coords on screen
    int idx;			//index on the panel list 
	Panel *parent;		// pointer to parent panel

};

class instrument_list	//list used by panels to keep instruments 
{public:
   instrument *instance;	
   instrument_list *next;
   instrument_list(){instance=NULL;next=NULL;};
};

class TB:public instrument
{ public:
    TB(int x, int y, float *i_SRC, Panel* i_parent);
	void RegisterMe(int index);
    void PaintMe();
	void RefreshMe();
	int last_d;
	float * SRC;
};
class Switch: public instrument 
{  public: 
	Switch(int x,int y,int i_pos,int i_num_pos,int i_sp, int *i_SRC,Panel* i_parent);
	virtual ~Switch(){};
	void RegisterMe(int index);
    void PaintMe();
	void RBD(int x, int y);
	void LBD(int x, int y);
	void BU();
	int pos;			//position of switch
    int num_pos;		//number of positions
	int spring;
	int *SRC;

};
class SFSwitch:public Switch
{ public:
	SFSwitch(int x,int y,int i_pos,int i_num_pos,int i_safed,Panel* i_parent);
	virtual ~SFSwitch();
	void PaintMe();
	void RegisterMe(int index);
	void RBD(int x, int y);
	void LBD(int x, int y);
private:
	SURFHANDLE temps;
	HDC hTEMPDC;			//SFSwitch needs a back-surface
	int safed;				//is the switch safed or not?
};

class EGauge:public instrument
{ public:
    char unit[5];							// units as displayed on the screen
    int MaxV,MinV;						// min & max values on the scale
    float scale;							// scale between displayed values and actual pointer value (usually 10,100,1000);
	float *SRC;
    EGauge(int x, int y, float* i_SRC,char i_unit[5],int i_min,int i_max, float i_scale,Panel* i_parent);
	virtual ~EGauge(){ oapiDestroySurface(temps);};
	void RegisterMe(int index);
	void PaintMe();
	void RefreshMe();
private:
	float lastdraw;
	SURFHANDLE temps; //need a back surf. but only surf,since we can use oapiBlt
};
class HGauge:public instrument
{ public:
    float*  SRC1;           	// pointer to first monitored value (is NULL-safe)
	float*  SRC2;				// pointer to second value 
	char   unit[15];			// units of measurement (as displayed on screen)
	int    Wdth,Hght;			// widht and height of Gauge ( drawing is redimensionated)
	int    MaxV,MinV;			// max & minimum values of gauge
    float    scale;				// scale between displayed values and actual source value (usually 10,100 or1000)
	int    NrFig,NrLin;			// nubmer of markers & numbers ( gradations) on the gauge
    int lastdraw1,lastdraw2;    // where did we last draw this ?
	HGauge(int x,int y, int i_w,int i_h,float* i_SRC1,float* i_SRC2,char i_unit[15],int i_min,int i_max, float i_scale,int i_fig,int i_lin,Panel *parent);
	virtual ~HGauge(){ oapiDestroySurface(temps);};
	void RegisterMe(int index);
	void PaintMe();
	void RefreshMe();
private:
	SURFHANDLE temps;
};
class DigClock:public instrument
{ public:
 char* SRC;
 char local[10];
 int len;
 bool init;
 int powered;
 SURFHANDLE local_srf;
 DigClock(int x,int y,char *i_SRC,int i_lng, Panel* i_parent);
 ~DigClock();
 void RegisterMe(int index);
 void PaintMe();
 void RefreshMe();
//private:
 //SURFHANDLE temps;
};
class Rotary:public instrument
{ public:
    char    names[12][7];    	// list of position names
    char    screentext[15];     // name of gauge
    int     set;				// position of rotary (0 being the leftmost)
    int     poznr;				// total number of positions 
    POINT   draw[6];			// data used to store the handle draw
    Rotary(int x,int y,char i_name[15],char i_names[7][12], int i_set, int i_poz,Panel* parent);
	void RegisterMe(int index);
	void PaintMe();
	void LBD(int x,int y);
	void RBD(int x,int y);

};     
class RotElement:public instrument
{ public:
     float* TRG[11];						// list of pointers 
     float** SRC;							// pointer to source 
     int*   set;							// pointer to Rotary.set
     int lastdraw;   						// last switched position
     RotElement(float** S,int* st,Panel *parent):instrument(0,0,parent)
       { SRC=S; set=st;lastdraw=-1000;type=39;};
	 void RegisterMe(int index);
     void RefreshMe();
};
class CB:public Switch
{ public:
    CB(int x,int y,int i_pos, int *i_SRC,Panel* i_parent);
	virtual ~CB(){};
	void RegisterMe(int index);
	void PaintMe();
	void LBD(int x,int y);
	void RBD(int x,int y);
	void RefreshMe();
};
class Slider:public instrument
{ public:
    int am_i;
	int *SRC;
    Slider(int x, int y, int *i_SRC, Panel* i_parent);
	virtual ~Slider(){};
	void RegisterMe(int index);
	void PaintMe();
	void RefreshMe();
};
class bounds
{ float *SRC;	//pointer to watch
  float max;  
  float min;
 public:
  bounds(){};
  bounds(float *i_SRC,float i_max,float i_min);
  bool test();
  bounds *next;
};
class CW:public instrument
{public:
   bounds b_list;
   bounds *last;
   char text[20];
   int alarm;	//are we activated?
   SURFHANDLE temps;
   CW(int x,int y,char *i_text,Panel *i_parent);
   virtual ~CW();
   void RegisterMe(int index);
   void PaintMe();
   void RefreshMe();
   void Watch(bounds *new_b);
};
class inst_MFD:public instrument
{public:
    int type;
    MFDSPEC mfdspecs;
	inst_MFD(int x, int y,int i_type,Panel *i_parent);
	void RegisterMe(int index);
	void PaintMe();
	void LBD(int x,int y);

};
class Bitmap:public instrument
{public:
   
   int wdht;
   int hght;
   int number;
   Bitmap(int x, int y, int w, int h, int index, Panel *i_parent);
   void RegisterMe(int index);
   void PaintMe();
};
class SSTRUCT_ITEM
{ public:
  OBJHANDLE vs;
  int port;
  int maxports;
  SSTRUCT_ITEM *next;
  SSTRUCT_ITEM(){next=NULL;};
  SSTRUCT_ITEM(OBJHANDLE i_vs);
  bool Add(SSTRUCT_ITEM *newitem);
  ~SSTRUCT_ITEM();
};

class Docker:public instrument
{public:
    int dockflap;

	int cgmode;                 // 0=auto, 1=manual
	int cgswitch;               // -1=left, 0=center, 1=right
	double cgofs;               // longitudinal offset of CG when attached to other object

	int sensormode;             // docking sensor mode (0=local/1=remote)
	SSTRUCT_ITEM *vs;			// curently selected ship
	SSTRUCT_ITEM list;			// list of vessels in superstructure 
	int portnr;					// active port number

    Docker(int x, int y, Panel *i_parent);
	void RegisterMe(int index);
	void PaintMe();
	void LBD(int x,int y);
	void RefreshMe();
	void BU();
	int BuildShipList(SSTRUCT_ITEM *ship);
};
class NAVFRQ:public instrument
{public:
    int nav;					//selecting nav 1 or 2
    DWORD frq1;					//frequency1
	DWORD frq2;					//frequency2
	DWORD step;
	double timer;
	int frswitch;				//up or down
	NAVFRQ(int x,int y, Panel *i_parent);
	void PaintMe();
	void RegisterMe(int index);
	void LBD(int x,int y);
	void RefreshMe();
	void BU();
};

class ADI:public instrument
{public:

   int init;
   float sphere_vert[256][3];
   float sphere_norm[256][3];
   float sphere_tex[256][2];
   unsigned int sphere_index[600];
   int tri; //number of triangle strip
   int list_name; //we store the rendering into a display list
   double radius;
   int function_mode;	//reference / GDC / Horizon
   int orbital_ecliptic;//orbital GDC or ecliptic
   int ref_handle;
   vector3 reference;
   vector3 now;
   vector3 target;
   float over_rate;
   //some stuff for OpenGL
   HDC		   hDC2;
   HGLRC       hRC;
   HBITMAP	   hBMP;
   HBITMAP hBMP_old;
   ADI(int x,int y,Panel *i_parent);
   virtual ~ADI();
   void InitGL();
   void MoveBall();
   void SetOrbital();
   void SetEcliptic();
   void SetEquatorial();
   void SetReference();
   void GetReference();
   void RegisterMe(int index);
   void PaintMe();
   void RefreshMe();
};

class Radar:public instrument
{ public:
   int range;	
   int vessel_index;		//index of target vessel
   int list_index;			//index in the list
   float phase;
   float last_antena_yaw;
   int new_range;
   int powered;
   SURFHANDLE radar_background;
   Radar(int x, int y, Panel *i_parent);
   virtual ~Radar();
   void RegisterMe(int index);
   void PaintMe();
   void RefreshMe();
   void LBD(int x, int y);
};

class FuelMeter:public instrument
{public:
    double old_fuel;
	int powered;
	FuelMeter(int x, int y, Panel *i_parent);
	void RegisterMe(int index);
    void PaintMe();
	void RefreshMe();
};
class VROT:public instrument
{public:
   int powered;
   VROT(int x, int y, Panel *i_parent);
   void RegisterMe(int index);
   void PaintMe();
   void RefreshMe();
};
#endif