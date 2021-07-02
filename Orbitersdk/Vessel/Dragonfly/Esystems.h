
#ifndef __ESYSTEMS_H_
#define __ESYSTEMS_H_

#include "thermal.h"
#include "orbitersdk.h"
#include "hsystems.h"

class e_object:public therm_obj
{ public:
    e_object *SRC; //for loading
    float Amperes; //status 
	float Volts;   //
	float power_load;	//how much do we need to produce 
    int c_breaker;		//circuit breaker;
	int tripped;		//is the cric. closed
	int atrip_handle;	//handles for auto-shut-down
	int reset_handle;	
	e_object *next;
	e_object();
	virtual void PLOAD(float amp);
	virtual void PUNLOAD(float amp);
	virtual void connect(e_object *new_src);
	virtual void refresh(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};

class E_system
{ public:
    e_object List;
	E_system();
	~E_system();
	e_object* AddSystem(e_object *object);
	void Refresh(double dt);
	void Load (FILEHANDLE scn);
	void Save (FILEHANDLE scn);
};
class Socket:public e_object
{public:
  e_object* TRG[4];
  e_object* SRC;
  int socket_handle;
  int curent;
  Socket(e_object *i_src,e_object *tg1,e_object *tg2,e_object *tg3);
  void refresh(double dt);
  void Load (FILEHANDLE scn);
  void Save (FILEHANDLE scn);
  

};

class FCell:public e_object
{  
 public:
	Valve *O2_SRC;
	Valve *H2_SRC;
	Tank *H20_waste;
	VentValve *H20_vent;
	
	float H2_flow,O2_flow;

    float max_power;	//in watts;
	float clogg; //not used yet? need to purge the fcell
	float reaction; //is it taking place?
	double reactant;
	int start_handle; //start, stop
	int purge_handle; //purge / no purge
	int status; //what are we doing? 0-stop, 1-starting, 2- running, 3- problem , 4- out of service
	float running; //for tb indicators only
	FCell(vector3 i_pos,Valve *o2,Valve *h2,VentValve *vent,Tank* waste,float r_amp);
	virtual void PLOAD(float amp);
	virtual void PUNLOAD(float amp);
	virtual void refresh(double dt);
	void Cloging(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);

};
class Battery:public e_object
{  //battery is a producer / consumer
	public:
	int load_handle;
	int load_cb;
	float loading;
    
	Battery(e_object *i_src, double i_power);
    double max_power;
	float power;   //in AmperesH at 28.8 V
	virtual void PLOAD(float amp);
	virtual void PUNLOAD(float amp);
	virtual void connect(e_object *new_src);
	virtual void refresh(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);

};

class DCbus: public e_object
{public:
    float branch_amps;
	DCbus(e_object *i_SRC);
	
	virtual void PLOAD(float amp);
	virtual void PUNLOAD(float amp);
	virtual void connect(e_object *new_src);
	virtual void refresh(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};
class ACbus: public e_object
{ public:
   float branch_amps;
   ACbus(e_object *i_SRC);

   	virtual void PLOAD(float amp);
	virtual void PUNLOAD(float amp);
	virtual void connect(e_object *new_src);
	virtual void refresh(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};

class Heater:public e_object
{public:
  Heater(therm_obj *i_term, float *iw_SRC, float i_max,float i_min,float i_power,float amps, e_object *i_SRC);
  int auto_w;
  float *w_SRC;
  float MaxP;
  float MinP;
  int on;
  int start_handle;
  float thermal_p; //thermic power
  float amp_cons;	//amperage consumption
  therm_obj *term;		//pointer to heated object
  virtual void refresh(double dt);
  virtual void Load(FILEHANDLE scn);
  virtual void Save(FILEHANDLE scn);
};


class Fan:public e_object
{public:
  Valve *h_SRC;
  Tank *TRG;
  float MaxP;
  float amp_cons;
  int on;
  int start_handle;
  Fan(Valve *ih_SRC,Tank *i_TRG,float i_max,float i_amps,e_object *i_SRC);
  virtual void refresh(double dt);
  virtual void Load(FILEHANDLE scn);
  virtual void Save(FILEHANDLE scn);
};
class Boiler:public Valve		//this in e_systems 'cause it needs a power source
{public:
   float amp_load;
   e_object *e_SRC;
   float trg_Temp;				//Needed temp
   float boil_Temp;				//at 1 atm =103kPa;
   float on;					//for TB indicators when boiler is off
   Boiler(int i_open,int ct,float i_maxf, Valve *i_src,float temps,float i_boil, e_object *ie_SRC);
   void refresh(double dt); //just for closing / open
   virtual double Flow(double _need,float dt);

};
class Clock:public e_object
{public:
   char time[10];
   int h_hour;
   int h_min;
   int h_sec;
   int h_stop;
   int direction;
   double timer;
   Clock();
   void refresh(double dt);
   void UpdateChar();
};
#endif
