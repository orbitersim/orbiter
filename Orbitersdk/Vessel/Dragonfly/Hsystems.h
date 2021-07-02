


#ifndef __HSYSTEMS_H
#define __HSYSTEMS_H
//this is quite commonly used, so better name them
#define O2_MMASS			32
#define O2_SPECIFICC		1.669
#define O2_BOILING			90.34
#define H2_MMASS			2
#define H2_SPECIFICC        9.668
#define H2_BOILING          20.5

#define H2O_MMASS			18
#define H2O_SPECIFICC		14
#define N2_MMASS				28
#define CO2_MMASS			44

#include "thermal.h"
#include "orbitersdk.h"
//base class for hydraulical objects
class h_object:public therm_obj
{ public:
	h_object *next;
	h_object();
	virtual void refresh(double dt);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};
//all the objects form a system, basically a chained list
class H_system
{ public:
    h_object List;
	H_system();
	~H_system();
	h_object* AddSystem(h_object *object);
	void Refresh(double dt);
	void Load (FILEHANDLE scn);
	void Save (FILEHANDLE scn);
};

class Valve :public h_object    //flow valve
{
public:
	float MaxF;	   //maximum flow in grams /sec, we are always asssuming laminar flow and constant visquosity
 	float Press;   //presure at valve;
	float pz;		//pozition of valve
	int ClosingTime; //time to close the valve in secs 
	Valve *SRC;	   //pipe source must be another valve of some source;
	int open;
	int open_handle;
	Valve(int i_open,int ct,float i_maxf, Valve *i_src); //normal constructor
	void Set(int i_open,int ct,float i_maxf, Valve *i_src); //similar constructor
	Valve();
	bool IsOpen();
	void refresh(double dt); //just for closing / open

	virtual double Flow(double _need, float dt); //we need this much, how much can you give? 
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};
class PValve:public Valve //valve with a pressure regulator
{public:
   	float MinP,MaxP;			//operating pressure of the valve
	PValve(int i_open,int ct,float max_p, float min_p,float i_maxf, Valve *i_src); //normal constructor
  	virtual double Flow(double _need, float dt); //we need this much, how much can you give? 
    void refresh(double dt); //just for closing / open
							//rest is inherited from Valve
};
class CrossValve:public Valve	// a valve with 3 sources. fuel can flow either way
{public:
    Valve *SRC1;
	Valve *SRC2;
	Valve *SRC3;
	double f1,f2,f3;
	double tf1,tf2,tf3;
	CrossValve();
	CrossValve(int i_open,int ct,float i_maxf,Valve *main, Valve *src1,Valve *src2, Valve *src3);
	void Set(int i_open,int ct,float i_maxf, Valve *main, Valve *src1,Valve *src2, Valve *src3);
	void refresh(double dt); //just for closing / open

	virtual double Flow(double _need, float dt); //we need this much, how much can you give? 

};
class Manifold: public h_object		//we needed the CrossValve to build Manifold
{
public:
	Valve  X[3];
	CrossValve OV[3];

	Manifold(Valve *src1,Valve *src2,Valve *src3,float maxf);

	void refresh(double dt);
//	virtual void Flow(double _need, float dt);/
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};


		
class Tank :public Valve //a tank is just a thermic object + a flow valve 
{
public:
	float Volm;					//Volume of the container tank
	float Mn;				    //Molecular number of the substance
	float Mols;					//Number of mols of substance in the tank
	float MinP,MaxP;			//operating pressure of the valve
	float Freezing;				//freezing energy
	Tank();
	Tank(vector3 i_pos, float volm); //where is the tank, what material, how heavy
	virtual void FillTank(float i_c, float kg, float temp, float moln,float min,float max,float fl); //fill it up
	virtual double Flow(double _need,float dt);
	void Set(vector3 i_pos, float volm); //where is the tank, what material, how heavy
	void refresh(double dt);
	void PutMass(double i_mass, double i_temp);
	virtual void Load(FILEHANDLE scn);
	virtual void Save(FILEHANDLE scn);
};	

class VentValve:public Valve
{
public:
	vector3 pos;				
	vector3 dir;
	PROPELLANT_HANDLE ph;
    THRUSTER_HANDLE th;
	VESSEL* vessel;
	VentValve();
	VentValve(VESSEL *i_vessel,vector3 i_p,vector3 i_dir,float w,float h,int i_open,int ct,float i_maxf, Valve *i_src); //normal constructor
	void Set(VESSEL *i_vessel,vector3 i_p,vector3 i_dir ,float w,float h,int i_open,int ct,float i_maxf, Valve *i_src); //similar constructor
	void refresh(double dt);
};

class PressValve:public Valve
{
public:
	Tank *tSRC;
	Tank *tTRG;
	PressValve();
	PressValve(int i_open,int ct,float i_maxf,Tank* i_SRC, Tank* I_TRG);
	void Set(int i_open,int ct,float i_maxf,Tank* i_SRC, Tank* I_TRG);
	void refresh(double dt);
};

class Room:public Tank		//room is a kinda of a tank w/ source
{ public:
	Room(vector3 i_pos, float volm,Valve *i_SRC); //where is the tank, what material, how heavy
    void refresh(double dt);
	virtual void FillTank(float i_c, float kg, float temp, float moln,float min,float max,float fl); //fill it up
};


#endif