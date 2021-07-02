#include "esystems.h"
#include <math.h>
#include <stdio.h>

e_object::e_object()
{next=NULL;};
void e_object::refresh(double dt)
{};

void e_object::PLOAD(float amp)
{};
void e_object::PUNLOAD(float amp)
{};
void e_object::connect(e_object *new_src)
{SRC=new_src;};
void e_object::Save(FILEHANDLE scn)
{};
void e_object::Load(FILEHANDLE scn)
{};
E_system::E_system()
{List.next=NULL;
};

E_system::~E_system()
{e_object *runner;
 e_object *gone;
runner=List.next;
while (runner) {gone=runner;
				runner=runner->next;
				delete gone;
};
};
e_object* E_system::AddSystem(e_object *object)
{ e_object *runner;
 runner=&List;
 while (runner->next) runner=runner->next;
 runner->next=object;
 object->next=NULL;
 return object;
};

void E_system::Refresh(double dt)
{ e_object *runner;
 runner=List.next;
 while (runner){ runner->refresh(dt);
				 runner=runner->next;}
};	
void E_system::Save(FILEHANDLE scn)
{ e_object *runner;
 runner=List.next;
 while (runner){ runner->Save(scn);
				 runner=runner->next;}
};
void E_system::Load(FILEHANDLE scn)
{ e_object *runner;
 runner=List.next;
 while (runner){ runner->Load(scn);
				 runner=runner->next;}
};

//------------------------ SOCKET CONNECTOR ---------------------------------------

Socket::Socket(e_object *i_src,e_object *tg1,e_object *tg2,e_object *tg3)
{
curent=-1; SRC=i_src; TRG[0]=tg1;TRG[1]=tg2;TRG[2]=tg3;
SRC->SRC=tg1;socket_handle=-1;
}
void Socket::refresh(double dt)
{
if (socket_handle!=curent)
			{curent=socket_handle;
             if (TRG[curent+1]) SRC->connect(TRG[curent+1]);
			};
};
void Socket::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    SOCKET %i", &socket_handle);
   curent=socket_handle+1; //make sure we re-conect
}

void Socket::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%i",curent);
	oapiWriteScenario_string (scn, "    SOCKET ", cbuf);
}

//----------------------------------- FUEL CELL --------------------------------------

FCell::FCell(vector3 i_pos,Valve *o2,Valve *h2,VentValve *vent,Tank* waste,float r_amp)
{ pos=i_pos;
  O2_SRC=o2;
  H2_SRC=h2;
  H20_vent=vent;
  //H20_vent->SRC=H20_waste;
  H20_waste=waste;
  //H20_waste->Set(_vector3(0,0,0),0.35);
  H20_waste->FillTank(14,50,288,34,4600,5000,150);
  Temp=290;//(O2_SRC->Temp+H2_SRC->Temp)/2;
  H20_waste->Temp=Temp;
  Volts=28.8;
  power_load=r_amp;
  max_power=7200; //watts 
  clogg=0.0; //no clog
  start_handle=2; //stopped
  purge_handle=-1; //no purging
  status=2; //stopped
  reaction=0; //no chemical react.
  SRC=NULL; //for now a FCell cannot have a source
  H2_flow=0;O2_flow=0;
}
void FCell::PLOAD(float amp)
{ power_load+=amp;};

void FCell::PUNLOAD(float amp)
{ power_load-=amp;};
void FCell::refresh(double dt)
{
//	double reaction; //amount of reactant needed;
//first we check the start_handle;
if (start_handle==-1) status=2; //stoped
if(start_handle==1) 
			status=1; //starting

if ((purge_handle==1)&&(status==0)) status=3; //purging;
switch (status)
{

case 2: //stopped, not much to do

	Volts=0;Amperes=0;
	reaction*=0.2;H2_flow=0;O2_flow=0;
	running=1;
	break;
case 1:// starting;
    H2_flow=H2_SRC->Flow(0.23,dt);
	O2_flow=O2_SRC->Flow(1.15,dt); //nominal flow
	
	if (H2_flow*5>O2_flow) { H2_SRC->mass+=(H2_flow - O2_flow/5);
							 H2_flow=O2_flow/5;
							}
	else { O2_SRC->mass+=(O2_flow-5*H2_flow);
			O2_flow=5*H2_flow;
		};
	reaction+=(H2_flow+O2_flow)/1.38*dt; //is it all there?
	//do temp / clogg functions
	Cloging(dt);
	H20_waste->PutMass((H2_flow+O2_flow)*dt,Temp);
	H20_waste->refresh(dt);//this is not in H_systems list   

	status=2;
	if (reaction>15) {status=0; //started
					  start_handle=2;};
	Amperes=power_load;
	
//	sprintf(oapiDebugString(),"%f",clogg);
	if (reaction>5) Volts=28.8*(reaction-5)/10;
	running=1;
	break;
case 0: //normal running
	//---- throtle of the fuel cell [0..1]

    reactant= power_load*28.8 / max_power / (1-log(1+clogg)); //clogg is preventing normal flow

    Volts=28.8; //we are trying to get 28.8V
    if (reactant>1.0) 
		{Volts= 28.8/reactant; reactant=1.0;};//set voltage to manage overmax loads
	
	//converting thurst into grams / second
	reactant=reactant*2.204;		//1.38 grams/sec = 2880 Watts
	
	//reactant is total flow of fuel to achieve needed electricity
	//proportion is H2 / O2 in mass if 1:5
	
    H2_flow=H2_SRC->Flow(reactant/6.0,dt);
	O2_flow=O2_SRC->Flow(5.0*reactant/6.0,dt);
	
	//return any excess reactants, 
	if (H2_flow*5>O2_flow) { H2_SRC->mass+=(H2_flow - O2_flow/5);
							 H2_flow=O2_flow/5;
							}
	else { O2_SRC->mass+=(O2_flow-5*H2_flow);
			O2_flow=5*H2_flow;
		};
	
	reaction=(H2_flow+O2_flow)/reactant; // must be 1 for nominal func, is
										 // <1 if not enough fuel
	
	
	Cloging(dt);
//    sprintf(oapiDebugString(),"%f %f %f %f",clogg,Temp,reaction,reactant);
	H20_waste->PutMass((H2_flow+O2_flow)*dt,Temp);
	H20_waste->refresh(dt);//this is not in H_systems list   
	running=0;
	Volts=Volts*reaction; //case of reaction problems :-)
	if (reaction) 	Amperes=power_load;
		else status=2;
	
	break;
case 3: //purging
    reactant= power_load*28.8 / max_power / (1-log(1+clogg)); //clogg is preventing normal flow

    Volts=28.8; //we are trying to get 28.8V
    if (reactant>1.0) 
		{Volts= 28.8/reactant; reactant=1.0;};//set voltage to manage overmax loads
	
	//converting thurst into grams / second
	reactant=15;		//15 grams/sec for purging
	
	//reactant is total flow of fuel to achieve needed electricity
	//proportion is H2 / O2 in mass if 1:5
	
    H2_flow=H2_SRC->Flow(reactant/6.0,dt);
	O2_flow=O2_SRC->Flow(5.0*reactant/6.0,dt);
	
	//return any excess reactants, 
	if (H2_flow*5>O2_flow) { H2_SRC->mass+=(H2_flow - O2_flow/5);
							 H2_flow=O2_flow/5;
							}
	else { O2_SRC->mass+=(O2_flow-5*H2_flow);
			O2_flow=5*H2_flow;
		};
	
	reaction=(H2_flow+O2_flow)/reactant; // must be 1 for nominal func, is
										 // <1 if not enough fuel
	
	
//	Cloging(dt);
	if (H2_flow) clogg+=(O2_flow/(reactant*H2_flow+0.1)-0.4)/100*dt; 
	//at 15gr/sec clogg should be negative
    if (clogg<0) clogg=0.0; //cannot get negative, let's be serious;
	
	energy=-(Amperes*Volts / max_power) * clogg*0.05*dt;
    Temp+=energy;

	H20_waste->PutMass((H2_flow+O2_flow)*dt,Temp);
	H20_waste->refresh(dt);//this is not in H_systems list   

	Volts=Volts*reaction; //case of reaction problems :-)
	if (reaction) 	Amperes=power_load;
	running=0;
	status=0; //status 0 is implicit
	break;

};
//sprintf(oapiDebugString(),"%0.4f %0.4f %0.4f %0.4f", H2_flow, O2_flow,clogg,reactant);
};

void FCell::Cloging(double dt)
{ if (H2_flow) clogg+=(O2_flow/(reactant*H2_flow+0.01)-0.4)/100000*dt; //stuff that gets in the way
  if (clogg<0) clogg=0.0; //cannot get negative, let's be serious;
  energy=(Amperes*Volts / max_power) * clogg*0.05*dt;
  //energy=(Amperes*Volts / max_power)*clogg * 0.05; //0.005 deg /seconf
  Temp+=energy;
};
void FCell::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    FCELL %i %f", &status,&clogg);
}

void FCell::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%i %0.4f ",status, clogg);
	oapiWriteScenario_string (scn, "    FCELL ", cbuf);
}
//-------------------------------------- BATTERY ---------------------------------
Battery::Battery(e_object *i_src, double i_power)
{SRC=i_src;
 power_load=0.0;
 load_handle=-1; //no loading;
 max_power=power=i_power;
 loading=0;
 c_breaker=1;
 //Volts=28.8;
};

void Battery::PLOAD(float amp)
{ power_load+=amp;};

void Battery::PUNLOAD(float amp)
{ power_load-=amp;};

void Battery::connect(e_object *new_src)
{ if (loading)  SRC->PUNLOAD(30);
  SRC=new_src;
  if (loading) SRC->PLOAD(30);
};

void Battery::refresh(double dt)
{ Amperes=power_load;
  Volts=28.8*c_breaker;
 power-=Amperes*Volts*dt;
 if (power<0) { Amperes=0;
				Volts=0 ;}; //dead battery;
 if ((load_handle==1) && (SRC) && (!loading))  {SRC->PLOAD(30); //30 amps loading
							     loading=1;load_handle=0;}
 if ((load_handle==-1) && (SRC) && (loading)){SRC->PUNLOAD(30);
								 loading=0;load_handle=0;} //terminate loading
 if (loading) {  if (load_cb) 
	                  power+=30*(SRC->Volts)*dt;
				 else load_handle=-1;
				}
};
void Battery::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    BATTERY %f %f %i", &loading,&power,&c_breaker);
   load_handle=loading;
}

void Battery::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%0.0f %0.4f %i",loading, power,c_breaker);
	oapiWriteScenario_string (scn, "    BATTERY ", cbuf);
}


//-------------------------- DIRECT CURRENT BUS -------------------------------
DCbus::DCbus(e_object *i_SRC)
{ SRC=i_SRC;
  branch_amps=0.0;
  Volts=28.8;
  Amperes=0;
  c_breaker=1;
  tripped=0;
  atrip_handle=1;
  reset_handle=0;
};

void DCbus::PLOAD(float amp)
{ branch_amps+=amp;
  if ((!tripped)&&(SRC)) SRC->PLOAD(amp);
};
void DCbus::PUNLOAD(float amp)
{ branch_amps-=amp;
  if ((!tripped)&&(SRC)) SRC->PUNLOAD(amp);
};

void DCbus::connect(e_object *new_src)
{ if (!tripped)if (SRC)  SRC->PUNLOAD(branch_amps);
 SRC=new_src;
 if (!tripped) if (SRC) SRC->PLOAD(branch_amps);
};
void DCbus::refresh(double dt)
{ if (!tripped){
		if (SRC)  Volts=SRC->Volts;
		else Volts=0.0;
		Amperes=branch_amps;
		if (atrip_handle==1)
				{if (Volts<25) c_breaker=0; //voltage limit
				if (Volts>30) c_breaker=1;
				};
		};

  if ((!tripped)&&(!c_breaker)) tripped=1; //we've tripped
  if ((tripped==2)&&(reset_handle==1)&&(c_breaker))
							tripped=-1;//we've been reset
  if ((!tripped)&&(reset_handle==-1)) tripped=1; 	//and we've been tripped ?

  if (tripped)
		{
	    if (tripped==1) //shutdown
					{SRC->PUNLOAD(branch_amps);
					Volts=0.0;Amperes=0.0;
					tripped=2;//2-we are down .
 					c_breaker=0;//pop the cb.
					};
		if ((tripped==-1))	//re-conect if c_b is on
					{SRC->PLOAD(branch_amps);
					tripped=0;
					};
		};

 

};
void DCbus::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    DC %f", &branch_amps);
}

void DCbus::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%0.4f ",branch_amps);
	oapiWriteScenario_string (scn, "    DC ", cbuf);
}

//------------------------ AC BUS -------------------------------------------------

ACbus::ACbus(e_object *i_SRC)
{ SRC=i_SRC;
  branch_amps=0.0;
  Volts=36;
  Amperes=0;
  tripped=0;
  c_breaker=1;
  atrip_handle=1;
  reset_handle=0;
};

void ACbus::PLOAD(float amp)
{ branch_amps+=amp;
  //need a bit of converstion for 36V
  if ((!tripped)&&(SRC)) SRC->PLOAD(amp*36/28.8);
};
void ACbus::PUNLOAD(float amp)
{ branch_amps-=amp;
  if ((!tripped)&&(SRC)) SRC->PUNLOAD(amp*36/28.8);
};

void ACbus::connect(e_object *new_src)
{ if (!tripped)if (SRC)  SRC->PUNLOAD(branch_amps*36/28.8);
 SRC=new_src;
 if (!tripped) if (SRC) SRC->PLOAD(branch_amps*36/28.8);
};
void ACbus::refresh(double dt)
{
if (!tripped){
		if (SRC)  Volts=(SRC->Volts)/28.8*36;
		else Volts=0.0;
		Amperes=branch_amps;
		if (atrip_handle==1)
				{if (Volts<30) c_breaker=0; //voltage limit
				 if (Volts>45) c_breaker=1;
				};
		};

  if ((!tripped)&&(!c_breaker)) tripped=1; //we've tripped
  if ((tripped==2)&&(reset_handle==1)&&(c_breaker)) tripped=-1;//we've been reset
  if ((!tripped)&&(reset_handle==-1)) tripped=1; 	//and we've been tripped ?

  if (tripped)
		{
	    if (tripped==1) //shutdown
					{SRC->PUNLOAD(branch_amps*36/28.8);
					Volts=0.0;Amperes=0.0;
					tripped=2;//2-we are down .
 					c_breaker=0;//pop the cb.
					};
		if ((tripped==-1))	//re-conect if c_b is on
					{SRC->PLOAD(branch_amps*36/28.8);
					tripped=0;
					};
		};



}
void ACbus::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    AC %f", &branch_amps);
}

void ACbus::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%0.4f ",branch_amps);
	oapiWriteScenario_string (scn, "    AC ", cbuf);
}

Heater::Heater(therm_obj *i_term,float *iw_SRC, float i_max,float i_min,float i_power,float amps,e_object *i_SRC)
{   w_SRC=iw_SRC;
	SRC=i_SRC;
	thermal_p=i_power;
	amp_cons=amps;
    term=i_term;
	auto_w=0;
	MaxP=i_max;
	MinP=i_min;
	start_handle=0;
	on=0;
};

void Heater::refresh(double dt)
{ 
if ((on)&&(start_handle==-1)){ on=0;auto_w=0;
 							    SRC->PUNLOAD(amp_cons);
								};
if (!(on)&&(start_handle==1)) {on=1;auto_w=0;
							   SRC->PLOAD(amp_cons);
								};
if (start_handle==0) {auto_w=1;start_handle=2;};

if (auto_w) { if ((on)&&(*w_SRC>MaxP)){ on=0;SRC->PUNLOAD(amp_cons);}
			  if((!on)&&(*w_SRC<MinP)){ on=1;SRC->PLOAD(amp_cons);}
			};
if ((on)&&(SRC->Volts))
	term->energy+=thermal_p*dt*1e3;

};
void Heater::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    HT %i", &on);
}

void Heater::Save(FILEHANDLE scn)
{   char cbuf[20];
	sprintf (cbuf, "%i ",on);
	oapiWriteScenario_string (scn, "    HT ", cbuf);
}
 			
Fan::Fan(Valve *ih_SRC,Tank *i_TRG,float i_max,float i_amps,e_object *i_SRC)
{h_SRC=ih_SRC;TRG=i_TRG;MaxP=i_max;amp_cons=i_amps,SRC=i_SRC;
start_handle=0;on=0;
};
void Fan::refresh(double dt)
{ float power;
if ((on)&&(start_handle==-1)) {on=0;
							  SRC->PUNLOAD(amp_cons);
								};
if((!on)&&(start_handle==1)) {on=1;	
							SRC->PLOAD(amp_cons);
							};
if ((on)&&(SRC->Volts))
		{ 
		if ((power=h_SRC->Press-TRG->Press)>MaxP) { power=h_SRC->MaxF*(SRC->Volts/28.8);
												//	power=power/MaxP;
													power=h_SRC->Flow(power,dt);
											   TRG->PutMass(power*dt,h_SRC->Temp);
												}
		};
if ((on) &&(!SRC->Volts)) start_handle=-1;
};
void Fan::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    FAN %i", &on);
}

void Fan::Save(FILEHANDLE scn)
{   char cbuf[20];
	sprintf (cbuf, "%i ",on);
	oapiWriteScenario_string (scn, "    FAN ", cbuf);
}

Boiler::Boiler(int i_open,int ct,float i_maxf, Valve *i_src,float temps,float i_boil, e_object *ie_SRC):Valve(i_open,ct,i_maxf,i_src)
{trg_Temp=temps; e_SRC=ie_SRC;amp_load=0; on=1; 
 boil_Temp=i_boil;
};
void Boiler::refresh(double dt)
{double need_e;
if (e_SRC->Volts)
{  need_e=(trg_Temp>SRC->Temp?trg_Temp-SRC->Temp:0);	//delta temp in K
           need_e*=c*mass;					//convert this into Joules
	//	   need_e*=dt;					//1 joule = 1wattsecond
		   e_SRC->PUNLOAD(amp_load);
		   amp_load=need_e/e_SRC->Volts;
		   e_SRC->PLOAD(amp_load);
		   energy=need_e;
		   if (!mass) need_e=-1;	//no mass, need to unfrost first		   
			};
Valve::refresh(dt); //normal valve refresh
if ((e_SRC->Volts)&&(need_e==-1)) Temp=trg_Temp;	
 
};

double Boiler::Flow(double _need,float dt)
{ //normal valve flow, but with boiling point check
double flow;
double B_temp=SRC->Press/101.3*boil_Temp;	//linear, sorry to complicate otherwise
flow=(_need>MaxF?MaxF:_need);
 //sprintf(oapiDebugString(),"%0.4f %0.4f %0.4f ",Press,B_temp,flow);
if (Temp<B_temp) return 0;	//no gas
if (!open) return 0;
flow=SRC->Flow(flow,dt);
mass+=flow;
return flow;

}

Clock::Clock()
{time[0]=0;h_stop=0;direction=1;timer=0;
h_hour=0;h_min=0;h_sec=0;
};

void Clock::refresh(double dt)
{ 
if ((h_hour==1)||(h_hour==-1)){timer+=h_hour*3600;UpdateChar();h_hour=2;};
if ((h_min==1)||(h_min==-1)){timer+=h_min*60;UpdateChar();h_min=2;};
if ((h_sec==1)||(h_sec==-1)){timer+=h_sec;UpdateChar();h_sec=2;};
if (h_stop==1)	{	//if working
		timer+=dt*direction;
		UpdateChar();
				}
if (h_stop==-1)strcpy(time,"88:88:88");//tester
}
void Clock::UpdateChar()
{ 
  int hh=(int)(timer/3600);
  if (hh*3600>timer) hh--;
  int mm=((timer-hh*3600)/60);
  int ss=(timer-hh*3600-mm*60);
  if (hh>23) while (hh>23) hh-=24;
  sprintf(time,"%2i:%2i:%2i",hh,mm,ss);
};