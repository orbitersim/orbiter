
#include "hsystems.h"
#include "orbitersdk.h"
#include <stdio.h>

const float CONST_R=8.31904f/1000.0f;
const float TEMP_PRESS_RATIO=0.07;


h_object::h_object()
{next=NULL;};

void h_object::refresh(double dt)
{};
H_system::H_system()
{List.next=NULL;
};
void h_object::Save(FILEHANDLE scn)
{};
void h_object::Load(FILEHANDLE scn)
{};

H_system::~H_system()
{h_object *runner;
 h_object *gone;
runner=List.next;
while (runner) {gone=runner;
				runner=runner->next;
				delete gone;
};
};
h_object* H_system::AddSystem(h_object *object)
{ h_object *runner;
 runner=&List;
 while (runner->next) runner=runner->next;
 runner->next=object;
 object->next=NULL;
 return object;
};

void H_system::Refresh(double dt)
{ h_object *runner;
 runner=List.next;
 while (runner){ runner->refresh(dt);
				 runner=runner->next;}
};	
void H_system::Save(FILEHANDLE scn)
{ h_object *runner;
 runner=List.next;
 while (runner){ runner->Save(scn);
				 runner=runner->next;}
};
void H_system::Load(FILEHANDLE scn)
{ h_object *runner;
 runner=List.next;
 while (runner){ runner->Load(scn);
				 runner=runner->next;}
};
//------------------------------------ NORMAL BASIC VALVE ------------------
Valve::Valve()
{};
Valve::Valve(int i_open,int ct,float i_maxf,Valve *i_src)
{Set(i_open,ct,i_maxf,i_src);
};
void Valve::Set(int i_open,int ct,float i_maxf,Valve *i_src)
{ open=i_open; MaxF=i_maxf; SRC=i_src;c=SRC->c;
  if (SRC->open) {Temp=SRC->Temp;Press=SRC->Press;}
  else {Press=0;Temp=0;}
 ClosingTime=ct;
 pz=0; //not moving;
 open_handle=2; //this is where one can comand an open/close
 mass=0;energy=0;
 Press=0.0;
};
bool Valve::IsOpen()
{if (open) return true;
 return false;
};
double Valve::Flow(double _need,float dt)
{double flow;

flow=(_need>MaxF?MaxF:_need);
if (!open) return 0;

flow=SRC->Flow(flow,dt);
mass+=flow;
return flow;

};

void Valve::refresh(double dt)
{
if ((open_handle !=2)&&(open_handle!=open)) //we've been asked to do somethinh
	{	pz+=ClosingTime*open_handle; 
		open_handle=2;
	};

if (pz<0)
{ pz+=dt; 
	if (pz>0) {pz=0.0;
				open=0;Press=0.0;} //we cut the pressure when  closed;temp is freezed (constant)
}

if (pz>0)
{pz-=dt;
if (pz<0){ pz=0.0;
			open=1;}
}
if (open)
{if (SRC->open) Temp=SRC->Temp;
 //deg =  Kw/sec * gr/sec * deg/(Kw*gr) 
 if (mass) Temp+=energy/mass/c; //if we have any flow, we heat up
 mass=0; energy=0;
 Press=(SRC->Press*SRC->open)*(Temp/SRC->Temp); //temp induced pressure. 
}
};

void Valve::Save(FILEHANDLE scn)
{char cbuf[50];
	sprintf (cbuf, "%i %0.4f", open,pz);
	oapiWriteScenario_string (scn, "    VALVE ", cbuf);
};
void Valve::Load(FILEHANDLE scn)
{ 
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    VALVE %i %f", &open,&pz);

};
PValve::PValve(int i_open,int ct,float max_p, float min_p,float i_maxf, Valve *i_src):Valve(i_open,ct,i_maxf,i_src)
{MinP=min_p;MaxP=max_p;
};
double PValve::Flow(double _need,float dt)
{
float flow;
  Press=SRC->Press; if (Press>MaxP) Press=MaxP;
  flow=(Press-MinP)/MaxP*MaxF*open; //how much does it flow at this pressure ??
  flow=(_need>flow?flow:_need);
  if (flow<0) flow=0; //nothing!!
  flow=SRC->Flow(flow,dt);
  mass+=flow; //flow the mass :-)]
  return flow;
};

void PValve::refresh(double dt)
{
if ((open_handle !=2)&&(open_handle!=open)) //we've been asked to do somethinh
	{	pz+=ClosingTime*open_handle; 
		open_handle=2;
	};

if (pz<0)
{ pz+=dt; 
	if (pz>0) {pz=0.0;
				open=0;Press=0.0;} //we cut the pressure when  closed;temp is freezed (constant)
}

if (pz>0)
{pz-=dt;
if (pz<0){ pz=0.0;
			open=1;}
}
if (open)
{if (SRC->open) Temp=SRC->Temp;
 //deg =  Kw/sec * gr/sec * deg/(Kw*gr) 
 if (mass) Temp+=energy/mass/c; //if we have any flow, we heat up
 mass=0; energy=0;
 Press=(SRC->Press*SRC->open)*(Temp/SRC->Temp); //temp induced pressure. 
 if (Press>MaxP) Press=MaxP;
}
};

//--------------------------- 3 WAY MANIFOLD ---------------------------
CrossValve::CrossValve():Valve()
{SRC1=NULL;SRC2=NULL;SRC3=NULL;};
CrossValve::CrossValve(int i_open,int ct,float i_maxf,Valve *main,Valve *src1,Valve *src2,Valve *src3):Valve(i_open,ct,i_maxf,main)
{SRC1=src1;SRC2=src2;SRC3=src3;
};
void CrossValve::Set(int i_open,int ct,float i_maxf,Valve *main,Valve *src1,Valve *src2,Valve *src3)
{Valve::Set(i_open,ct,i_maxf,main);
SRC1=src1;SRC2=src2;SRC3=src3;
}
double CrossValve::Flow(double _need, float dt)
{
double flow;
int num_SRC;
double t_P;
flow=(_need>MaxF?MaxF:_need);

if (!open) return 0;

if (SRC1->open)	//is the cross-feed valve open?
{ 
	t_P=(SRC->Press*SRC->open+SRC2->Press*SRC2->SRC->open+SRC3->Press*SRC3->SRC->open); 
	if (Press==0) return 0;//sorry no pressure!! **-) how can this be	
	tf1+=f1=SRC->Flow(flow * (SRC->Press*SRC->open/t_P),dt);
	tf2+=f2=SRC2->Flow(flow * (SRC2->Press*SRC2->SRC->open/t_P),dt);
	tf3+=f3= SRC3->Flow(flow * (SRC3->Press*SRC3->SRC->open/t_P),dt);
	SRC1->mass-=(f2+f3);//this goes the other way 
	//messy .. very messy
	
	mass+=(f1+f2+f3);
	return (f1+f2+f3);
}

f1=SRC->Flow(flow,dt);
mass+=f1;
return f1;
}
void CrossValve::refresh(double dt)
{ int num_SRC;
  float new_Temp;
if ((open_handle !=2)&&(open_handle!=open)) //we've been asked to do somethinh
	{	pz+=ClosingTime*open_handle; 
		open_handle=2;
	};

if (pz<0)
{ pz+=dt; 
	if (pz>0) {pz=0.0;
				open=0;Press=0.0;}
}

if (pz>0)
{pz-=dt;
if (pz<0){ pz=0.0;
			open=1;}
}
if (open)
{	if (SRC1->open) {	
		    //TODO TEMP!!!1
			if (SRC1->mass<0) //is it comming in or out?
			    Temp=((SRC->Temp*f1)+(SRC1->Temp*(f2+f3)))/mass;
			else Temp=SRC->Temp;
			 num_SRC=0;Press=0;
			 Press=(SRC2->Press>SRC3->Press?SRC2->Press:SRC3->Press);
			if (SRC->Press*SRC->open>Press) Press=SRC->Press; //get max pres?
			if (mass){new_Temp=Temp+energy/mass/c; 
			          energy=0;
					 Press=(Press)*(new_Temp/Temp); //temp induced pressure. 
					 Temp=new_Temp;}
			mass=0;
			
			
				}
	else  //if no xfeed, then normal from source
	{
		Temp=SRC->Temp;
		Press=SRC->Press*SRC->open;
		if (mass){ Temp+=energy/mass/c;
		             energy=0;
					Press=(SRC->Press)*(Temp/SRC->Temp); //temp induced pressure. 
					};
		mass=0; 
	}

}
tf1=tf2=tf3=0;
}; 	 	

Manifold::Manifold(Valve *src1, Valve *src2, Valve *src3,float maxf)
{ 
   X[0].Set(1,2,maxf,src1);X[1].Set(1,2,maxf,src2);X[2].Set(1,2,maxf,src3);
  OV[0].Set(1,2,maxf,src1,&X[0],&X[1],&X[2]);
  OV[1].Set(1,2,maxf,src2,&X[1],&X[0],&X[2]); 
  OV[2].Set(1,2,maxf,src3,&X[2],&X[1],&X[0]);

};

void Manifold::refresh(double dt)
{double dtemp;
 double tm;



if (X[0].mass<0) //reverse flow, must get other temp
{  tm=X[0].mass;
   X[0].refresh(dt);
   X[0].Temp=(X[1].Temp*OV[0].tf2+X[2].Temp*OV[0].tf3)/(OV[0].tf2+OV[0].tf3);
   dtemp=X[0].Temp+X[0].energy/-tm/X[0].c;
   X[0].Press=(X[1].Press>X[2].Press?X[1].Press:X[2].Press);
   if (X[1].mass*X[2].mass<0)X[0].Press=(X[1].mass>0?X[1].Press:X[2].Press);
   //X[0].Press=(X[1].Press*OV[0].f2+X[2].Press*OV[0].f3)/(OV[0].f2+OV[0].f3);
   X[0].Press=X[0].Press*(dtemp/X[0].Temp);
   X[0].Temp=dtemp;
}
else X[0].refresh(dt);

if (X[1].mass<0) //reverse flow, must get other temp
{  tm=X[1].mass;
   X[1].refresh(dt);
   X[1].Temp=(X[0].Temp*OV[1].tf2+X[2].Temp*OV[1].tf3)/(OV[1].f2+OV[1].tf3);
   dtemp=X[1].Temp+X[1].energy/-tm/X[1].c;
   X[1].Press=(X[2].Press>X[0].Press?X[2].Press:X[0].Press);
   if (X[0].mass*X[2].mass<0)X[1].Press=(X[0].mass>0?X[0].Press:X[2].Press);
   //X[1].Press=(X[0].Press*OV[1].f2+X[2].Press*OV[1].f3)/(OV[1].f2+OV[1].f3);
   X[1].Press=X[1].Press*(dtemp/X[1].Temp);
   X[1].Temp=dtemp;
}
else X[1].refresh(dt);

if (X[2].mass<0) //reverse flow, must get other temp
{  tm=X[2].mass;
   X[2].refresh(dt);
   X[2].Temp=(X[1].Temp*OV[2].tf2+X[0].Temp*OV[2].tf3)/(OV[2].f2+OV[2].tf3);
   dtemp=X[2].Temp+X[2].energy/-tm/X[2].c;
   X[2].Press=(X[1].Press>X[0].Press?X[1].Press:X[0].Press);
   if (X[1].mass*X[0].mass<0) X[2].Press=(X[1].mass>0?X[1].Press:X[0].Press);
	   //(X[1].Press*OV[2].f2+X[0].Press*OV[2].f3)/(OV[2].f2+OV[2].f3);
   X[2].Press=X[2].Press*(dtemp/X[2].Temp);
   X[2].Temp=dtemp;
}
else X[2].refresh(dt);

OV[0].refresh(dt); OV[1].refresh(dt);OV[2].refresh(dt);

}
void Manifold::Load(FILEHANDLE scn)
{
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    MANIFOLD  %i %i %i %i %i %i", &X[0].open,&X[1].open,&X[2].open,&OV[0].open,&OV[1].open,&OV[2].open);

}
void Manifold::Save(FILEHANDLE scn)
{
    char cbuf[50];
	sprintf (cbuf, "%i %i %i %i %i %i", X[0].open,X[1].open,X[2].open,OV[0].open,OV[1].open,OV[2].open);
	oapiWriteScenario_string (scn, "    MANIFOLD ", cbuf);
}
//-------------------------------------- TANK ----------------------------------
Tank::Tank()
{Set(_vector3(0,0,0),0);
};
Tank::Tank(vector3 i_pos,float volm)
{Set(i_pos,volm);
};
void Tank::Set(vector3 i_pos,float volm)
{ pos=i_pos; Volm=volm;ClosingTime=3;open_handle=2;Freezing=0.0;
  pz=0.0;open=1;energy=0.0;
};

void Tank::FillTank(float i_c,float i_kg,float temp,float moln,float min,float max,float fl)
{ c=i_c;Mn=moln;mass=i_kg;MinP=min;MaxP=max;
  SetTemp(temp);MaxF=fl;Temp=temp;
  Mols=mass/Mn;
  Press=Mols*CONST_R*GetTemp()/Volm;

}

double Tank::Flow(double _need,float dt)
{ float flow;
  flow=(Press-MinP)/MaxP*MaxF*open; //how much does it flow at this pressure ??
  flow=(_need>flow?flow:_need);
  if (Temp<Freezing) flow=0;
  if (flow<0) flow=0; //nothing!!
  mass-=flow*dt; //then substract the mass :-)]
  return flow;
}

void Tank::refresh(double dt)
{//same valve stuff, 
if ((open_handle !=2)&&(open_handle!=open)) //we've been asked to do somethinh
	{pz+=ClosingTime*open_handle; 
	open_handle=2;
	}

if (pz<0)
{ pz+=dt; 
if (pz>0) {pz=0.0;
			open=0;
			}
}

if (pz>0)
{pz-=dt;
if (pz<0) {pz=0.0;
			open=1;
			}
}
//but also with some press computations;
 float P2;
 Mols=mass/Mn;
 P2=Mols*CONST_R*Temp/Volm;
 Temp-=Temp*TEMP_PRESS_RATIO*(Press-P2)/Press;        //press. induced temp-decrease
 Temp+=energy/mass/c;							  //temp from Qenergy / heaters
  energy=0.0;
 Press=Mols*CONST_R*Temp/Volm;		 //now we can calculate true pressure
};

void Tank::PutMass(double i_mass, double i_temp) //add this much subst, at this temp;
{double t_mass;
 t_mass=(i_mass*i_temp+mass*Temp) / (mass+i_mass);
 Temp=t_mass;
 mass+=i_mass;
};

void Tank::Save(FILEHANDLE scn)
{   char cbuf[50];
	sprintf (cbuf, "%i %0.4f %0.0f %0.2f", open,pz,mass,Temp);
	oapiWriteScenario_string (scn, "    TANK ", cbuf);
};
void Tank::Load(FILEHANDLE scn)
{ 
   char *line;
   oapiReadScenario_nextline (scn, line);
   sscanf (line,"    TANK %i %f %f %f", &open,&pz,&mass,&Temp);

}

VentValve::VentValve()
{};
VentValve::VentValve(VESSEL *i_vessel,vector3 i_p,vector3 i_dir,float w,float h,int i_open,int ct,float i_maxf, Valve *i_src):Valve(i_open,ct,i_maxf,i_src)
{pos=i_p; dir=i_dir.normalize();													//need a way to inquire force for a vessel
 vessel=i_vessel;
 ph=vessel->CreatePropellantResource(0.005);
 th=vessel->CreateThruster(_V(pos.x,pos.y,pos.z),_V(dir.x,dir.y,dir.z),10*MaxF,ph,1e99);
 vessel->AddExhaust(th,w,h);
};
void VentValve::Set(VESSEL *i_vessel,vector3 i_p,vector3 i_dir,float w,float h ,int i_open,int ct,float i_maxf, Valve *i_src)
{Valve::Set(i_open,ct,i_maxf,i_src);pos=i_p; dir=i_dir;													//need a way to inquire force for a vessel
 pos=i_p; dir=i_dir.normalize();													//need a way to inquire force for a vessel
 vessel=i_vessel;
 ph=vessel->CreatePropellantResource(0.005);
 th=vessel->CreateThruster(_V(pos.x,pos.y,pos.z),_V(dir.x,dir.y,dir.z),10*MaxF,ph,1e99);
 vessel->AddExhaust(th,w,h);
};

void VentValve::refresh(double dt)
{ Valve::refresh(dt);
  float flow;
  mass=0.0;
  if (open)  mass=SRC->Flow(MaxF,dt);
  
  if (mass) {    vessel->SetPropellantMass(ph,0.005);
	             vessel->SetThrusterLevel(th,mass/MaxF);
				 vessel->SetEmptyMass(vessel->GetEmptyMass()-(mass/1000)*dt);
				}
  else vessel->SetThrusterLevel(th,0.0);
 
}
//--------------------------------- PressValve ---------------------------------
PressValve::PressValve()
{}

PressValve::PressValve(int i_open,int ct,float i_maxf,Tank* i_SRC, Tank* i_TRG):Valve(i_open,ct,i_maxf,i_SRC)
{ tSRC=i_SRC; tTRG=i_TRG; }

void PressValve::Set(int i_open,int ct,float i_maxf,Tank* i_SRC, Tank* i_TRG)
{ Valve::Set(i_open,ct,i_maxf,i_SRC);
  tSRC=i_SRC; tTRG=i_TRG; }

void PressValve::refresh(double dt)
{Valve::refresh(dt);

float dv;
	if ((open) && (tTRG->Volm>0.01) && (tTRG->Press<tSRC->Press))
		{ dv=dt*MaxF*(tSRC->Press - tTRG->Press)/101.325;
		tSRC->Volm+=dv;
		tTRG->Volm-=dv;
		}

};

Room::Room(vector3 i_pos,float volm,Valve *i_SRC):Tank(i_pos,volm)
{SRC=i_SRC;
};
void Room::FillTank(float i_c,float i_kg,float temp,float moln,float min,float max,float fl)
{ c=i_c;Mn=moln;mass=i_kg;MinP=min;MaxP=max;
  SetTemp(temp);MaxF=fl;Temp=temp;
  Mols=mass/Mn;
  Press=Mols*CONST_R*GetTemp()/Volm;

}
void Room::refresh(double dt)
{if ((open_handle !=2)&&(open_handle!=open)) //we've been asked to do somethinh
	{pz+=ClosingTime*open_handle; 
	open_handle=2;
	}

if (pz<0)
{ pz+=dt; 
if (pz>0) {pz=0.0;
			open=0;
			}
}

if (pz>0)
{pz-=dt;
if (pz<0) {pz=0.0;
			open=1;
			}
}
//but also with some press computations;
 float P2;
 Mols=mass/Mn;
 P2=Mols*CONST_R*Temp/Volm;
 Temp-=Temp*TEMP_PRESS_RATIO*(Press-P2)/Press;        //press. induced temp-decrease
 Temp+=energy/mass/c;							  //temp from Qenergy / heaters
  energy=0.0;
 Press=Mols*CONST_R*Temp/Volm;		 //now we can calculate true pressure
 if (Press<SRC->Press) {	//we can flow some stuff
						P2=SRC->Flow(SRC->MaxF*((SRC->Press-Press)/101.3)/10,dt);
					//mass+=P2*dt;
						PutMass(P2*dt,SRC->Temp);
						};
};