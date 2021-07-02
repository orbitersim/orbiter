/* Copyright (c) 2007 Duncan Sharpe, Steve Arch
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
** 
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
** 
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.*/

#ifndef __BASEFUNCTION_H
#define __BASEFUNCTION_H

#include "mfdvartypes.h"
#include "TransXFunction.h"
#include "planfunction.h"

class transxstate;


class basefunction : public TransXFunction
{
private:
	void switchplantype();
	class basefunction *previousfunc,*nextfunc;
	void setnextfunc(class basefunction *temp){nextfunc=temp;};
	void setpreviousfunc(class basefunction *temp){previousfunc=temp;};
	void switchmanoeuvremode();
	void switchadvanced();
	void loadplan(int plan);
	void Getmode2hypo(VECTOR3 *targetvel);
	virtual bool initialisevars();
	virtual void dolowpriaction();
	int iplantype,iplan;
	bool interceptflag;
	bool previousexists;
	class mapfunction *mappointer;
	class plan *planpointer;
	int interceptwith;
	OrbitTime mode2orbittime,deltavel;
protected:
	Intercept primary;
	OrbitElements craft, rmin, basisorbit, hypormaj, target, context;
	OBJHANDLE hcontext;
	Graph graph;
	MFDvarmoon m_target,m_minor;
	MFDvardiscrete m_planauto, m_plantype,m_planinitial,m_planthrough,m_planminor;
	MFDvardiscrete m_manoeuvremode,m_updbaseorbit;
	MFDvarfloat m_prograde,m_outwardvel,m_chplvel;
	MFDvarMJD m_ejdate;
	MFDvardiscrete m_intwith,m_graphprj,m_scale,m_advanced;
	MFDsemiintdiscrete m_orbitsahead;//Overloaded double
	double oldorbitsahead;
	int gettargettype(){return m_plantype;};
	void autoplan();
public:
	class basefunction *getpreviousfunc(){return previousfunc;};
	class basefunction *getnextfunc(){return nextfunc;};
	void basefunction::onplaceindeletebuffer();//Do not call unless you know what it's for.
	void calculate(VECTOR3 *targetvel);
	int calcnewview(int oldview,bool firststage);
	bool soistatus();
	void processvisiblevars();
	void updateplan();//Actually changes the plan
	void setplanstate(int plantype,int plan);//selects the type of plan to be carried out
	void setnextplanstate(int plantype,int plan,int targettype);
	void getplanstate(int *xplantype,int *xplan,int *targettype);
	virtual void doupdate(Sketchpad *sketchpad, int tw, int th,int viewmode);

	virtual void saveself(FILEHANDLE scn);
	virtual void restoreself(FILEHANDLE scn);
	void handlesfornextfunction(OBJHANDLE *thmajor, OBJHANDLE *thminor);
	class plan *getplanpointer(){return planpointer;};
	OrbitElements getcraftorbit(){return craft;};
	OrbitElements getpassforwardorbit();
	void getcraftorbitattarget(OrbitElements *tcraft);
	OrbitElements getcontextorbit(){return context;};//Returns copy of context orbit
	OrbitElements getminororbit(){return rmin;};
	OrbitElements getmanoeuvreorbit(){return hypormaj;};
	OrbitElements gettargetorbit(){return target;};//Returns copy of target orbit
	basefunction(class transxstate *tstate, class basefunction *tpreviousfunc,OBJHANDLE thmajor, OBJHANDLE thminor,OBJHANDLE thcraft);
	basefunction(class transxstate *tstate, class basefunction *tpreviousfunc, class basefunction *templbase, OBJHANDLE thcraft);
	~basefunction();
};

#endif