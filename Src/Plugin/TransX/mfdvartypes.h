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

#ifndef __MFDVARTYPES_H
#define __MFDVARTYPES_H

#include "mapfunction.h"
#include "mfdvariable.h"
#include "doublelink.h"
#include <memory>

#define MAX_STRING_LENGTH	40

class liststring : public listelement
{
	char buffer[MAX_STRING_LENGTH];
public:
	char *getbuffer(){return buffer;};//returns pointer to actual buffer.
	liststring(bool manageme = true);
};

class MFDvarshiplist : public MFDvariable
{
private:
	class dbllist shiplisthead;//Used to contain the ship names
	class dblliter *iterator;//Iterator for moving through list
	bool initialised;//States whether this variable has been initialised yet or not
public:
	void initbybody(OBJHANDLE craft,bool reset);//Used to create list to select from
	void addtolist(char *name);
	virtual bool show(oapi::Sketchpad *sketchpad, int width, int line);
	virtual OBJHANDLE gethandle() const;
	virtual void showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const{};
	virtual void ch_adjmode(){};
	virtual void chm_adjmode(){};
	virtual void enter_variable();
	virtual void inc_variable();
	virtual void dec_variable();
	void init(MFDvarhandler *vars,int viewmode1, int viewmode2, char *vname);
	MFDvarshiplist();
	virtual ~MFDvarshiplist();

	virtual void getsaveline(char *buffer) const{};//This MFDvariable type doesn't support load/save
	virtual bool loadvalue(char *buffer){return true;};//
protected:
	virtual void InheritValues(MFDvariable *var) {};	// do nothing
};

class MFDvarmoon : public MFDvariable {
protected:
	enum AdjustMode
	{
		Planet,
		Craft
	};
	OBJHANDLE target;
	OBJHANDLE centralbody;
	class mapfunction *mappointer;
	int value;
	char intbuffer[30];
	AdjustMode adjMode;
public:
	virtual void enter_variable();
	virtual void inc_variable();
	virtual void dec_variable();
	virtual void setall(class MFDvariable *var);
	bool validate();
	void initvalidate();
	virtual bool show(oapi::Sketchpad *sketchpad, int width, int line);
	virtual void ch_adjmode();					// toggles between craft and planet/moon
	virtual void chm_adjmode() {ch_adjmode();};	// toggles between craft and planet/moon
	virtual void showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const;
	virtual int getvalue() const;
	virtual OBJHANDLE gethandle() const;
	virtual bool loadvalue(char *buffer);
	virtual bool SetVariableBody(char *str);
	virtual void getsaveline(char *buffer) const;
	void updatecentralbody(OBJHANDLE tcentral){centralbody=tcentral;};
	operator int(){return value;}
	void init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname,OBJHANDLE tcentralbody);
protected:
	virtual void InheritValues(MFDvariable *var) {value = ((MFDvarmoon*)var)->value;};
};

class MFDvarfloat : public MFDvariable {
protected:
	enum AdjustMode
	{
		Rough,
		Coarse,
		Medium,
		Fine,
		Super,
		Ultra,
		Hyper,
		Micro,
		AutoMin,
		Reset
	};

	double value,defaultvalue; // Value of the variable
	double min; // Minimum legal value
	double max; //Maximum legal value
	double increment; //Increment fraction
	double logborder; // Number below which increment is linear scaled
	double inputvalue;
	AdjustMode adjMode;
    double GetAdjuster();
    bool IsAdjusterSpecialCase();

public:
	operator double() {return value;};
	double operator = (double tvalue){value=tvalue;return value;};
	virtual bool floatvalidate(char * str, double * dfinal, double valcurrent, double valdefault); // GDI
	virtual bool SetVariableFloat(char *str);
	virtual void enter_variable();
	virtual void inc_variable(); // Increase the variable
	virtual void dec_variable(); //Decrease the variable
	virtual void ch_adjmode();
	virtual void chm_adjmode();
	virtual void showadjustment(oapi::Sketchpad *sketchpad, int width, int line) const;
	bool show(oapi::Sketchpad *sketchpad, int width, int line);
	double getvalue() const; //Get the value
	void setvalue(double tvalue);
	virtual void getsaveline(char *buffer) const;
	virtual bool loadvalue(char *buffer);
	void init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname, double vvalue, double vmin, double vmax, double vincrement, double vlogborder);
	bool ShouldBeOptimised(); // Could be optimised actively, or passively, through the date
	MFDvarfloat();
	~MFDvarfloat();
private:
    double CalcAdjustedValue(bool positive, double adjuster);

protected:
	virtual void InheritValues(MFDvariable *var) {value = ((MFDvarfloat*)var)->value;};
};

class MFDvarMJD: public MFDvarfloat {
public:
	bool show(oapi::Sketchpad *sketchpad, int width, int line);
	virtual void inc_variable(); // Increase the variable
	virtual void dec_variable(); //Decrease the variable

	double operator = (double tvalue){value=tvalue;return value;};
private:
    void CalcAdjustedValue(bool positive);
};

class MFDvardiscrete: public MFDvariable {
private:
	char label[5][15]; //Label array
	int value; // Variable indicating the projection type
	int limit; // Highest permissible value - Note - 5 labels MAX!!
public:
	void inc_variable(); //Change to next projection
	operator int() {return value;};
	int operator = (int tvalue){value=tvalue;return value;};
	bool show(oapi::Sketchpad *sketchpad, int width, int line);
	virtual void getsaveline(char *buffer) const;
	virtual bool loadvalue(char *buffer);
	void init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname, int vvalue, int vlimit, char *st1, char *st2, char *st3, char *st4, char *st5);
protected:
	virtual void InheritValues(MFDvariable *var) {value = ((MFDvardiscrete*)var)->value;};
};

class MFDsemiintdiscrete: public MFDvariable {
	int value;//variable indicating twice the actual value
public:
	virtual void inc_variable() {value++;};
	virtual void dec_variable() {if (value>0) value--;};
	operator double(){return value*0.5;};
	double operator = (double tvalue){value=int(2.0*tvalue);return value;};
	virtual void getsaveline(char *buffer) const;
	virtual bool loadvalue(char *buffer);
	void init(MFDvarhandler *vars,int viewmode1,int viewmode2,char *vname,int tvalue);
	virtual bool show(oapi::Sketchpad *sketchpad, int width, int line);
	MFDsemiintdiscrete(){value=0;};
protected:
	virtual void InheritValues(MFDvariable *var) {value = ((MFDsemiintdiscrete*)var)->value;};
};

class MFDvarangle: public MFDvarfloat {
private:
	bool loop; //If true, then loops from min to max
public:
	virtual bool SetVariableAngle(char *str); // Must we?...
	virtual void enter_variable();
	double operator = (double tvalue){setvalue(tvalue);return tvalue;};
	virtual void inc_variable(); // Increase the variable
	virtual void dec_variable(); //Decrease the variable
	bool show(oapi::Sketchpad *sketchpad, int width, int line);
	double getsin() const;
	double getcos() const;
	void init(MFDvarhandler *vars,char *vname, bool vloop);
};

#endif
