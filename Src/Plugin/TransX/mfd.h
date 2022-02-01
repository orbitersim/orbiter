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

#ifndef __MFDGLOBALS_H
#define __MFDGLOBALS_H


// Needed due to the implementation of the selection functions in Orbiter
bool SelectVariableBody(void *id, char *str, void *usrdata);
bool SelectVariableFloat(void *id, char *str, void *usrdata);
bool SelectVariableAngle(void *id, char *str, void *usrdata); // hurr
DLLCLBK void opcDLLInit (HINSTANCE hDLL);
DLLCLBK void opcDLLExit (HINSTANCE hDLL);
double cosangle(const VECTOR3 &veca,const VECTOR3 &vecb);
double length2my(const VECTOR3 &vector);
void getinvrotmatrix(VECTOR3 arot, MATRIX3 *invrotmatrix);
MATRIX3 getinvmatrix(const MATRIX3 mat);
double getdeterminant(const MATRIX3 mat);
VECTOR3 GetRotationToTarget(VESSEL * vessel, const VECTOR3 & target);
double GetBurnTime(VESSEL *vessel, double deltaV);
double GetBurnStart(VESSEL *vessel, THGROUP_TYPE thGroupType, double instantaneousBurnTime, double deltaV);

// Standard formatting function
void TextShow(oapi::Sketchpad *sketchpad, const char *label, int wpos, int hpos, double value);
void TextShow(oapi::Sketchpad *sketchpad, const char *label, int wpos, int hpos, OBJHANDLE handle);
void TextForm(char *buffer,const char *label,double value);

const double GRAVITY=6.67259e-11; // Gravitational constant

#endif
