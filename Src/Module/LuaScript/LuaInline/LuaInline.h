// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//           ORBITER MODULE: LUA Inline Interpreter
//                  Part of the ORBITER SDK
//            Copyright (C) 2007 Martin Schweiger
//                   All rights reserved
//
// LuaInline.h
// This library is loaded by the Orbiter core on demand to provide
// interpreter instances to modules via API requests.
//
// Notes:
// * LuaInline.dll must be placed in the Orbiter root directory
//   (not in the Modules subdirectory). It is loaded automatically
//   by Orbiter when required. It must not be loaded manually via
//   the Launchpad "Modules" tab.
// * LuaInline.dll depends on LuaInterpreter.dll and Lua5.1.dll
//   which must also be present in the Orbiter root directory.
// ==============================================================

#ifndef __LUAINLINE_H
#define __LUAINLINE_H

#include "Interpreter.h"

// ==============================================================
// class InterpreterList: interface

class InterpreterList: public oapi::Module {
public:
	struct Environment {    // interpreter environment
		Environment();
		~Environment();
		Interpreter *CreateInterpreter ();
		Interpreter *interp;  // interpreter instance
		bool termInterp;      // interpreter kill flag
	};

	InterpreterList (HINSTANCE hDLL);
	~InterpreterList ();

	void clbkSimulationEnd () override;
	void clbkPostStep (double simt, double simdt, double mjd) override;
	void clbkSimulationStart (RenderMode mode) override;
	void clbkDeleteVessel (OBJHANDLE hVessel) override;
	
	Environment *AddInterpreter ();
	int DelInterpreter (Environment *env);

private:

	Environment **list;     // interpreter list
	DWORD nlist;            // list size
	DWORD nbuf;             // buffer size
};

#endif // !__LUAINLINE_H