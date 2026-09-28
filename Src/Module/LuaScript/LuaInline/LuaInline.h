// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ==============================================================
//           ORBITER MODULE: LUA Inline Interpreter
//                  Part of the ORBITER SDK
//            Copyright (C) 2007-2026 Martin Schweiger
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
#ifdef __linux__
#include <thread>
#include <future>
#endif // __linux__

// ==============================================================
// class InterpreterList: interface

class InterpreterList: public oapi::Module {
public:
	struct Environment {    // interpreter environment
		Environment();
		~Environment();
		Interpreter *CreateInterpreter ();
		Interpreter *interp;  // interpreter instance
#ifndef __linux__
		HANDLE hThread;       // interpreter thread
#else // __linux__
		std::thread *hThread; // interpreter thread
		std::future<unsigned int> thExit; // not upstream: thread end, for the timed wait on the thread
#endif // __linux__
		bool termInterp;      // interpreter kill flag
		bool singleCmd;       // terminate after single command
		char *cmd;            // interpreter command
#ifndef __linux__
		static unsigned int WINAPI InterpreterThreadProc (LPVOID context);
#else // __linux__
		static unsigned int InterpreterThreadProc (void *context);
#endif // __linux__
	};

#ifndef __linux__
	InterpreterList (HINSTANCE hDLL);
#else // __linux__
	InterpreterList (void *hDLL);
#endif // __linux__
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
