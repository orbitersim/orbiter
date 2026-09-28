// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __MFDINTERPRETER_H
#define __MFDINTERPRETER_H

#include "Interpreter.h"
#ifdef __linux__
#include <thread>
#include <future>
#endif // __linux__

#define NCHAR 80 // characters per line in console buffer
#define NLINE 50 // number of buffered lines

class InterpreterList;

// ==============================================================
// MFD interpreter class interface

class MFDInterpreter: public Interpreter {
public:
	struct LineSpec {
		char buf[NCHAR];
		COLORREF col;
		LineSpec *prev, *next;
	};

	MFDInterpreter ();
	void SetSelf (OBJHANDLE hV);
	void LoadAPI();
	void AddLine (const char *line, COLORREF col);
	inline LineSpec *FirstLine() const { return lineFirst; }
	inline DWORD LineCount() const { return nline; }
	void term_strout (const char *str, bool iserr=false);
	void term_clear ();

protected:
	static int termOut (lua_State *L);
	static int termLineUp (lua_State *L);
	static int termSetVerbosity (lua_State *L);
	static int termClear (lua_State *L);

private:
	LineSpec *lineFirst, *lineLast;
	DWORD nline;
};

// ==============================================================
// Interpreter repository

class InterpreterList {
public:
	struct Environment {
		Environment (OBJHANDLE hV);
		~Environment();
		MFDInterpreter *CreateInterpreter (OBJHANDLE hV);
		MFDInterpreter *interp;
#ifndef __linux__
		HANDLE hThread;
#else // __linux__
		std::thread *hThread;
		std::future<unsigned int> thExit; // not upstream: thread end, for the timed wait on the thread
#endif // __linux__
		char cmd[1024];
#ifndef __linux__
		static unsigned int WINAPI InterpreterThreadProc (LPVOID context);
#else // __linux__
		static unsigned int InterpreterThreadProc (void *context);
#endif // __linux__
	};
	struct VesselInterp {
		OBJHANDLE hVessel;
		Environment **env;
		DWORD nenv;
	} *list;
	DWORD nlist;
	DWORD nbuf;

	InterpreterList();
	~InterpreterList();

	void Update (double simt, double simdt, double mjd);

	Environment *AddInterpreter (OBJHANDLE hV);
	bool DeleteInterpreter (OBJHANDLE hV, DWORD idx);
	bool DeleteInterpreters (OBJHANDLE hV);
	void DeleteList ();
	VesselInterp *FindVesselInterp (OBJHANDLE hV);
	Environment *FindInterpreter (OBJHANDLE hV, DWORD idx);
	DWORD InterpreterCount (OBJHANDLE hV);
};

#endif // !__MFDINTERPRETER_H
