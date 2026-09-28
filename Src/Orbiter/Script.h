// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __SCRIPT_H
#define __SCRIPT_H

#include "Orbiter.h"

class ScriptInterface {
public:
	ScriptInterface (Orbiter *pOrbiter);
	INTERPRETERHANDLE NewInterpreter();
	int DelInterpreter (INTERPRETERHANDLE);
	INTERPRETERHANDLE RunInterpreter (const char *cmd);
	bool ExecScriptCmd (INTERPRETERHANDLE hInterp, const char *cmd);
	bool AsyncScriptCmd (INTERPRETERHANDLE hInterp, const char *cmd);
	lua_State *GetLua (INTERPRETERHANDLE hInterp);

protected:
#ifndef __linux__
	HINSTANCE LoadInterpreterLib();
#else // __linux__
	void *LoadInterpreterLib(); // HINSTANCE -> dlopen handle
#endif // __linux__
	
private:
	Orbiter *orbiter;
#ifndef __linux__
	HINSTANCE hLib;
#else // __linux__
	void *hLib;
#endif // __linux__
};

#endif // !__INTERPRETER_H
