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
	HINSTANCE LoadInterpreterLib();
	
private:
	Orbiter *orbiter;
	HINSTANCE hLib;
};

#endif // !__INTERPRETER_H