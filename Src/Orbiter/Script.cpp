// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define OAPI_IMPLEMENTATION

#include "Script.h"
#ifdef __linux__
#include "Util.h"
#endif // __linux__

const char *path = ".";
const char *libname = "LuaInline";

ScriptInterface::ScriptInterface (Orbiter *pOrbiter)
{
	orbiter = pOrbiter;
	hLib = NULL;
}

#ifndef __linux__
HINSTANCE ScriptInterface::LoadInterpreterLib ()
#else // __linux__
void *ScriptInterface::LoadInterpreterLib ()
#endif // __linux__
{
	hLib = orbiter->LoadModule (path, libname);
	return hLib;
}

INTERPRETERHANDLE ScriptInterface::NewInterpreter ()
{
	if (!hLib && !LoadInterpreterLib()) return 0;
#ifndef __linux__
	INTERPRETERHANDLE(*proc)() = (INTERPRETERHANDLE(*)())GetProcAddress (hLib, "opcNewInterpreter");
#else // __linux__
	INTERPRETERHANDLE(*proc)() = (INTERPRETERHANDLE(*)())ModuleProc(hLib, "opcNewInterpreter");
#endif // __linux__
	if (proc) {
		INTERPRETERHANDLE hInterp = proc();
		return hInterp;
	}
	return 0;
}

int ScriptInterface::DelInterpreter (INTERPRETERHANDLE hInterp)
{
	if (!hLib && !LoadInterpreterLib()) return 0;
#ifndef __linux__
	int(*proc)(INTERPRETERHANDLE) = (int(*)(INTERPRETERHANDLE))GetProcAddress (hLib, "opcDelInterpreter");
#else // __linux__
	int(*proc)(INTERPRETERHANDLE) = (int(*)(INTERPRETERHANDLE))ModuleProc(hLib, "opcDelInterpreter");
#endif // __linux__
	if (proc) return proc(hInterp);
	else return 3;
}

INTERPRETERHANDLE ScriptInterface::RunInterpreter (const char *cmd)
{
	if (!hLib && !LoadInterpreterLib()) return NULL;
#ifndef __linux__
	INTERPRETERHANDLE(*proc)(const char*) = (INTERPRETERHANDLE(*)(const char*))GetProcAddress (hLib, "opcRunInterpreter");
#else // __linux__
	INTERPRETERHANDLE(*proc)(const char*) = (INTERPRETERHANDLE(*)(const char*))ModuleProc(hLib, "opcRunInterpreter");
#endif // __linux__
	INTERPRETERHANDLE hInterp = NULL;
	if (proc) hInterp = proc(cmd);
	return hInterp;
}

bool ScriptInterface::ExecScriptCmd (INTERPRETERHANDLE hInterp, const char *cmd)
{
	if (!hLib && !LoadInterpreterLib()) return false;
#ifndef __linux__
	bool(*proc)(INTERPRETERHANDLE,const char*) = (bool(*)(INTERPRETERHANDLE,const char*))GetProcAddress (hLib, "opcExecScriptCmd");
#else // __linux__
	bool(*proc)(INTERPRETERHANDLE,const char*) = (bool(*)(INTERPRETERHANDLE,const char*))ModuleProc(hLib, "opcExecScriptCmd");
#endif // __linux__
	if (proc) return proc(hInterp, cmd);
	else      return false;
}

bool ScriptInterface::AsyncScriptCmd (INTERPRETERHANDLE hInterp, const char *cmd)
{
	if (!hLib && !LoadInterpreterLib()) return false;
#ifndef __linux__
	bool(*proc)(INTERPRETERHANDLE,const char*) = (bool(*)(INTERPRETERHANDLE,const char*))GetProcAddress (hLib, "opcAsyncScriptCmd");
#else // __linux__
	bool(*proc)(INTERPRETERHANDLE,const char*) = (bool(*)(INTERPRETERHANDLE,const char*))ModuleProc(hLib, "opcAsyncScriptCmd");
#endif // __linux__
	if (proc) return proc(hInterp, cmd);
	else      return false;
}

lua_State *ScriptInterface::GetLua (INTERPRETERHANDLE hInterp)
{
	if (!hLib && !LoadInterpreterLib()) return NULL;
#ifndef __linux__
	lua_State*(*proc)(INTERPRETERHANDLE)=(lua_State*(*)(INTERPRETERHANDLE))GetProcAddress(hLib, "opcGetLua");
#else // __linux__
	lua_State*(*proc)(INTERPRETERHANDLE)=(lua_State*(*)(INTERPRETERHANDLE))ModuleProc(hLib, "opcGetLua");
#endif // __linux__
	if (proc) return proc(hInterp);
	else      return NULL;
}
