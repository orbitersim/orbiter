// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LUACONSOLE_H
#define __LUACONSOLE_H

#include "OrbiterAPI.h"
#include "ModuleAPI.h"
#include "ConsoleInterpreter.h"
#include <memory>

#define NLINE 100 // number of buffered lines

class LuaConsoleDlg;
enum class LineType {
	LUA_IN,
	LUA_OUT,
	LUA_OUT_ERROR,
};

class LuaConsole: public oapi::Module {
	friend class ConsoleInterpreter;
	friend class ConsoleConfig;
public:
	LuaConsole (HINSTANCE hDLL);
	~LuaConsole ();

	void clbkSimulationStart (RenderMode mode);
	void clbkSimulationEnd ();
	void clbkPreStep (double simt, double simdt, double mjd);

	HWND Open ();
	void Close ();

	void AddLine(const char *str, LineType type = LineType::LUA_OUT);
	void Clear();

private:
	static unsigned int WINAPI InterpreterThreadProc (LPVOID context);
	static void OpenDlgClbk (void *context); // called when user requests console window
	Interpreter *CreateInterpreter ();
	HANDLE hThread;    // interpreter thread handle
	bool termInterp;

	Interpreter *interp; // interpreter instance
	DWORD dwCmd;    // custom command id
	LuaConsoleDlg *hDlg;
	char cConsoleCmd[4096];
};

#endif // !__LUA_CONSOLE_H