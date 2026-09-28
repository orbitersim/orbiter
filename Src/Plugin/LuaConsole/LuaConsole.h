// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LUACONSOLE_H
#define __LUACONSOLE_H

#include "OrbiterAPI.h"
#include "ModuleAPI.h"
#include "ConsoleInterpreter.h"
#include <memory>
#ifdef __linux__
#include <thread>
#include <future>
#endif // __linux__

#define NLINE 100 // number of buffered lines

class LuaConsoleDlg;
#ifdef __linux__
class ConsoleConfig; // g++ doesn't take the friend declaration below as a declaration (MSVC does)
#endif // __linux__
enum class LineType {
	LUA_IN,
	LUA_OUT,
	LUA_OUT_ERROR,
};

class LuaConsole: public oapi::Module {
	friend class ConsoleInterpreter;
	friend class ConsoleConfig;

public:
#ifndef __linux__
	LuaConsole (HINSTANCE hDLL);
#else // __linux__
	LuaConsole (void *hDLL);
#endif // __linux__
	~LuaConsole ();

	void clbkSimulationStart (RenderMode mode);
	void clbkSimulationEnd ();
	void clbkPreStep (double simt, double simdt, double mjd);

#ifndef __linux__
	HWND Open ();
#else // __linux__
	QWidget *Open ();
#endif // __linux__
	void Close ();

	void AddLine(const char *str, LineType type = LineType::LUA_OUT);
	void Clear();

private:
#ifndef __linux__
	static unsigned int WINAPI InterpreterThreadProc (LPVOID context);
#else // __linux__
	static unsigned int InterpreterThreadProc (void *context);
#endif // __linux__
	static void OpenDlgClbk (void *context); // called when user requests console window
	Interpreter *CreateInterpreter ();
#ifndef __linux__
	HANDLE hThread;    // interpreter thread handle
#else // __linux__
	std::thread *hThread; // interpreter thread handle
	std::future<unsigned int> thExit; // not upstream: thread end, for the timed wait on the thread
#endif // __linux__
	bool termInterp;

	Interpreter *interp; // interpreter instance
	DWORD dwCmd;    // custom command id
	int dwMenuCmd;    // custom command id
	LuaConsoleDlg *hDlg;
	char cConsoleCmd[4096];
};

#endif // !__LUA_CONSOLE_H
