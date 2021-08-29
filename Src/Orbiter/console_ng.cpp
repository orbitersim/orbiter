// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "console_ng.h"
#include "Orbiter.h"
#include "DlgMgr.h"
#include "Psys.h"
#include "Vessel.h"
#include "Log.h"
#include "DlgFocus.h"
#include "DlgMap.h"
#include "DlgInfo.h"
#include "DlgTacc.h"
#include "DlgFunction.h"
#include "DlgRecorder.h"
#include "DlgHelp.h"
#include "ConsoleManager.h"
#include "resource.h"

extern PlanetarySystem* g_psys;
extern Vessel* g_focusobj;
extern TimeData td;

static DWORD WINAPI InputProc(LPVOID);
static INT_PTR CALLBACK ServerDlgProc(HWND, UINT, WPARAM, LPARAM);
static void ConsoleOut(const char* msg);

static HANDLE hMutex = 0;
static HANDLE s_hStdO = NULL;
static char cConsoleCmd[1024] = "\0";
static orbiter::ConsoleNG* s_console = NULL; // access to console instance from message callback functions

orbiter::ConsoleNG::ConsoleNG(Orbiter* pOrbiter)
    : m_pOrbiter(pOrbiter)
    , m_hWnd(NULL)
    , m_hStatWnd(NULL)
    , m_hThread(NULL)
{
    static const PSTR title = "Orbiter Server Console";
    static SIZE_T stackSize = 4096;

    s_console = this;

    ConsoleManager::ShowConsole(true);
    DWORD id;
    SetConsoleTitle(title);
    m_hWnd = GetConsoleWindow();
    m_hThread = CreateThread(NULL, stackSize, InputProc, this, 0, &id);
    s_hStdO = GetStdHandle(STD_OUTPUT_HANDLE);
    SetLogOutFunc(&ConsoleOut); // clone log output to console
}

orbiter::ConsoleNG::~ConsoleNG()
{
	DestroyStatDlg();
	SetLogOutFunc(0);
	if (m_hThread) {
		TerminateThread(m_hThread, 0);
	}
	s_console = NULL;
	s_hStdO = NULL;
}

bool orbiter::ConsoleNG::ParseCmd()
{
	if (!cConsoleCmd[0]) return false;
	char cmd[1024], cbuf[256], * pc, * ppc;

	WaitForSingleObject(hMutex, 1000);
	strcpy(cmd, cConsoleCmd + 1);
	cConsoleCmd[0] = '\0';
	ReleaseMutex(hMutex);

	DWORD i;
	if (!_strnicmp(cmd, "help", 4)) {
		pc = trim_string(cmd + 4);
		if (!_strnicmp(pc, "help", 4)) {
			Echo("Brief onscreen help for console commands.");
			Echo("Type \"help\" followed by a top-level command to get information for this command.");
		}
		else if (!_strnicmp(pc, "exit", 4)) {
			Echo("Exits the simulation session and returns to the Launchpad dialog.");
		}
		else if (!_strnicmp(pc, "vessel", 6)) {
			ppc = trim_string(pc + 6);
			if (!_strnicmp(ppc, "list", 4)) {
				Echo("Lists all vessels in the current session.");
			}
			else if (!_strnicmp(ppc, "count", 5)) {
				Echo("Prints the number of vessels in the current session.");
			}
			else if (!_strnicmp(ppc, "focus", 5)) {
				Echo("Prints the name of the current focus vessel.");
			}
			else if (!_strnicmp(ppc, "del", 3)) {
				Echo("vessel del <name> -- Destroy vessel <name>.");
			}
			else {
				Echo("Vessel-specific commands. The following sub-commands are recognized:\n");
				Echo("list count focus del\n");
				Echo("Type \"help vessel <subcommand>\" to get information for a command.");
			}
		}
		else if (!_strnicmp(pc, "time", 4)) {
			Echo("Output current simulation time.");
			Echo("time syst  --  Session up time (seconds)");
			Echo("time simt  --  Simulation time (seconds)");
			Echo("time mjd   --  Absolute simulation time (MJD format)");
			Echo("time ut    --  Absolute simulation time (UT format)");
			Echo("Without arguments, all 4 time values are displayed.");
		}
		else if (!_strnicmp(pc, "tacc", 4)) {
			Echo("Display or set time acceleration factor.");
			Echo("tacc <x>  --  Set new time acceleration factor x.");
			Echo("Without argument, prints the current time acceleration factor.");
		}
		else if (!_strnicmp(pc, "pause", 5)) {
			Echo("Pause/resume simulation session.");
			Echo("pause on      --  pause simulation");
			Echo("pause off     --  resume simulation");
			Echo("pause toggle  --  toggle pause/resume state");
			Echo("Without arguments, the current simulation state is displayed.");
		}
		else if (!_strnicmp(pc, "step", 4)) {
			Echo("Display momentary simulation step length and steps per second.");
		}
		else if (!_strnicmp(pc, "dlg", 3)) {
			Echo("Open a dialog.");
			Echo("dlg focus    -- Open the vessel selction dialog");
			Echo("dlg map      -- Open the map window");
			Echo("dlg info     -- Open the object info dialog");
			Echo("dlg tacc     -- Open the time acceleration dialog");
			Echo("dlg help     -- Open the help dialog");
			Echo("dlg record   -- Open the flight recorder dialog");
			Echo("dlg function -- Open the plugin function list");
		}
		else if (!_strnicmp(pc, "gui", 3)) {
			Echo("Toggles the display of a dialog box that continuously monitors the simulation");
			Echo("state.");
		}
		else {
			Echo("The following top-level commands are available:\n");
			Echo("  help exit vessel time tacc pause step dlg gui\n");
			Echo("To get help for a command, type \"help <cmd>\"");
		}
	}
	else if (!_strnicmp(cmd, "exit", 4)) {
		m_pOrbiter->CloseSession();
		return true;
	}
	else if (!_strnicmp(cmd, "vessel", 6)) {
		pc = trim_string(cmd + 6);
		if (!_strnicmp(pc, "list", 4)) {
			for (i = 0; i < g_psys->nVessel(); i++)
				Echo(g_psys->GetVessel(i)->Name());
			return true;
		}
		else if (!_strnicmp(pc, "count", 5)) {
			_itoa(g_psys->nVessel(), cbuf, 10);
			Echo(cbuf);
		}
		else if (!_strnicmp(pc, "focus", 5)) {
			Echo(g_focusobj->Name());
		}
		else if (!_strnicmp(pc, "del", 3)) {
			Vessel* v = g_psys->GetVessel(trim_string(pc + 3), true);
			if (v) v->RequestDestruct();
		}
	}
	else if (!_strnicmp(cmd, "tacc", 4)) {
		double w;
		if (sscanf(trim_string(cmd + 4), "%lf", &w) == 1)
			m_pOrbiter->SetWarpFactor(w);
		else {
			sprintf(cbuf, "Time acceleration is %0.1f", td.Warp());
			Echo(cbuf);
		}
	}
	else if (!_strnicmp(cmd, "time", 4)) {
		pc = trim_string(cmd + 4);
		if (!_strnicmp(pc, "simt", 4)) {
			sprintf(cbuf, "%0.1f", td.SimT0);
		}
		else if (!_strnicmp(pc, "syst", 4)) {
			sprintf(cbuf, "%0.1f", td.SysT0);
		}
		else if (!_strnicmp(pc, "mjd", 3)) {
			sprintf(cbuf, "%0.6f", td.MJD0);
		}
		else if (!_strnicmp(pc, "ut", 2)) {
			strcpy(cbuf, DateStr(td.MJD0));
		}
		else {
			sprintf(cbuf, "SysT=%0.1f SimT=%0.1f, MJD=%0.6f, UT=%s", td.SysT0, td.SimT0, td.MJD0, DateStr(td.MJD0));
		}
		Echo(cbuf);
	}
	else if (!_strnicmp(cmd, "pause", 5)) {
		pc = trim_string(cmd + 5);
		if (!_strnicmp(pc, "on", 2)) m_pOrbiter->Pause(true);
		else if (!_strnicmp(pc, "off", 3)) m_pOrbiter->Pause(false);
		else if (!_strnicmp(pc, "toggle", 6)) m_pOrbiter->TogglePause();
		sprintf_s(cbuf, 256, "Simulation %s", m_pOrbiter->IsRunning() ? "running" : "paused");
		Echo(cbuf);
	}
	else if (!_strnicmp(cmd, "step", 4)) {
		sprintf_s(cbuf, 256, "dt=%f, FPS=%f", td.SimDT, td.FPS());
		Echo(cbuf);
	}
	else if (!_strnicmp(cmd, "gui", 3)) {
		if (!DestroyStatDlg())
			m_hStatWnd = CreateDialog(m_pOrbiter->GetInstance(), MAKEINTRESOURCE(IDD_SERVER), m_hWnd, ServerDlgProc);
	}
	else if (!_strnicmp(cmd, "dlg", 3)) {
		DialogManager* pDlgMgr = m_pOrbiter->DlgMgr();
		if (pDlgMgr) {
			pc = trim_string(cmd + 3);
			if (!_strnicmp(pc, "focus", 5))
				pDlgMgr->EnsureEntry<DlgFocus>();
			else if (!_strnicmp(pc, "map", 3))
				pDlgMgr->EnsureEntry<DlgMap>();
			else if (!_strnicmp(pc, "info", 4))
				pDlgMgr->EnsureEntry<DlgInfo>();
			else if (!_strnicmp(pc, "tacc", 4))
				pDlgMgr->EnsureEntry<DlgTacc>();
			else if (!_strnicmp(pc, "function", 8))
				pDlgMgr->EnsureEntry<DlgFunction>();
			else if (!_strnicmp(pc, "record", 6))
				pDlgMgr->EnsureEntry<DlgRecorder>();
			else if (!_strnicmp(pc, "help", 4))
				pDlgMgr->EnsureEntry<DlgHelp>();
		}
	}
	return false;
}

void orbiter::ConsoleNG::Echo(const char* str) const
{
	ConsoleOut(str);
}

void orbiter::ConsoleNG::EchoIntro() const
{
	Echo("-----------------\nOrbiter NG (no graphics)");
	Echo("Running in server mode (no graphics client attached).");
	Echo("Type \"help\" for a list of commands.\n");
}

bool orbiter::ConsoleNG::DestroyStatDlg()
{
	if (m_hStatWnd) {
		DestroyWindow(m_hStatWnd);
		m_hStatWnd = NULL;
		return true;
	}
	else
		return false;
}


DWORD WINAPI InputProc(LPVOID context)
{
	DWORD count, c;
	char cbuf[1024];
	HANDLE hStdI = GetStdHandle(STD_INPUT_HANDLE);
	HANDLE hStdO = GetStdHandle(STD_OUTPUT_HANDLE);
	orbiter::ConsoleNG* console = (orbiter::ConsoleNG*)context;
	SetConsoleMode(hStdI, ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
	SetConsoleTextAttribute(hStdI, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
	hMutex = CreateMutex(NULL, FALSE, NULL);
	for (;;) {
		ReadConsole(hStdI, cbuf, 1024, &count, NULL);
		WriteConsole(hStdO, "> ", 2, &c, NULL);

		WaitForSingleObject(hMutex, 1000);
		cConsoleCmd[0] = 'x';
		memcpy(cConsoleCmd + 1, cbuf, count);
		cConsoleCmd[count - 1] = '\0'; // eliminates CR
		ReleaseMutex(hMutex);
	}
	return 0;
}

INT_PTR CALLBACK ServerDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		SetTimer(hDlg, 1, 1000, NULL);
		return TRUE;
	case WM_TIMER:
		if (s_console)
			s_console->GetOrbiter()->UpdateServerWnd(hDlg);
		return 0;
	case WM_COMMAND:
		switch (LOWORD(wParam)) {
		case IDOK:
			if (s_console)
				s_console->GetOrbiter()->CloseSession();
		}
		break;
	case WM_CLOSE:
		if (s_console)
			s_console->DestroyStatDlg();
		return 0;
	case WM_DESTROY:
		KillTimer(hDlg, 1);
		return 0;
	}
	return FALSE;
}

void ConsoleOut(const char* msg)
{
	if (!s_hStdO) return;
	DWORD count;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	SetConsoleTextAttribute(s_hStdO, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
	GetConsoleScreenBufferInfo(s_hStdO, &csbi);
	csbi.dwCursorPosition.X = 0;
	SetConsoleCursorPosition(s_hStdO, csbi.dwCursorPosition);
	WriteConsole(s_hStdO, msg, strlen(msg), &count, NULL);
	SetConsoleTextAttribute(s_hStdO, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
	WriteConsole(s_hStdO, "\n> ", 3, &count, NULL);
}
