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
#ifdef __linux__
#include "ResDialog.h"
#include <QKeyEvent>
#include <QTimer>
#include <mutex>
#include <poll.h>
#include <unistd.h>
#include <errno.h>
#endif // __linux__

extern PlanetarySystem* g_psys;
extern Vessel* g_focusobj;
extern TimeData td;

#ifndef __linux__
static DWORD WINAPI InputProc(LPVOID);
static INT_PTR CALLBACK ServerDlgProc(HWND, UINT, WPARAM, LPARAM);
#else // __linux__
static void InputProc(std::atomic<bool>* stop);
static void ConsoleLine(const char* line);
static void ServerDlgProc(QWidget* hDlg, void* context);
#endif // __linux__
static void ConsoleOut(const char* msg);

#ifndef __linux__
static HANDLE hMutex = 0;
static HANDLE s_hStdO = NULL;
#else // __linux__
static std::mutex hMutex;
static FILE* s_hStdO = NULL;
#endif // __linux__
static char cConsoleCmd[1024] = "\0";
static orbiter::ConsoleNG* s_console = NULL; // access to console instance from message callback functions

orbiter::ConsoleNG::ConsoleNG(Orbiter* pOrbiter)
    : m_pOrbiter(pOrbiter)
    , m_hWnd(NULL)
    , m_hStatWnd(NULL)
#ifndef __linux__
    , m_hThread(NULL)
#else // __linux__
    , m_stop(false)
#endif // __linux__
{
    static PCSTR title = "Orbiter Server Console";
#ifndef __linux__
    static SIZE_T stackSize = 4096;
#endif // !__linux__

    s_console = this;

    ConsoleManager::ShowConsole(true);
#ifndef __linux__
    DWORD id;
    SetConsoleTitle(title);
    m_hWnd = GetConsoleWindow();
	if (ConsoleManager::IsConsoleExclusive())
		DeleteMenu(GetSystemMenu(m_hWnd, false), SC_CLOSE, MF_BYCOMMAND);
    m_hThread = CreateThread(NULL, stackSize, InputProc, this, 0, &id);
    s_hStdO = GetStdHandle(STD_OUTPUT_HANDLE);
#else // __linux__
    if (isatty(STDOUT_FILENO)) {
        printf("\033]0;%s\007", title); // SetConsoleTitle
        fflush(stdout);
    }
    ConsoleManager::SetConsoleTitle(title);
    m_hWnd = ConsoleManager::ConsoleWindow(); // NULL on a terminal; Orbiter's console window has no close button
    ConsoleManager::SetConsoleInput(&ConsoleLine);
    m_thread = std::thread(InputProc, &m_stop);
    s_hStdO = stdout;
#endif // __linux__
    SetLogOutFunc(&ConsoleOut); // clone log output to console
}

orbiter::ConsoleNG::~ConsoleNG()
{
	DestroyStatDlg();
	SetLogOutFunc(0);
#ifndef __linux__
	if (WaitForSingleObject(m_hThread, 1000) == WAIT_TIMEOUT) {
		TerminateThread(m_hThread, 0);
	}
	if (hMutex) {
		CloseHandle(hMutex);
		hMutex = 0;
#else // __linux__
	ConsoleManager::SetConsoleInput(NULL);
	m_stop = true; // the thread polls stdin and exits within 200 ms
	if (m_thread.joinable())
		m_thread.join();
	if (isatty(STDOUT_FILENO)) {
		fputs("\033[0m", stdout); // leave the terminal in its default colours
		fflush(stdout);
#endif // __linux__
	}
	s_console = NULL;
	s_hStdO = NULL;
}

bool orbiter::ConsoleNG::ParseCmd()
{
	if (!cConsoleCmd[0]) return false;
	char cmd[1024], cbuf[256], * pc, * ppc;

#ifndef __linux__
	WaitForSingleObject(hMutex, 1000);
	strcpy(cmd, cConsoleCmd + 1);
	cConsoleCmd[0] = '\0';
	ReleaseMutex(hMutex);
#else // __linux__
	{
		std::lock_guard<std::mutex> lock(hMutex);
		strcpy(cmd, cConsoleCmd + 1);
		cConsoleCmd[0] = '\0';
	}
#endif // __linux__

	DWORD i;
#ifndef __linux__
	if (!_strnicmp(cmd, "help", 4)) {
#else // __linux__
	if (!strncasecmp(cmd, "help", 4)) {
#endif // __linux__
		pc = trim_string(cmd + 4);
#ifndef __linux__
		if (!_strnicmp(pc, "help", 4)) {
#else // __linux__
		if (!strncasecmp(pc, "help", 4)) {
#endif // __linux__
			Echo("Brief onscreen help for console commands.");
			Echo("Type \"help\" followed by a top-level command to get information for this command.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "exit", 4)) {
#else // __linux__
		else if (!strncasecmp(pc, "exit", 4)) {
#endif // __linux__
			Echo("Exits the simulation session and returns to the Launchpad dialog.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "vessel", 6)) {
#else // __linux__
		else if (!strncasecmp(pc, "vessel", 6)) {
#endif // __linux__
			ppc = trim_string(pc + 6);
#ifndef __linux__
			if (!_strnicmp(ppc, "list", 4)) {
#else // __linux__
			if (!strncasecmp(ppc, "list", 4)) {
#endif // __linux__
				Echo("Lists all vessels in the current session.");
			}
#ifndef __linux__
			else if (!_strnicmp(ppc, "count", 5)) {
#else // __linux__
			else if (!strncasecmp(ppc, "count", 5)) {
#endif // __linux__
				Echo("Prints the number of vessels in the current session.");
			}
#ifndef __linux__
			else if (!_strnicmp(ppc, "focus", 5)) {
#else // __linux__
			else if (!strncasecmp(ppc, "focus", 5)) {
#endif // __linux__
				Echo("Prints the name of the current focus vessel.");
			}
#ifndef __linux__
			else if (!_strnicmp(ppc, "del", 3)) {
#else // __linux__
			else if (!strncasecmp(ppc, "del", 3)) {
#endif // __linux__
				Echo("vessel del <name> -- Destroy vessel <name>.");
			}
			else {
				Echo("Vessel-specific commands. The following sub-commands are recognized:\n");
				Echo("list count focus del\n");
				Echo("Type \"help vessel <subcommand>\" to get information for a command.");
			}
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "time", 4)) {
#else // __linux__
		else if (!strncasecmp(pc, "time", 4)) {
#endif // __linux__
			Echo("Output current simulation time.");
			Echo("time syst  --  Session up time (seconds)");
			Echo("time simt  --  Simulation time (seconds)");
			Echo("time mjd   --  Absolute simulation time (MJD format)");
			Echo("time ut    --  Absolute simulation time (UT format)");
			Echo("Without arguments, all 4 time values are displayed.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "tacc", 4)) {
#else // __linux__
		else if (!strncasecmp(pc, "tacc", 4)) {
#endif // __linux__
			Echo("Display or set time acceleration factor.");
			Echo("tacc <x>  --  Set new time acceleration factor x.");
			Echo("Without argument, prints the current time acceleration factor.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "pause", 5)) {
#else // __linux__
		else if (!strncasecmp(pc, "pause", 5)) {
#endif // __linux__
			Echo("Pause/resume simulation session.");
			Echo("pause on      --  pause simulation");
			Echo("pause off     --  resume simulation");
			Echo("pause toggle  --  toggle pause/resume state");
			Echo("Without arguments, the current simulation state is displayed.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "step", 4)) {
#else // __linux__
		else if (!strncasecmp(pc, "step", 4)) {
#endif // __linux__
			Echo("Display momentary simulation step length and steps per second.");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "dlg", 3)) {
#else // __linux__
		else if (!strncasecmp(pc, "dlg", 3)) {
#endif // __linux__
			Echo("Open a dialog.");
			Echo("dlg focus    -- Open the vessel selction dialog");
			Echo("dlg map      -- Open the map window");
			Echo("dlg info     -- Open the object info dialog");
			Echo("dlg tacc     -- Open the time acceleration dialog");
			Echo("dlg help     -- Open the help dialog");
			Echo("dlg record   -- Open the flight recorder dialog");
			Echo("dlg function -- Open the plugin function list");
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "gui", 3)) {
#else // __linux__
		else if (!strncasecmp(pc, "gui", 3)) {
#endif // __linux__
			Echo("Toggles the display of a dialog box that continuously monitors the simulation");
			Echo("state.");
		}
		else {
			Echo("The following top-level commands are available:\n");
			Echo("  help exit vessel time tacc pause step dlg gui\n");
			Echo("To get help for a command, type \"help <cmd>\"");
		}
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "exit", 4)) {
#else // __linux__
	else if (!strncasecmp(cmd, "exit", 4)) {
#endif // __linux__
		m_pOrbiter->CloseSession();
		return true;
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "vessel", 6)) {
#else // __linux__
	else if (!strncasecmp(cmd, "vessel", 6)) {
#endif // __linux__
		pc = trim_string(cmd + 6);
#ifndef __linux__
		if (!_strnicmp(pc, "list", 4)) {
#else // __linux__
		if (!strncasecmp(pc, "list", 4)) {
#endif // __linux__
			for (i = 0; i < g_psys->nVessel(); i++)
				Echo(g_psys->GetVessel(i)->Name());
			return true;
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "count", 5)) {
#else // __linux__
		else if (!strncasecmp(pc, "count", 5)) {
#endif // __linux__
			sprintf(cbuf, "%zu", g_psys->nVessel());
			Echo(cbuf);
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "focus", 5)) {
#else // __linux__
		else if (!strncasecmp(pc, "focus", 5)) {
#endif // __linux__
			Echo(g_focusobj->Name());
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "del", 3)) {
#else // __linux__
		else if (!strncasecmp(pc, "del", 3)) {
#endif // __linux__
			Vessel* v = g_psys->GetVessel(trim_string(pc + 3), true);
			if (v) v->RequestDestruct();
		}
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "tacc", 4)) {
#else // __linux__
	else if (!strncasecmp(cmd, "tacc", 4)) {
#endif // __linux__
		double w;
		if (sscanf(trim_string(cmd + 4), "%lf", &w) == 1)
			m_pOrbiter->SetWarpFactor(w);
		else {
			sprintf(cbuf, "Time acceleration is %0.1f", td.Warp());
			Echo(cbuf);
		}
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "time", 4)) {
#else // __linux__
	else if (!strncasecmp(cmd, "time", 4)) {
#endif // __linux__
		pc = trim_string(cmd + 4);
#ifndef __linux__
		if (!_strnicmp(pc, "simt", 4)) {
#else // __linux__
		if (!strncasecmp(pc, "simt", 4)) {
#endif // __linux__
			sprintf(cbuf, "%0.1f", td.SimT0);
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "syst", 4)) {
#else // __linux__
		else if (!strncasecmp(pc, "syst", 4)) {
#endif // __linux__
			sprintf(cbuf, "%0.1f", td.SysT0);
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "mjd", 3)) {
#else // __linux__
		else if (!strncasecmp(pc, "mjd", 3)) {
#endif // __linux__
			sprintf(cbuf, "%0.6f", td.MJD0);
		}
#ifndef __linux__
		else if (!_strnicmp(pc, "ut", 2)) {
#else // __linux__
		else if (!strncasecmp(pc, "ut", 2)) {
#endif // __linux__
			strcpy(cbuf, DateStr(td.MJD0));
		}
		else {
			sprintf(cbuf, "SysT=%0.1f SimT=%0.1f, MJD=%0.6f, UT=%s", td.SysT0, td.SimT0, td.MJD0, DateStr(td.MJD0));
		}
		Echo(cbuf);
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "pause", 5)) {
#else // __linux__
	else if (!strncasecmp(cmd, "pause", 5)) {
#endif // __linux__
		pc = trim_string(cmd + 5);
#ifndef __linux__
		if (!_strnicmp(pc, "on", 2)) m_pOrbiter->Pause(true);
		else if (!_strnicmp(pc, "off", 3)) m_pOrbiter->Pause(false);
		else if (!_strnicmp(pc, "toggle", 6)) m_pOrbiter->TogglePause();
		sprintf_s(cbuf, 256, "Simulation %s", m_pOrbiter->IsRunning() ? "running" : "paused");
#else // __linux__
		if (!strncasecmp(pc, "on", 2)) m_pOrbiter->Pause(true);
		else if (!strncasecmp(pc, "off", 3)) m_pOrbiter->Pause(false);
		else if (!strncasecmp(pc, "toggle", 6)) m_pOrbiter->TogglePause();
		snprintf(cbuf, 256, "Simulation %s", m_pOrbiter->IsRunning() ? "running" : "paused");
#endif // __linux__
		Echo(cbuf);
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "step", 4)) {
		sprintf_s(cbuf, 256, "dt=%f, FPS=%f", td.SimDT, td.FPS());
#else // __linux__
	else if (!strncasecmp(cmd, "step", 4)) {
		snprintf(cbuf, 256, "dt=%f, FPS=%f", td.SimDT, td.FPS());
#endif // __linux__
		Echo(cbuf);
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "gui", 3)) {
		if (!DestroyStatDlg())
			m_hStatWnd = CreateDialog(m_pOrbiter->GetInstance(), MAKEINTRESOURCE(IDD_SERVER), m_hWnd, ServerDlgProc);
#else // __linux__
	else if (!strncasecmp(cmd, "gui", 3)) {
		if (!DestroyStatDlg()) {
			m_hStatWnd = oapiCreateResDialog(m_pOrbiter->GetInstance(), IDD_SERVER, NULL, m_hWnd);
			if (m_hStatWnd)
				ServerDlgProc(m_hStatWnd, this);
		}
#endif // __linux__
	}
#ifndef __linux__
	else if (!_strnicmp(cmd, "dlg", 3)) {
#else // __linux__
	else if (!strncasecmp(cmd, "dlg", 3)) {
#endif // __linux__
		DialogManager* pDlgMgr = m_pOrbiter->DlgMgr();
		if (pDlgMgr) {
			pc = trim_string(cmd + 3);
#ifndef __linux__
			if (!_strnicmp(pc, "focus", 5))
#else // __linux__
			if (!strncasecmp(pc, "focus", 5))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgFocus>();
#ifndef __linux__
			else if (!_strnicmp(pc, "map", 3))
#else // __linux__
			else if (!strncasecmp(pc, "map", 3))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgMap>();
#ifndef __linux__
			else if (!_strnicmp(pc, "info", 4))
#else // __linux__
			else if (!strncasecmp(pc, "info", 4))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgInfo>();
#ifndef __linux__
			else if (!_strnicmp(pc, "tacc", 4))
#else // __linux__
			else if (!strncasecmp(pc, "tacc", 4))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgTacc>();
#ifndef __linux__
			else if (!_strnicmp(pc, "function", 8))
#else // __linux__
			else if (!strncasecmp(pc, "function", 8))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgFunction>();
#ifndef __linux__
			else if (!_strnicmp(pc, "record", 6))
#else // __linux__
			else if (!strncasecmp(pc, "record", 6))
#endif // __linux__
				pDlgMgr->EnsureEntry<DlgRecorder>();
#ifndef __linux__
			else if (!_strnicmp(pc, "help", 4))
#else // __linux__
			else if (!strncasecmp(pc, "help", 4))
#endif // __linux__
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
	Echo("Type \"help\" for a list of commands.");
	Echo("Type \"exit\" to return to the Launchpad dialog.\n");
}

bool orbiter::ConsoleNG::DestroyStatDlg()
{
	if (m_hStatWnd) {
#ifndef __linux__
		DestroyWindow(m_hStatWnd);
#else // __linux__
		m_hStatWnd->deleteLater();
#endif // __linux__
		m_hStatWnd = NULL;
		return true;
	}
	else
		return false;
}


#ifndef __linux__
DWORD WINAPI InputProc(LPVOID context)
#else // __linux__
// SetConsoleTextAttribute: ANSI white, bright for intensity, only on a terminal
static void ConsoleColour(FILE* f, bool intensity)
#endif // __linux__
{
#ifndef __linux__
	DWORD count, c;
#else // __linux__
	if (isatty(fileno(f)))
		fputs(intensity ? "\033[1;37m" : "\033[0;37m", f);
}

void InputProc(std::atomic<bool>* stop)
{
	ssize_t count;
#endif // __linux__
	char cbuf[1024];
#ifndef __linux__
	HANDLE hStdI = GetStdHandle(STD_INPUT_HANDLE);
	HANDLE hStdO = GetStdHandle(STD_OUTPUT_HANDLE);
	orbiter::ConsoleNG* console = (orbiter::ConsoleNG*)context;
	SetConsoleMode(hStdI, ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
	SetConsoleTextAttribute(hStdI, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
	hMutex = CreateMutex(NULL, FALSE, NULL);
	if (!hMutex)
		return 1;
#else // __linux__
	// ReadConsole line input and echo are the terminal's canonical mode
#endif // __linux__
	for (;;) {
#ifndef __linux__
		if (!ReadConsole(hStdI, cbuf, 1024, &count, NULL))
#else // __linux__
		pollfd pfd = { STDIN_FILENO, POLLIN, 0 };
		int res = poll(&pfd, 1, 200);
		if (*stop) break;
		if (res == 0) continue;
		if (res < 0 && errno == EINTR) continue;
		if (res < 0 || (count = read(STDIN_FILENO, cbuf, sizeof(cbuf) - 2)) <= 0)
#endif // __linux__
			break; // Console not available, exiting
#ifndef __linux__
		WriteConsole(hStdO, "> ", 2, &c, NULL);
#else // __linux__
		fputs("> ", stdout);
		fflush(stdout);
#endif // __linux__

#ifndef __linux__
		WaitForSingleObject(hMutex, 1000);
		cConsoleCmd[0] = 'x';
		strncpy(cConsoleCmd + 1, cbuf, count);
		cConsoleCmd[count - 1] = '\0'; // eliminates CR
		ReleaseMutex(hMutex);
#else // __linux__
		{
			std::lock_guard<std::mutex> lock(hMutex);
			cConsoleCmd[0] = 'x';
			strncpy(cConsoleCmd + 1, cbuf, count);
			cConsoleCmd[count + 1] = '\0';
			cConsoleCmd[strcspn(cConsoleCmd, "\r\n")] = '\0'; // eliminates CR/LF
		}
#endif // __linux__

		// handle "exit" directly so we can terminate the console thread in an orderly fashion
#ifndef __linux__
		if (!strncmp(cbuf, "exit\r\n", 6)) {
#else // __linux__
		if (!strncmp(cbuf, "exit\n", 5) || !strncmp(cbuf, "exit\r\n", 6)) {
#endif // __linux__
			break;
		}
	}
#ifndef __linux__
	return 0;
#endif // !__linux__
}

#ifdef __linux__
// ReadConsole from Orbiter's console window: one typed line, on the GUI thread
void ConsoleLine(const char* line)
{
	std::lock_guard<std::mutex> lock(hMutex);
	cConsoleCmd[0] = 'x';
	snprintf(cConsoleCmd + 1, sizeof(cConsoleCmd) - 1, "%s", line);
}

#endif // __linux__
#ifndef __linux__
INT_PTR CALLBACK ServerDlgProc(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
#else // __linux__
// WM_TIMER, WM_COMMAND and WM_CLOSE become a QTimer, the command handler and a close event hook
void ServerDlgProc(QWidget* hDlg, void* context)
#endif // __linux__
{
#ifndef __linux__
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
#else // __linux__
	QTimer* timer = new QTimer(hDlg); // SetTimer; destroyed with the dialog (KillTimer)
	QObject::connect(timer, &QTimer::timeout, hDlg, [hDlg]() {
		if (s_console)
			s_console->GetOrbiter()->UpdateServerWnd(hDlg);
	});
	timer->start(1000);
	oapiConnectDlgCommands(hDlg, [](int id, int code, QWidget* hCtrl) {
		switch (id) {
		case IDOK:
			if (s_console)
				s_console->GetOrbiter()->CloseSession();
		}
	});
	new EventHook(hDlg, [](QObject*, QEvent* event) {
		if (event->type() == QEvent::Close) {
			if (s_console)
				s_console->DestroyStatDlg();
			event->ignore();
			return true;
		}
		if (event->type() == QEvent::KeyPress && static_cast<QKeyEvent*>(event)->key() == Qt::Key_Escape)
			return true; // IDCANCEL is not handled
		return false;
	});
#endif // __linux__
}

void ConsoleOut(const char* msg)
{
	if (!s_hStdO) return;
#ifndef __linux__
	DWORD count;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	SetConsoleTextAttribute(s_hStdO, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE);
	GetConsoleScreenBufferInfo(s_hStdO, &csbi);
	csbi.dwCursorPosition.X = 0;
	SetConsoleCursorPosition(s_hStdO, csbi.dwCursorPosition);
	WriteConsole(s_hStdO, msg, strlen(msg), &count, NULL);
	SetConsoleTextAttribute(s_hStdO, FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY);
	WriteConsole(s_hStdO, "\n> ", 3, &count, NULL);
#else // __linux__
	if (ConsoleManager::WriteConsole(msg)) // Orbiter's console window
		return;
	ConsoleColour(s_hStdO, false);
	fputc('\r', s_hStdO); // cursor to column 0
	fputs(msg, s_hStdO);
	ConsoleColour(s_hStdO, true);
	fputs("\n> ", s_hStdO);
	fflush(s_hStdO);
#endif // __linux__
}
