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
#include "OrbiterPlatform.h"

extern PlanetarySystem* g_psys;
extern Vessel* g_focusobj;
extern TimeData td;

static void ConsoleOut(const char* msg);

orbiter::ConsoleNG::ConsoleNG(Orbiter* pOrbiter)
	: m_pOrbiter(pOrbiter)
	, m_bStats(false)
	, m_statTick(0)
{
	const CFG_CMDLINEPRM& prm = pOrbiter->Cfg()->CfgCmdlinePrm;
	bool scripted = !prm.LaunchScenario.empty() && prm.bFastExit &&
		(prm.FrameLimit || prm.MaxSysTime > 0.0 || prm.MaxSimTime > 0.0);
	oplat::ConsoleMode mode = scripted ? oplat::CONSOLE_OUTPUT_ONLY :
		oplat::StdinIsTerminal() ? oplat::CONSOLE_TERMINAL : oplat::CONSOLE_WINDOW;
	oplat::ConsoleOpen("Orbiter Server Console", mode);
	SetLogOutFunc(&ConsoleOut); // clone log output to console
}

orbiter::ConsoleNG::~ConsoleNG()
{
	SetLogOutFunc(0);
	oplat::ConsoleSetStatus("");
	oplat::ConsoleClose();
}

bool orbiter::ConsoleNG::ParseCmd()
{
	if (m_bStats) UpdateStats();
	std::string line;
	bool any = false;
	while (oplat::ConsolePoll(line)) {
		char cmd[1024];
		strncpy(cmd, line.c_str(), sizeof(cmd) - 1);
		cmd[sizeof(cmd) - 1] = '\0';
		any |= ParseLine(cmd);
		if (!m_pOrbiter->IsSessionActive()) break;
	}
	return any;
}

void orbiter::ConsoleNG::UpdateStats()
{
	DWORD t = GetTickCount();
	if (t - m_statTick < 1000) return;
	m_statTick = t;
	char cbuf[512];
	snprintf(cbuf, sizeof(cbuf), "SysT %0.0fs   SimT %0.0fs   MJD %0.5f   Warp %0.1fx   dt %f   FPS %0.1f   Vessels %zu",
		td.SysT0, td.SimT0, td.MJD0, td.Warp(), td.SimDT, td.FPS(), g_psys ? g_psys->nVessel() : (size_t)0);
	oplat::ConsoleSetStatus(cbuf);
}

bool orbiter::ConsoleNG::ParseLine(char* cmd)
{
	char cbuf[256], *pc, *ppc;
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
			sprintf(cbuf, "%zu", g_psys->nVessel());
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
		m_bStats = !m_bStats;
		m_statTick = 0;
		if (m_bStats) UpdateStats();
		else oplat::ConsoleSetStatus("");
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
	Echo("Type \"help\" for a list of commands.");
	Echo("Type \"exit\" to return to the Launchpad dialog.\n");
}

void ConsoleOut(const char* msg)
{
	oplat::ConsoleWrite(msg);
}
