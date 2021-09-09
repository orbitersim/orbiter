// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include <set>
#include "cmdline.h"
#include "Orbiter.h"
#include "Launchpad.h"

CommandLine::CommandLine(const PSTR cmdLine)
{
	m_cmdLine = (std::string(cmdLine ? cmdLine : ""));
	ParseCmdLine(cmdLine);
}

const char* CommandLine::CmdLine() const
{
	return m_cmdLine.c_str();
}

bool CommandLine::GetOption(UINT id, const std::string** value) const
{
	for (auto it = optionList.cbegin(); it < optionList.cend(); it++)
		if (it->key->id == id) {
			*value = &it->strVal;
			return true;
		}
	return false;
}

void CommandLine::ParseCmdLine(const PSTR cmdLine)
{
	PSTR pc = cmdLine;
	bool groupKey = false;

	while (*pc) {
		Option option;
		option.key = 0;
		if (ParseNextOption(pc, groupKey, option)) {
			optionList.push_back(option);
		}
	}
}

bool CommandLine::ParseNextOption(PSTR& cmdLine, bool& groupKey, Option& option)
{
	bool isLongKey;
	bool isQuotedVal;
	bool isQuotedParam = false; // Is full parameter quoted? "--scenario=my_scenario"

	// Parse next key
	if (!groupKey) {
		while (*cmdLine == ' ' || *cmdLine == '\t') cmdLine++; // skip whitespace
		if (*cmdLine == '"')
		{
			cmdLine++;
			isQuotedParam = true;
		}
		if (*cmdLine == '\0')
			return false; // nothing left to parse
		if (*cmdLine != '-')
			return false; // parse error: key indicator not found
		else
			cmdLine++;
		if (isLongKey = (*cmdLine == '-'))
			cmdLine++;
	}
	else {
		isLongKey = false;
	}
	char* termKeyChar = (isLongKey ? " \t=" : " \t"); // '=' as key-value separator only allowed for long keys
	std::set<char> termK(termKeyChar, termKeyChar + strlen(termKeyChar) + 1); // include '\0' in set
	PSTR endKey = cmdLine;
	while (termK.find(*endKey) == termK.end())
		endKey++;
	size_t keyLen = endKey - cmdLine;
	if (groupKey = (!isLongKey && keyLen > 1)) // concatenated short keys
		keyLen = 1;
	option.strKey = std::string(cmdLine, keyLen);
	if (groupKey) {
		cmdLine++; // advance only current group key character
		option.strVal.clear();
		return true;
	}
	else {
		cmdLine = endKey; // advance command line pointer
	}
	
	// Parse value, if present
	while (*cmdLine == ' ' || *cmdLine == '\t') cmdLine++; // skip whitespace
	if (*cmdLine == '\0' || *cmdLine == '-') { // end of options or next item is key: no value
		option.strVal.clear();
		return true;
	}
	if (*cmdLine == '=') {
		if (isLongKey)
			cmdLine++;    // skip '='
		else
			return false; // '=' not allowed after short keys: parse error
	}
	else if (isLongKey)
		return false;     // for long keys, '=' is mandatory before values
	if (isQuotedVal = (*cmdLine == '\"'))
		cmdLine++; // skip starting quotes
	char* termValChar = (isQuotedParam || isQuotedVal ? "\"" : " \t"); // for quoted values, only accept quotes as terminator
	std::set<char> termV(termValChar, termValChar + strlen(termValChar) + 1); // include '\0' in set
	PSTR endVal = cmdLine;
	while (termV.find(*endVal) == termV.end())
		endVal++;
	size_t valLen = endVal - cmdLine;
	option.strVal = std::string(cmdLine, valLen);
	cmdLine = endVal; // advance command line pointer
	if ((isQuotedParam || isQuotedVal) && *cmdLine == '\"')
		cmdLine++;  // skip trailing quotes

	return true;
}

void CommandLine::MapKeys()
{
	std::vector<Key>& keys = KeyList();
	for (auto it = optionList.begin(); it < optionList.end(); it++) {
		bool isLong = (it->strKey.size() > 1);
		bool found = false;
		for (auto it_key = keys.begin(); it_key < keys.end(); it_key++) {
			if (isLong) {
				if (!stricmp(it_key->longName, it->strKey.c_str()))
					found = true;
			}
			else {
				if (it_key->shortName == it->strKey[0])
					found = true;
			}
			if (found) {
				it->key = &(*it_key);
				break;
			}
		}
	}
}

void CommandLine::ApplyOptions()
{
	for (auto it = optionList.begin(); it < optionList.end(); it++) {
		if (it->key) {
			ApplyOption(it->key, it->strVal);
		}
	}
}




orbiter::CommandLine::CommandLine(Orbiter* pOrbiter, const PSTR cmdLine)
	: ::CommandLine(cmdLine)
	, m_pOrbiter(pOrbiter)
{
	CFG_CMDLINEPRM& cfg = m_pOrbiter->Cfg()->CfgCmdlinePrm;
	cfg.LoadPlugins.clear();

	MapKeys();
	ApplyOptions();
}

std::vector<::CommandLine::Key>& orbiter::CommandLine::KeyList() const
{
	static std::vector<CommandLine::Key> keyList = {
		{ KEY_HELP, "help", 'h', false},
		{ KEY_SCENARIO, "scenario", 'S', true},
		{ KEY_SCENARIOX, "scenariox", 's', true},
		{ KEY_FASTEXIT, "fastexit", 'x', false},
		{ KEY_OPENVIDEO, "openvideotab", 'v', false},
		{ KEY_KEEPLOG, "keeplog", 'l', false},
		{ KEY_FIXEDSTEP, "fixedstep", 'f', true},
		{ KEY_MAXSYSTIME, "maxsystime", 'T', true},
		{ KEY_MAXSIMTIME, "maxsimtime", 't', true},
		{ KEY_FRAMECOUNT, "maxframes", '_', true},
		{ KEY_PLUGIN, "plugin", 'p', true}
	};
	return keyList;
}

void orbiter::CommandLine::ApplyOption(const Key* key, const std::string& value)
{
	int res;
	size_t s;
	double f;
	CFG_CMDLINEPRM& cfg = m_pOrbiter->Cfg()->CfgCmdlinePrm;

	switch (key->id) {
	case KEY_HELP:
		PrintHelpAndExit();
		break;
	case KEY_FASTEXIT:
		cfg.bFastExit = true;
		break;
	case KEY_OPENVIDEO:
		cfg.bOpenVideoTab = true;
		break;
	case KEY_KEEPLOG:
		cfg.bAppendLog = true;
		break;
	case KEY_SCENARIO:
		cfg.LaunchScenario = value;
		break;
	case KEY_SCENARIOX:
		cfg.LaunchScenario = value;
		cfg.bFastExit = true;
		break;
	case KEY_FIXEDSTEP:
		res = sscanf(value.c_str(), "%lf", &f);
		if (res == 1)
			cfg.FixedStep = f;
		break;
	case KEY_MAXSYSTIME:
		res = sscanf(value.c_str(), "%lf", &f);
		if (res == 1)
			cfg.MaxSysTime = f;
		break;
	case KEY_MAXSIMTIME:
		res = sscanf(value.c_str(), "%lf", &f);
		if (res == 1)
			cfg.MaxSimTime = f;
		break;
	case KEY_FRAMECOUNT:
		res = sscanf(value.c_str(), "%zu", &s);
		if (res == 1)
			cfg.FrameLimit = s;
		break;
	case KEY_PLUGIN:
		cfg.LoadPlugins.push_back(value);
		break;
	}
}

void orbiter::CommandLine::PrintHelpAndExit() const
{
	// Get console output
	if (AttachConsole(ATTACH_PARENT_PROCESS) || AllocConsole()) {
		freopen("CONOUT$", "w", stdout);
	}

	std::cout << "\nOrbiter Space Flight Simulator" << std::endl;
	std::cout << "orbiter.exe [options]\n\n";
	std::cout << "Options:\n";
	std::cout << "  --help, -h: Print this help page and exit.\n";
	std::cout << "  --scenario=<scn>, -S <scn>: Launch scenario <scn>\n";
	std::cout << "  --scenariox=<scn>, -s <scn>: Launch scenario <scn>, exit on close\n";
	std::cout << "  --fastexit, -x: Exit Orbiter after simulation session\n";
	std::cout << "  --openvideotab, -v: Open Launchpad on video tab\n";
	std::cout << "  --keeplog, -l: Append log to previous session\n";
	std::cout << "  --fixedstep=<s>, -f <s>: Enforce fixed time step length (seconds)\n";
	std::cout << "  --maxsystime=<t>, -T <t>: Terminate session after <t> seconds\n";
	std::cout << "  --maxsimtime=<t>, -t <t>: Terminate session at simulation time <t>\n";
	std::cout << "  --maxframes=<f>: Terminate session after <f> time frames\n";
	std::cout << "  --plugin=<pg>, -p <pg>: Load plugin <pg> (from Modules\\Plugin\\<pg>.dll)\n";
	std::cout << std::endl;

	exit(0);
}

orbiter::CommandLine& orbiter::CommandLine::InstanceImpl(Orbiter* pOrbiter, const PSTR cmdLine)
{
	static orbiter::CommandLine instance{ pOrbiter, cmdLine };
	return instance;
}
