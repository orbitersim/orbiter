// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include "cmdline.h"
#include "Orbiter.h"

orbiter::CommandLine::Key orbiter::CommandLine::m_keyList[] = {
	{ KEY_HELP, "help", 'h', false},
	{ KEY_SCENARIO, "scenario", 'S', true},
	{ KEY_SCENARIOX, "scenariox", 's', true},
	{ KEY_FASTEXIT, "fastexit", 'x', false},
	{ KEY_OPENVIDEO, "openvideotab", 'v', false},
	{ KEY_KEEPLOG, "keeplog", 'l', false}
};

orbiter::CommandLine::CommandLine(Orbiter* pOrbiter, const PSTR cmdLine)
	: m_pOrbiter(pOrbiter)
{
	m_launchScenario.clear();
	m_keepLog = false;
	ParseCmdLine(cmdLine);
}

const char* orbiter::CommandLine::LaunchScenario() const
{
	return (m_launchScenario.size() ? m_launchScenario.c_str() : 0);
}

bool orbiter::CommandLine::KeepLog() const
{
	return m_keepLog;
}

void orbiter::CommandLine::ParseCmdLine(const PSTR cmdLine)
{
	enum ParseState {
		PARSE_KEY,
		PARSE_VALUE
	} state = PARSE_KEY;

	PSTR pc = cmdLine;
	Key* key;
	std::string value;

	while (*pc) {
		switch (state) {
		case PARSE_KEY:
			key = ParseKey(pc);
			if (!key) return;
			if (key->hasArgument)
				state = PARSE_VALUE;
			else
				ApplyOption(key);
			break;
		case PARSE_VALUE:
			value = ParseValue(pc);
			ApplyOption(key, value);
			state = PARSE_KEY;
			break;
		}
	}
}

orbiter::CommandLine::Key* orbiter::CommandLine::ParseKey(PSTR &cmdLine)
{
	bool longKey;
	Key* key = 0;

	while (*cmdLine == ' ' || *cmdLine == '\t') cmdLine++; // skip whitespace
	if (*cmdLine == '-')
		cmdLine++;
	else
		return 0;
	if (*cmdLine == '\0')
		return 0;
	if (longKey = (*cmdLine == '-'))
		cmdLine++;

	if (longKey) {
		PSTR keyEnd = cmdLine;
		while (*keyEnd && *keyEnd != ' ' && *keyEnd != '\t' && *keyEnd != '=')
			keyEnd++;
		size_t keyLen = keyEnd - cmdLine;
		for (int i = 0; i < ARRAYSIZE(m_keyList); i++) {
			if (keyLen == strlen(m_keyList[i].longName) && !strnicmp(cmdLine, m_keyList[i].longName, keyLen)) {
				key = m_keyList + i;
				break;
			}
		}
		cmdLine = keyEnd;
		if (key && key->hasArgument) { // consume '='
			while (*cmdLine == ' ' || *cmdLine == '\t') cmdLine++; // skip whitespace
			if (*cmdLine == '=')
				cmdLine++;
			else
				return 0; // parse error: value wasn't preceeded by '='
		}
	}
	else {
		char c = *cmdLine;
		for (int i = 0; i < ARRAYSIZE(m_keyList); i++) {
			if (c == m_keyList[i].shortName) {
				key = m_keyList + i;
			}
		}
		cmdLine++;
	}
	return key;
}

std::string orbiter::CommandLine::ParseValue(PSTR &cmdLine)
{
	std::string value;

	while (*cmdLine == ' ' || *cmdLine == '\t') cmdLine++; // skip whitespace
	PSTR valEnd = cmdLine;
	if (*valEnd == '\"') { // value enclosed with ""
		valEnd++;
		while (*valEnd && *valEnd != '\"')
			valEnd++;
		if (*valEnd == '\"') { // argument opened with " must be closed with "
			valEnd++;
			size_t valLen = valEnd - cmdLine;
			value = std::string(cmdLine + 1, valLen - 2);
		}
	}
	else {
		while (*valEnd && *valEnd != ' ' && *valEnd != '\t')
			valEnd++;
		size_t valLen = valEnd - cmdLine;
		value = std::string(cmdLine, valLen);
	}
	cmdLine = valEnd; // advance cmdLine
	return value;
}

void orbiter::CommandLine::ApplyOption(const Key* key)
{
	if (key) {
		switch (key->id) {
		case KEY_HELP:
			PrintHelpAndExit();
			break;
		case KEY_FASTEXIT:
			m_pOrbiter->SetFastExit(true);
			break;
		case KEY_OPENVIDEO:
			m_pOrbiter->OpenVideoTab();
			break;
		case KEY_KEEPLOG:
			m_keepLog = true;
			break;
		}
	}
}

void orbiter::CommandLine::ApplyOption(const Key* key, const std::string& value)
{
	if (key) {
		switch (key->id) {
		case KEY_SCENARIO:
			m_launchScenario = value;
			break;
		case KEY_SCENARIOX:
			m_launchScenario = value;
			m_pOrbiter->SetFastExit(true);
			break;
		}
	}
}

void orbiter::CommandLine::PrintHelpAndExit()
{
	// Get console output
	if (AttachConsole(ATTACH_PARENT_PROCESS) || AllocConsole()) {
		freopen("CONOUT$", "w", stdout);
	}

	std::cout << "\nOrbiter Space Flight Simulator" << std::endl;
	std::cout << "orbiter.exe [options]\n\n";
	std::cout << "Options:\n";
	std::cout << "\t--help, -h: Print this help page and exit.\n";
	std::cout << "\t--scenario=<scn>, -S <scn>: Launch scenario <scn>\n";
	std::cout << "\t--scenariox=<scn>, -s <scn>: Launch scenario <scn>, exit on close\n";
	std::cout << "\t--fastexit, -f: Exit Orbiter after simulation session\n";
	std::cout << "\t--openvideotab, -v: Open Launchpad on video tab\n";
	std::cout << "\t--keeplog, -l: Append log to previous session\n";
	std::cout << std::endl;

	exit(0);
}
