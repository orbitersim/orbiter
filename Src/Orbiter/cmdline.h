// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __cmdline_h
#define __cmdline_h

#include <windows.h>
#include <vector>
#include <string>

class Orbiter;

// ----------------------------------------------------------------------
// Generic command line parser

class CommandLine
{
public:
	CommandLine(CommandLine const&) = delete;
	void operator=(CommandLine const&) = delete;

	const char* CmdLine() const;
	bool GetOption(unsigned int id, const std::string** value) const;

protected:
	struct Key {
		unsigned int id;
		const char* longName;
		char shortName;
		bool hasArgument;
	};
	struct Option {
		Key* key;
		std::string strKey;
		std::string strVal;
	};
	std::vector<Option> optionList;

	CommandLine(int argc, const char **cmdLine);
	void ParseCmdLine();
	bool ParseNextOption(const char*& cmdLine, bool& groupKey, Option& option);
	void MapKeys();
	void ApplyOptions();
	virtual void ApplyOption(const Key* key, const std::string& value) {}
	virtual std::vector<Key>& KeyList() const = 0;

private:
	std::string m_cmdLine;
};


// ----------------------------------------------------------------------
// Orbiter-specific command line parser

namespace orbiter {

	class CommandLine : public ::CommandLine
	{
	public:
		static CommandLine& Instance() { return InstanceImpl(); }
		static void Parse(Orbiter* pOrbiter, int argc, const char **cmdLine) { InstanceImpl(pOrbiter, argc, cmdLine); }

		CommandLine(CommandLine const&) = delete;
		void operator=(CommandLine const&) = delete;

		enum KeyId {
			KEY_HELP,
			KEY_SCENARIO,
			KEY_SCENARIOX,
			KEY_FASTEXIT,
			KEY_OPENVIDEO,
			KEY_KEEPLOG,
			KEY_FIXEDSTEP,
			KEY_MAXSYSTIME,
			KEY_MAXSIMTIME,
			KEY_FRAMECOUNT,
			KEY_PLUGIN
		};

	protected:
		virtual std::vector<Key>& KeyList() const;
		virtual void ApplyOption(const Key* key, const std::string& value);
		void PrintHelpAndExit() const;

	private:
		CommandLine(Orbiter* pOrbiter, int argc, const char **cmdLine);
		static CommandLine& InstanceImpl(Orbiter* pOrbiter = 0, int argc = 0, const char **cmdLine = 0);
		Orbiter* m_pOrbiter;
	};

}

#endif // !__cmdline_h
