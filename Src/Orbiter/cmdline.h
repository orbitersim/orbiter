// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __cmdline_h
#define __cmdline_h

#include <windows.h>

class Orbiter;

namespace orbiter {

	class CommandLine
	{
	public:
		CommandLine(Orbiter* pOrbiter, const PSTR cmdLine);
		const char* LaunchScenario() const;
		bool KeepLog() const;

	protected:
		void ParseCmdLine(const PSTR cmdLine);

	private:
		enum KeyId {
			KEY_HELP,
			KEY_SCENARIO,
			KEY_SCENARIOX,
			KEY_FASTEXIT,
			KEY_OPENVIDEO,
			KEY_KEEPLOG
		};
		struct Key {
			KeyId id;
			PSTR longName;
			char shortName;
			bool hasArgument;
		};
		static Key m_keyList[];
		Orbiter* m_pOrbiter;
		std::string m_launchScenario;
		bool m_keepLog;

		Key* ParseKey(PSTR &cmdLine);
		std::string ParseValue(PSTR &cmdLine);
		void ApplyOption(const Key* key);
		void ApplyOption(const Key* key, const std::string &value);
		void PrintHelpAndExit();
	};

}

#endif // !__cmdline_h