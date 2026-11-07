// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __console_ng_h
#define __console_ng_h

#include <windows.h>

class Orbiter;

namespace orbiter {

	class ConsoleNG {
	public:
		ConsoleNG(Orbiter* pOrbiter);
		~ConsoleNG();

		Orbiter* GetOrbiter() const { return m_pOrbiter; }
		HWND WindowHandle() const { return NULL; }
		bool ParseCmd();
		void Echo(const char* str) const;
		void EchoIntro() const;

	private:
		bool ParseLine(char* cmd);
		void UpdateStats();

		Orbiter* m_pOrbiter;
		bool m_bStats;      // show simulation state monitor
		DWORD m_statTick;   // last state monitor update [ms]
	};

}

#endif // !__console_ng_h
