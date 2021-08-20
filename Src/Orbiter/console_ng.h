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
		HWND WindowHandle() const { return m_hWnd; }
		bool ParseCmd();
		void Echo(const char* str) const;
		void EchoIntro() const;
		bool DestroyStatDlg();

	private:

		Orbiter* m_pOrbiter;
		HWND m_hWnd;       // console window handle
		HWND m_hStatWnd;   // stats dialog
		HANDLE m_hThread;  // console thread handle
	};

}

#endif // !__console_ng_h
