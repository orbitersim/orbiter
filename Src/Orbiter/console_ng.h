// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __console_ng_h
#define __console_ng_h

#ifndef __linux__
#include <windows.h>
#else // __linux__
#include "OrbiterPlatform.h"
#include <atomic>
#include <thread>
#endif // __linux__

class Orbiter;

namespace orbiter {

	class ConsoleNG {
	public:
		ConsoleNG(Orbiter* pOrbiter);
		~ConsoleNG();

		Orbiter* GetOrbiter() const { return m_pOrbiter; }
#ifndef __linux__
		HWND WindowHandle() const { return m_hWnd; }
#else // __linux__
		QWindow *WindowHandle() const { return m_hWnd; }
#endif // __linux__
		bool ParseCmd();
		void Echo(const char* str) const;
		void EchoIntro() const;
		bool DestroyStatDlg();

	private:

		Orbiter* m_pOrbiter;
#ifndef __linux__
		HWND m_hWnd;       // console window handle
		HWND m_hStatWnd;   // stats dialog
		HANDLE m_hThread;  // console thread handle
#else // __linux__
		QWindow *m_hWnd;   // console window handle (the launching terminal has none)
		QWidget *m_hStatWnd; // stats dialog
		std::thread m_thread; // console thread
		std::atomic<bool> m_stop; // asks the console thread to exit
#endif // __linux__
	};

}

#endif // !__console_ng_h
