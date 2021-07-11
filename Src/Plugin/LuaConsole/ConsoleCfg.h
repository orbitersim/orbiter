#ifndef __CONSOLECFG_H
#define __CONSOLECFG_H

//#include "Orbitersdk.h"
#include "LuaConsole.h"

class ConsoleConfig: public LaunchpadItem {
	friend class LuaConsole;

public:
	ConsoleConfig (HINSTANCE hDLL);
	char *Name ();
	char *Description ();
	bool clbkOpen (HWND hLaunchpad);
	int  clbkWriteConfig ();
	void InitDialog (HWND hDlg);
	void CloseDialog (HWND hDlg);
	void SetDefault();
	bool ReadConfig ();
	void Apply (HWND hDlg);

protected:
	DWORD fontsize;

private:
	static BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM);

	static ConsoleConfig *cc;
	static const char *cfgfile;
	HINSTANCE hModule;
};

#endif // !__CONSOLECFG_H
