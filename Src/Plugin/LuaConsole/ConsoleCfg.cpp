#include "ConsoleCfg.h"
#include "resource.h"

ConsoleConfig *ConsoleConfig::cc = 0;
const char *ConsoleConfig::cfgfile = "Modules\\Console.cfg";

ConsoleConfig::ConsoleConfig (HINSTANCE hDLL): LaunchpadItem ()
{
	hModule = hDLL;
	SetDefault ();
	ReadConfig ();
}

char *ConsoleConfig::Name ()
{
	return "Console Configuration";
}

char *ConsoleConfig::Description ()
{
	static char *desc = "Customize the appearance and behaviour of the inline Lua Console.\r\n\r\nThe console allows to run scripts during a simulation session.";
	return desc;
	bool clbkOpen (HWND hLaunchpad);
}

bool ConsoleConfig::clbkOpen (HWND hLaunchpad)
{
	cc = this; // keep a global pointer to be used by the message handlers (ugly)
	return OpenDialog (hModule, hLaunchpad, IDD_CONFIG, DlgProc);
}

int ConsoleConfig::clbkWriteConfig ()
{
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_OUT, CONFIG);
	if (!hFile) return 1;
	oapiWriteItem_int (hFile, "FSIZE", fontsize);
	oapiCloseFile (hFile, FILE_OUT);
	return 0;
}

void ConsoleConfig::InitDialog (HWND hDlg)
{
	char cbuf[256];
	sprintf (cbuf, "%d", fontsize);
	SetWindowText (GetDlgItem (hDlg, IDC_FONTSIZE), cbuf);
}

void ConsoleConfig::CloseDialog (HWND hDlg)
{
	EndDialog (hDlg, 0);
}

void ConsoleConfig::SetDefault ()
{
	fontsize = 14;
}

bool ConsoleConfig::ReadConfig ()
{
	int d;
	FILEHANDLE hFile = oapiOpenFile (cfgfile, FILE_IN, CONFIG);
	if (!hFile) {
		MessageBeep (-1);
		return false;
	}
	if (oapiReadItem_int (hFile, "FSIZE", d)) fontsize = (DWORD)d;
	oapiCloseFile (hFile, FILE_IN);
	return true;
}

void ConsoleConfig::Apply (HWND hDlg)
{
	char cbuf[256];
	DWORD d;
	GetWindowText (GetDlgItem (hDlg, IDC_FONTSIZE), cbuf, 256);
	if (sscanf (cbuf, "%d", &d) == 1) fontsize = d;
}

BOOL CALLBACK ConsoleConfig::DlgProc (HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch (uMsg) {
	case WM_INITDIALOG:
		cc->InitDialog (hDlg);
		break;
	case WM_COMMAND:
		switch (LOWORD (wParam)) {
		case IDOK:
			cc->Apply (hDlg);
			// fall through
		case IDCANCEL:
			cc->CloseDialog (hDlg);
			return 0;
		}
		break;
	}
	return 0;
}
