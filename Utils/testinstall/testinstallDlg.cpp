// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// verinstDlg.cpp : implementation file
//

#include "stdafx.h"
#include "testinstall.h"
#include "testinstallDlg.h"
#include <io.h>
#include <fstream>
#include <iostream>
#include <process.h>
#include <direct.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

using namespace std;

const char *logname = "install.log";

// CverinstDlg dialog




CverinstDlg::CverinstDlg(const CverinstApp* pParent)
	: CDialog(CverinstDlg::IDD, NULL)
{
	app = pParent;
	m_hIcon = AfxGetApp()->LoadIcon(IDI_ORBITER);
}

void CverinstDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CverinstDlg, CDialog)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
	ON_EN_CHANGE(IDC_EDIT1, &CverinstDlg::OnEnChangeEdit1)
	ON_BN_CLICKED(IDC_BUTTON1, &CverinstDlg::OnBnClickedCfg)
	ON_BN_CLICKED(IDOK, &CverinstDlg::OnBnClickedOk)
END_MESSAGE_MAP()


// CverinstDlg message handlers

LRESULT CverinstDlg::DefWindowProc(UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg) {
	case WM_USER+1:
		PerformTests1();
		return FALSE;
	case WM_TIMER:
		if (hDxdiag) {
			DWORD ecode;
			GetExitCodeProcess (hDxdiag, &ecode);
			if (ecode != STILL_ACTIVE) {
				KillTimer (1);
				hDxdiag = 0;
				PerformTests2 (ecode);
			}
		}
		break;
	}
	return CDialog::DefWindowProc (msg, wParam, lParam);
}

BOOL CverinstDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	// TODO: Add extra initialization here

	HWND hList = GetDlgItem(IDC_LIST1)->m_hWnd;
	LVCOLUMN lvc;
	LVITEM lvi;
	lvc.mask = LVCF_FMT | LVCF_TEXT | LVCF_WIDTH;
	lvc.fmt = LVCFMT_LEFT;
	lvc.pszText = "Test";
	lvc.cx = 300;
	ListView_InsertColumn (hList, 0, &lvc);
	lvc.pszText = "Result";
	lvc.cx = 100;
	ListView_InsertColumn (hList, 1, &lvc);

	const int ntest = 3;
	static char *teststr[ntest] = {"Checking directory structure", "Checking runtime libraries", "Checking DirectX"};
	static char *testres[3] = {"Testing", "OK", "Failed"};
	for (int i = 0; i < ntest; i++) {
		lvi.mask = LVIF_TEXT;
		lvi.pszText = teststr[i];
		lvi.iItem = i;
		lvi.iSubItem = 0;
		ListView_InsertItem (hList, &lvi);
	}

	allsucceed = true;
	PostMessage (WM_USER+1, 0, 0);

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CverinstDlg::PerformTests1 () 
{
	ofstream ofs (logname);
	ofs << "Orbiter installation verification.\r\n";
	echo ("This diagnostic utility performs a few tests to verify a valid");
	echo ("Orbiter installation. Normally, this test needs to be executed");
	echo ("only once after installation. To run it again, execute the");
	echo ("'testinstall' utility in the Install subdirectory.");

	bool testok;
	SetAction (0, 0);
	testok = TestSubdirs();
	if (!testok) allsucceed = false;
	SetAction (0, testok ? 1:2);

	if (!testok) {
		echo ("Invalid directory structure!");
		echo ("If you have installed Orbiter by extracting a Zip archive,");
		echo ("you may have forgotten to preserve the archive directory structure.");
		echo ("You should repair the installation:");
		echo ("- For installations from Zip file: Extract the zip archive again,");
		echo ("  and make sure to preserve the directory structure of the");
		echo ("  archive.");
		echo ("- For installations from MSI file: Run the MSI installation file");
		echo ("  again, and choose the 'Repair' option.");
		MessageBox ("Invalid directory structure. Orbiter was not installed correctly",
			"Verification error", MB_OK);
	}

	SetAction (1, 0);
	testok = TestRuntime();
	if (!testok) allsucceed = false;
	SetAction (1, testok ? 1:2);

	SetAction (2, 0);
	hDxdiag = StartTestDirectX();
	SetTimer (1, 100, NULL);
}

void CverinstDlg::PerformTests2 (DWORD res)
{
	bool testok;
	testok = FinishTestDirectX (res);
	if (!testok) allsucceed = false;
	SetAction (2, testok ? 1:2);

	GetDlgItem(IDCANCEL)->ShowWindow (SW_SHOW);

	if (FinishTests()) {
		GetDlgItem(IDOK)->ShowWindow (SW_SHOW);
		GetDlgItem(IDC_BUTTON1)->ShowWindow (SW_SHOW);

		echo ("-----------------------------------------------------");
		echo ("Orbiter installation verification complete.");
		if (allsucceed) {
			echo ("No problems found.");
		} else {
			echo ("Potential problems found.");
			echo ("Please examine the installation log file (install.log)");
			echo ("and fix the problems before launching Orbiter.");
		}
	}
}
	
// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CverinstDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this function to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CverinstDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}

void CverinstDlg::echo (const char *line)
{
	ofstream ofs (logname, ios::app);
	ofs << line << endl;
	HWND hLog = GetDlgItem(IDC_EDIT1)->m_hWnd;
	SendDlgItemMessage (IDC_EDIT1, EM_SETSEL, -1, -1);
	SendDlgItemMessage (IDC_EDIT1, EM_REPLACESEL, FALSE, (LPARAM)line);
	SendDlgItemMessage (IDC_EDIT1, EM_REPLACESEL, FALSE, (LPARAM)"\r\n");
	SendDlgItemMessage (IDC_EDIT1, EM_SCROLL, SB_LINEDOWN, 0);
}

void CverinstDlg::SetAction (int item, int action)
{
	static char *actionstr[3] = {"Testing", "OK", "Failed"};
	LVITEM lvi;
	lvi.mask = LVIF_TEXT;
	lvi.pszText = actionstr[action];
	lvi.iItem = item;
	lvi.iSubItem = 1;
	HWND hList = GetDlgItem(IDC_LIST1)->m_hWnd;
	ListView_SetItem (hList, &lvi);
}

bool CverinstDlg::TestSubdirs()
{
	echo ("-----------------------------------------------------");
	echo ("Test 1: Directory structure");

	const int ndir = 10;
	const char *subdir[ndir] = {"Config", "Meshes", "Textures", "Scenarios", "Doc", "Script", "Modules", "Flights", "Html", "Install"};
	int i;
	intptr_t res;
	struct _finddata_t ft;
	bool isok = true;
	char cbuf[1024];

	strcpy (cbuf, "\tOrbiter root directory:\t ");
	_getcwd(cbuf+25, 1024);
	echo (cbuf);

	for (i = 0; i < ndir; i++) {
		sprintf (cbuf, "\tFolder %s\t ", subdir[i]);
		//strcpy (cbuf, "\tFolder ");
		//memcpy (cbuf+8, subdir[i], strlen(subdir[i]));
		if ((res = _findfirst(subdir[i], &ft)) == -1 || (ft.attrib & _A_SUBDIR) == 0) {
			strcat (cbuf, "not found!");
			isok = false;
			_findclose (res);
		} else {
			strcat (cbuf, "found.");
		}
		echo (cbuf);
	}
	return isok;
}

bool CverinstDlg::TestRuntime()
{
	const char *testvcr = "Install\\testvcr.exe";
	const char *vc8redist = "Install\\vcredist_x86.exe";
	intptr_t res;
	bool installok = true;

	echo ("-----------------------------------------------------");
	echo ("Test 2: C++ runtime libraries");

	res = _spawnl (_P_WAIT, testvcr, testvcr, NULL);

	if (res) {
		// runtime libraries not found
		echo ("\tRuntime libraries not found!\r\n");
		echo ("The Microsoft Visual-C++ runtime libraries required to run Orbiter");
		echo ("appear not to be present on your system.");
		echo ("You can install them now, or you can install them manually later by");
		echo ("running the 'Install\\vcredist_x86.exe' utility.");
		echo ("Note: The runtime libraries will be installed in your Windows system.");
		echo ("If you want to uninstall them in the future, use the 'Add and remove");
		echo ("Programs' facility in your Control panel, and remove the 'Microsoft");
		echo ("Visual C++ 2005 Redistributable' Runtime libraries.\r\n");
		if (MessageBox ("Do you want to install the Visual C++ 2005 runtime libraries now?",
			"Runtime libraries not found.", MB_YESNO) == IDOK) {
			echo ("Installing runtime libraries ...");
			res = _spawnl (_P_WAIT, vc8redist, vc8redist, NULL);
			if (res || _spawnl (_P_WAIT, testvcr, testvcr, NULL)) {
				echo ("Problem installing runtime libraries.");
				echo ("You must install the runtime libraries manually.");
				installok = false;
			} else {
				echo ("Runtime libraries installed successfully.");
			}
		} else {
			installok = false;
		}
	} else {
		echo ("\tRuntime libraries ok.");
	}
	return installok;
}


HANDLE CverinstDlg::StartTestDirectX ()
{
	echo ("-----------------------------------------------------");
	echo ("Test 3: Checking DirectX");

	return (HANDLE)_spawnlp (_P_NOWAIT, "dxdiag.exe", "dxdiag.exe", "/whql:off", "/t", "dxdiag.log", NULL);
}

bool CverinstDlg::FinishTestDirectX (DWORD res)
{
	char cbuf[1024], *c;
	bool installok = true;

	if (res) {
		echo ("\tdxdiag.exe not found!\r\n");
		echo ("This could indicate a problem with the DirectX drivers installed on your system.");
		echo ("Please ensure that DirectX version 7 or higher is installed on your computer.\r\n");
		installok = false;
	} else {
		echo ("\tScanning dxdiag output:\r\n");
		ifstream ifs ("dxdiag.log");
		bool d3d7dll = false;
		double dxver = 0.0;
		while (ifs.getline (cbuf, 1024)) {
			for (c = cbuf; *c == ' ' && *c != '\0'; c++);
			if (!_strnicmp (c, "DirectX Version:", 16)) {
				sscanf (c+24, "%lf", &dxver);
				echo (c);
			} else if (!_strnicmp (c, "Processor:", 10)) {
				echo (c);
			} else if (!_strnicmp (c, "Memory:", 7)) {
				echo (c);
			} else if (!_strnicmp (c, "Card name:", 10)) {
				echo (c);
			} else if (!_strnicmp (c, "d3dim700.dll:", 13)) {
				echo (c);
				d3d7dll = true;
			}
		}
		if (dxver < 7.0) {
			echo ("DirectX version >= 7 required!");
			installok = false;
		}
		if (!d3d7dll) {
			//echo (ofs, "D3D7 dynamic library not found!");
			//installok = false;
		}
		echo ("For full DirectX diagnostics, see file dxdiag.log.");
	}
	return installok;
}

bool CverinstDlg::FinishTests ()
{
	if (app->firstrun) {
		const char *orbiter = "orbiter.exe";
		int rename_res, err_id;
		intptr_t res;
		struct _finddata_t ft;
		char cbuf[1024];
		echo ("-----------------------------------------------------");
		echo ("Moving Orbiter executable into place.");
		if ((res = _findfirst("Install\\orbiter.bin", &ft)) != -1) {
			_findclose(res);
			if ((res = _findfirst(orbiter, &ft)) != -1) {
				_findclose(res);
				rename_res = rename (orbiter, "Install\\testinstall.exe");
				if (rename_res) {
					err_id = errno;
					if (err_id == EACCES || err_id == EEXIST) {
						echo ("Overwriting existing Install\\testinstall.exe");
						rename_res = remove ("Install\\testinstall.exe");
						if (rename_res) {
							err_id = errno;
							sprintf (cbuf, "Error removing Install\\testinstall.exe");
							echo (cbuf);
							strerror_s (cbuf, 1024, err_id);
							echo (cbuf);
							return false;
						} else {
							rename_res = rename (orbiter, "Install\\testinstall.exe");
							if (rename_res) {
								sprintf (cbuf, "Error renaming %s to Install\\testinstall.exe", orbiter);
								echo (cbuf);
								return false;
							}
						}
					} else {
						sprintf (cbuf, "Error renaming %s to Install\\testinstall.exe", orbiter);
						echo (cbuf);
						strerror_s (cbuf, 1024, err_id);
						echo (cbuf);
						return false;
					}
				}
			}
			rename_res = rename("Install\\orbiter.bin", orbiter);
			if (rename_res) {
				err_id = errno;
				sprintf (cbuf, "Error renaming Install\\orbiter.bin to %s", orbiter);
				echo (cbuf);
				strerror_s (cbuf, 1024, errno);
				echo (cbuf);
				return false;
			}
		} else {
			echo ("Warning: Install\\orbiter.bin not found.");
			echo ("If this is a first-time installation, you may need to repair");
			echo ("your Orbiter installation, or extract it from zip archive");
			echo ("again.");
			return false;
		}
	}
	return true;
}

void CverinstDlg::OnEnChangeEdit1()
{
	// TODO:  If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDialog::OnInitDialog()
	// function and call CRichEditCtrl().SetEventMask()
	// with the ENM_CHANGE flag ORed into the mask.

	// TODO:  Add your control notification handler code here
}

void CverinstDlg::OnBnClickedCfg()
{
	const char *orbiter = "orbiter.exe";
	_execl (orbiter, orbiter, "-v", NULL);
}

void CverinstDlg::OnBnClickedOk()
{
	const char *orbiter = "orbiter.exe";
	_execl (orbiter, orbiter, "-S", "2010 Edition\\Welcome", NULL);
	OnOK();
}
