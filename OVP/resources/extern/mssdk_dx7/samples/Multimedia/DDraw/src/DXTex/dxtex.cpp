// dxtex.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "dxtex.h"

#include "MainFrm.h"
#include "ChildFrm.h"
#include "dxtexDoc.h"
#include "dxtexView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



/////////////////////////////////////////////////////////////////////////////
// CDxtexDocManager::DoPromptFileName - overridden to allow importing of
// BMPs as well as DDSs into CDxtexDocs.
BOOL CDxtexDocManager::DoPromptFileName(CString& fileName, UINT nIDSTitle,
			DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate)
{
	CFileDialog dlgFile(bOpenFileDialog);

	CString title;
	VERIFY(title.LoadString(nIDSTitle));

	dlgFile.m_ofn.Flags |= lFlags;

	CString strFilter;
	CString strDefault;

	if (bOpenFileDialog)
	{
		strFilter += "Image Files (*.dds, *.bmp)";
		strFilter += (TCHAR)'\0';   // next string please
		strFilter += _T("*.dds;*.bmp");
		strFilter += (TCHAR)'\0';   // last string
		dlgFile.m_ofn.nMaxCustFilter++;
	}
	else
	{
		strFilter += "Image Files (*.dds)";
		strFilter += (TCHAR)'\0';   // next string please
		strFilter += _T("*.dds");
		strFilter += (TCHAR)'\0';   // last string
		dlgFile.m_ofn.nMaxCustFilter++;
	}

	// append the "*.*" all files filter
	CString allFilter;
	VERIFY(allFilter.LoadString(AFX_IDS_ALLFILTER));
	strFilter += allFilter;
	strFilter += (TCHAR)'\0';   // next string please
	strFilter += _T("*.*");
	strFilter += (TCHAR)'\0';   // last string
	dlgFile.m_ofn.nMaxCustFilter++;

	dlgFile.m_ofn.lpstrFilter = strFilter;
	dlgFile.m_ofn.lpstrTitle = title;
	dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);

	int nResult = dlgFile.DoModal();
	fileName.ReleaseBuffer();
	return nResult == IDOK;
};


/////////////////////////////////////////////////////////////////////////////
// CDxTxCommandLineInfo

CDxtexCommandLineInfo::CDxtexCommandLineInfo(VOID)
{
	m_dwFourCC = 0;
	m_bAlphaComing = FALSE;
	m_bMipMap = FALSE;
}


void CDxtexCommandLineInfo::ParseParam(const TCHAR* pszParam,BOOL bFlag,BOOL bLast)
{
	if (lstrcmpiA(pszParam, "DXT1") == 0)
	{
		m_dwFourCC = FOURCC_DXT1;
	}
	else if (lstrcmpiA(pszParam, "DXT2") == 0)
	{
		m_dwFourCC = FOURCC_DXT2;
	}
	else if (lstrcmpiA(pszParam, "DXT3") == 0)
	{
		m_dwFourCC = FOURCC_DXT3;
	}
	else if (lstrcmpiA(pszParam, "DXT4") == 0)
	{
		m_dwFourCC = FOURCC_DXT4;
	}
	else if (lstrcmpiA(pszParam, "DXT5") == 0)
	{
		m_dwFourCC = FOURCC_DXT5;
	}
	else if (bFlag && tolower(pszParam[0]) == 'a')
	{
		m_bAlphaComing = TRUE;
	}
	else if (!bFlag && m_bAlphaComing)
	{
		m_strFileNameAlpha = pszParam;
		m_bAlphaComing = FALSE;
	}
	else if (bFlag && tolower(pszParam[0]) == 'm')
	{
		m_bMipMap = TRUE;
	}
	else if (!bFlag && !m_strFileName.IsEmpty())
	{
		m_strFileNameSave = pszParam;
	}

	CCommandLineInfo::ParseParam(pszParam, bFlag, bLast);
}



/////////////////////////////////////////////////////////////////////////////
// CDxtexApp

BEGIN_MESSAGE_MAP(CDxtexApp, CWinApp)
	//{{AFX_MSG_MAP(CDxtexApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDxtexApp construction

CDxtexApp::CDxtexApp()
{
	// Place all significant initialization in InitInstance
	m_pdd = NULL;
	m_pd3d = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CDxtexApp destruction

CDxtexApp::~CDxtexApp()
{
	ReleasePpo(&m_pdd);
	ReleasePpo(&m_pd3d);
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CDxtexApp object

CDxtexApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CDxtexApp initialization

BOOL CDxtexApp::InitInstance()
{
	// Change the registry key under which our settings are stored.
	SetRegistryKey(_T("Microsoft"));

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	m_pDocManager = new CDxtexDocManager;
	
	CMultiDocTemplate* pDocTemplate;
	pDocTemplate = new CMultiDocTemplate(
		IDR_DXTXTYPE,
		RUNTIME_CLASS(CDxtexDoc),
		RUNTIME_CLASS(CChildFrame), // custom MDI child frame
		RUNTIME_CLASS(CDxtexView));
	AddDocTemplate(pDocTemplate);

	// create main MDI Frame window
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pMainFrame;

	// Initialize DirectDraw
	HRESULT hr;
	if (FAILED(hr = DirectDrawCreateEx(NULL, (VOID**)&m_pdd, IID_IDirectDraw7, NULL)))
		return FALSE;

	if (FAILED(hr = m_pdd->SetCooperativeLevel(NULL, DDSCL_NORMAL)))
		return FALSE;

	if (FAILED(hr = m_pdd->QueryInterface(IID_IDirect3D7, (VOID**)&m_pd3d)))
		return FALSE;

	// Parse command line for standard shell commands, DDE, file open
	CDxtexCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);
	// Prevent automatic "New" at startup:
	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileNew)
		cmdInfo.m_nShellCommand = CCommandLineInfo::FileNothing;

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;

	// See if we loaded a document
	POSITION posTemp = GetFirstDocTemplatePosition();
	CDxtexDoc* pdoc = NULL;
	POSITION pos = pDocTemplate->GetFirstDocPosition();
	if (pos != NULL)
		pdoc = (CDxtexDoc*)pDocTemplate->GetNextDoc(pos);

	if (!cmdInfo.m_strFileNameAlpha.IsEmpty())
	{
		if (pdoc != NULL)
		{
			pdoc->LoadAlphaBmp(cmdInfo.m_strFileNameAlpha);
		}
	}
	if (cmdInfo.m_bMipMap)
	{
		if (pdoc != NULL)
		{
			pdoc->GenerateMipMaps();
		}
	}
	if (cmdInfo.m_dwFourCC != 0)
	{
		if (pdoc != NULL)
		{
			pdoc->Compress(cmdInfo.m_dwFourCC, TRUE);
		}
	}
	if (!cmdInfo.m_strFileNameSave.IsEmpty())
	{
		if (pdoc != NULL)
		{
			pdoc->OnSaveDocument(cmdInfo.m_strFileNameSave);
		}
		return FALSE; // Prevent UI from coming up
	}

	// The main window has been initialized, so show and update it.
	pMainFrame->ShowWindow(m_nCmdShow);
	pMainFrame->UpdateWindow();

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	CString	m_strVersion;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	TCHAR szFile[MAX_PATH];
	CString strVersion;
	UINT cb;
	DWORD dwHandle;
	BYTE FileVersionBuffer[1024];
	VS_FIXEDFILEINFO* pVersion = NULL;

	GetModuleFileName(NULL, szFile, MAX_PATH);

	cb = GetFileVersionInfoSize(szFile, &dwHandle/*ignored*/);
	if (cb > 0)
	{
		if (cb > sizeof(FileVersionBuffer))
			cb = sizeof(FileVersionBuffer);

		if (GetFileVersionInfo(szFile, 0, cb, &FileVersionBuffer))
		{
			pVersion = NULL;
			if (VerQueryValue(&FileVersionBuffer, "\\", (VOID**)&pVersion, &cb)
				&& pVersion != NULL) 
			{
				strVersion.Format("Version %d.%02d.%02d.%04d", 
					HIWORD(pVersion->dwFileVersionMS),
					LOWORD(pVersion->dwFileVersionMS), 
					HIWORD(pVersion->dwFileVersionLS), 
					LOWORD(pVersion->dwFileVersionLS));
			}
		}
	}

	//{{AFX_DATA_INIT(CAboutDlg)
	m_strVersion = strVersion;
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	DDX_Text(pDX, IDC_VERSION, m_strVersion);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CDxtexApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}
