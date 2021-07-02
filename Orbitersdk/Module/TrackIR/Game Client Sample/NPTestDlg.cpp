// NPTestDlg.cpp : implementation file
//

#include "stdafx.h"
#include "NPTest.h"
#include "NPTestDlg.h"

#include "NPClient.h"
#include "NPClientWraps.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////
// Global Data //////////////////////////////////////////////////////////////
/////////////////
CString gcsDLLPath = "C:\\NP Client Interface 2\\Game Client DLL\\Debug";

char gpTextBuf[16384]; // 16k text buffer
unsigned long	NPFrameSignature;
unsigned long	NPStaleFrames;

/////////////////
// Static data //////////////////////////////////////////////////////////////
/////////////////
//
static CNPTestDlg* pNPTestDlg;


//////////////////////////
// Function Prototypes //////////////////////////////////////////////////////
/////////////////////////
//
NPRESULT client_HandleTrackIRData();


/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNPTestDlg dialog

CNPTestDlg::CNPTestDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CNPTestDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNPTestDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CNPTestDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNPTestDlg)
	DDX_Control(pDX, IDC_EDIT1, m_eTextOut);
	DDX_Control(pDX, IDC_NPSHOWDATA, m_eNPShowData);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNPTestDlg, CDialog)
	//{{AFX_MSG_MAP(CNPTestDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNPTestDlg message handlers

BOOL CNPTestDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	m_nTimerMessageNum = 0;
	m_nDspBufMaxLineCount = 100;    // Keep the last 100 lines of text in
									// the edit box buffer.

	pNPTestDlg = this; // Pointer to this object, for access by the notify callback

	DisplayLine( "*** NaturalPoint Game Client API Test Application ***" );

    // Initialize the TrackIR software
    TrackIR_Enhanced_Init();


    // The following timer is used to simulate the per-frame update of an 
    // gaming application, where it will check for new Tracking Data.

	// Start the timer routine, and set the call interval at 17 milliseconds 
    // (~ 60 times per second). The TrackIR hardware can actually update
    // at up to 120 times per second, though.
		SetTimer( 1, 17, NULL );	
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CNPTestDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CNPTestDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

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


HCURSOR CNPTestDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;

} // CNPTestDlg::OnQueryDragIcon()


void CNPTestDlg::DisplayLine( LPCTSTR pszTextLine )
{
	CString csTextLine = pszTextLine;

	int nTextBufLen = m_eTextOut.GetWindowTextLength();
	int nLineLen = csTextLine.GetLength();
	int nNewBufSize = nTextBufLen + nLineLen + 1;

	if( 0 != nTextBufLen )
		m_eTextOut.GetWindowText( gpTextBuf, sizeof(gpTextBuf) );
	else
		gpTextBuf[0] = '\0';

	// Set pDisplayPtr to the beginning of the full text buffer
	char* pDisplayPtr = gpTextBuf;

	// Count total # lines in the text buffer
	char* pText = gpTextBuf;
	int nTextBufLineCount = 0;
	while( pText < gpTextBuf + nTextBufLen )
		{
		if( '\r' == *pText )
			nTextBufLineCount++;
		pText++;
		}
	//
	// Any time the line count exceeds the number specified by the 
	// member variable m_nDspBufMaxLineCount, we need to find the
	// point in the buffer where we can truncate everything preceding
	// it and only keep the maximum number of lines that are indicated
	// by the m_nDspBufMaxLineCount value.
	//
	if( nTextBufLineCount > m_nDspBufMaxLineCount )
		{
		pText = gpTextBuf; // Start at the beginning
		int nLinesSkipped = 0;
		while( nLinesSkipped < nTextBufLineCount - m_nDspBufMaxLineCount )
			{
			if( '\r' == *pText )
				nLinesSkipped++;
			pText++;
			}	 		
		// Now pText is pointing at the location within the text buffer
		// where there are exactly m_nDspBufMaxLineCount lines from that
		// location to the end of the buffer.  The data preceding this
		// location will effectively get dumped once we call SetWindowText()
		// on the edit control and pass this pointer to it.
		pDisplayPtr = pText;
		// Also set nTextBufLineCount to the correct value.
		nTextBufLineCount = m_nDspBufMaxLineCount;
		} // if( more lines than we need to keep )
		
	// Next, clean any leading or trailing whitespace from the input line text
	// and then append a shiny new CR/LF
	csTextLine.TrimLeft();
	csTextLine.TrimRight(); 		
	csTextLine += "\r\n";

	strcat( pDisplayPtr, csTextLine );
		
	// Output the modified text buffer to the edit control
	m_eTextOut.SetWindowText( pDisplayPtr );

	// If there are more than 17 lines being displayed, scroll the edit control
	// to keep the most recently-added lines visible at the bottom of the control.
	if( nTextBufLineCount > 17 )
		m_eTextOut.LineScroll( nTextBufLineCount - 17, 0 );	

} // CNPTestDlg::DisplayLine()


void CNPTestDlg::DisplayData( CString tstr)
{
	m_eNPShowData.SetWindowText(tstr);
}



//
// Timer routine for test purposes -- simply pumps numbered text messages to the
// output window to check UI message scrolling, etc.
//
void CNPTestDlg::OnTimer(UINT nIDEvent) 
{
	NPRESULT result;
	CString csTimerMsg;

	m_nTimerMessageNum++;

	// "Poll" the NPClient interface for new TrackIR data, and process it if found
	result = client_HandleTrackIRData();

	if (result == NP_ERR_NO_DATA)
	{
//		csTimerMsg.Format( "No new data on timer call %d", m_nTimerMessageNum);
//		DisplayLine( csTimerMsg );
	}


	CDialog::OnTimer(nIDEvent);

} // CNPTestDlg::OnTimer()

void CNPTestDlg::OnOK() 
{
    // De-initialize the TrackIR Enhanced Interface
	TrackIR_Enhanced_Shutdown();

    // Quit the application
	CDialog::OnOK();
}



void CNPTestDlg::GetDllLocation(LPTSTR pszPath)
{
    if (pszPath == NULL)
        return;
        
	//find path to NPClient.dll
	HKEY pKey = NULL;
	//open the registry key 
	if (::RegOpenKeyEx(HKEY_CURRENT_USER,
		"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location",
		0,
		KEY_READ,
		&pKey) != ERROR_SUCCESS)
	{
					//error condition
			MessageBox("DLL Location key not present");
	}

	//get the value from the key
	LPTSTR pszValue;
	DWORD dwSize;
	//first discover the size of the value
	if (RegQueryValueEx(pKey,
		"Path",
		NULL,
		NULL,
		NULL,
		&dwSize) == ERROR_SUCCESS)
	{
		//allocate memory for the buffer for the value
		pszValue = (LPTSTR) malloc(dwSize);
		if (pszValue == NULL)//error
			MessageBox("insufficient memory!");

        //now get the value
        if (RegQueryValueEx(pKey,
			"Path",
			NULL,
			NULL,
			(LPBYTE) pszValue,
			&dwSize) == ERROR_SUCCESS)
		{
			//everything worked
			::RegCloseKey(pKey);
			strcpy(pszPath, pszValue);

            free(pszValue);

			return;
		}
		else//error
		{
			MessageBox("Error reading location key!");
		}
			
	}
	::RegCloseKey(pKey);
    strcpy(pszPath, "Error");
	return;
}



// ************************************************************ 
// ***     TrackIR Enhanced SDK Initialization procedure    ***
// ***    This implementation uses the DLL wrapper module   ***
// ************************************************************ 
void CNPTestDlg::TrackIR_Enhanced_Init()
{

    NPRESULT result;

    // Zero TrackIR SDK Related counters
	NPFrameSignature = 0;
	NPStaleFrames = 0;


    // Locate the TrackIR Enhanced DLL
    TCHAR szPath[MAX_PATH * 2];

    GetDllLocation(szPath);
    DisplayLine(szPath);     
	
    // Initialize the the TrackIR Enhanced DLL
	result = NPClient_Init( szPath );
	if( NP_OK == result )
		DisplayLine( "NPClient interface -- initialize OK." );
	else
		DisplayLine( "Error initializing NPClient interface!!" );


    // Register your applications Window Handle 
	result = NP_RegisterWindowHandle( GetSafeHwnd() );
	if( NP_OK == result )
		DisplayLine( "NPClient : Window handle registration successful." );
	else
		DisplayLine( "NPClient : Error registering window handle!!" );


	// Query for the NaturalPoint TrackIR software version
	unsigned short wNPClientVer;
	result = NP_QueryVersion( &wNPClientVer );
	if( NP_OK == result )
		{
		CString csMajorVer, csMinorVer, csVerMsg;
		csMajorVer.Format( "%d", (wNPClientVer >> 8) );
		csMinorVer.Format( "%02d", (wNPClientVer & 0x00FF) );
		csVerMsg.Format( "NaturalPoint software version is %s.%s", csMajorVer, csMinorVer );
		DisplayLine( csVerMsg );
		}
	else
		DisplayLine( "NPClient : Error querying NaturalPoint software version!!" );


    // Choose the Axes that you want tracking data for
	unsigned int DataFields = 0;
    
    // Rotation Axes
	DataFields |= NPPitch;
	DataFields |= NPYaw;
	DataFields |= NPRoll;

    // Translation Axes
	DataFields |= NPX;
	DataFields |= NPY;
	DataFields |= NPZ;

    // Register the Axes selection with the TrackIR Enhanced interface
	NP_RequestData(DataFields);


    // It is *required* that your application registers the Developer ID 
    // assigned by NaturalPoint!

    // Your assigned developer ID needs to be inserted below!    
    #define NP_DEVELOPER_ID 10301

    // NOTE : The title of your project must show up 
    // in the list of supported titles shown in the Profiles
    // tab of the TrackIR software, if it does not then the
    // TrackIR software will *not* transmit data to your
    // application. If your title is not present in the list, 
    // you may need to have the TrackIR software perform a
    // game list update (to download support for new Developer IDs)
    // using the menu item under the "Help" or "Update" menu.

    NP_RegisterProgramProfileID(NP_DEVELOPER_ID);


    // Stop the cursor
	result = NP_StopCursor();
	if (result == NP_OK)
		DisplayLine("Cursor stopped");
	else
		DisplayLine("NPCient : Error stopping cursor");


    // Request that the TrackIR software begins sending Tracking Data
	result = NP_StartDataTransmission();
	if (result == NP_OK)
		DisplayLine("Data transmission started");
	else
		DisplayLine("NPCient : Error starting data transmission");

}


// ************************************************************ 
// ***       TrackIR Enhanced SDK Shutdown procedure        ***
// ************************************************************ 

void CNPTestDlg::TrackIR_Enhanced_Shutdown( )
{

    // Request that the TrackIR software stop sending Tracking Data
    NP_StopDataTransmission();
    
    
    // Un-register your applications Windows Handle
    NP_UnregisterWindowHandle();
}



// ************************************************************* 
// ***   TrackIR Enhanced SDK example new frame procedure    ***
// ***   (called by the timer function for this application) ***
// ************************************************************* 

NPRESULT client_HandleTrackIRData()
{
	TRACKIRDATA tid;
    CString csDataRxMsg;
	CString t_str;

    // Query the TrackIR Enhanced Interface for the latest data
	NPRESULT result = NP_GetData( &tid );

    // If the call succeeded, then we have data to process
	if( NP_OK == result )
	{
        // Make sure the remote interface is active
		if (tid.wNPStatus == NPSTATUS_REMOTEACTIVE)
		{
            // Compare the last frame signature to the current one if 
            // they are not the same then the data is new
			if (NPFrameSignature != tid.wPFrameSignature)
			{

                // In your own application, this is where you would utilize
                // the Tracking Data for View Control / etc.

				// Display the Tracking Data
				t_str.Format( "Rotation : NPPitch = %04.02f, NPYaw = %04.02f, NPRoll = %04.02f \r\nTranslation : NPX = %04.02f, NPY = %04.02f, NPZ = %04.02f \r\nInformation NPStatus = %d, Frame = %d", 
                               tid.fNPPitch, 
                               tid.fNPYaw, 
                               tid.fNPRoll, 
                               tid.fNPX, 
                               tid.fNPY, 
                               tid.fNPZ, 
                               tid.wNPStatus, 
                               tid.wPFrameSignature );
				pNPTestDlg->DisplayData(t_str);
				NPFrameSignature = tid.wPFrameSignature;
				NPStaleFrames = 0;
				
				//
				// All other data fields in TRACKIRDATA can be handled in a similar way.
				//
			}
			else
			{
				// Either there is no tracking data, the user has
				// paused the trackIR, or the call happened before
				// the TrackIR was able to update the interface
				// with new data
				
				if (NPStaleFrames > 30)
				{
					t_str.Format("No New Data. Paused or Not Tracking?", NPStaleFrames);
				}
				else
				{
                    NPStaleFrames++;
					t_str.Format("No New Data for %d frames", NPStaleFrames);
				}
				pNPTestDlg->DisplayData(t_str);
				result = NP_ERR_NO_DATA;
			}
		}
		else
		{
			// The user has set the device out of trackIR Enhanced Mode
			// and into Mouse Emulation mode with the hotkey
			t_str.Format("User Disabled");
			pNPTestDlg->DisplayData(t_str);
			result = NP_ERR_NO_DATA;
		}

	} 

	return result;
} 
