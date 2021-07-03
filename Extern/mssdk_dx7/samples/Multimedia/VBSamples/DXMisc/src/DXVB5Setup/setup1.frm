VERSION 5.00
Begin VB.Form frmSetup1 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00400000&
   BorderStyle     =   0  'None
   Caption         =   "VB5 Setup Toolkit"
   ClientHeight    =   1770
   ClientLeft      =   225
   ClientTop       =   1590
   ClientWidth     =   7950
   ClipControls    =   0   'False
   DrawStyle       =   5  'Transparent
   FillStyle       =   0  'Solid
   BeginProperty Font 
      Name            =   "Times New Roman"
      Size            =   24
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   -1  'True
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "setup1.frx":0000
   LinkMode        =   1  'Source
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   118
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   530
   WhatsThisHelp   =   -1  'True
   WindowState     =   2  'Maximized
   Begin VB.Label lblModify 
      AutoSize        =   -1  'True
      BorderStyle     =   1  'Fixed Single
      Caption         =   $"setup1.frx":0442
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   450
      Left            =   15
      TabIndex        =   1
      Top             =   15
      Visible         =   0   'False
      Width           =   7860
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblDDE 
      AutoSize        =   -1  'True
      BorderStyle     =   1  'Fixed Single
      Caption         =   "This label is used for DDE connection to the Program Manager"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   15
      TabIndex        =   0
      Top             =   1515
      Visible         =   0   'False
      Width           =   4485
   End
End
Attribute VB_Name = "frmSetup1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'
' Can't put this is a resource because it indicated resource load failure, must localize separately
'
Const mstrRESOURCELOADFAIL$ = "An error occurred while initializing string resources used by Setup."

'-----------------------------------------------------------
' SUB: DrawBackGround
'
' Draws the 'blue wash' screen and prints the 'shadowed'
' app setup title
'-----------------------------------------------------------
'
Private Sub DrawBackGround()
    Const intBLUESTART% = 255
    Const intBLUEEND% = 0
    Const intBANDHEIGHT% = 2
    Const intSHADOWSTART% = 8
    Const intSHADOWCOLOR% = 0
    Const intTEXTSTART% = 4
    Const intTEXTCOLOR% = 15

    Dim sngBlueCur As Single
    Dim sngBlueStep As Single
    Dim intFormHeight As Integer
    Dim intFormWidth As Integer
    Dim intY As Integer

    '
    'Get system values for height and width
    '
    intFormHeight = ScaleHeight
    intFormWidth = ScaleWidth

    '
    'Calculate step size and blue start value
    '
    sngBlueStep = intBANDHEIGHT * (intBLUEEND - intBLUESTART) / intFormHeight
    sngBlueCur = intBLUESTART

    '
    'Paint blue screen
    '
    For intY = 0 To intFormHeight Step intBANDHEIGHT
        Line (-1, intY - 1)-(intFormWidth, intY + intBANDHEIGHT), RGB(0, 0, sngBlueCur), BF
        sngBlueCur = sngBlueCur + sngBlueStep
    Next intY

    '
    'Print 'shadowed' appname
    '
    CurrentX = intSHADOWSTART
    CurrentY = intSHADOWSTART
    ForeColor = QBColor(intSHADOWCOLOR)
    Print Caption
    CurrentX = intTEXTSTART
    CurrentY = intTEXTSTART
    ForeColor = QBColor(intTEXTCOLOR)
    Print Caption
End Sub
Private Sub Form_Load()
'
' Most of the work for Setup1 takes place in Form_Load()
' and is mostly driven by the information found in the
' SETUP.LST file.  To customize the Setup1 functionality,
' you will generally want to modify SETUP.LST.
' Particularly, information regarding the files you are
' installing is all stored in SETUP.LST.  The only
' exceptions are the Remote Automation files RacMgr32.Exe
' and AutMgr32.Exe which require special handling below
' with regards to installing their icons in a special
' program group.
'
' Some customization can also be done by editing the code
' below in Form_Load or in other parts of this program.
' Places that are more likely to need customization are
' documented with suggestions and examples in the code.
'
    Const strINI_FILES$ = "Files"                           'default section to install
    Const strEXT_GRP$ = "GRP"                               'extension for progman group
    Const SW_HIDE = 0

    Dim strGroupName As String                              'Name of Program Group
    Dim sFile As FILEINFO                                   'first Files= line info
    gfRegDAO = False
    
    On Error GoTo MainError

    SetFormFont Me
    Me.Font.Size = 24
    Me.Font.Bold = True
    '
    'Initialize linespacing variables for message box calls, etc.
    '
    LF$ = Chr$(10)
    LS$ = LF$ & LF$
    CRLF = Chr$(13) & Chr$(10)
    '
    'Initialize string resources used by global vars and forms/controls
    '
    GetStrings
    
    '
    'Get Windows and Windows\System directories
    '
    gstrWinDir = GetWindowsDir()
    gstrWinSysDir = GetWindowsSysDir()

    '
    ' If the Windows System directory is a subdirectory of the
    ' Windows directory, the proper place for installation of
    ' files specified in the setup.lst as $(WinSysDest) is always
    ' the Windows \System directory.  If the Windows \System
    ' directory is *not* a subdirectory of the Windows directory,
    ' then the user is running a shared version of Windows.  In
    ' this case, if the user does not have write access to the
    ' shared system directory, we change the system files
    ' destination to the windows directory
    '
    If InStr(gstrWinSysDir, gstrWinDir) = 0 Then
        If WriteAccess(gstrWinSysDir) = False Then
            gstrWinSysDir = gstrWinDir
        End If
    End If

    '
    ' The command-line arguments must be processed as early
    ' as possible, because without them it is impossible to
    ' call the app removal program to clean up after an aborted
    ' setup.
    '
    ProcessCommandLine Command$, gfSilent, gstrSilentLog, gfSMS, gstrMIFFile, gstrSrcPath, gstrAppRemovalLog, gstrAppRemovalEXE
    gfNoUserInput = (gfSilent Or gfSMS)
    
    AddDirSep gstrSrcPath

    '
    ' The Setup Bootstrapper (SETUP.EXE) copies SETUP1.EXE and SETUP.LST to
    ' the end user's windows directory.  Information required for setup such
    ' as setup flags and fileinfo is read from the copy of SETUP.LST found in
    ' that directory.
    '
    gstrSetupInfoFile = gstrWinDir & gstrFILE_SETUP
    
    gstrAppName = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPNAME)
    If gstrAppName = gstrNULL Then
        MsgError ResolveResString(resNOSETUPLST), MB_OK Or MB_ICONSTOP, gstrSETMSG
        gstrTitle = ResolveResString(resSETUP, "|1", gstrAppName)
        ExitSetup Me, gintRET_FATAL
    End If
    
    gstrTitle = ResolveResString(resSETUP, "|1", gstrAppName)
    If gfSilent Then LogSilentMsg gstrTitle & CRLF
    '
    ' This is the default name of the group in which to install the icons.
    '
    gstrDefGroup = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_DEFGROUP)
    If gstrDefGroup = gstrNULL Then
        gstrDefGroup = gstrAppName
    End If

    '
    ' Display the background "blue-wash" setup screen as soon as we get the title
    '
    ShowMainForm

    '
    ' Display the welcome dialog
    '
    ShowWelcomeForm

    '
    ' Get name of application's executable file.  This name will be added to the
    ' program manager/Explorer.  Only the EXE Name should be entered under this key in
    ' the setup information file (SETUP.LST).
    '
    gstrAppExe = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPEXE)
    '
    ' Get that name of the app for uninstalling.  In most cases, this will be
    ' the same as gstrAppExe except when the application is a shared component.
    ' gstrAppToUninstall is used by SMS uninstall to determine the location
    ' of the uninstall files.
    '
    gstrAppToUninstall = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPTOUNINSTALL)
    
    '
    ' If this flag is set, then the default destination directory is used
    ' without question, and the user is never given a chance to change it.
    ' This is intended for installing an .EXE/.DLL as a component rather
    ' than as an application in an application directory.  In this case,
    ' having an application directory does not really make sense.
    '
    If ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_FORCEUSEDEFDEST) = "1" Then
        gfForceUseDefDest = True
    End If
    
    '
    ' Read default destination directory.  If the name specified conflicts
    ' with the name of a file, then prompt for a new default directory
    '
    gstrDestDir = ResolveDestDir(ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPDIR))
    While FileExists(gstrDestDir) = True Or gstrDestDir = gstrNULL
        If MsgError(ResolveResString(resBADDEFDIR), MB_OKCANCEL Or MB_ICONQUESTION, gstrSETMSG) = IDCANCEL Then
            ExitSetup Me, gintRET_FATAL
        End If
        
        If gfNoUserInput = True Then
            ExitSetup Me, gintRET_FATAL
        Else
            ShowPathDialog gstrDIR_DEST
        End If
    Wend

    '
    ' Ensure a trailing backslash on the destination directory
    '
    AddDirSep gstrDestDir

    Do
        '
        ' Display install button and default directory.  The user
        ' can change the destination directory from here.
        '
        ShowBeginForm

        '
        ' This would be a good place to display an option dialog, allowing the user
        ' a chance to select installation options: samples, docs, help files, etc.
        ' Results of this dialog would be checked in the loop below
        '
        'ShowOptionsDialog (Function you could write with option check boxes, etc.)
        '

        '
        ' Initialize "table" of drives used and disk space array
        '
        InitDiskInfo

        SetMousePtr gintMOUSE_HOURGLASS
        ShowStaticMessageDialog ResolveResString(resDISKSPACE)

        '
        ' For every section in SETUP.LST that will be installed, call CalcDiskSpace
        ' with the name of the section
        '
        CalcDiskSpace strINI_FILES
        'CalcDiskSpace "MySection"
        'CalcDiskSpace "MyOtherSection"
        '
        ' If you created an options dialog, you need to check results here to
        ' determine whether disk space needs to be calculated (if the option(s)
        ' will be installed)
        '
        'If chkInstallSamples.Value = TRUE then
        '    CalcDiskSpace "Samples"
        'End If
        '

        HideStaticMessageDialog
        SetMousePtr gintMOUSE_DEFAULT

    '
    ' After all CalcDiskSpace calls are complete, call CheckDiskSpace to check
    ' the results and display warning form (if necessary).  If the user wants
    ' to try another destination directory (or cleanup and retry) then
    ' CheckDiskSpace will return False
    '
    Loop While CheckDiskSpace() = False
    
    '
    ' Starts logging to the setup logfile (will be used for application removal)
    '
    EnableLogging gstrAppRemovalLog
    '
    ' Should go ahead and force the application directory to be created,
    ' since the application removal logfile will later be copied there.
    '
    MakePath gstrDestDir, False 'User may not ignore errors here
    
    '
    ' Create the main program group if one is wanted/needed.
    '
    Const fDefCreateGroupUnderWin95 = False
    '
    ' If fDefCreateGroupUnderWin95 is set to False (this is the default), then no
    ' program group will be created under Win95 unless it is absolutely necessary.
    '
    ' By default under Windows 95, no group should be created, and the
    ' single program icon should be placed directly under the
    ' Start>Programs menu (unless there are other, user-defined icons to create
    '
    Dim cIcons As Integer            ' Count of how many icons are required.
    Dim fAdditionalIcons As Boolean
    '
    ' There are two default reasons why we will install more icons than
    ' simply the program executable:
    '   1) If we are installing user-defined icons (by customizing this procedure or
    '      or by customizing SETUP.LST)
    '   2) If we are creating a program removal icon (whenever we're running under NT)
    '
    ' Read through the SETUP.LST file and determine how many icons are needed.
    '
    cIcons = CountIcons(strINI_FILES)
    '
    ' Do the same for other sections in SETUP.LST if you've added your own.
    '
    'cIcons = cIcons + CountIcons("MySection")
    'cIcons = cIcons + CountIcons("MyOtherSection")
    
    '
    ' If you have modified this procedure to install more icons, make sure you set
    ' this variable to True, so that a program group will be created for the icons.
    ' Currently, it is True only if more then one entry was found in the "Files"
    ' section of SETUP.LST that specified an icon or if this is NT 3.51.
    '
    fAdditionalIcons = False
    fAdditionalIcons = fAdditionalIcons Or (cIcons > 1)
    fAdditionalIcons = fAdditionalIcons Or (Not TreatAsWin95())
    '
    ' The following variable determines whether or not we create a program
    ' group for icons.  It is controlled by fNoGroupUnderWin95,
    ' fAdditionalIcons, and FTreatAsWin95().
    '
    Dim fCreateGroup As Boolean
    If TreatAsWin95() Then
        '
        ' Win95 only:
        ' We create a program group only if we have additional icons besides
        ' the application executable (if any), or if fDefCreateGroupUnderWin95
        ' has been set to True to override this default behavior.
        '
        fCreateGroup = fDefCreateGroupUnderWin95 Or fAdditionalIcons
    Else
        '
        ' Win32 NT only:
        ' We must always create a Program Manager group
        ' because we always create an icon for the application removal program.
        '
        fCreateGroup = True
    End If
    
    strGroupName = ""
    If fCreateGroup Then
        strGroupName = frmGroup.GroupName(frmSetup1, gstrDefGroup)
        fMainGroupWasCreated = True
    End If

    '
    ' Show copy form and set copy gauge percentage to zero
    '
    SetMousePtr gintMOUSE_HOURGLASS
    ShowCopyDialog
    UpdateStatus frmCopy.picStatus, 0, True

    '
    ' Always start with Disk #1
    '
    gintCurrentDisk = 1
    '
    ' For every section in SETUP.LST that needs to be installed, call CopySection
    ' with the name of the section
    '
    
    CopySection strINI_FILES
    'CopySection "MySection"
    'CopySection "MyOtherSection"
        
    '
    ' If you created an options dialog, you need to check results here to
    ' determine whether to copy the files in the particular section(s).
    '
    'If chkInstallSamples.Value = TRUE then
    '    CopySection "Samples"
    'End If
    '

    UpdateStatus frmCopy.picStatus, 1, True
    
    HideCopyDialog

    '
    ' If we installed AXDIST.EXE, we now need to run it
    ' so it will install any additional files it contains.
    '
    If gfAXDist = True Then
        '
        'Synchronously shell out and run the utility with the correct switches
        '
        If FileExists(gstrAXDISTInstallPath) Then
            FSyncShell gstrAXDISTInstallPath, SW_HIDE
        End If
    End If
    '
    ' If we installed WINt351.EXE, we now need to run it
    ' so it will install any additional files it contains.
    '
    If gfWINt351 = True Then
        '
        'Synchronously shell out and run the utility with the correct switches
        '
        If FileExists(gstrWINt351InstallPath) Then
            FSyncShell gstrWINt351InstallPath, SW_HIDE
        End If
    End If
    '
    ' Now, do all the 'invisible' update things that are required
    '
    SetMousePtr gintMOUSE_DEFAULT
    ShowStaticMessageDialog ResolveResString(resUPDATING)

    '
    ' Register all the files that have been saved in the registration array.  The
    ' CopySection API adds a registration entry (when required) if a file is copied.
    '
    RegisterFiles
    
    '
    ' Register all the licenses that appear in the [Licenses] section of
    ' Setup.lst.
    '
    RegisterLicenses
    
    '
    ' If any DAO files were installed, we need to add some special
    ' keys to the registry to support it so that links will work
    ' in OLE Database fields.
    '
    If gfRegDAO = True Then
        RegisterDAO
    End If
    
    CheckForAndInstallDirectX strINI_FILES
    '
    ' Create program icons (or links, i.e. shortcuts).
    '
    If (fMainGroupWasCreated = True) Or ((cIcons > 0) And TreatAsWin95()) Then
        ShowStaticMessageDialog ResolveResString(resPROGMAN)
        CreateIcons strINI_FILES, strGroupName
        '
        ' Do the same for other sections in SETUP.LST if you've added your own.
        '
        'CreateIcons "MySection", strGroupName
        'CreateIcons "MyOtherSection", strGroupName
        '
    End If
    '
    ' If you still need to create more icons, insert code here, and make certain
    ' that you have set the variable fAdditionalIcons to True above
    '
    ' If Not fAdditionalIcons Then
    '   MsgError "Internal Setup Customization Error: fAdditionalIcons = False", vbOKOnly Or vbExclamation, gstrTitle
    '   ExitSetup Me, gintRET_FATAL
    ' End If
    ' CreateOSLink frmSetup1, strGroupName, gsDest.strAppDir & "My Exe 1.exe", "My Exe 1 command-line arguments", "My Exe 1"
    ' CreateOSLink frmSetup1, strGroupName, gsDest.strAppDir & "My Exe 2.exe", "My Exe 2 command-line arguments", "My Exe 2"
    '



    '
    ' Create a separate program group and icons for the Remote Automation
    ' Connection Manager and the Automation Manager, if either has been
    ' installed.
    ' This program group is entirely separate from the one created for the
    ' application program (if any), because it will be shared by all
    ' VB applications which install them.
    '
    ' NOTE: This is NOT the place to install additional icons.  This is
    ' NOTE: handled after the Remote Automation icons have been created.
    '
    ShowStaticMessageDialog ResolveResString(resPROGMAN)
    If gsDest.strAUTMGR32 <> "" Or gsDest.strRACMGR32 <> "" Then
        'At least one of these programs was installed.  Go ahead
        'and create the program group.
        Dim strRemAutGroupName As String
        
        strRemAutGroupName = ResolveResString(resREMAUTGROUPNAME)
        '
        ' Create the group for the Remote Automation Icons.  Note that
        ' since the user cannot choose the name of this group, there is
        ' no way at this point to correct an error if one occurs.  Therefore,
        ' fCreateOSProgramGroup will abort setup, without returning, if there
        ' is an error.
        '
        fCreateOSProgramGroup frmSetup1, strRemAutGroupName, False, False

        'Now create the icons for AUTMGR32.EXE and RACMGR32.EXE
        If gsDest.strRACMGR32 <> "" Then
            CreateOSLink frmSetup1, strRemAutGroupName, gsDest.strRACMGR32, "", ResolveResString(resRACMGR32ICON), False
        End If
        If gsDest.strAUTMGR32 <> "" Then
            CreateOSLink frmSetup1, strRemAutGroupName, gsDest.strAUTMGR32, "", ResolveResString(resAUTMGR32ICON), False
        End If
    End If

    '
    'Register the per-app path
    '
    If gstrAppExe <> "" Then
        Dim strPerAppPath As String
        strPerAppPath = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPPATH)
        AddPerAppPath gstrAppExe, gsDest.strAppDir, strPerAppPath
    End If

ExitSetup:
    HideStaticMessageDialog
    RestoreProgMan
    If fWithinAction() Then
        'By now, all logging actions should have been either aborted or committed.
        MsgError ResolveResString(resSTILLWITHINACTION), vbExclamation Or vbOKOnly, gstrTitle
        ExitSetup Me, gintRET_FATAL
    End If
    MoveAppRemovalFiles strGroupName
    '
    ' If we installed AXDIST.EXE, we now need to delete it.
    ' It's a self-extracting file that has already extracted.
    '
    If FileExists(gstrAXDISTInstallPath) Then
        Kill gstrAXDISTInstallPath
    End If
    '
    ' If we installed WINt351.EXE, we now need to delete it.
    ' It's a self-extracting file that has already extracted.
    '
    If FileExists(gstrWINt351InstallPath) Then
        Kill gstrWINt351InstallPath
    End If
    
    ExitSetup Me, gintRET_FINISHEDSUCCESS

MainError:
    Dim iRet As Integer
    iRet = MsgError(Error$ & LS$ & ResolveResString(resUNEXPECTED), MB_RETRYCANCEL Or MB_ICONEXCLAMATION, gstrTitle)
    If gfNoUserInput Then iRet = IDCANCEL
    Select Case iRet
        Case IDRETRY
            Resume
        Case IDCANCEL
            ExitSetup Me, gintRET_ABORT
            Resume
        'End Case
    End Select
End Sub

'-----------------------------------------------------------
' SUB: HideCopyDialog
'
' Unloads the copy files status form
'-----------------------------------------------------------
'
Private Sub HideCopyDialog()
    Unload frmCopy
End Sub

'-----------------------------------------------------------
' SUB: HideStaticMessageDialog
'
' Unloads the setup messages form
'-----------------------------------------------------------
'
Private Sub HideStaticMessageDialog()
    Unload frmMessage
End Sub

'-----------------------------------------------------------
' SUB: ShowBeginForm
'
' Displays the begin setup form
'-----------------------------------------------------------
'
Private Sub ShowBeginForm()
    If gfNoUserInput Then
        If IsValidDestDir(gstrDestDir) = False Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    Else
        frmBegin.Show 1
    End If
End Sub

'-----------------------------------------------------------
' SUB: ShowCopyDialog
'
' Displays the copy files status form
'-----------------------------------------------------------
'
Private Sub ShowCopyDialog()
    CenterForm frmCopy
    frmCopy.Show
    frmCopy.Refresh
    If gfNoUserInput = True Then
        frmCopy.cmdExit.Visible = False
    Else
        frmCopy.cmdExit.SetFocus
    End If
End Sub

'-----------------------------------------------------------
' SUB: ShowMainForm
'
' Displays the main setup 'blue wash' form
'-----------------------------------------------------------
'
Private Sub ShowMainForm()
    Me.Caption = gstrTitle
    Me.Show
    DrawBackGround
    Me.Refresh
End Sub

'-----------------------------------------------------------
' SUB: ShowStaticMessageDialog
'
' Displays a setup message in a 'box' of the appropriate
' size for the message
'
' IN: [strMessage] - message to display
'-----------------------------------------------------------
'
Private Sub ShowStaticMessageDialog(ByVal strMessage As String)
    Dim frm As Form

    Set frm = frmMessage
    frm.lblMsg.Caption = strMessage

    '
    'Default height is twice the height of the setup icon.
    'If the height of the message text is greater, then
    'increase the form height to the label height plus
    'half an icon height
    '
    frm.ScaleHeight = frm.imgMsg.Height * 2
    If frm.lblMsg.Height > frm.ScaleHeight Then
        frm.ScaleHeight = frm.lblMsg.Height + frm.imgMsg.Height * 0.5
    End If

    '
    'Vertically center the icon and label within the form
    '
    frm.imgMsg.Top = frm.ScaleHeight / 2 - frm.imgMsg.Height / 2
    frm.lblMsg.Top = frm.ScaleHeight / 2 - frm.lblMsg.Height / 2

    CenterForm frm

    frm.Show
    frm.Refresh
End Sub

'-----------------------------------------------------------
' SUB: ShowWelcomeForm
'
' Displays the welcome to setup form
'-----------------------------------------------------------
'
Private Sub ShowWelcomeForm()
    If Not gfNoUserInput Then
        frmWelcome.Show 1
    End If
End Sub

'-----------------------------------------------------------
' SUB: GetStrings
'
' Loads string resources into global vars and forms/controls
'-----------------------------------------------------------
'
Private Sub GetStrings()
    On Error GoTo GSErr
    
    gstrSETMSG = ResolveResString(resSETMSG)
    
    Exit Sub
    
GSErr:
    MsgError mstrRESOURCELOADFAIL, MB_ICONSTOP Or MB_OK, gstrNULL
    ExitSetup Me, gintRET_FATAL
End Sub

