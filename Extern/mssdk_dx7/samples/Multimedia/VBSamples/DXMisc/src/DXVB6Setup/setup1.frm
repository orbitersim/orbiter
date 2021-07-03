VERSION 5.00
Begin VB.Form frmSetup1 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00400000&
   BorderStyle     =   0  'None
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
   HasDC           =   0   'False
   Icon            =   "setup1.frx":0000
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   118
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   530
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

'
' Can't put this is a resource because it indicated resource load failure, must localize separately
'
Private Const mstrRESOURCELOADFAIL$ = "An error occurred while initializing string resources used by Setup."

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
    Const intRed% = 1
    Const intGreen% = 2
    Const intBlue% = 4
    Const intBackRed% = 8
    Const intBackGreen% = 16
    Const intBackBlue% = 32
    Dim sngBlueCur As Single
    Dim sngBlueStep As Single
    Dim intFormHeight As Integer
    Dim intFormWidth As Integer
    Dim intY As Integer
    Dim iColor As Integer
    Dim iRed As Single, iBlue As Single, iGreen As Single
    
    '
    'Get system values for height and width
    '
    intFormHeight = ScaleHeight
    intFormWidth = ScaleWidth

    If Len(ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_COLOR)) = 0 Then
        iColor = intBlue
    Else
        iColor = CInt(ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_COLOR))
    End If
    'Calculate step size and blue start value
    '
    sngBlueStep = intBANDHEIGHT * (intBLUEEND - intBLUESTART) / intFormHeight
    sngBlueCur = intBLUESTART

    '
    'Paint blue screen
    '
    For intY = 0 To intFormHeight Step intBANDHEIGHT
        If iColor And intBlue Then iBlue = sngBlueCur
        If iColor And intRed Then iRed = sngBlueCur
        If iColor And intGreen Then iGreen = sngBlueCur
        If iColor And intBackBlue Then iBlue = 255 - sngBlueCur
        If iColor And intBackRed Then iRed = 255 - sngBlueCur
        If iColor And intBackGreen Then iGreen = 255 - sngBlueCur
        Line (-1, intY - 1)-(intFormWidth, intY + intBANDHEIGHT), RGB(iRed, iGreen, iBlue), BF
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
' installing is all stored in SETUP.LST.  Exceptions include
' the Remote Automation files RacMgr32.Exe and AutMgr32.Exe
' and special redistributable packages such as mdac_typ.exe.
' These require special handling below.
'
' Some customization can also be done by editing the code
' below in Form_Load or in other parts of this program.
' Places that are more likely to need customization are
' documented with suggestions and examples in the code.
'

    '
    'Uncomment these three lines for debugging.  To debug:
    '1) Rebuild Setup1.exe and rebuild the cab file
    '   to include the new Setup1.exe.
    '2) Run setup.exe against the new cab
    '3) When the message box appears, open the Setup1 project
    '   in VB, paste the command line from the clipboard into the
    '   Project/Properties/Make/Command Line Arguments field.
    '4) F5 in VB.
    '
    'Clipboard.Clear
    'Clipboard.SetText Command$
    'MsgBox Command$
    
    Const fDefCreateGroupUnderWin95 = False

    Dim strGroupName As String                              'Name of Program Group
    Dim oFont As StdFont
    Dim lChar As Long
    Dim cIcons As Integer            ' Count of how many icons are required.
    Dim cGroups As Integer           ' Count of how many groups are required.
    Dim fCreateGroup As Boolean

    Dim iLoop As Integer
    Dim sUCASEStartMenuKey As String
    Dim sUCASEProgramsMenuKey As String
    Dim sGroup As String

    Dim strRemAutGroupName As String

    Dim strPerAppPath As String

    Dim iRet As Integer

    gfRegDAO = False
    
    On Error GoTo MainError

    SetFormFont Me
    'All the controls and the form are sharing the
    'same font object, so create a new font object
    'for the form so that the appearance of all the
    'controls are not changed also
    Set oFont = New StdFont
    With oFont
        .Size = 24
        .Bold = True
        .Italic = True
        .Charset = lblModify.Font.Charset
        .Name = lblModify.Font.Name
    End With
    Set Font = oFont
    '
    'Initialize string resources used by global vars and forms/controls
    '
    GetStrings
    
    '
    'Get Windows, Windows\Fonts, and Windows\System directories
    '
    gstrWinDir = GetWindowsDir()
    gstrWinSysDir = GetWindowsSysDir()
    gstrFontDir = GetWindowsFontDir()

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
    ' Avoid Option Compare Text and use explicit UCase comparisons because there
    ' is a Unicode character (&H818F) which is equal to a path separator when
    ' using Option Compare Text.
    If InStr(UCase$(gstrWinSysDir), UCase$(gstrWinDir)) <> 1 Then
        If Not WriteAccess(gstrWinSysDir) Then
            gstrWinSysDir = gstrWinDir
        End If
    End If

    '
    ' The command-line arguments must be processed as early
    ' as possible, because without them it is impossible to
    ' call the app removal program to clean up after an aborted
    ' setup.
    '
#If SMS Then
    ProcessCommandLine Command$, gfSilent, gstrSilentLog, gfSMS, gstrMIFFile, gstrSrcPath, gstrAppRemovalLog, gstrAppRemovalEXE
    gfNoUserInput = (gfSilent Or gfSMS)
#Else
    ProcessCommandLine Command$, gfSilent, gstrSilentLog, gstrSrcPath, gstrAppRemovalLog, gstrAppRemovalEXE
    gfNoUserInput = gfSilent
#End If

    AddDirSep gstrSrcPath

    '
    ' The Setup Bootstrapper (SETUP.EXE) copies SETUP1.EXE and SETUP.LST to
    ' the end user's windows directory.  Information required for setup such
    ' as setup flags and fileinfo is read from the copy of SETUP.LST found in
    ' that directory.
    '
    gstrSetupInfoFile = gstrWinDir & gstrFILE_SETUP
    'Get the Appname (this will be shown on the blue wash screen)
    gstrAppName = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPNAME)
    gintCabs = CInt(ReadIniFile(gstrSetupInfoFile, gstrINI_BOOT, gstrINI_CABS))
    If Len(gstrAppName) = 0 Then
        MsgError ResolveResString(resNOSETUPLST), vbOKOnly Or vbCritical, gstrSETMSG
        gstrTitle = ResolveResString(resSETUP, gstrPIPE1, gstrAppName)
        ExitSetup Me, gintRET_FATAL
    End If

    gstrAppExe = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPEXE)
    gstrTitle = ResolveResString(resSETUP, gstrPIPE1, gstrAppName)
    If gfSilent Then LogSilentMsg gstrTitle & vbCrLf

    'Get a temporary directory to use
    gsTEMPDIR = String$(255, 0)
    lChar = GetTempPath(255, gsTEMPDIR)
    gsTEMPDIR = Left$(gsTEMPDIR, lChar)
    AddDirSep gsTEMPDIR
    gsTEMPDIR = gsTEMPDIR & ReadIniFile(gstrSetupInfoFile, gstrINI_BOOT, gsINI_TEMPDIR)
    AddDirSep gsTEMPDIR
    '
    ' Get the name of the CAB
    '
    gsCABFULLNAME = gstrWinDir & ReadIniFile(gstrSetupInfoFile, gstrINI_BOOT, gstrINI_CABNAME)
    '
    ' Display the background "blue-wash" setup screen as soon as we get the title
    '
    ShowMainForm
    '
    ' Display the welcome dialog
    '
    ShowWelcomeForm
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
    Do While FileExists(gstrDestDir) Or Len(gstrDestDir) = 0
        If MsgError(ResolveResString(resBADDEFDIR), vbOKCancel Or vbQuestion, gstrSETMSG) = vbCancel Then
            ExitSetup Me, gintRET_FATAL
        End If

        If gfNoUserInput Then
            ExitSetup Me, gintRET_FATAL
        Else
            ShowPathDialog
        End If
    Loop
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

        SetMousePtr vbHourglass
        ShowStaticMessageDialog ResolveResString(resDISKSPACE)
        '
        ' For every section in SETUP.LST that will be installed, call CalcDiskSpace
        ' with the name of the section
        '
        CalcDiskSpace gstrINI_FILES
        'CalcDiskSpace "MySection"
        'CalcDiskSpace "MyOtherSection"
        '
        ' If you created an options dialog, you need to check results here to
        ' determine whether disk space needs to be calculated (if the option(s)
        ' will be installed)
        '
        'If chkInstallSamples.Value then
        '    CalcDiskSpace "Samples"
        'End If
        '

        HideStaticMessageDialog
        SetMousePtr vbDefault

    '
    ' After all CalcDiskSpace calls are complete, call CheckDiskSpace to check
    ' the results and display warning form (if necessary).  If the user wants
    ' to try another destination directory (or cleanup and retry) then
    ' CheckDiskSpace will return False
    '
    Loop Until CheckDiskSpace()
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
    '
    ' If fDefCreateGroupUnderWin95 is set to False (this is the default), then no
    ' program group will be created under Win95 unless it is absolutely necessary.
    '
    ' By default under Windows 95, no group should be created, and the
    ' single program icon should be placed directly under the
    ' Start>Programs menu (unless there are other, user-defined icons to create
    '
    '
    ' Read through the SETUP.LST file and determine how many icons are needed.
    '
    cIcons = CountIcons(gsICONGROUP)
    cGroups = CountGroups(gsICONGROUP)
    '
    ' Do the same for other sections in SETUP.LST if you've added your own.
    '
    'cIcons = cIcons + CountIcons("MySection")
    'cIcons = cIcons + CountIcons("MyOtherSection")
    
    '
    ' The following variable determines whether or not we create a program
    ' group for icons.  It is controlled by fNoGroupUnderWin95,
    ' fAdditionalIcons, and FTreatAsWin95().
    '
    fCreateGroup = (cGroups > 0)
    
    If fCreateGroup Then
        For iLoop = 0 To cGroups - 1
            sGroup = GetGroup(gsICONGROUP, iLoop)
            strGroupName = vbNullString
            Select Case UCase$(sGroup)
                Case UCase$(gsSTARTMENUKEY), UCase$(gsPROGMENUKEY)
                    ' Skip start menu and programs - they're already there and don't
                    ' need to be created.
                Case Else
                    strGroupName = frmGroup.GroupName(frmSetup1, sGroup, GetPrivate(gsICONGROUP, iLoop), GetStart(gsICONGROUP, iLoop))
                    If UCase$(sGroup) <> UCase$(strGroupName) Then
                        SetGroup gsICONGROUP, iLoop, strGroupName
                    End If
            End Select
            fMainGroupWasCreated = True
        Next
    End If
    
    ' Before we begin copying files, check for mdac_typ
    ' and if we find it, spawn that off first.  We will tell
    ' it to never reboot, and check at the end to see if we need to.
    DoEvents
    If CheckDataAccess Then
        'We need to install data access.  Display message.
        ShowStaticMessageDialog ResolveResString(resINSTALLADO)
        InstallDataAccess
        HideStaticMessageDialog
    End If
    '
    ' Show copy form and set copy gauge percentage to zero
    '
    SetMousePtr vbHourglass
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
    
    CopySection gstrINI_FILES
    'CopySection "MySection"
    'CopySection "MyOtherSection"
        
    '
    ' If you created an options dialog, you need to check results here to
    ' determine whether to copy the files in the particular section(s).
    '
    'If chkInstallSamples.Value then
    '    CopySection "Samples"
    'End If
    '
    UpdateStatus frmCopy.picStatus, 1, True

    HideCopyDialog
    '
    ' Now, do all the 'invisible' update things that are required
    '
    SetMousePtr vbDefault
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
    If gfRegDAO Then
        RegisterDAO
    End If
    CheckForAndInstallDirectX gstrINI_FILES, Me.hWnd
    
    '
    ' Create program icons (or links, i.e. shortcuts).
    '
    If fMainGroupWasCreated Or (cIcons > 0) Then
        ShowStaticMessageDialog ResolveResString(resPROGMAN)
        CreateIcons gsICONGROUP
        '
        ' Do the same for other sections in SETUP.LST if you've added your own.
        '
        'CreateIcons "MySection"
        'CreateIcons "MyOtherSection"
        '
    End If
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
    If Len(gsDest.strAUTMGR32) > 0 Or Len(gsDest.strRACMGR32) > 0 Then
        'At least one of these programs was installed.  Go ahead
        'and create the program group.
        strRemAutGroupName = ResolveResString(resREMAUTGROUPNAME)
        '
        ' Create the group for the Remote Automation Icons.  Note that
        ' since the user cannot choose the name of this group, there is
        ' no way at this point to correct an error if one occurs.  Therefore,
        ' fCreateShellGroup will abort setup, without returning, if there
        ' is an error.
        '
        fCreateShellGroup strRemAutGroupName, False, False

        'Now create the icons for AUTMGR32.EXE and RACMGR32.EXE
        If Len(gsDest.strRACMGR32) > 0 Then
            CreateShellLink gsDest.strRACMGR32, strRemAutGroupName, vbNullString, ResolveResString(resRACMGR32ICON), True, gsPROGMENUKEY, False
        End If
        If Len(gsDest.strAUTMGR32) > 0 Then
            CreateShellLink gsDest.strAUTMGR32, strRemAutGroupName, vbNullString, ResolveResString(resAUTMGR32ICON), True, gsPROGMENUKEY, False
        End If
    End If
    '
    'Register the per-app path
    '
    If Len(gstrAppExe) > 0 Then
        strPerAppPath = ReadIniFile(gstrSetupInfoFile, gstrINI_SETUP, gstrINI_APPPATH)
        AddPerAppPath gstrAppExe, gsDest.strAppDir, strPerAppPath
    End If

ExitSetup:
    HideStaticMessageDialog
    If fWithinAction() Then
        'By now, all logging actions should have been either aborted or committed.
        MsgError ResolveResString(resSTILLWITHINACTION), vbExclamation Or vbOKOnly, gstrTitle
        ExitSetup Me, gintRET_FATAL
    End If
    MoveAppRemovalFiles strGroupName

    ExitSetup Me, gintRET_FINISHEDSUCCESS

MainError:
    iRet = MsgError(Err.Description & vbLf & vbLf & ResolveResString(resUNEXPECTED), vbRetryCancel Or vbExclamation, gstrTitle)
    If gfNoUserInput Then iRet = vbCancel
    Select Case iRet
        Case vbRetry
            Resume
        Case vbCancel
            ExitSetup Me, gintRET_ABORT
            Resume
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
        If Not IsValidDestDir(gstrDestDir) Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    Else
        frmBegin.Show vbModal
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
    If gfNoUserInput Then
        frmCopy.cmdExit.Visible = False
    End If
    frmCopy.Show
    frmCopy.Refresh
    If frmCopy.cmdExit.Visible Then
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
    Caption = gstrTitle
    Show
    DrawBackGround
    Refresh
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
        frmWelcome.Show vbModal
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
    MsgError mstrRESOURCELOADFAIL, vbCritical Or vbOKOnly, vbNullString
    ExitSetup Me, gintRET_FATAL
End Sub

Private Sub Form_Unload(Cancel As Integer)
    CleanUpCabs
End Sub
