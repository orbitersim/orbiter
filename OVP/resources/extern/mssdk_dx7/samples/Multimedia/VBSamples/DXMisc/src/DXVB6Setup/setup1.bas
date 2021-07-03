Attribute VB_Name = "basSetup1"
Option Explicit
Option Compare Text
'
'Public Constants
'

Public Enum OverwriteReturnVal
    owYes
    owNo
    owNoToAll
End Enum

Public Enum FileComparison
    fcOlder
    fcEquivalent
    fcNewer
End Enum

'DirectX Setup Install constants
Private Const DSETUP_DDRAWDRV As Long = 8
Private Const DSETUP_DSOUNDDRV As Long = 16
Private Const DSETUP_DXCORE As Long = 65536
Private Const DSETUP_DIRECTX As Long = (DSETUP_DXCORE Or DSETUP_DDRAWDRV Or DSETUP_DSOUNDDRV)
Private Const DSETUP_TESTINSTALL As Long = 131072
Private Const DSETUP_NTINSTALL As Long = 524288
Private Const DSETUPERR_SUCCESS_RESTART As Long = 1
Private Const DSETUPERR_SUCCESS As Long = 0
Private Const DSETUP_VERSION As Long = &H40000

'DirectX Setup Install routines
Private Declare Function DirectXSetup Lib "dsetup.dll" Alias "DirectXSetupA" (ByVal hWnd As Long, ByVal lpszRootPath As String, ByVal dwFlags As Long) As Long
Private Declare Function DirectXSetupGetVersion Lib "dsetup.dll" (dwVersion As Long, dwRevision As Long) As Long

'Return values for setup toolkit functions
Public Const gintRET_CONT% = 1
Public Const gintRET_CANCEL% = 2
Public Const gintRET_EXIT% = 3
Public Const gintRET_ABORT% = 4
Public Const gintRET_FATAL% = 5
Public Const gintRET_FINISHEDSUCCESS% = 6 'Used only as parameter to ExitSetup at end of successful install

'Error levels for GetAppRemovalCmdLine()
Public Const APPREMERR_NONE = 0 'no error
Public Const APPREMERR_FATAL = 1 'fatal error
Public Const APPREMERR_NONFATAL = 2 'non-fatal error, user chose to abort
Public Const APPREMERR_USERCANCEL = 3 'user chose to cancel (no error)

'Beginning of lines in [Files], [Bootstrap], and [Licenses] sections of SETUP.LST
Public Const gstrINI_FILE$ = "File"
Public Const gstrINI_REMOTE$ = "Remote"
Public Const gstrINI_LICENSE$ = "License"
'
' Command line constants
'
'These should remain lowercase
Public Const gstrSILENTSWITCH = "s"
#If SMS Then
Public Const gstrSMSSWITCH = "q"
#End If
'
' Icon Information
'
Public Const gsGROUP As String = "Group"
Public Const gsICON As String = "Icon"
Public Const gsTITLE As String = "Title"
Public Const gsICONGROUP As String = "IconGroups"

Public Const gstrINI_BOOTFILES$ = "Bootstrap Files"

'Font info
'These should remain uppercase
Public Const gsEXT_FONTTTF As String = "TTF"
Public Const gsEXT_FONTFON As String = "FON"
Private Declare Function AddFontResource Lib "gdi32" Alias "AddFontResourceA" (ByVal lpFilename As String) As Long
 
'Registry files (execute them based on .reg extension)
Public Const gsREGEDIT As String = "regedit /s "
Public Const gsEXT_REG As String = "reg"
'
'Type Definitions
'
Public Type FILEINFO                                        ' Setup information file line format
    intDiskNum As Integer                                   ' disk number
    fDestDirRecognizedBySetupExe As Boolean                 ' Does setup.exe recognize this destination directory macro?
    strSrcName As String                                    ' name of source file
    strDestName As String                                   ' name of destination file
    strDestDir As String                                    ' destination directory
    strRegister As String                                   ' registration info
    fShared As Boolean                                      ' whether the file is shared or private
    fSystem As Boolean                                      ' whether the file is a system file (i.e. should be installed but never removed)
    varDate As Date                                         ' file date
    lFileSize As Long                                       ' file size
    sVerInfo As VERINFO                                     ' file version number
    strReserved As String                                   ' Reserved. Leave empty, or error.
    strProgramIconTitle As String                                ' Caption for icon in program group
    strProgramIconCmdLine As String                         ' Command Line for icon in program group
End Type

Public Type DISKINFO                                        ' Disk drive information
    lAvail As Long                                          ' Bytes available on drive
    lReq As Long                                            ' Bytes required for setup
    lMinAlloc As Long                                       ' minimum allocation unit
End Type

Public Type DESTINFO                                        ' save dest dir for certain files
    strAppDir As String
    strAUTMGR32 As String
    strRACMGR32 As String
End Type

Public Type REGINFO                                         ' save registration info for files
    strFilename As String
    strRegister As String
    
    'The following are used only for remote server registration
    strNetworkAddress As String
    strNetworkProtocol As String
    intAuthentication As Integer
    fDCOM As Boolean      ' True if DCOM, otherwise False
End Type

'
'Public Variables
'
Public gstrSETMSG As String
Public gintRetVal As Integer                                  'return value for form based functions
Public gstrAppName As String                                'name of app being installed
Public gintCabs As Long
Public gstrTitle As String                                  '"setup" name of app being installed
Public gstrDestDir As String                                'dest dir for application files
Public gstrAppExe As String                                 'name of app .EXE being installed
Public gstrAppToUninstall As String                         ' Name of app exe/ocx/dll to be uninstalled.  Should be the same as gstrAppExe in most cases.
Public gstrSrcPath As String                                'path of source files
Public gstrSetupInfoFile As String                          'pathname of SETUP.LST file
Public gstrWinDir As String                                 'windows directory
Public gstrFontDir As String                                'windows\font directory
Public gstrWinSysDir As String                              'windows\system directory
Public gsDiskSpace() As DISKINFO                            'disk space for target drives
Public gcolDrivesUsed As Collection                         'dest drives used by setup
Public glTotalCopied As Long                                'total bytes copied so far
Public gintCurrentDisk As Integer                           'current disk number being installed
Public gsDest As DESTINFO                                   'dest dirs for certain files
Public gstrAppRemovalLog As String                           'name of the app removal logfile
Public gstrAppRemovalEXE As String                           'name of the app removal executable
Public gfAppRemovalFilesMoved As Boolean                     'whether or not the app removal files have been moved to the application directory
Public gfForceUseDefDest As Boolean                         'If set to true, then the user will not be prompted for the destination directory
Public fMainGroupWasCreated As Boolean                     'Whether or not a main folder/group has been created
Public gfRegDAO As Boolean                                 ' If this gets set to true in the code, then
                                                           ' we need to add some registration info for DAO
                                                           ' to the registry.

Public gfDXReboot As Boolean                                ' we need to reboot because of DirectX
Public gsCABFULLNAME As String
Public gsTEMPDIR As String

Public Const gsINI_CABNAME As String = "Cab"
Public Const gsINI_TEMPDIR As String = "TmpDir"
'
'Form/Module Constants
'

'SetFileTime junk
Public Type FileTime
    dwLowDateTime As Long
    dwHighDateTime As Long
End Type
Public Type SYSTEMTIME
    wYear As Integer
    wMonth As Integer
    wDayOfWeek As Integer
    wDay As Integer
    wHour As Integer
    wMinute As Integer
    wSecond As Integer
    wMilliseconds As Integer
End Type

Public Const GENERIC_WRITE As Long = &H40000000
Public Const GENERIC_READ As Long = &H80000000
Public Const FILE_ATTRIBUTE_NORMAL As Long = &H80
Public Const FILE_FLAG_WRITE_THROUGH As Long = &H80000000
Public Const OPEN_EXISTING As Long = 3
Public Const INVALID_HANDLE_VALUE As Long = -1
Public Const ERROR_SHARING_VIOLATION As Long = 32

Public Declare Function LocalFileTimeToFileTime Lib "Kernel32" (lpFileTime As FileTime, lpLocalFileTime As FileTime) As Long
Public Declare Function CreateFile Lib "Kernel32" Alias "CreateFileA" (ByVal lpFilename As String, ByVal dwDesiredAccess As Long, ByVal dwShareMode As Long, ByVal lpSecurityAttributes As Long, ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, ByVal hTemplateFile As Long) As Long
Public Declare Function SetFileTime Lib "Kernel32" (ByVal hFile As Long, lpCreationTime As FileTime, lpLastAccessTime As FileTime, lpLastWriteTime As FileTime) As Long
Public Declare Function CloseHandle Lib "Kernel32" (ByVal hObject As Long) As Long
Public Declare Function SystemTimeToFileTime Lib "Kernel32" (lpSystemTime As SYSTEMTIME, lpFileTime As FileTime) As Long
Public Declare Function VariantChangeTypeEx Lib "oleaut32.dll" (ByVal pvArgDest As Long, ByVal pvArgSrc As Long, ByVal LCID As Long, ByVal wFlags As Integer, ByVal VarType As Integer) As Long
Public Declare Function VariantTimeToSystemTime Lib "oleaut32.dll" (ByVal vtime As Date, lpSystemTime As SYSTEMTIME) As Long

'Special file names
Private Const mstrFILE_APPREMOVALLOGBASE$ = "ST6UNST"               'Base name of the app removal logfile
Private Const mstrFILE_APPREMOVALLOGEXT$ = ".LOG"                   'Default extension for the app removal logfile
'These should remain uppercase
Private Const mstrFILE_AUTMGR32 = "AUTMGR32.EXE"
Private Const mstrFILE_RACMGR32 = "RACMGR32.EXE"
Private Const mstrFILE_RICHED32$ = "RICHED32.DLL"

'setup information file registration macros
'These should remain uppercase
Private Const mstrDLLSELFREGISTER$ = "$(DLLSELFREGISTER)"
Private Const mstrEXESELFREGISTER$ = "$(EXESELFREGISTER)"
Private Const mstrTLBREGISTER$ = "$(TLBREGISTER)"
Private Const mstrREMOTEREGISTER$ = "$(REMOTE)"
Private Const mstrVBLREGISTER$ = "$(VBLREGISTER)"  ' Bug 5-8039

'
'Form/Module Variables
'
Private msRegInfo() As REGINFO                                  'files to be registered
Private mlTotalToCopy As Long                                   'total bytes to copy
Private mstrVerTmpName As String                                'temp file name for VerInstallFile API

' Hkey cache (used for logging purposes)
Private Type HKEY_CACHE
    hKey As Long
    strHkey As String
End Type

Private hkeyCache() As HKEY_CACHE

' Registry manipulation API's (32-bit)
Public Const HKEY_CLASSES_ROOT = &H80000000
Public Const HKEY_CURRENT_USER = &H80000001
Public Const HKEY_LOCAL_MACHINE = &H80000002
Public Const HKEY_USERS = &H80000003
Private Const ERROR_SUCCESS = 0&
Private Const ERROR_NO_MORE_ITEMS = 259&

Private Const REG_SZ = 1
Private Const REG_BINARY = 3
Private Const REG_DWORD = 4


Private Declare Function OSRegCloseKey Lib "advapi32" Alias "RegCloseKey" (ByVal hKey As Long) As Long
Private Declare Function OSRegCreateKey Lib "advapi32" Alias "RegCreateKeyA" (ByVal hKey As Long, ByVal lpszSubKey As String, phkResult As Long) As Long
Private Declare Function OSRegDeleteKey Lib "advapi32" Alias "RegDeleteKeyA" (ByVal hKey As Long, ByVal lpszSubKey As String) As Long
Private Declare Function OSRegEnumKey Lib "advapi32" Alias "RegEnumKeyA" (ByVal hKey As Long, ByVal iSubKey As Long, ByVal lpszName As String, ByVal cchName As Long) As Long
Private Declare Function OSRegOpenKey Lib "advapi32" Alias "RegOpenKeyA" (ByVal hKey As Long, ByVal lpszSubKey As String, phkResult As Long) As Long
Private Declare Function OSRegQueryValueEx Lib "advapi32" Alias "RegQueryValueExA" (ByVal hKey As Long, ByVal lpszValueName As String, ByVal dwReserved As Long, lpdwType As Long, lpbData As Any, cbData As Long) As Long
Private Declare Function OSRegSetValueEx Lib "advapi32" Alias "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal Reserved As Long, ByVal dwType As Long, ByVal lpData As String, ByVal cbData As Long) As Long
Private Declare Function OSRegSetValueNumEx Lib "advapi32" Alias "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal Reserved As Long, ByVal dwType As Long, lpData As Any, ByVal cbData As Long) As Long

Public Declare Sub lstrcpyn Lib "Kernel32" (ByVal strDest As String, ByVal strSrc As Any, ByVal lBytes As Long)
Private Declare Function GetCurrentProcessId Lib "Kernel32" () As Long
Public Declare Function ExtractFileFromCab Lib "vb6stkit.dll" (ByVal Cab As String, ByVal File As String, ByVal Dest As String, ByVal iCab As Long, ByVal sSrc As String) As Long
'Reboot info
Public Const ANYSIZE_ARRAY = 1

Private Type LARGE_INTEGER
    lowpart As Long
    highpart As Long
End Type

Private Type LUID_AND_ATTRIBUTES
    pLuid As LARGE_INTEGER
    Attributes As Long
End Type

Private Type TOKEN_PRIVILEGES
    PrivilegeCount As Long
    Privileges(ANYSIZE_ARRAY) As LUID_AND_ATTRIBUTES
End Type

Public Const TOKEN_ADJUST_PRIVILEGES = 32
Public Const TOKEN_QUERY = 8
Public Const SE_PRIVILEGE_ENABLED As Long = 2

Private Declare Function LookupPrivilegeValue Lib "advapi32.dll" Alias "LookupPrivilegeValueA" (ByVal lpSystemName As String, ByVal lpName As String, lpLuid As LARGE_INTEGER) As Long
Private Declare Function GetCurrentProcess Lib "Kernel32" () As Long
Private Declare Function AdjustTokenPrivileges Lib "advapi32.dll" (ByVal TokenHandle As Long, ByVal DisableAllPrivileges As Long, NewState As TOKEN_PRIVILEGES, ByVal BufferLength As Long, PreviousState As TOKEN_PRIVILEGES, ReturnLength As Long) As Long
Private Declare Function OpenProcessToken Lib "advapi32.dll" (ByVal ProcessHandle As Long, ByVal DesiredAccess As Long, TokenHandle As Long) As Long
'Exit the program and return an error code
Private Declare Sub ExitProcess Lib "Kernel32" (ByVal uExitCode As Long)

Private mfDontAskOnSpaceErr As Boolean

Private Const msDRIVE_INDEX_SEPARATOR As String = "|"

'-----------------------------------------------------------
' SUB: AddPerAppPath
'
' Adds an application's full pathname and per-app path to the
'   system registry (this is currently only meaningful to
'   Windows 95).
'
' IN: [strAppExe] - app EXE name, not including path
'     [strAppDir] - full path of EXE, not including filename
'     [strAppPath] - per-app path for this application
'       (semicolon-separated list of directory path names)
'       If this is the empty string, no per-app path
'       is registered, but the full pathname of the
'       exe IS still registered.
'
' OUT:
'   Example registry entries:
'     HKEY_LOCAL_MACHINE\[strPathsBaseKeyName]\MyApp.Exe
'       [Default]=C:\Program Files\MyApp\MyApp.Exe
'       [Path]=C:\Program Files\MyApp;C:\Program Files\MyApp\System
'-----------------------------------------------------------
'
Public Sub AddPerAppPath(ByVal strAppExe As String, ByVal strAppDir As String, ByVal strPerAppPath As String)
    Dim strPathsBaseKeyName As String
    Const strAppPaths$ = "App Paths"
    Const strAppPathKeyName = "Path"
    Dim fOk As Boolean
    Dim hKey As Long
    
    AddDirSep strAppDir
    
    ' Create the new key, whose name is based on the app's name
    If Not RegCreateKey(HKEY_LOCAL_MACHINE, RegPathWinCurrentVersion, strAppPaths & gstrSEP_DIR & strAppExe, hKey) Then
        GoTo Err
    End If
    
    fOk = True
    
    ' Default value indicates full EXE pathname
    fOk = fOk And RegSetStringValue(hKey, vbNullString, strAppDir & strAppExe)
    
    ' [Path] value indicates the per-app path
    If Len(strPerAppPath) > 0 Then
        fOk = fOk And RegSetStringValue(hKey, strAppPathKeyName, strPerAppPath)
    End If
    
    If Not fOk Then
        GoTo Err
    End If
    
    RegCloseKey hKey
    
    Exit Sub
    
Err:
    RegCloseKey hKey

    MsgError ResolveResString(resERR_REG), vbExclamation Or vbOKOnly, gstrTitle
    '
    ' If we are running an SMS install, we can't continue.
    '
#If SMS Then
    If gfSMS Then
        ExitSetup frmSetup1, gintRET_FATAL
    End If
#End If
End Sub

'-----------------------------------------------------------
' FUNCTION: AddQuotesToFN
'
' Given a pathname (directory and/or filename), returns
'   that pathname surrounded by double quotes if the
'   path contains spaces or commas.  This is required for
'   setting up an icon correctly, since otherwise such paths
'   would be interpreted as a pathname plus arguments.
'-----------------------------------------------------------
'
Private Function AddQuotesToFN(ByVal strFilename) As String
    If InStr(strFilename, " ") Or InStr(strFilename, ",") Then
        AddQuotesToFN = gstrQUOTE & strFilename & gstrQUOTE
    Else
        AddQuotesToFN = strFilename
    End If
End Function

'-----------------------------------------------------------
' SUB: CalcDiskSpace
'
' Calculates disk space required for installing the files
' listed in the specified section of the setup information
' file (SETUP.LST)
'-----------------------------------------------------------
'
Public Sub CalcDiskSpace(ByVal strSection As String)
    Dim lDestFileSpace As Long

    Dim intIdx As Integer
    Dim intDrvIdx As Integer
    Dim sFile As FILEINFO
    Dim strDrive As String
    Dim lThisFileSpace As Long
    Dim sDestFile As String

    intIdx = 1

    On Error GoTo CalcDSError

    '
    'For each file in the specified section, read info from the setup info file
    '
    Do While ReadSetupFileLine(strSection, intIdx, sFile)
        '
        'Get the dest drive used for this file.  If this is the first file using
        'the drive for a destination, add the drive to the drives used 'table',
        'allocate an array element for the holding the drive info, and get
        'available disk space and minimum allocation unit
        '
        GetDrive sFile.strDestDir, strDrive
        intDrvIdx = DriveIndexFromDrive(strDrive)

        '
        'Calculate size of the dest final (file size + minimum allocation for drive)
        '
        lThisFileSpace = CalcFinalSize(sFile.lFileSize, strDrive)
        mlTotalToCopy = mlTotalToCopy + lThisFileSpace

        '
        'If the file already exists, then if we copy it at all, we'll be
        'replacing it.  So, we get the size of the existing dest file so
        'that we can subtract it from the amount needed later.
        '
        sDestFile = sFile.strDestDir & sFile.strDestName
        If FileExists(sDestFile) Then
            lDestFileSpace = FileLen(sDestFile)
        Else
            lDestFileSpace = 0
        End If

        '
        'Subtract size of existing dest file, if applicable and then accumulate
        'space required
        '
        lThisFileSpace = lThisFileSpace - lDestFileSpace
        If lThisFileSpace < 0 Then
            lThisFileSpace = 0
        End If

        gsDiskSpace(intDrvIdx).lReq = gsDiskSpace(intDrvIdx).lReq + lThisFileSpace

        intIdx = intIdx + 1
    Loop

    Exit Sub

CalcDSError:
    MsgError Err.Description & vbLf & vbLf & ResolveResString(resCALCSPACE), vbCritical, gstrSETMSG
    ExitSetup frmMessage, gintRET_FATAL
End Sub

'-----------------------------------------------------------
' SUB: CalcFinalSize
'
' Computes the space required for a file of the size
' specified on the given dest path.  This includes the
' file size plus a padding to ensure that the final size
' is a multiple of the minimum allocation unit for the
' dest drive
'-----------------------------------------------------------
'
Private Function CalcFinalSize(lBaseFileSize As Long, strDestPath As String) As Long
    Dim lMinAlloc As Long
    Dim intPadSize As Long

    lMinAlloc = gsDiskSpace(DriveIndexFromPath(strDestPath)).lMinAlloc
    intPadSize = lMinAlloc - (lBaseFileSize Mod lMinAlloc)
    If intPadSize = lMinAlloc Then
        intPadSize = 0
    End If

    CalcFinalSize = lBaseFileSize + intPadSize
End Function

'-----------------------------------------------------------
' FUNCTION: DriveIndexFromPath
'
' Given a path, returns the index of the corresponding
' drive. Adds the drive if not yet encountered.
'-----------------------------------------------------------
'
Public Function DriveIndexFromPath(Path As String) As Integer
    Dim sDrive As String

    GetDrive Path, sDrive
    DriveIndexFromPath = DriveIndexFromDrive(sDrive)
End Function

'-----------------------------------------------------------
' FUNCTION: DriveIndexFromDrive
'
' Given a drive, returns the index. Adds the drive if not
' yet encountered.
'-----------------------------------------------------------
'
Public Function DriveIndexFromDrive(Drive As String) As Integer
    Dim nIndex As Integer

    On Error Resume Next
    SplitDriveData (gcolDrivesUsed.Item(Drive)), , nIndex
    On Error GoTo 0
    If nIndex = 0 Then
        nIndex = AddDrive(Drive)
    End If
    DriveIndexFromDrive = nIndex
End Function

'-----------------------------------------------------------
' FUNCTION: DriveFromDriveIndex
'
' Given an index, returns the drive.
'-----------------------------------------------------------
'
Public Function DriveFromDriveIndex(Index As Integer) As String
    Dim sDrive As String

    SplitDriveData (gcolDrivesUsed.Item(Index)), sDrive
    DriveFromDriveIndex = sDrive
End Function

'-----------------------------------------------------------
' SUB: SplitDriveData
'
' Outputs the index and string from the data for a
' particular drive.
'-----------------------------------------------------------
'
Private Sub SplitDriveData(Data As String, Optional ByRef Drive As String, Optional ByRef Index As Integer)
    Dim nSepPos As Long
    Dim sData As String

    nSepPos = InStr(1, Data, msDRIVE_INDEX_SEPARATOR)
    Index = Val(Left$(Data, nSepPos - 1))
    Drive = Mid$(Data, nSepPos + Len(msDRIVE_INDEX_SEPARATOR))
End Sub

'-----------------------------------------------------------
' FUNCTION: AddDrive
'
' Adds a drive to the list of drives, and returns the
' corresponding index. Note: The Drive parameter must always
' be the output from GetDrive.
'-----------------------------------------------------------
'
Public Function AddDrive(Drive As String) As Long
    Dim nIndex As Integer
    Dim sValue As String

    nIndex = gcolDrivesUsed.Count + 1
    sValue = nIndex & msDRIVE_INDEX_SEPARATOR & Drive
    gcolDrivesUsed.Add sValue, Drive
    ReDim Preserve gsDiskSpace(nIndex)
    gsDiskSpace(nIndex).lAvail = GetDiskSpaceFree(Drive)
    gsDiskSpace(nIndex).lMinAlloc = GetDrivesAllocUnit(Drive)
    AddDrive = nIndex
End Function

'-----------------------------------------------------------
' FUNCTION: DriveCount
'
' Returns the number of drives in our global list.
'-----------------------------------------------------------
'
Public Function DriveCount() As Integer
    DriveCount = gcolDrivesUsed.Count
End Function

'-----------------------------------------------------------
' SUB: CenterForm
'
' Centers the passed form just above center on the screen
'-----------------------------------------------------------
'
Public Sub CenterForm(frm As Form)
    SetMousePtr vbHourglass

    frm.Top = (Screen.Height * 0.85) \ 2 - frm.Height \ 2
    frm.Left = Screen.Width \ 2 - frm.Width \ 2

    SetMousePtr vbDefault
End Sub
'-----------------------------------------------------------
' SUB: UpdateDateTime
'
' Updates the date/time for bootstrap files
'-----------------------------------------------------------
'
Private Sub UpdateDateTime()
    Dim intIdx As Integer
    Dim sFile As FILEINFO
    Dim lTime As FileTime
    Dim hFile As Long
    '
    'For each file in the specified section, read info from the setup info file
    '
    intIdx = 1
    Do While ReadSetupFileLine(gstrINI_BOOTFILES, intIdx, sFile)
        Dim sCurDate As String, sFileDate As String
        
        sFileDate = Format$(FileDateTime(sFile.strDestDir & sFile.strDestName), "m/d/yyyy h:m")
        sCurDate = Format$(Now, "m/d/yyyy h:m")

        If sFileDate = sCurDate Then
            lTime = GetFileTime(sFile.varDate)
            hFile = CreateFile(sFile.strDestDir & sFile.strDestName, GENERIC_WRITE Or GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
            SetFileTime hFile, lTime, lTime, lTime
            DoEvents
            CloseHandle hFile
        End If
        intIdx = intIdx + 1
    Loop
    
End Sub

'-----------------------------------------------------------
' FUNCTION: CheckDiskSpace
'
' Reads from the space required array generated by calling
' the 'CalcDiskSpace' function and determines whether there
' is sufficient free space on all of the drives used for
' installation
'
' Returns: True if there is enough space, False otherwise
'-----------------------------------------------------------
'
Public Function CheckDiskSpace() As Boolean
    Dim intIdx As Integer
    Dim intTmpDrvIdx As Integer
    Dim lDiskSpaceLeft As Long
    Dim lMostSpaceLeft As Long

    '
    'Default to True (enough space on all drives)
    '
    CheckDiskSpace = True

    '
    'For each drive that is the destination for one or more files, compare
    'the space available to the space required.
    '
    For intIdx = 1 To DriveCount
        lDiskSpaceLeft = gsDiskSpace(intIdx).lAvail - gsDiskSpace(intIdx).lReq
        If lDiskSpaceLeft < 0 Then
            If Not CheckDSAskSpace Then
                CheckDiskSpace = False
                Exit Function
            End If
        Else
            '
            'If no "TMP" drive was found, or if the "TMP" drive wasn't ready,
            'save the index of the drive and the amount of space on the drive
            'which will have the most free space.
            '
            If lDiskSpaceLeft > lMostSpaceLeft Then
                lMostSpaceLeft = lDiskSpaceLeft
                intTmpDrvIdx = intIdx
            End If
        End If
    Next

    If intTmpDrvIdx > 0 Then
        If gsDiskSpace(intTmpDrvIdx).lAvail < gsDiskSpace(intTmpDrvIdx).lReq Then
            CheckDiskSpace = CheckDSAskSpace
        End If
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: CheckDiskSpace
'
' Reads from the space required array generated by calling
' the 'CalcDiskSpace' function and determines whether there
' is sufficient free space on all of the drives used for
' installation
'
' Returns: True if there is enough space, False otherwise
'-----------------------------------------------------------
'
Private Function CheckDSAskSpace() As Boolean
    '
    'if the user hasn't been prompted before in the event of not enough free space,
    'then display table of drive space and allow them to (basically) abort, retry,
    'or ignore.
    '
    If Not mfDontAskOnSpaceErr Then
        If gfNoUserInput Then
            If gfSilent Then
                LogSilentMsg ResolveResString(resLBLNOSPACE)
            End If
#If SMS Then
            If gfSMS Then
                LogSMSMsg ResolveResString(resLBLNOSPACE)
            End If
#End If
            ExitSetup frmSetup1, gintRET_FATAL
        Else
            frmDskSpace.Show vbModal
        End If

        If gintRetVal <> gintRET_CONT Then
            Exit Function
        Else
            mfDontAskOnSpaceErr = True
        End If
    End If
    CheckDSAskSpace = True
End Function

'-----------------------------------------------------------
' FUNCTION: CheckDrive
'
' Check to see if the specified drive is ready to be read
' from.  In the case of a drive that holds removable media,
' this would mean that formatted media was in the drive and
' that the drive door was closed.
'
' IN: [strDrive] - drive to check
'     [strCaption] - caption if the drive isn't ready
'
' Returns: True if the drive is ready, False otherwise
'-----------------------------------------------------------
'
Public Function CheckDrive(ByVal strDrive As String, ByVal strCaption As String) As Boolean
    Dim strDir As String
    Dim strMsg As String
    Dim strNewDrive As String

    On Error Resume Next

    SetMousePtr vbHourglass

    GetDrive strDrive, strNewDrive
    Do
        '
        'Attempt to read the current directory of the specified drive. If
        'an error occurs, we assume that the drive is not ready
        '
        Err.Clear
        strDir = Dir$(strNewDrive)
        If Err.Number = 0 Then
            CheckDrive = True
            Exit Do
        Else
            If IsUNCName(strDrive) Then
                strMsg = Err.Description & vbLf & vbLf & ResolveResString(resCANTREADUNC, gstrPIPE1, strDrive) & vbLf & vbLf & ResolveResString(resCHECKUNC)
            Else
                strMsg = Err.Description & vbLf & vbLf & ResolveResString(resDRVREAD) & strDrive & vbLf & vbLf & ResolveResString(resDRVCHK)
            End If
            If MsgError(strMsg, vbExclamation Or vbRetryCancel, strCaption) = vbCancel Then
                'The user wants to cancel, so fall out of the loop and return
                '   False implicitly.
                'CheckDrive = False (implicit)
                Exit Do
            End If
            'In the NoUserInput case, MsgError above will return the default
            '   button value, which is vbRetry. Rather than retrying, we want to
            '   exit.
            If gfNoUserInput Then
                ExitSetup frmSetup1, gintRET_FATAL
            End If
        End If
    Loop

    SetMousePtr vbDefault

    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: CopyFile
'
' Uses the Windows VerInstallFile API to copy a file from
' the specified source location/name to the destination
' location/name.
' If the file is successfully updated and the file is a
' shared file (fShared = True), then the
' files reference count is updated (32-bits only)
'
' IN: [strSrcDir] - directory where source file is located
'     [strDestDir] - destination directory for file
'     [strSrcName] - name of source file
'     [strDestName] - name of destination file
'
' PRECONDITION: NewAction() must have already been called
'               for this file copy (of type either
'               gstrKEY_SHAREDFILE or gstrKEY_PRIVATE --
'               see CopySection for an example of how
'               this works).  See NewAction() and related
'               functions in LOGGING.BAS for comments on
'               using the logging function.
'               Either CommitAction() or AbortAction() will
'               allows be called by this procedure, and
'               should not be done by the caller.
'
' Returns: True if copy was successful, False otherwise
'
' POSTCONDITION: The current action will be either committed or
'                aborted.
'-----------------------------------------------------------
'
Private Function CopyFile(ByVal strSrcDir As String, ByVal strDestDir As String, ByVal strSrcName As String, ByVal strDestName As String, ByVal fShared As Boolean, ByVal fSystem As Boolean) As Boolean
    Const intUNKNOWN% = 0
    Const intCOPIED% = 1
    Const intNOCOPY% = 2
    Const intFILEUPTODATE% = 3

    '
    'VerInstallFile() Flags
    '
    Const VIFF_FORCEINSTALL% = &H1
    Const VIF_TEMPFILE& = &H1
    Const VIF_SRCOLD& = &H4
    Const VIF_DIFFLANG& = &H8
    Const VIF_DIFFCODEPG& = &H10
    Const VIF_DIFFTYPE& = &H20
    Const VIF_WRITEPROT& = &H40
    Const VIF_FILEINUSE& = &H80
    Const VIF_OUTOFSPACE& = &H100
    Const VIF_ACCESSVIOLATION& = &H200
    Const VIF_SHARINGVIOLATION = &H400
    Const VIF_CANNOTCREATE = &H800
    Const VIF_CANNOTDELETE = &H1000
    Const VIF_CANNOTRENAME = &H2000
    Const VIF_OUTOFMEMORY = &H8000&
    Const VIF_CANNOTREADSRC = &H10000
    Const VIF_CANNOTREADDST = &H20000
    Const VIF_BUFFTOOSMALL = &H40000

    Static fIgnoreWarn As Boolean 'user warned about ignoring error?

    Dim strMsg As String
    Dim lRC As Long
    Dim lpTmpNameLen As Long
    Dim intFlags As Integer
    Dim intRESULT As Integer
    Dim fFileAlreadyExisted
    Dim strExt As String

    On Error Resume Next

    '
    'Ensure that the source file is available for copying
    '
    If Not DetectFile(strSrcDir & strSrcName) Then
        AbortAction
        Exit Function
    End If
    
    '
    ' Make sure that the Destination path (including path, filename, commandline args, etc.
    ' is not longer than the max allowed.
    '
    If Not fCheckFNLength(strDestDir & strDestName) Then
        AbortAction
        strMsg = ResolveResString(resCANTCOPYPATHTOOLONG) & vbLf & vbLf & ResolveResString(resCHOOSENEWDEST) & vbLf & vbLf & strDestDir & strDestName
        MsgError strMsg, vbOKOnly, gstrSETMSG
        ExitSetup frmCopy, gintRET_FATAL
    End If
    '
    'Make the destination directory, prompt the user to retry if there is an error
    '
    If Not MakePath(strDestDir) Then
        AbortAction ' Abort file copy
        Exit Function
    End If

    '
    'Make sure we have the LFN (long filename) of the destination directory
    '
    strDestDir = GetLongPathName(strDestDir)
    '
    'GetLongPathName will string the final '\' off a path, so we need to restore
    'it.
    '
    AddDirSep strDestDir
    
    '
    'Setup for VerInstallFile call
    '
    lpTmpNameLen = gintMAX_SIZE
    mstrVerTmpName = String$(lpTmpNameLen, 0)
    intFlags = VIFF_FORCEINSTALL
    fFileAlreadyExisted = FileExists(strDestDir & strDestName)

    intRESULT = intUNKNOWN

    Do While intRESULT = intUNKNOWN
        'VerInstallFile under Windows 95 does not handle
        '  long filenames, so we must give it the short versions.
        Dim strShortSrcName As String
        Dim strShortDestName As String
        Dim strShortSrcDir As String
        Dim strShortDestDir As String
        Dim nFile As Integer

        strShortSrcName = strSrcName
        strShortSrcDir = strSrcDir
        strShortDestName = strDestName
        strShortDestDir = strDestDir

        On Error GoTo UnexpectedErr
        If Not IsWindowsNT() Then
            'This conversion is not necessary under Windows NT
            If Not FileExists(strDestDir & strDestName) Then
                'If the destination file does not already
                '  exist, we create a dummy with the correct
                '  (long) filename so that we can get its
                '  short filename for VerInstallFile.
                nFile = FreeFile
                Open strDestDir & strDestName For Output Access Write As #nFile
                Close #nFile
            End If
            SeparatePathAndFileName GetShortPathName(strSrcDir & strSrcName), strShortSrcDir, strShortSrcName
            SeparatePathAndFileName GetShortPathName(strDestDir & strDestName), strShortDestDir, strShortDestName
        End If
        On Error Resume Next
        lRC = VerInstallFile(intFlags, strShortSrcName, strShortDestName, strShortSrcDir, strShortDestDir, 0&, mstrVerTmpName, lpTmpNameLen)
        If Err.Number <> 0 Then
            '
            'If the version DLL couldn't be found, then abort setup
            '
            ExitSetup frmCopy, gintRET_FATAL
        End If

        If lRC = 0 Then
            '
            'File was successfully installed, increment reference count if needed
            '
            'One more kludge for long filenames: VerInstallFile may have renamed
            'the file to its short version if it went through with the copy.
            'Therefore we simply rename it back to what it should be.
            Name strDestDir & strShortDestName As strDestDir & strDestName
            intRESULT = intCOPIED
        ElseIf lRC And VIF_SRCOLD Then
            '
            'Source file was older, so not copied, the existing version of the file
            'will be used.  Increment reference count if needed
            '
            intRESULT = intFILEUPTODATE
        ElseIf lRC And (VIF_DIFFLANG Or VIF_DIFFCODEPG Or VIF_DIFFTYPE) Then
            '
            'We retry and force installation for these cases.  You can modify the code
            'here to prompt the user about what to do.
            '
            intFlags = VIFF_FORCEINSTALL
        ElseIf lRC And VIF_WRITEPROT Then
            strMsg = ResolveResString(resWRITEPROT)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        ElseIf lRC And VIF_FILEINUSE Then
            strMsg = ResolveResString(resINUSE)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        ElseIf lRC And VIF_OUTOFSPACE Then
            GetDrive strDestDir, strMsg
            strMsg = ResolveResString(resOUTOFSPACE, gstrPIPE1, strMsg)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        ElseIf lRC And VIF_ACCESSVIOLATION Then
            strMsg = ResolveResString(resACCESSVIOLATION)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        ElseIf lRC And VIF_SHARINGVIOLATION Then
            strMsg = ResolveResString(resSHARINGVIOLATION)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        ElseIf lRC And VIF_OUTOFMEMORY Then
            strMsg = ResolveResString(resOUTOFMEMORY)
            CFMsg strMsg, strDestDir, strDestName, fIgnoreWarn, intRESULT
        Else
            '
            ' For these cases, we generically report the error and do not install the file
            ' unless this is an SMS install; in which case we abort.
            '
            If lRC And VIF_CANNOTCREATE Then
                strMsg = ResolveResString(resCANNOTCREATE)
            ElseIf lRC And VIF_CANNOTDELETE Then
                strMsg = ResolveResString(resCANNOTDELETE)
            ElseIf lRC And VIF_CANNOTRENAME Then
                strMsg = ResolveResString(resCANNOTRENAME)
            ElseIf lRC And VIF_CANNOTREADSRC Then
                strMsg = ResolveResString(resCANNOTREADSRC)
            ElseIf lRC And VIF_CANNOTREADDST Then
                strMsg = ResolveResString(resCANNOTREADDST)
            ElseIf lRC And VIF_BUFFTOOSMALL Then
                strMsg = ResolveResString(resBUFFTOOSMALL)
            End If

            strMsg = strMsg & ResolveResString(resNOINSTALL)
            MsgError strMsg, vbOKOnly Or vbExclamation, gstrTitle
#If SMS Then
            If gfSMS Then
                ExitSetup frmSetup1, gintRET_FATAL
            End If
#End If
            intRESULT = intNOCOPY
        End If
    Loop

    '
    'If there was a temp file left over from VerInstallFile, remove it
    '
    If lRC And VIF_TEMPFILE Then
        Kill mstrVerTmpName
    End If

    'Get the UCase of the extension, for use below.
    strExt = UCase$(Extension(strDestName))
    'Abort or commit the current Action, and do reference counting
    Select Case intRESULT
        Case intNOCOPY
            AbortAction
        Case intCOPIED
            DecideIncrementRefCount strDestDir & strDestName, fShared, fSystem, fFileAlreadyExisted
            Select Case strExt
                Case gsEXT_FONTFON, gsEXT_FONTTTF 'strExt, gsEXT_FONTFON, and gsEXT_FONTTTF are uppercase
                    'do nothing
                Case Else
                    AddActionNote ResolveResString(resLOG_FILECOPIED)
                    CommitAction
            End Select
            CopyFile = True
        Case intFILEUPTODATE
            'Note: This should never occur - we've already checked to see if the
            '   file was up to date before calling VerInstallFile. But we'll
            '   leave it here for completeness.
            DecideIncrementRefCount strDestDir & strDestName, fShared, fSystem, fFileAlreadyExisted
            Select Case strExt
                Case gsEXT_FONTFON, gsEXT_FONTTTF 'strExt, gsEXT_FONTFON, and gsEXT_FONTTTF are uppercase
                    'do nothing
                Case Else
                    AddActionNote ResolveResString(resLOG_FILEUPTODATE)
                    CommitAction
            End Select
            CopyFile = True
        Case Else
            AbortAction ' Defensive - this shouldn't be reached
    End Select

    Exit Function

UnexpectedErr:
    MsgError Err.Description & vbLf & vbLf & ResolveResString(resUNEXPECTED), vbOKOnly Or vbExclamation, gstrTitle
    ExitSetup frmCopy, gintRET_FATAL
End Function

'-----------------------------------------------------------
' SUB: CFMsg
'
' Displays a message related to a file copy issue.
'-----------------------------------------------------------
'
Private Sub CFMsg(strMsg As String, strDestDir As String, strDestName As String, fIgnoreWarn As Boolean, intRESULT As Integer)
    Const intNOCOPY% = 2
    Dim intMsgRet As Integer

    strMsg = strDestDir & strDestName & vbLf & vbLf & strMsg
    intMsgRet = MsgError(strMsg, vbAbortRetryIgnore Or vbExclamation Or vbDefaultButton2, gstrTitle)
    If gfNoUserInput Then intMsgRet = vbAbort
    Select Case intMsgRet
        Case vbAbort
            ExitSetup frmCopy, gintRET_ABORT
        Case vbIgnore
            If fIgnoreWarn Then
                intRESULT = intNOCOPY
            Else
                fIgnoreWarn = True
                strMsg = strMsg & vbLf & vbLf & ResolveResString(resWARNIGNORE)
                If MsgError(strMsg, vbYesNo Or vbQuestion Or vbDefaultButton2, gstrTitle) = vbYes Then
                    intRESULT = intNOCOPY
                End If
            End If
    End Select
End Sub

'-----------------------------------------------------------
' SUB: CopySection
'
' Attempts to copy the files that need to be copied from
' the named section of the setup info file (SETUP.LST)
'
' IN: [strSection] - name of section to copy files from
'-----------------------------------------------------------
'
Public Sub CopySection(ByVal strSection As String)
    Dim strNewSrc As String
    Dim intIdx As Integer
    Dim sFile As FILEINFO
    Dim strLastFile As String
    Dim intRC As Integer
    Dim lThisFileSize As Long
    Dim strSrcDir As String
    Dim strDestDir As String
    Dim strSrcName As String
    Dim strDestName As String
    Dim strRegister As String
    Dim fFileWasUpToDate As Boolean
    Dim strMultDirBaseName As String
    Dim strMsg As String
    Dim strDetectPath As String
    Dim fOverWrite As Boolean
    Static fOverwriteAll As Boolean
    Dim strExt As String
    Dim sCurDate As String
    Dim sFileDate As String
    Dim lTime As FileTime
    Dim hFile As Long
    Dim frm As frmOverwrite
    Dim strDestVer As String
    Dim owValue As OverwriteReturnVal
    Dim fcValue As FileComparison

    On Error Resume Next

    strMultDirBaseName = ResolveResString(resCOMMON_MULTDIRBASENAME)
    intIdx = 1

    If Not FileExists(gsTEMPDIR) Then
        MkDir gsTEMPDIR
    End If
    '
    'For each file in the specified section, read info from the setup info file
    '
    Do While ReadSetupFileLine(strSection, intIdx, sFile)
        intRC = 0

        fFileWasUpToDate = False

        If UCase$(sFile.strSrcName) = UCase$(gstrAT & gstrFILE_MDAG) Then
            'We don't need to extract mdac_typ twice
            GoTo CSContinue
        End If
        If IsFileADXRedistFile(sFile.strSrcName) Then
            'We don't need to extract the DX Runtime more than once
            GoTo CSContinue
        End If
        
        strNewSrc = gsTEMPDIR & sFile.strDestName
        ExtractFileFromCab gsCABFULLNAME, sFile.strSrcName, strNewSrc, gintCabs, gstrSrcPath
        If FileExists(strNewSrc) Then
            sFile.strSrcName = gsTEMPDIR & sFile.strDestName
            sFile.intDiskNum = gintCurrentDisk
        End If
        '
        ' If a new disk is called for, or if for some reason we can't find the
        ' source path (user removed the install floppy, for instance) then
        ' prompt for the next disk.  The PromptForNextDisk function won't
        ' actually prompt the user unless it determines that the source drive
        ' contains removeable media or is a network connection.  Also, we don't
        ' prompt if this is a silent install.  It will fail later on a silent
        ' install when it can't find the file.
        '
        If (Not gfNoUserInput) And (sFile.intDiskNum <> gintCurrentDisk Or Not DirExists(gstrSrcPath)) Then
            PromptForNextDisk sFile.intDiskNum, sFile.strSrcName
        End If

        strSrcName = sFile.strSrcName
        '
        ' The file could exist in either the main source directory or
        ' in a subdirectory named DISK1, DISK2, etc.  Set the appropriate
        ' path.  If it's in neither place, it is an error and will be
        ' handled later.
        '
        If FileExists(strSrcName) Then
            strSrcDir = gsTEMPDIR
        Else
            '
            ' Can't find the file.
            '
            strDetectPath = gstrSrcPath & strMultDirBaseName & CStr(sFile.intDiskNum)
            If Not DirExists(strDetectPath) Then
                strDetectPath = gstrSrcPath
            End If
            strMsg = ResolveResString(resCOMMON_CANTFINDSRCFILE, gstrPIPE1, strDetectPath & gstrSEP_DIR & strSrcName)
            MsgError strMsg, vbExclamation Or vbOKOnly, gstrTitle
            ExitSetup frmCopy, gintRET_FATAL
        End If

        strDestDir = sFile.strDestDir
        strDestName = sFile.strDestName
        
        'We need to go ahead and create the destination directory, or else
        'GetLongPathName() may fail
        If Not MakePath(strDestDir) Then
            intRC = vbIgnore
        End If

        If intRC <> vbIgnore Then
            strDestDir = GetLongPathName(strDestDir)
            '
            'GetLongPathName will string the final '\' off a path, so we need to
            'restore it.
            '
            AddDirSep strDestDir

            frmCopy.lblDestFile.Caption = strDestDir & sFile.strDestName
            frmCopy.lblDestFile.Refresh

            'Cache the UCase of the extension, for use below
            strExt = UCase$(Extension(sFile.strDestName))
            If UCase$(strDestName) = UCase$(gstrFILE_MDAG) Then
                '
                ' mdac_typ.EXE is installed temporarily.  We'll be
                ' deleting it at the end of setup.  Set mdag = True
                ' so we know we need to delete it later.
                '
                NewAction gstrKEY_TEMPFILE, gstrQUOTE & strDestDir & strDestName & gstrQUOTE
                gfMDag = True
                gstrMDagInstallPath = strDestDir & strDestName
            ElseIf strExt = gsEXT_FONTTTF Then 'Both strExt and gsEXT_FONTTTF are uppercase
                'No new actions for fonts
            ElseIf strExt = gsEXT_FONTFON Then 'Both strExt and gsEXT_FONTFON are uppercase
                'No new actions for fonts
            ElseIf sFile.fShared Then
                NewAction gstrKEY_SHAREDFILE, gstrQUOTE & strDestDir & strDestName & gstrQUOTE
            ElseIf sFile.fSystem Then
                NewAction gstrKEY_SYSTEMFILE, gstrQUOTE & strDestDir & strDestName & gstrQUOTE
            ElseIf strExt = UCase$(gsEXT_REG) Then 'strExt is uppercase; gsEXT_REG is not
                If UCase$(Extension(sFile.strRegister)) = UCase$(gsEXT_REG) Then
                    'No new actions for registration files.
                Else
                    NewAction gstrKEY_PRIVATEFILE, gstrQUOTE & strDestDir & strDestName & gstrQUOTE
                End If
            Else
                NewAction gstrKEY_PRIVATEFILE, gstrQUOTE & strDestDir & strDestName & gstrQUOTE
            End If
        End If
        
        '
        'If the file info just read from SETUP.LST is the application .EXE
        '(i.e.; it's the value of the AppExe Key in the [Setup] section,
        'then save it's full pathname for later use
        '
        If UCase$(strDestName) = UCase$(gstrAppExe) Then
            '
            'Used for creating a program manager icon in Form_Load of SETUP1.FRM
            'and for registering the per-app path
            '
            gsDest.strAppDir = strDestDir
        End If

        'Special case for RICHED32.DLL
        '-- we only install this file under Windows 95, not under Windows NT
        If UCase$(strDestName) = mstrFILE_RICHED32 Then 'mstrFILE_RICHED32 is uppercase
            If Not IsWindows95() Then
                'We're not running under Win95 - do not install this file.
                intRC = vbIgnore
                LogNote ResolveResString(resCOMMON_RICHED32NOTCOPIED, gstrPIPE1, strDestName)
                AbortAction
            End If
        End If

        strRegister = sFile.strRegister

        lThisFileSize = CalcFinalSize(sFile.lFileSize, sFile.strDestDir)
        '
        'Next we'll deal with whether this file needs updrading.
        '
        If intRC <> vbIgnore Then
            'Assume the file does need upgrading.
            fOverWrite = True
            'Unless the user has already decided to upgrade all files, check and
            'see if this file needs upgrading.
            owValue = owNo
            If fOverwriteAll Then
                fcValue = SourceFileIsNewer(sFile, strSrcName, strSrcDir, strDestName, strDestDir, strDestVer)
                If sFile.fDestDirRecognizedBySetupExe And FileInUse(strDestDir & strDestName) Then
                    If fcValue = fcOlder Then
                        '
                        'If the destination file is in use and it's a file
                        'that Setup.Exe recognizes, then there is no chance
                        'of succeeding a file upgrade, so just assume a Yes
                        'return from the dialog.
                        '
                        owValue = owYes
                    'Else
                    '    This will lead to an error when the file is copied, but
                    '    the error is legitimate.
                    End If
                ElseIf fcValue = fcEquivalent Then
                    owValue = owYes
                End If
            Else
                Select Case SourceFileIsNewer(sFile, strSrcName, strSrcDir, strDestName, strDestDir, strDestVer)
                    Case fcEquivalent
                        owValue = owYes
                    Case fcOlder
                        If sFile.fDestDirRecognizedBySetupExe And FileInUse(strDestDir & strDestName) Then
                            '
                            'See previous comment.
                            '
                            owValue = owYes
                        Else
                            '
                            'Source file is not newer than destination file;
                            'prompt user for what to do
                            '
                            Set frm = New frmOverwrite
                            frm.FileName = strDestDir & strDestName
                            frm.Version = strDestVer
                            frm.Description = GetFileDescription(strDestDir & strDestName)
                            frm.Show vbModal, frmSetup1
                            owValue = frm.ReturnVal
                            Set frm = Nothing
                        End If
                End Select
            End If
            Select Case owValue
                Case owYes 'Keep this file; don't upgrade
                    fOverWrite = False
                    'We won't be copying the file below, so finish dealing
                    '   with it now.
                    intRC = vbIgnore
                    fFileWasUpToDate = True
                    If strExt = gsEXT_FONTTTF Then 'Both strExt and gsEXT_FONTTTF are uppercase
                        'Do nothing - fonts are not logged nor
                        '   refcounted.
                    ElseIf strExt = gsEXT_FONTFON Then 'Both strExt and gsEXT_FONTFON are uppercase
                        'Do nothing - fonts are not logged nor
                        '   refcounted.
                    Else
                        DecideIncrementRefCount strDestDir & strDestName, sFile.fShared, sFile.fSystem, True
                        AddActionNote ResolveResString(resLOG_FILEUPTODATE)
                        CommitAction
                    End If
                Case owNoToAll 'Overwrite all files
                    fOverwriteAll = True
                'Case owNo 'Overwrite the file
                    'We assumed this above, so we don't need to do
                    '   anything now.
                    'fOverWrite = True
            End Select
        End If
        '
        'After all of this, if we're still ready to copy, then give it a whirl!
        '
        If intRC <> vbIgnore Then
            ' CopyFile will increment the reference count for us, and will either
            ' commit or abort the current Action.
            'Turn off READONLY flag in case we copy.
            SetAttr strDestDir & strDestName, vbNormal
            If UCase$(Extension(sFile.strRegister)) <> UCase$(gsEXT_REG) Then
                If CopyFile(strSrcDir, strDestDir, strDestName, strDestName, sFile.fShared, sFile.fSystem) Then
                    intRC = 0
                Else
                    intRC = vbIgnore
                End If
            End If
        End If
        '
        'Register fonts
        '
        Select Case strExt
        Case gsEXT_FONTTTF, gsEXT_FONTFON 'strExt, gsEXT_FONTTTF, and gsEXT_FONTFON are all uppercase
            AddFontResource strDestDir & strDestName
        End Select
        '
        'Save the paths of certain files for later use, if they were
        'successfully installed or were already on the system
        '
        If (intRC = 0 Or fFileWasUpToDate) Then
            Select Case UCase$(strDestName)
                Case mstrFILE_AUTMGR32 'This is uppercase
                    '
                    'Used for creating an icon if installed
                    '
                    gsDest.strAUTMGR32 = strDestDir & mstrFILE_AUTMGR32
                Case mstrFILE_RACMGR32 'This is uppercase
                    '
                    'Used for creating an icon if installed
                    '
                    gsDest.strRACMGR32 = strDestDir & mstrFILE_RACMGR32
            End Select
            '
            'If we successfully copied the file, and if registration information was
            'specified in the setup info file, save the registration info into an
            'array so that we can register all files requiring it in one fell swoop
            'after all the files have been copied.
            '
            If Len(strRegister) > 0 Then
                Err.Clear
                ReDim Preserve msRegInfo(UBound(msRegInfo) + 1)

                If Err.Number <> 0 Then
                    ReDim msRegInfo(0)
                End If

                msRegInfo(UBound(msRegInfo)).strFilename = strDestDir & strDestName

                Select Case UCase$(strRegister)
                    Case mstrDLLSELFREGISTER, mstrEXESELFREGISTER, mstrTLBREGISTER, mstrVBLREGISTER 'These are all uppercase
                        'Nothing in particular to do
                    Case mstrREMOTEREGISTER 'This is uppercase
                        'We need to look for and parse the corresponding "RemoteX=..." line
                        If Not ReadSetupRemoteLine(strSection, intIdx, msRegInfo(UBound(msRegInfo))) Then
                            MsgError ResolveResString(resREMOTELINENOTFOUND, gstrPIPE1, strDestName, gstrPIPE2, gstrINI_REMOTE & CStr(intIdx)), vbExclamation Or vbOKOnly, gstrTitle
                            ExitSetup frmSetup1, gintRET_FATAL
                        End If
                    Case Else
                        '
                        'If the registration info specified the name of a file with
                        'registration info (which we assume if a registration macro
                        'was not specified), then we also assume that, if no path
                        'information is available, this reginfo file is in the same
                        'directory as the file it registers
                        '
                        strRegister = ResolveDestDirs(strRegister)
                        If InStr(strRegister, gstrSEP_DIR) = 0 Then 'This search is case-sensitive, as it should be
                            strRegister = strSrcDir & strRegister
                        End If
                End Select

                If UCase$(Extension(strRegister)) = UCase$(gsEXT_REG) Then
                    SyncShell gsREGEDIT & strQuoteString(strRegister), INFINITE
                End If
                msRegInfo(UBound(msRegInfo)).strRegister = strRegister
            End If
        End If

        strLastFile = sFile.strDestName

CSContinue:
        '
        'Update the copy status bar.  We need to do the update regardless of whether a
        'file was actually copied or not.
        '
        glTotalCopied = glTotalCopied + lThisFileSize
        UpdateStatus frmCopy.picStatus, glTotalCopied / mlTotalToCopy

        sFileDate = Format$(FileDateTime(sFile.strDestDir & sFile.strDestName), "m/d/yyyy h:m")
        sCurDate = Format$(Now, "m/d/yyyy h:m")

        If sFileDate = sCurDate Then
            lTime = GetFileTime(sFile.varDate)
            hFile = CreateFile(sFile.strDestDir & sFile.strDestName, GENERIC_WRITE Or GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
            SetFileTime hFile, lTime, lTime, lTime
            DoEvents
            CloseHandle hFile
        Else
            '
            'Give a chance for the 'Cancel' button command to be processed if it was pressed
            '
            DoEvents
        End If
        'Delete the files after copy...
        SetAttr gsTEMPDIR & sFile.strDestName, vbNormal
        Kill gsTEMPDIR & sFile.strDestName
        intIdx = intIdx + 1
    Loop

    Err.Clear
End Sub

'-----------------------------------------------------------
' FUNCTION: SourceFileIsNewer
'
' Determines whether a file to be installed is newer than an
' existing file already on the system.
'
' IN: [sFile] - structure containing information about the source file
'     [strSrcName] - name of source file
'     [strSrcDir] - location of source file
'     [strDestName] - name of destination file
'     [strDestDir] - destination directory for file
' OUT: [strDestVer] - a string representing the version of the destination file
'
' Returns: True if there is no existing (destination) file.
'          True if the destination file does exist and the
'          source file has a newer version.
'          True if the destination file does exist but one
'          or both files does not have version information
'          and the source file has a newer timestamp.
'          False otherwise
'-----------------------------------------------------------
'
Private Function SourceFileIsNewer(sFile As FILEINFO, strSrcName As String, strSrcDir As String, strDestName As String, strDestDir As String, ByRef strDestVer As String) As FileComparison
    Dim fSrcVer As Boolean
    Dim sSrcVerInfo As VERINFO
    Dim fRemoteReg As Boolean
    Dim sDestVerInfo As VERINFO
    Dim datDest As Date
    '
    'The stuff below tries to save some time by pre-checking whether a file
    'should be installed before VerInstallFile does its thing.
    'Basically, if both files have version numbers, they are compared.
    'Otherwise, we compare date.
    '
    On Error Resume Next
    strDestVer = vbNullString
    '
    'Always attempt to get the source file version number.  If the setup
    'info file did not contain a version number (sSrcVerInfo.nMSHi =
    'gintNOVERINFO), we attempt to read the version number from the source
    'file.
    '
    fSrcVer = True
    sSrcVerInfo = sFile.sVerInfo
    If sSrcVerInfo.FileVerPart1 = gintNOVERINFO Then
        fSrcVer = GetFileVerStruct(strSrcDir & strSrcName, sSrcVerInfo)
    End If
    '
    'If there is an existing destination file with version information, then
    'compare its version number to the source file version number.
    '
    If fSrcVer Then
        fRemoteReg = (UCase$(sFile.strRegister) = mstrREMOTEREGISTER) 'mstrREMOTEREGISTER is uppercase
        If GetFileVerStruct(strDestDir & strDestName, sDestVerInfo, fRemoteReg) Then
            With sDestVerInfo
                strDestVer = CStr(.FileVerPart1) & "." & _
                             CStr(.FileVerPart2) & "." & _
                             CStr(.FileVerPart3) & "." & _
                             CStr(.FileVerPart4)
            End With
            'Both source and destinations have versions. Compare them.
            SourceFileIsNewer = IsNewerVer(sSrcVerInfo, sDestVerInfo)
            Err.Clear
            Exit Function
        End If
    End If
    '
    'Since neither file has a version, the best we can do is compare dates.
    '
    Err.Clear
    datDest = FileDateTime(strDestDir & strDestName)
    If Err.Number = 0 Then
        If sFile.varDate < datDest Then
            SourceFileIsNewer = fcNewer
        ElseIf sFile.varDate = datDest Then
            SourceFileIsNewer = fcEquivalent
        Else
            SourceFileIsNewer = fcOlder
        End If
    Else
        'Evidently the destination file does not exist. Therefore the source
        '   file should be copied and can be considered newer.
        SourceFileIsNewer = fcNewer
    End If
    Err.Clear
End Function

'-----------------------------------------------------------
' SUB: fCreateShellGroup
'
' Creates a new program group off of Start>Programs in the
' Windows 95 shell if the specified folder doesn't already exist.
'
' IN: [strFolderName] - text name of the folder.
'                      This parameter may not contain
'                      backslashes.
'                      ex: "My Application" - this creates
'                        the folder Start>Programs>My Application
'     [fRetOnerr] - Whether or not this routine should return if
'                   there is an error creating the group.  If false,
'                   setup aborts and does not return.  Set this to
'                   true if the user can do something to correct the
'                   error.  E.g., they entered a group name in the
'                   Choose Program Group dialog as opposed to calling
'                   this routine when creating the Remote Automation
'                   group in which the user had no control.
'     [fLog] - Whether or not to write to the logfile (default
'                is true if missing)
'-----------------------------------------------------------
'
Public Function fCreateShellGroup(ByVal strFolderName As String, fRetOnErr As Boolean, Optional ByVal fLog As Boolean = True, Optional ByVal fPriv As Boolean = True, Optional ByVal fStartMenu As Boolean = False) As Boolean
    Dim oMalloc As IVBMalloc
    Dim fSuccess As Boolean
    Dim sPath As String
    Dim IDL As Long
    Dim lPrograms As SpecialFolderIDs

    ReplaceDoubleQuotes strFolderName
    
    If Len(strFolderName) = 0 Then
        Exit Function
    End If

Retry:
    If IsWindows95() Then
        fPriv = True
    End If
    If fPriv Then
        If fStartMenu Then
            lPrograms = sfidSTARTMENU
        Else
            lPrograms = sfidPROGRAMS
        End If
    Else
        If fStartMenu Then
            lPrograms = sfidCOMMON_STARTMENU
        Else
            lPrograms = sfidCOMMON_PROGRAMS
        End If
    End If
    ' Fill the item id list with the pointer of each folder item, rtns 0 on success
    If SHGetSpecialFolderLocation(frmSetup1.hWnd, lPrograms, IDL) = NOERROR Then
        sPath = String$(gintMAX_PATH_LEN, 0)
        SHGetPathFromIDListA IDL, sPath
        SHGetMalloc oMalloc
        oMalloc.Free IDL
        sPath = StringFromBuffer(sPath)
    End If
    AddDirSep sPath
    sPath = sPath & strFolderName
    fSuccess = MakePath(sPath)
    If Not fSuccess Then
        If gfNoUserInput Or (MsgError(ResolveResString(resCANTCREATEPROGRAMGROUP, gstrPIPE1, strFolderName), vbRetryCancel Or vbExclamation, gstrTitle)) = vbCancel Then
            ExitSetup frmSetup1, gintRET_EXIT
            GoTo Retry
        End If
        '
        ' Determine if we should return so the user can
        ' correct the situation.
        '
        If Not fRetOnErr Then
            '
            ' Return so we can exit setup.
            '
            GoTo Retry
        End If
    End If

    fCreateShellGroup = fSuccess
End Function

'-----------------------------------------------------------
' SUB: CreateShellLink
'
' Creates (or replaces) a link in either Start>Programs or
' any of its immediate subfolders in the Windows 95 shell.
'
' IN: [strLinkPath] - full path to the target of the link
'                     Ex: 'c:\Program Files\My Application\MyApp.exe"
'     [strLinkArguments] - command-line arguments for the link
'                     Ex: '-f -c "c:\Program Files\My Application\MyApp.dat" -q'
'     [strLinkName] - text caption for the link
'     [fLog] - Whether or not to write to the logfile (default
'                is true if missing)
'
' OUT:
'   The link will be created in the folder strGroupName
'-----------------------------------------------------------
'
Public Sub CreateShellLink(ByVal strLinkPath As String, ByVal strGroupName As String, ByVal strLinkArguments As String, ByVal strLinkName As String, ByVal fPrivate As Boolean, sParent As String, Optional ByVal fLog As Boolean = True)
    Dim fSuccess As Boolean
    Dim intMsgRet As Integer

    If fLog Then
        NewAction gstrKEY_SHELLLINK, gstrQUOTE & strUnQuoteString(strGroupName) & gstrQUOTE & ", " & gstrQUOTE & strUnQuoteString(strLinkName) & gstrQUOTE
    End If

    strLinkName = strUnQuoteString(strLinkName)
    strLinkPath = strUnQuoteString(strLinkPath)
    
    If StrPtr(strLinkArguments) = 0 Then strLinkArguments = ""

Retry:
    fSuccess = OSfCreateShellLink(strGroupName, strLinkName, strLinkPath, strLinkArguments, fPrivate, sParent)  'the path should never be enclosed in double quotes
    If fSuccess Then
        If fLog Then
            CommitAction
        End If
    Else
        intMsgRet = MsgError(ResolveResString(resCANTCREATEPROGRAMICON, gstrPIPE1, strLinkName), vbAbortRetryIgnore Or vbExclamation, gstrTitle)
        If gfNoUserInput Then
            intMsgRet = vbAbort
        End If
        Select Case intMsgRet
            Case vbAbort
                ExitSetup frmSetup1, gintRET_ABORT
                GoTo Retry
            Case vbRetry
                GoTo Retry
            Case vbIgnore
                If fLog Then
                    AbortAction
                End If
        End Select
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION: DecideIncrementRefCount
'
' Increments the reference count of a file under 32-bits
' if the file is a shared file.
'
' IN: [strFullPath] - full pathname of the file to reference
'                     count.  Example:
'                     'C:\MYAPP\MYAPP.DAT'
'     [fShared] - whether the file is shared or private
'     [fSystem] - The file is a system file
'     [fFileAlreadyExisted] - whether or not the file already
'                             existed on the hard drive
'                             before our setup program
'-----------------------------------------------------------
'
Private Sub DecideIncrementRefCount(ByVal strFullPath As String, ByVal fShared As Boolean, ByVal fSystem As Boolean, ByVal fFileAlreadyExisted As Boolean)
    'Reference counting takes place under both Windows 95 and Windows NT
    If fShared Or fSystem Then
        IncrementRefCount strFullPath, fFileAlreadyExisted
    End If
End Sub
            
'-----------------------------------------------------------
' FUNCTION: DetectFile
'
' Detects whether the specified file exists.  If it can't
' be found, the user is given the opportunity to abort,
' retry, or ignore finding the file.  This call is used,
' for example, to ensure that a floppy with the specified
' file name is in the drive before continuing.
'
' IN: [strFileName] - name of file to detect, usually
'                     should include full path, Example:
'                     'A:\MYAPP.DAT'
'
' Returns: TRUE if the file was detected, False if
'          the user chose ignore when the file couldn't
'          be found, or calls ExitSetup upon 'Abort'
'-----------------------------------------------------------
'
Private Function DetectFile(ByVal strFilename As String) As Boolean
    Dim strMsg As String
    Dim iRet As Integer

    DetectFile = True

    Do Until FileExists(strFilename)
        strMsg = ResolveResString(resCANTOPEN) & vbLf & vbLf & strFilename
        iRet = MsgError(strMsg, vbAbortRetryIgnore Or vbExclamation Or vbDefaultButton2, gstrSETMSG)
        If gfNoUserInput Then iRet = vbAbort
        Select Case iRet
            Case vbAbort
                ExitSetup frmCopy, gintRET_ABORT
            Case vbIgnore
                DetectFile = False
                Exit Do
        End Select
    Loop
End Function

'-----------------------------------------------------------
' SUB: EtchedLine
'
' Draws an 'etched' line upon the specified form starting
' at the X,Y location passed in and of the specified length.
' Coordinates are in the current ScaleMode of the passed
' in form.
'
' IN: [frmEtch] - form to draw the line upon
'     [intX1] - starting horizontal of line
'     [intY1] - starting vertical of line
'     [intLength] - length of the line
'-----------------------------------------------------------
'
Public Sub EtchedLine(frmEtch As Form, ByVal intX1 As Integer, ByVal intY1 As Integer, ByVal intLength As Integer)
    Const lWHITE& = vb3DHighlight
    Const lGRAY& = vb3DShadow

    Dim sngPixelX As Single
    Dim sngPixelY As Single

    sngPixelX = frmEtch.ScaleX(1, vbPixels, frmEtch.ScaleMode)
    sngPixelY = frmEtch.ScaleY(1, vbPixels, frmEtch.ScaleMode)

    frmEtch.Line (intX1, intY1)-(intX1 + intLength + sngPixelX, intY1 + sngPixelY), lWHITE, BF
    frmEtch.Line (intX1, intY1)-(intX1 + intLength, intY1), lGRAY
End Sub

'-----------------------------------------------------------
' SUB: ExeSelfRegister
'
' Synchronously runs the file passed in (which should be
' an executable file that supports the /REGSERVER switch,
' for instance, a VB-generated ActiveX Component .EXE).
'
' IN: [strFileName] - .EXE file to register
'-----------------------------------------------------------
'
Private Sub ExeSelfRegister(ByVal strFilename As String)
    Const strREGSWITCH$ = " /REGSERVER"
    '
    'Synchronously shell out and run the .EXE with the self registration switch
    '
    SyncShell AddQuotesToFN(strFilename) & strREGSWITCH, INFINITE, , True
End Sub

'-----------------------------------------------------------
' SUB: ExitSetup
'
' Handles shutdown of the setup app.  Depending upon the
' value of the intExitCode parm, may prompt the user and
' exit the sub if the user chooses to cancel the exit
' process.
'
' IN: [frm] - active form to unload upon exit
'     [intExitCode] - code specifying exit action
'-----------------------------------------------------------
'
Public Sub ExitSetup(frm As Form, intExitCode As Integer)
    Const sKEY As String = "Software\Microsoft\Windows\CurrentVersion\RunOnce\Setup"
    Const sValue As String = "Configuring Data Access"

    Const iSUCCESS = 0
    Const iFAIL = 1
    Dim strMsg As String
    Dim strSilent As String
    Dim fNeedReboot As Boolean
    Dim iRet As Integer

    Dim sRet As String
    Dim hKey As Long

    Dim nErrorLevel As Integer

    On Error Resume Next
    '
    ' If we aren't running in silent or sms mode give
    ' the user a chance to try again, if applicable.
    '
    If Not gfNoUserInput Then
        Select Case intExitCode
            Case gintRET_EXIT
                '
                'If user chose an Exit or Cancel button
                '
                If MsgWarning(ResolveResString(resASKEXIT), vbQuestion Or vbYesNo Or vbDefaultButton2, gstrTitle) = vbNo Then
                    Exit Sub
                End If
            Case gintRET_ABORT
                '
                'If user chose to abort before a pending action
                '
                strMsg = ResolveResString(resINCOMPLETE) & vbLf & vbLf & ResolveResString(resQUITNOW) & vbLf & vbLf
                strMsg = strMsg & ResolveResString(resQUITSETUP)
                If MsgWarning(strMsg, vbQuestion Or vbYesNo Or vbDefaultButton2, gstrSETMSG) = vbNo Then
                    Exit Sub
                End If
        End Select
    End If

    'Abort any pending actions
    Do While fWithinAction()
        AbortAction
    Loop
    '
    'Close all files
    '
    Close
    '
    'Clean up any temporary files from VerInstallFile
    '
    Kill mstrVerTmpName

    If frm.hWnd <> frmSetup1.hWnd Then
        Unload frm
    End If
    
    If frmSetup1.Visible Then frmSetup1.SetFocus

    '
    'Give appropriate ending message depending upon exit code
    '
    Select Case intExitCode
        Case gintRET_EXIT, gintRET_ABORT
#If SMS Then
            gfSMSStatus = False
#End If
            strMsg = ResolveResString(resINTERRUPTED, gstrPIPE1, gstrAppName) & vbLf & vbLf & ResolveResString(resCANRUN, gstrPIPE1, gstrAppName)
            MsgWarning strMsg, vbOKOnly Or vbCritical, gstrTitle
        Case gintRET_FATAL
#If SMS Then
            gfSMSStatus = False
#End If
            MsgError ResolveResString(resERROR, gstrPIPE1, gstrAppName), vbOKOnly Or vbCritical, gstrTitle
        Case gintRET_FINISHEDSUCCESS
#If SMS Then
            gfSMSStatus = True
            '
            ' Don't log this message to SMS since it is only a confirmation.
            '
            gfDontLogSMS = True
#End If
            MsgFunc ResolveResString(resSUCCESS, gstrPIPE1, gstrAppName), vbOKOnly, gstrTitle
        Case Else
            strMsg = ResolveResString(resINTERRUPTED, gstrPIPE1, gstrAppName) & vbLf & vbLf & ResolveResString(resCANRUN, gstrPIPE1, gstrAppName)
            MsgWarning strMsg, vbOKOnly Or vbCritical, gstrTitle
    End Select

    'Stop logging
    DisableLogging

    ' Clean up a successful installation
    If (intExitCode = gintRET_FINISHEDSUCCESS) Then
        'Check to see if we need to reboot for mdac_typ
        If RegOpenKey(HKEY_LOCAL_MACHINE, sKEY, hKey) Then
            If RegQueryStringValue(hKey, sValue, sRet) Then
                'We need to reboot
                'Warn the user before rebooting.  If they choose to reboot, do so, otherwise
                'Warn them again.
                If MsgBox(ResolveResString(resREBOOT), vbYesNo Or vbInformation, gstrTitle) = vbYes Then
                    fNeedReboot = True
                Else
                    fNeedReboot = False
                    intExitCode = gintRET_FATAL
                    MsgBox ResolveResString(resREBOOTNO), vbOKOnly Or vbExclamation, gstrTitle
                End If
            End If
        End If
        'Check to see if we need to reboot for DX Setup
        If gfDXReboot Then
            'We need to reboot
            'Warn the user before rebooting.  If they choose to reboot, do so, otherwise
            'Warn them again.
            If MsgBox(ResolveResString(resREBOOT), vbYesNo Or vbInformation, gstrTitle) = vbYes Then
                fNeedReboot = True
            Else
                fNeedReboot = False
                intExitCode = gintRET_FATAL
                MsgBox ResolveResString(resREBOOTNO), vbOKOnly Or vbExclamation, gstrTitle
            End If
        End If
    Else
        'Setup has been aborted for one reason or another
        If Len(gstrAppRemovalEXE) > 0 Then
            '
            ' We don't want to log this message to sms because it is
            ' only a confirmation message.
            '
#If SMS Then
            gfDontLogSMS = True
#End If
            MsgFunc ResolveResString(resLOG_ABOUTTOREMOVEAPP), vbInformation Or vbOKOnly, gstrTitle
            
            Err.Clear
            '
            ' Ready to run the installer.  Determine if this is a
            ' silent uninstall or not.
            '
            If gfSilent Then
                strSilent = gstrSilentLog
            Else
                strSilent = vbNullString
            End If

            Select Case intExitCode
                Case gintRET_FATAL
                    nErrorLevel = APPREMERR_FATAL
                Case gintRET_EXIT
                    nErrorLevel = APPREMERR_USERCANCEL
                Case gintRET_ABORT
                    nErrorLevel = APPREMERR_NONFATAL
                Case Else
                    nErrorLevel = APPREMERR_FATAL
            End Select

#If SMS Then
            Shell GetAppRemovalCmdLine(gstrAppRemovalEXE, gstrAppRemovalLog, strSilent, gfSMS, nErrorLevel, True), vbNormalFocus
#Else
            Shell GetAppRemovalCmdLine(gstrAppRemovalEXE, gstrAppRemovalLog, strSilent, nErrorLevel, True), vbNormalFocus
#End If
            If Err.Number <> 0 Then
                MsgError Err.Description & vbLf & vbLf & ResolveResString(resLOG_CANTRUNAPPREMOVER), vbExclamation Or vbOKOnly, gstrTitle
            End If

            'Since the app removal program will attempt to delete this program and all of our runtime
            'files, we should exit as soon as possible (otherwise the app remover will not be
            'able to remove these files)
        End If
        
        'Note: We do not delete the logfile if an error occurs.
        'The application removal EXE will do that if needed.
        
    End If
    
    Unload frmSetup1

#If SMS Then
    If gfSMS Then
        WriteMIF gstrMIFFile, gfSMSStatus, gstrSMSDescription
    End If
#End If

    'Try the reboot (if necessary)...
    If fNeedReboot Then RebootSystem
    'End the program
    If intExitCode = gintRET_FINISHEDSUCCESS Then
        ExitProcess iSUCCESS
    Else
        ExitProcess iFAIL
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION: ProcessCommandLine
'
' Processes the command-line arguments
'
' OUT: Fills in the passed-in byref parameters as appropriate
'-----------------------------------------------------------
'
#If SMS Then
Public Sub ProcessCommandLine(ByVal strCommand As String, ByRef fSilent As Boolean, ByRef strSilentLog As String, ByRef fSMS As Boolean, ByRef strMIFFile As String, ByRef strSrcPath As String, ByRef strAppRemovalLog As String, ByRef strAppRemovalEXE As String)
#Else
Public Sub ProcessCommandLine(ByVal strCommand As String, ByRef fSilent As Boolean, ByRef strSilentLog As String, ByRef strSrcPath As String, ByRef strAppRemovalLog As String, ByRef strAppRemovalEXE As String)
#End If
    Dim fErr As Boolean
    Dim intAnchor As Integer
    
    strSrcPath = vbNullString
    strAppRemovalLog = vbNullString
    
    strCommand = Trim$(strCommand)
    
    '
    ' First, check to see if this is supposed to be a silent
    ' install (/s on the command line followed by
    ' a log file name) and set global variables appropriately.
    '
    ' If you are designing a silent install, the /s
    ' command line parameter should be added to the setup.exe
    ' command.  It will automatically be passed to setup1 as the
    ' first parameter.
    '
    ' The filename that follows the /s parameter must
    ' include the full path name.
    '
    intAnchor = InStr(LCase$(strCommand), gstrSwitchPrefix2 & gstrSILENTSWITCH)
    If intAnchor > 0 Then
        fSilent = True
        strCommand = Trim$(Mid$(strCommand, intAnchor + 2))
        strSilentLog = strExtractFilenameArg(strCommand, fErr)
        If fErr Then GoTo BadCommandLine
    Else
        fSilent = False
    End If
#If SMS Then
    fSMS = False
#End If

    '
    ' We expect to find the source directory,
    ' name/path of the logfile, and name/path
    ' of the app removal executable, separated only by
    ' spaces
    '
    strSrcPath = strExtractFilenameArg(strCommand, fErr)
    If fErr Then GoTo BadCommandLine

    strAppRemovalLog = strExtractFilenameArg(strCommand, fErr)
    If fErr Then GoTo BadCommandLine

    strAppRemovalEXE = strExtractFilenameArg(strCommand, fErr)
    If fErr Then GoTo BadCommandLine

    ' Both the app removal logfile and executable must exist
    If Not FileExists(strAppRemovalLog) Then
        GoTo BadAppRemovalLog
    End If

    If Not FileExists(strAppRemovalEXE) Then
        GoTo BadAppRemovalEXE
    End If

    ' Last check:  There should be nothing else on the command line
    strCommand = Trim$(strCommand)
    If Len(strCommand) > 0 Then
        GoTo BadCommandLine
    End If

Exit Sub
BadAppRemovalLog:
    MsgError ResolveResString(resCANTFINDAPPREMOVALLOG, gstrPIPE1, strAppRemovalLog), vbExclamation Or vbOKOnly, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
    
BadAppRemovalEXE:
    MsgError ResolveResString(resCANTFINDAPPREMOVALEXE, gstrPIPE1, strAppRemovalEXE), vbExclamation Or vbOKOnly, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
    
BadCommandLine:
    MsgError ResolveResString(resBADCOMMANDLINE), vbExclamation Or vbOKOnly, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
End Sub

'-----------------------------------------------------------
' FUNCTION: GetDrivesAllocUnit
'
' Gets the minimum file size allocation unit for the
' specified drive
'
' IN: [strDrive] - Drive to get allocation unit for
'
' Returns: minimum allocation unit of drive, or -1 if
'          this value couldn't be determined
'-----------------------------------------------------------
'
Private Function GetDrivesAllocUnit(ByVal strDrive As String) As Long
    Dim lRet As Long
    Dim lBytes As Long
    Dim lSect As Long
    Dim lClust As Long
    Dim lTot As Long

    Dim strDriveNew As String

    On Error Resume Next

    If GetDrive(strDrive, strDriveNew) Then
        lRet = GetDiskFreeSpace(strDriveNew, lSect, lBytes, lClust, lTot)
        If Err.Number = 0 Then
            If lRet <> 0 Then
                GetDrivesAllocUnit = lSect * lBytes
                Exit Function
            End If
        End If
    End If

    MsgError Err.Description & vbLf & vbLf & ResolveResString(resALLOCUNIT) & strDrive, vbExclamation, gstrTitle
#If SMS Then
    If gfSMS Then
        ExitSetup frmSetup1, gintRET_FATAL
    End If
#End If

    GetDrivesAllocUnit = -1

    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: GetDrive
'
' Returns True if it finds the drive portion of a path and
' False otherwise.
'
' IN: [strPath] - Any local or UNC path. The path must
' include at least the drive letter for local drives and the
' share for network drives. Following are examples of
' allowed syntax:
'
' C, C:, C:setup, C:\, C:\setup, \\server\share,
' \\server\share\, \\server\share\sub.
'
' OUT: [strDrive] - If strPath represents a valid drive,
' strDrive will return it in this form: '_:\' for local
' drives and '\\_\_\' for UNC drives. Either way, it will
' always end in a '\' and it will always be lowercase.
'-----------------------------------------------------------
'
Public Function GetDrive(ByVal strPath As String, ByRef strDrive As String) As Boolean
    Dim lSep As Long
    Dim lSepLast As Long
    Dim i As Long

    lSep = InStr(strPath, gstrSEP_DIR)
    Select Case lSep
    Case 3
        'This must be like 'C:\'
        If InStr(2, strPath, gstrSEP_DRIVE) = 2 Then
            'This looks right.
            strDrive = LCase$(Left$(strPath, 3))
            GetDrive = True
        End If
    Case 2
        'Can't be a drive
    Case 1
        'This must be a UNC because it starts with "\". So, we're looking for
        '   '\\...\...[\[...]]'.
        lSep = InStr(2, strPath, gstrSEP_DIR)
        If lSep = 2 Then
            'So far, this matches a UNC.
            lSep = InStr(3, strPath, gstrSEP_DIR)
            If lSep > 3 Then
                'This really looks like a valid UNC.
                If lSep < Len(strPath) Then
                    'So far so good.
                    lSepLast = InStr(lSep + 1, strPath, gstrSEP_DIR)
                    Select Case lSepLast
                    Case Is > lSep + 1
                        'We've found '\\...\...\'. We're satisfied that this is
                        '   a valid UNC.
                        strDrive = LCase$(Left$(strPath, lSepLast))
                        GetDrive = True
                    Case 0
                        'We've found '\\...\...'. We're satisfied that this is a
                        '   valid UNC.
                        strDrive = LCase$(strPath & gstrSEP_DIR)
                        GetDrive = True
                    End Select
                End If
            End If
        End If
    Case Else
        'We'll allow no '\' if it's like 'C' or 'C:[...]'
        Select Case Len(strPath)
        Case 1
            'This must be like 'C'. Add ':\' and return it.
            strDrive = LCase$(strPath & gstrSEP_DRIVE & gstrSEP_DIR)
            GetDrive = True
        Case Is > 1
            'This must be like 'C:...'
            If InStr(2, strPath, gstrSEP_DRIVE) = 2 Then
                'This looks like 'C:...'
                strDrive = LCase$(Left$(strPath, 2) & gstrSEP_DIR)
                GetDrive = True
            End If
        End Select
    End Select
End Function

'-----------------------------------------------------------
' FUNCTION: GetAppRemovalCmdLine
'
' Returns the correct command-line arguments (including
' path to the executable for use in calling the
' application removal executable)
'
' IN: [strAppRemovalEXE] - Full path/filename of the app removal EXE
'     [strAppRemovalLog] - Full path/filename of the app removal logfile
'     [strSilentLog] - Full path/filename of the file to log messages to when in silent mode.
'                       If this is an empty string then silent mode is turned off for uninstall.
'     [fSMS] - Boolean.  If True, we have been doing an SMS install and must tell the Uninstaller
'              to also do an SMS uninstall.  SMS is the Microsoft Systems Management Server.
'     [nErrorLevel] - Error level:
'                        APPREMERR_NONE - no error
'                        APPREMERR_FATAL - fatal error
'                        APPREMERR_NONFATAL - non-fatal error, user chose to abort
'                        APPREMERR_USERCANCEL - user chose to cancel (no error)
'     [fWaitForParent] - True if the application removal utility should wait
'                        for the parent (this process) to finish before starting
'                        to remove files.  Otherwise it may not be able to remove
'                        this process' executable file, depending upon timing.
'                        Defaults to False if not specified.
'-----------------------------------------------------------
'
#If SMS Then
Private Function GetAppRemovalCmdLine(ByVal strAppRemovalEXE As String, ByVal strAppRemovalLog, ByVal strSilentLog As String, ByVal fSMS As Boolean, ByVal nErrorLevel As Integer, Optional fWaitForParent As Boolean = False)
#Else
Private Function GetAppRemovalCmdLine(ByVal strAppRemovalEXE As String, ByVal strAppRemovalLog, ByVal strSilentLog As String, ByVal nErrorLevel As Integer, Optional fWaitForParent As Boolean = False)
#End If
    Dim strEXE As String
    Dim strLog As String
    Dim strSilent As String
    Dim strErrLevel As String
    Dim strForce As String
    Dim strWait As String
    Dim strSMS As String

    Dim curProcessId As Currency
    Dim Wrap As Currency
    Dim lProcessId As Long
    Dim cProcessId As Currency

    strEXE = AddQuotesToFN(strAppRemovalEXE)
    strLog = "-n " & gstrQUOTE & GetLongPathName(strAppRemovalLog) & gstrQUOTE
    If gfSilent And Len(strSilentLog) > 0 Then
        strSilent = "/s " & gstrQUOTE & strSilentLog & gstrQUOTE
    End If
    
#If SMS Then
    If fSMS Then
        strSMS = " /q "
    End If
#End If

    If nErrorLevel <> APPREMERR_NONE Then
        strErrLevel = "-e " & Format$(nErrorLevel)
        strForce = " -f"
    End If
    If fWaitForParent Then
        Wrap = 2 * (CCur(&H7FFFFFFF) + 1)

        'Always print as an unsigned long
        lProcessId = GetCurrentProcessId()
        cProcessId = lProcessId
        If cProcessId < 0 Then cProcessId = cProcessId + Wrap

        strWait = " -w " & str$(cProcessId)
    End If

    GetAppRemovalCmdLine = strEXE & " " & strLog & " " & strSilent & " " & strSMS & strErrLevel & strForce & strWait
End Function

'-----------------------------------------------------------
' FUNCTION: IncrementRefCount
'
' Increments the reference count on a file in the registry
' so that it may properly be removed if the user chooses
' to remove this application.
'
' IN: [strFullPath] - FULL path/filename of the file
'     [fFileAlreadyExisted] - indicates whether the given
'                             file already existed on the
'                             hard drive
'-----------------------------------------------------------
'
Private Sub IncrementRefCount(ByVal strFullPath As String, ByVal fFileAlreadyExisted As Boolean)
    Dim strSharedDLLsKey As String
    Dim fSuccess As Boolean
    Dim hKey As Long
    Dim lCurRefCount As Long

    strSharedDLLsKey = RegPathWinCurrentVersion() & "\SharedDLLs"
    
    'We must always use the LFN for the filename, so that we can uniquely
    'and accurately identify the file in the registry.
    strFullPath = GetLongPathName(strFullPath)
    
    'Get the current reference count for this file
    fSuccess = RegCreateKey(HKEY_LOCAL_MACHINE, strSharedDLLsKey, vbNullString, hKey)
    If fSuccess Then
        If Not RegQueryRefCount(hKey, strFullPath, lCurRefCount) Then
            'No current reference count for this file
            If fFileAlreadyExisted Then
                'If there was no reference count, but the file was found
                'on the hard drive, it means one of two things:
                '  1) This file is shipped with the operating system
                '  2) This file was installed by an older setup program
                '     that does not do reference counting
                'In either case, the correct conservative thing to do
                'is assume that the file is needed by some application,
                'which means it should have a reference count of at
                'least 1.  This way, our application removal program
                'will not delete this file.
                lCurRefCount = 1

            Else
                lCurRefCount = 0
            End If
        End If
        
        'Increment the count in the registry
        fSuccess = RegSetNumericValue(hKey, strFullPath, lCurRefCount + 1, False)
        If Not fSuccess Then
            GoTo DoErr
        End If
        RegCloseKey hKey
    Else
        GoTo DoErr
    End If
    
DoErr:
    'An error message should have already been shown to the user
End Sub

'-----------------------------------------------------------
' FUNCTION: InitDiskInfo
'
' Called before calculating disk space to initialize
' values used/determined when calculating disk space
' required.
'-----------------------------------------------------------
'
Public Sub InitDiskInfo()
    '
    'Initialize "table" of drives used and disk space array
    '
    Set gcolDrivesUsed = New Collection
    Erase gsDiskSpace

    mlTotalToCopy = 0
End Sub

'-----------------------------------------------------------
' FUNCTION: IsDisplayNameUnique
'
' Determines whether a given display name for registering
'   the application removal executable is unique or not.  This
'   display name is the title which is presented to the
'   user in Windows 95's control panel Add/Remove Programs
'   applet.
'
' IN: [hkeyAppRemoval] - open key to the path in the registry
'                       containing application removal entries
'     [strDisplayName] - the display name to test for uniqueness
'
' Returns: True if the given display name is already in use,
'          False if otherwise
'-----------------------------------------------------------
'
Private Function IsDisplayNameUnique(ByVal hkeyAppRemoval As Long, ByVal strDisplayName As String) As Boolean
    Dim lIdx As Long
    Dim strSubkey As String
    Dim strDisplayNameExisting As String
    Const strKEY_DISPLAYNAME$ = "DisplayName"
    Dim hkeyExisting As Long

    
    IsDisplayNameUnique = True
    
    lIdx = 0
    Do
        Select Case RegEnumKey(hkeyAppRemoval, lIdx, strSubkey)
            Case ERROR_NO_MORE_ITEMS
                'No more keys - must be unique
                Exit Do
            Case ERROR_SUCCESS
                'We have a key to some application removal program.  Compare its
                '  display name with ours
                If RegOpenKey(hkeyAppRemoval, strSubkey, hkeyExisting) Then
                    If RegQueryStringValue(hkeyExisting, strKEY_DISPLAYNAME, strDisplayNameExisting) Then
                        If UCase$(strDisplayNameExisting) = UCase$(strDisplayName) Then
                            'There is a match to an existing display name
                            IsDisplayNameUnique = False
                            RegCloseKey hkeyExisting
                            Exit Do
                        End If
                    End If
                    RegCloseKey hkeyExisting
                End If
            Case Else
                'Error, we must assume it's unique.  An error will probably
                '  occur later when trying to add to the registry
                Exit Do
        End Select
        lIdx = lIdx + 1
    Loop
End Function

'-----------------------------------------------------------
' FUNCTION: IsNewerVer
'
' Compares two file version structures and determines
' whether the source file version is newer (greater) than
' the destination file version.  This is used to determine
' whether a file needs to be installed or not
'
' IN: [sSrcVer] - source file version information
'     [sDestVer] - dest file version information
'
' Returns: True if source file is newer than dest file,
'          False if otherwise
'-----------------------------------------------------------
'
Private Function IsNewerVer(sSrcVer As VERINFO, sDestVer As VERINFO) As FileComparison
    'For the top three version fields, if the source is greater, immediately
    '   return True. If the destination is greater, immediately return False.
    '   If they are equal, fall through to check the next field. For the last
    '   version field, if the source is greater, immediately return True.
    '   Otherwise, either the destination is newer or the files are the same.
    '   Either way, immediately return False.
    IsNewerVer = fcOlder
    With sSrcVer
        If .FileVerPart1 > sDestVer.FileVerPart1 Then GoTo INVNewer
        If .FileVerPart1 < sDestVer.FileVerPart1 Then Exit Function

        If .FileVerPart2 > sDestVer.FileVerPart2 Then GoTo INVNewer
        If .FileVerPart2 < sDestVer.FileVerPart2 Then Exit Function

        If .FileVerPart3 > sDestVer.FileVerPart3 Then GoTo INVNewer
        If .FileVerPart3 < sDestVer.FileVerPart3 Then Exit Function

        If .FileVerPart4 > sDestVer.FileVerPart4 Then GoTo INVNewer
        If .FileVerPart4 = sDestVer.FileVerPart4 Then IsNewerVer = fcEquivalent
    End With
'Either the files are the same or the destination's 4th field is greater. Either
'   way, return False.
Exit Function
INVNewer:
    IsNewerVer = fcNewer
End Function

'-----------------------------------------------------------
' FUNCTION: IsValidDestDir
'
' Determines whether or not the destination directory
' specifed in the "DefaultDir" key of the [Setup] section
' in SETUP.LST or a destination dir entered by the user
' is not a subdirectory of the source directory.
'
' Notes: [gstrSrcPath] - points to the source directory
'        [strDestDir] - points to the dest directory
'
' Returns: True if dest dir is a valid location, False
'          otherwise
'-----------------------------------------------------------
'
Public Function IsValidDestDir(strDestDir As String) As Integer
    Dim strMsg As String

    '
    ' Both of these paths, strDestDir and gstrSrcPath, are *always*
    ' in the format 'X:\' or 'X:\DIRNAME\'.
    '
    If InStr(UCase$(strDestDir), UCase$(gstrSrcPath)) = 1 Then
        strMsg = ResolveResString(resDIRSPECIFIED) & vbLf & strDestDir & vbLf & ResolveResString(resSAMEASSRC)
        MsgFunc strMsg, vbOKOnly Or vbExclamation, gstrTitle
    Else
        IsValidDestDir = True
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: MakePath
'
' Creates the specified directory path
'
' IN: [strDirName] - name of the dir path to make
'     [fAllowIgnore] - whether or not to allow the user to
'                      ignore any encountered errors.  If
'                      false, the function only returns
'                      if successful.  If missing, this
'                      defaults to True.
'
' Returns: True if successful, False if error and the user
'          chose to ignore.  (The function does not return
'          if the user selects ABORT/CANCEL on an error.)
'-----------------------------------------------------------
'
Public Function MakePath(ByVal strDir As String, Optional ByVal fAllowIgnore As Boolean = True) As Boolean
    Dim intButtons As Integer
    Dim strMsg As String
    Dim iRet As Integer

    Do
        If MakePathAux(strDir) Then
            MakePath = True
            Exit Function
        Else
            strMsg = ResolveResString(resMAKEDIR, gstrPIPE1, strDir)
            If fAllowIgnore Then
                intButtons = vbAbortRetryIgnore
            Else
                intButtons = vbRetryCancel
            End If
            iRet = MsgError(strMsg, intButtons Or vbExclamation Or vbDefaultButton2, gstrSETMSG)
            '
            ' if we are running silent then we
            ' can't continue.  Previous MsgError
            ' took care of write silent log entry.
            '
            If gfNoUserInput Then
                ExitSetup frmCopy, gintRET_FATAL
            End If
            
            Select Case iRet
                Case vbAbort, vbCancel
                    ExitSetup frmCopy, gintRET_ABORT
                Case vbIgnore
                    MakePath = False
                    Exit Function
                Case vbRetry
            End Select
        End If
    Loop
End Function

'----------------------------------------------------------
' SUB: MoveAppRemovalFiles
'
' Moves the app removal logfile to the application directory,
' and registers the app removal executable with the operating
' system.
'----------------------------------------------------------
'
Public Sub MoveAppRemovalFiles(ByVal strGroupName As String)
    Dim strNewAppRemovalLogName As String
    Dim iExt As Integer

    'Get rid of the cabs in the windows folder.
    CleanUpCabs
    'Get rid of the temp dir
    'Bug fix for #6-34583
    KillTempFolder
    'Find a unique name for the app removal logfile in the
    'application directory
    
    '...First try the default extension
    strNewAppRemovalLogName = gstrDestDir & mstrFILE_APPREMOVALLOGBASE & mstrFILE_APPREMOVALLOGEXT
    If FileExists(strNewAppRemovalLogName) Then
        '...Next try incrementing integral extensions
        Do
            If iExt > 999 Then
                GoTo CopyErr
            End If
            

            strNewAppRemovalLogName = gstrDestDir & mstrFILE_APPREMOVALLOGBASE & gstrSEP_EXT & Format$(iExt, "000")
            If Not FileExists(strNewAppRemovalLogName) Then
                Exit Do 'Unique name was found
            Else
                iExt = iExt + 1
            End If
        Loop
    End If

    On Error GoTo CopyErr
    FileCopy gstrAppRemovalLog, strNewAppRemovalLogName
    
    'Now we need to start logging in the new logfile, so that the
    'creation of the application removal icon under NT gets logged.
    EnableLogging strNewAppRemovalLogName

    On Error GoTo 0
    If Not RegisterAppRemovalEXE(gstrAppRemovalEXE, strNewAppRemovalLogName, strGroupName) Then
        MsgError ResolveResString(resCANTREGISTERAPPREMOVER), vbExclamation Or vbOKOnly, gstrTitle
        ExitSetup frmSetup1, gintRET_FATAL
    End If
    
    'Now we can delete the original logfile, since we no longer have a reference
    'to it, and start using the new logfile
    On Error Resume Next
    Kill gstrAppRemovalLog
    
    'This temporary app removal logfile should no longer be used
    gstrAppRemovalLog = strNewAppRemovalLogName
    gfAppRemovalFilesMoved = True
    
    Exit Sub
    
CleanUpOnErr:
    On Error Resume Next
    Kill strNewAppRemovalLogName
    On Error GoTo 0
    MsgError ResolveResString(resCANTCOPYLOG, gstrPIPE1, gstrAppRemovalLog), vbExclamation Or vbOKOnly, gstrTitle
    ExitSetup Screen.ActiveForm, gintRET_FATAL
    
CopyErr:
    Resume CleanUpOnErr
End Sub

'----------------------------------------------------------
' SUB: CleanUpCabs
'
' Cleans up temporary cab files.
'----------------------------------------------------------
'
Public Sub CleanUpCabs()
    Dim lCount As Long
    Dim sCab As String
    Dim sTemp As String

    'Get rid of the cab file(s) in the windows dir (if any).
    'Regardless of how many cabs there are, gsCABFULLNAME will specify
    '   the name of the first cab.
    If FileExists(gsCABFULLNAME) Then
        Kill gsCABFULLNAME
    End If
    If gintCabs > 1 Then
        'If there is more than one cab, then the cabname will end in
        '   1.cab. Remove this part and add the appropriate number,
        '   starting with 2.
        sTemp = Left$(gsCABFULLNAME, Len(gsCABFULLNAME) - 5)
        lCount = 2
        Do
            sCab = sTemp & CStr(lCount) & gstrSEP_EXT & gsINI_CABNAME
            If FileExists(sCab) Then
                Kill sCab
            Else
                Exit Sub
            End If
            lCount = lCount + 1
            'We don't need this check before the first iteration because
            '   we already checked that gintCabs > 1 and we explicitly
            '   set lCount = 2.
            If lCount > gintCabs Then
                Exit Sub
            End If
        Loop
    End If
End Sub

'-----------------------------------------------------------
' SUB: KillTempFolder
' BUG FIX #6-34583
'
' Deletes the temporary files stored in the temp folder
'-----------------------------------------------------------
'
Private Sub KillTempFolder()
    Const sWILD As String = "*.*"
    Dim sFile As String
    
    On Error Resume Next
    
    sFile = Dir$(gsTEMPDIR & sWILD, vbHidden Or vbReadOnly Or vbSystem)
    Do While Len(sFile) > 0
        SetAttr gsTEMPDIR & sFile, vbNormal
        Kill gsTEMPDIR & sFile
        sFile = Dir$
    Loop
    RmDir gsTEMPDIR
End Sub

'-----------------------------------------------------------
' SUB: ParseDateTime
'
' Same as CDate with a string argument, except that it
' ignores the current localization settings.  This is
' important because SETUP.LST always uses the same
' format for dates.
'
' IN: [strDate] - string representing the date in
'                 the format mm/dd/yy or mm/dd/yyyy
' OUT: The date which strDate represents
'-----------------------------------------------------------
'
Private Function ParseDateTime(ByVal strDateTime As String) As Date
    Dim Var As Variant

    Var = strDateTime
    If 0 = VariantChangeTypeEx(VarPtr(Var), VarPtr(Var), &H409, 0, vbDate) Then
        ParseDateTime = Var
    Else
        'Raise same error as CDate
        Err.Raise 13
    End If
End Function

'-----------------------------------------------------------
' SUB: PromptForNextDisk
'
' If the source media is removable or a network connection,
' prompts the user to insert the specified disk number
' containing the filename which is used to determine that
' the correct disk is inserted.
'
' IN: [intDiskNum] - disk number to insert
'     [strDetectFile] - file to search for to ensure that
'                       the correct disk was inserted
'
' Notes: [gstrSrcPath] - used to identify the source drive
'-----------------------------------------------------------
'
Private Sub PromptForNextDisk(ByVal intDiskNum As Integer, ByVal strDetectFile As String)
    Static intDrvType As Integer

    Dim intRC As Integer
    Dim strMsg As String
    Dim strDrive As String
    Dim strMultDirBaseName As String
    Dim strDetectPath As String

    On Error Resume Next

    strMultDirBaseName = ResolveResString(resCOMMON_MULTDIRBASENAME)
    '
    'Get source drive and, if we haven't yet determined it, get the
    'source drive type
    '
    
    strDrive = Left$(gstrSrcPath, 2)
    If intDrvType = 0 Then
        If IsUNCName(strDrive) Then
            intDrvType = intDRIVE_REMOTE
            strDrive = gstrSrcPath
        Else
            intDrvType = GetDriveType(Asc(strDrive) - 65)
        End If
    End If

    Do While SrcFileMissing(gstrSrcPath, strDetectFile, intDiskNum)
        Select Case intDrvType
            Case 0, intDRIVE_REMOVABLE, intDRIVE_CDROM
                strMsg = ResolveResString(resINSERT) & vbLf & ResolveResString(resDISK) & Format$(intDiskNum)
                strMsg = strMsg & ResolveResString(resINTO) & strDrive
            Case intDRIVE_REMOTE
                strMsg = ResolveResString(resCHKCONNECT) & strDrive
            Case intDRIVE_FIXED
                If DirExists(gstrSrcPath & strMultDirBaseName & Format$(intDiskNum)) Then
                    strDetectPath = gstrSrcPath & strMultDirBaseName & Format$(intDiskNum)
                Else
                    strDetectPath = gstrSrcPath
                End If
                strMsg = ResolveResString(resCOMMON_CANTFINDSRCFILE, gstrPIPE1, strDetectPath & gstrSEP_DIR & strDetectFile)
        End Select

        Beep
        intRC = MsgFunc(strMsg, vbOKCancel Or vbExclamation, gstrSETMSG)
        '
        ' We should always fail if in silent or sms mode.
        '
        If intRC = vbCancel Or gfNoUserInput Then
            ExitSetup frmCopy, gintRET_EXIT
        End If
    Loop

    gintCurrentDisk = intDiskNum
End Sub

'-----------------------------------------------------------
' FUNCTION: SrcFileMissing
'
' Tries to locate the file strSrcFile by first looking
' in the strSrcDir directory, then in the DISK(x+1)
' directory if it exists.
'
' IN: [strSrcDir] - Directory/Path where file should be.
'     [strSrcFile] - File we are looking for.
'     [intDiskNum] - Disk number we are expecting file
'                    to be on.
'
' Returns: True if file not found; otherwise, false
'-----------------------------------------------------------
'
Private Function SrcFileMissing(ByVal strSrcDir As String, ByVal strSrcFile As String, ByVal intDiskNum As Integer) As Boolean
    Dim fFound As Boolean
    Dim strMultDirBaseName As String
    
    AddDirSep strSrcDir
    '
    ' First check to see if it's in the main src directory.
    ' This would happen if someone copied the contents of
    ' all the floppy disks to a single directory on the
    ' hard drive.  We should allow this to work.
    '
    ' This test would also let us know if the user inserted
    ' the wrong floppy disk or if a network connection is
    ' unavailable.
    '
    If FileExists(strSrcDir & strSrcFile) Then
        fFound = True
        GoTo doneSFM
    End If
    '
    ' Next try the DISK(x) subdirectory of the main src
    ' directory.  This would happen if the floppy disks
    ' were copied into directories named DISK1, DISK2,
    ' DISK3,..., DISKN, etc.
    '
    strMultDirBaseName = ResolveResString(resCOMMON_MULTDIRBASENAME)
    If FileExists(strSrcDir & ".." & gstrSEP_DIR & strMultDirBaseName & Format$(intDiskNum) & gstrSEP_DIR & strSrcFile) Then
        fFound = True
        GoTo doneSFM
    End If
    
doneSFM:
    SrcFileMissing = Not fFound
End Function
'-----------------------------------------------------------
' FUNCTION: ReadIniFile
'
' Reads a value from the specified section/key of the
' specified .INI file
'
' IN: [strIniFile] - name of .INI file to read
'     [strSection] - section where key is found
'     [strKey] - name of key to get the value of
'
' Returns: non-zero terminated value of .INI file key
'-----------------------------------------------------------
'
Public Function ReadIniFile(ByVal strIniFile As String, ByVal strSection As String, ByVal strKey As String) As String
    Dim strBuffer As String

    '
    'If successful read of .INI file, strip any trailing zero returned by the Windows API GetPrivateProfileString
    '
    strBuffer = Space$(gintMAX_SIZE)

    If GetPrivateProfileString(strSection, strKey, vbNullString, strBuffer, gintMAX_SIZE, strIniFile) Then
        ReadIniFile = StringFromBuffer(strBuffer)
    End If
End Function

'-----------------------------------------------------------
' SUB: ReadSetupFileLine
'
' Reads the requested 'FileX=' key from the specified
' section of the setup information file (SETUP.LST).
'
' IN: [strSection] - name of section to read from SETUP.LST,
'                    Ex: "Files"
'     [intFileNum] - file number index to read
'
' OUT: [sFile] - FILEINFO Type variable that, after parsing,
'                holds the information for the file
'                described.
'
' Returns: True if the requested info was successfully read,
'          False otherwise
'-----------------------------------------------------------
'
Public Function ReadSetupFileLine(ByVal strSection As String, ByVal intFileNum As Integer, sFile As FILEINFO) As Integer
    Dim strLine As String
    Dim strMsg As String
    Dim intOffset As Integer
    Dim intAnchor As Integer
    Dim fDone As Integer
    Dim fErr As Boolean
    Dim strVersion As String
    Dim strFilename As String

    Dim strInitialDestDir As String

    Dim strShareType As String

    sFile.fSystem = False
    sFile.fShared = False
    sFile.fDestDirRecognizedBySetupExe = False
    
    '
    ' Read the requested line, if unable to read it (Len(strLine) = 0) then exit
    '
    strLine = ReadIniFile(gstrSetupInfoFile, strSection, gstrINI_FILE & Format$(intFileNum))
    If Len(strLine) = 0 Then
        Exit Function
    End If

    '
    'source file name, ensure it's not a UNC name
    '
    intAnchor = 1
    sFile.strSrcName = strExtractFilenameItem(strLine, intAnchor, fErr)
    If fErr Then GoTo RSFLError

    intAnchor = intAnchor + 1 'Skip past the comma
    
    '
    'dest file name, ensure it's not a UNC name
    '
    If Left$(sFile.strSrcName, 1) = gstrAT Then
        sFile.strDestName = Mid$(sFile.strSrcName, 2)
    Else
        sFile.strDestName = sFile.strSrcName
    End If
    SeparatePathAndFileName sFile.strDestName, , strFilename
    '
    'parse and resolve destination directory
    '
    intOffset = intGetNextFldOffset(intAnchor, strLine, gstrCOMMA)
    If intOffset > 0 Then
        strInitialDestDir = Mid$(strLine, intAnchor, intOffset - intAnchor)
        If InStr(UCase$(strInitialDestDir), UCase$(gstrWINSYSDESTSYSFILE)) = 1 Then
            sFile.fSystem = True
        End If
        If InStr(UCase$(strInitialDestDir), UCase$(gstrDAODEST)) = 1 Then
            '
            ' Special case for DAO destinations.  If there
            ' are any DAO files, we need to add special
            ' DAO reg info later.  gfRegDAO tells us to do that.
            '
            gfRegDAO = True
        End If
        sFile.strDestDir = ResolveDestDir(strInitialDestDir, , sFile.fDestDirRecognizedBySetupExe)
        If sFile.strDestDir <> "?" Then
            sFile.strDestDir = ResolveDir(sFile.strDestDir, False, False)
            If Len(sFile.strDestDir) = 0 Then 'Or IsUNCName(sFile.strDestDir) Then
                GoTo RSFLError
            End If
        End If
    Else
        GoTo RSFLError
    End If

    '
    'file registration information
    '
    intAnchor = intOffset + 1
    intOffset = intGetNextFldOffset(intAnchor, strLine, gstrCOMMA)
    If intOffset > 0 Then
        sFile.strRegister = Mid$(strLine, intAnchor, intOffset - intAnchor)
    Else
        GoTo RSFLError
    End If

    '
    'Extract file share type
    '
    intAnchor = intOffset + 1
    intOffset = intGetNextFldOffset(intAnchor, strLine, gstrCOMMA)
    sFile.fShared = False
    If intOffset > 0 Then
        strShareType = Mid$(strLine, intAnchor, intOffset - intAnchor)
        Select Case UCase$(strShareType)
            Case UCase$(mstrPRIVATEFILE)
                sFile.fShared = False
            Case UCase$(mstrSHAREDFILE)
                If sFile.fSystem Then
                    'A file cannot be both system and shared
                    GoTo RSFLError
                End If
                
                sFile.fShared = True
            Case Else
                GoTo RSFLError
        End Select
    End If
    
    '
    'Extract file date and convert to a date variant
    '
    intAnchor = intOffset + 1
    intOffset = intGetNextFldOffset(intAnchor, strLine, gstrCOMMA)
    If intOffset > 0 Then
        On Error GoTo RSFLError
        sFile.varDate = ParseDateTime(Mid$(strLine, intAnchor, intOffset - intAnchor))
        On Error GoTo 0
    End If

    '
    'Get file size
    '
    intAnchor = intOffset + 1
    intOffset = intGetNextFldOffset(intAnchor, strLine, gstrCOMMA)
    If intOffset > 0 Then
        sFile.lFileSize = Val(Mid$(strLine, intAnchor, intOffset - intAnchor))
    Else
        GoTo RSFLError
    End If

    '
    ' Get the version number, otherwise flag that there is no version info
    '
    intAnchor = intOffset + 1
    If intOffset > 0 Then
        strVersion = Trim$(Right$(strLine, Len(strLine) - intOffset))
        If Len(strVersion) = 0 Then
            sFile.sVerInfo.FileVerPart1 = gintNOVERINFO
        Else
            PackVerInfo strVersion, sFile.sVerInfo
        End If
    Else
        GoTo RSFLError
    End If
    
RSFLDone:
    ReadSetupFileLine = True
    Exit Function

RSFLError:
    strMsg = gstrSetupInfoFile & vbLf & vbLf & ResolveResString(resINVLINE) & vbLf & vbLf
    strMsg = strMsg & ResolveResString(resSECTNAME) & strSection & vbLf & strLine
    MsgError strMsg, vbCritical, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
End Function

'-----------------------------------------------------------
' SUB: ReadSetupRemoteLine
'
' Reads the requested 'RemoteX=' key from the specified
' section of the setup information file (SETUP.LST).
'
' IN: [strSection] - name of section to read from SETUP.LST,
'                    Ex: "Files"
'     [intFileNum] - remote number index to read
'
' OUT: [rInfo] - REGINFO Type variable that, after parsing,
'                holds the information for the line
'                described.
'
' Returns: True if the requested info was successfully read,
'          False otherwise
'
' Notes: Remote server lines in the setup information file
'        have the following format:
'
'        address,protocol,authentication-level
'
'        [address] - network address of the server, if known
'        [protocol] - network protocol name, if known
'        [authentication level] - authentication level (or 0 for default)
'-----------------------------------------------------------
'
Private Function ReadSetupRemoteLine(ByVal strSection As String, ByVal intFileNum As Integer, rInfo As REGINFO) As Integer
    Dim strLine As String
    Dim strMsg As String
    Dim intAnchor As Integer
    Dim intOffset As Integer
    Dim fErr As Boolean

    Const intMaxAuthentication = 6
    Dim strAuthentication As String

    '
    'Read the requested line, if unable to read it (Len(strLine) = 0) then exit
    '
    strLine = ReadIniFile(gstrSetupInfoFile, strSection, gstrINI_REMOTE & Format$(intFileNum))
    If Len(strLine) = 0 Then
        Exit Function
    End If

    '
    'Get the network address
    '
    intAnchor = 1
    If Mid$(strLine, intAnchor, 1) = gstrCOMMA Then
        rInfo.strNetworkAddress = vbNullString
    Else
        rInfo.strNetworkAddress = strExtractFilenameItem(strLine, intAnchor, fErr)
    End If
    If fErr Then GoTo RSRLError
    intAnchor = intAnchor + 1 'Skip past the comma

    '
    'Get the network protocol
    '
    If Mid$(strLine, intAnchor, 1) = gstrCOMMA Then
        rInfo.strNetworkProtocol = vbNullString
    Else
        rInfo.strNetworkProtocol = strExtractFilenameItem(strLine, intAnchor, fErr)
    End If
    If fErr Then GoTo RSRLError
    intAnchor = intAnchor + 1 'Skip past the comma

    '
    'Get the authentication level (must be a single digit
    '  in the range 0..6)
    '
    strAuthentication = Mid$(strLine, intAnchor, 1)
    If Len(strAuthentication) <> 1 Then GoTo RSRLError
    If (Asc(strAuthentication) < Asc("0")) Or (Asc(strAuthentication) > Asc("9")) Then GoTo RSRLError
    rInfo.intAuthentication = Val(strAuthentication)
    If rInfo.intAuthentication > intMaxAuthentication Then GoTo RSRLError
    '
    ' Is this dcom or remote automation?
    '
    intAnchor = InStr(intAnchor + 1, strLine, gstrCOMMA)
    If intAnchor > 0 Then
        rInfo.fDCOM = (UCase$(Trim$(Mid$(strLine, intAnchor + 1))) = gstrDCOM) 'gstrDCOM is uppercase.
    End If
    
    ReadSetupRemoteLine = True
    Exit Function

RSRLError:
    strMsg = gstrSetupInfoFile & vbLf & vbLf & ResolveResString(resINVLINE) & vbLf & vbLf
    strMsg = strMsg & ResolveResString(resSECTNAME) & strSection & vbLf & strLine
    MsgError strMsg, vbCritical, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
End Function

'-----------------------------------------------------------
' FUNCTION: RegCloseKey
'
' Closes an open registry key.
'
' Returns: True on success, else False.
'-----------------------------------------------------------
'
Private Function RegCloseKey(ByVal hKey As Long) As Boolean
    Dim lResult As Long

    lResult = OSRegCloseKey(hKey)
    RegCloseKey = (lResult = ERROR_SUCCESS)
End Function

'-----------------------------------------------------------
' FUNCTION: RegCreateKey
'
' Opens (creates if already exists) a key in the system registry.
'
' IN: [hkey] - The HKEY parent.
'     [lpszSubKeyPermanent] - The first part of the subkey of
'         'hkey' that will be created or opened.  The application
'         removal utility (32-bit only) will never delete any part
'         of this subkey.  May NOT be an empty string.
'     [lpszSubKeyRemovable] - The subkey of hkey\lpszSubKeyPermanent
'         that will be created or opened.  If the application is
'         removed (32-bit only), then this entire subtree will be
'         deleted, if it is empty at the time of application removal.
'         If this parameter is an empty string, then the entry
'         will not be logged.
'
' OUT: [phkResult] - The HKEY of the newly-created or -opened key.
'
' Returns: True if the key was created/opened OK, False otherwise
'   Upon success, phkResult is set to the handle of the key.
'-----------------------------------------------------------
'
Private Function RegCreateKey(ByVal hKey As Long, ByVal lpszSubKeyPermanent As String, ByVal lpszSubKeyRemovable As String, phkResult As Long) As Boolean
    Dim lResult As Long
    Dim strHkey As String
    Dim fLog As Boolean
    Dim strSubKeyFull As String

    If Len(lpszSubKeyPermanent) = 0 Then
        Exit Function
    End If
    
    If Left$(lpszSubKeyRemovable, 1) = "\" Then
        lpszSubKeyRemovable = Mid$(lpszSubKeyRemovable, 2)
    End If

    fLog = (Len(lpszSubKeyRemovable) > 0)
    
    If Len(lpszSubKeyRemovable) > 0 Then
        strSubKeyFull = lpszSubKeyPermanent & "\" & lpszSubKeyRemovable
    Else
        strSubKeyFull = lpszSubKeyPermanent
    End If
    strHkey = strGetHKEYString(hKey)

    If fLog Then
        NewAction _
          gstrKEY_REGKEY, _
          gstrQUOTE & strHkey & "\" & lpszSubKeyPermanent & gstrQUOTE _
            & ", " & gstrQUOTE & lpszSubKeyRemovable & gstrQUOTE
    End If

    lResult = OSRegCreateKey(hKey, strSubKeyFull, phkResult)
    If lResult = ERROR_SUCCESS Then
        RegCreateKey = True
        If fLog Then
            CommitAction
        End If
        AddHkeyToCache phkResult, strHkey & "\" & strSubKeyFull
    Else
        MsgError ResolveResString(resERR_REG), vbOKOnly Or vbExclamation, gstrTitle
        If fLog Then
            AbortAction
        End If
        If gfNoUserInput Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: RegDeleteKey
'
' Deletes an existing key in the system registry.
'
' Returns: True on success, False otherwise
'-----------------------------------------------------------
'
Private Function RegDeleteKey(ByVal hKey As Long, ByVal lpszSubKey As String) As Boolean
    Dim lResult As Long

    lResult = OSRegDeleteKey(hKey, lpszSubKey)
    RegDeleteKey = (lResult = ERROR_SUCCESS)
End Function

'-----------------------------------------------------------
' SUB: RegEdit
'
' Calls REGEDIT to add the information in the specifed file
' to the system registry.  If your .REG file requires path
' information based upon the destination directory given by
' the user, then you will need to write and call a .REG fixup
' routine before performing the registration below.
'
' WARNING: Use of this functionality under Win32 is not recommended,
' WARNING: because the application removal utility does not support
' WARNING: undoing changes that occur as a result of calling
' WARNING: REGEDIT on an arbitrary .REG file.
' WARNING: Instead, it is recommended that you use the RegCreateKey(),
' WARNING: RegOpenKey(), RegSetStringValue(), etc. functions in
' WARNING: this module instead.  These make entries to the
' WARNING: application removal logfile, thus enabling application
' WARNING: removal to undo such changes.
'
' IN: [strRegFile] - name of file containing reg. info
'-----------------------------------------------------------
'
Private Sub RegEdit(ByVal strRegFile As String)
    Const strREGEDIT$ = "REGEDIT /S "

    Dim fShellOK As Integer

    On Error Resume Next

    If FileExists(strRegFile) Then
        strRegFile = AddQuotesToFN(strRegFile)
        
        fShellOK = SyncShell(strREGEDIT & strRegFile, INFINITE, , True)
        frmSetup1.Refresh
    Else
        MsgError ResolveResString(resCANTFINDREGFILE, gstrPIPE1, strRegFile), vbExclamation Or vbOKOnly, gstrTitle
        ExitSetup frmSetup1, gintRET_FATAL
    End If

    Err.Clear
End Sub

'-----------------------------------------------------------
' FUNCTION: RegEnumKey
'
' Enumerates through the subkeys of an open registry
' key (returns the "i"th subkey of hkey, if it exists)
'
' Returns:
'   ERROR_SUCCESS on success.  strSubkeyName is set to the name of the subkey.
'   ERROR_NO_MORE_ITEMS if there are no more subkeys (32-bit only)
'   anything else - error
'-----------------------------------------------------------
'
Private Function RegEnumKey(ByVal hKey As Long, ByVal i As Long, strKeyName As String) As Long
    Dim strResult As String
    
    strResult = Space$(300)
    RegEnumKey = OSRegEnumKey(hKey, i, strResult, Len(strResult))
    strKeyName = StringFromBuffer(strResult)
End Function

'-----------------------------------------------------------
' SUB: RegisterDAO
'
' Special keys need to be added to the registry if
' DAO is installed.  This routine adds those keys.
'
' Note, these keys will not be uninstalled.
'-----------------------------------------------------------
'
Public Sub RegisterDAO()
    Const strDAOKey = "CLSID\{F7A9C6E0-EFF2-101A-8185-00DD01108C6B}"
    Const strDAOKeyVal = "OLE 2.0 Link"
    Const strDAOInprocHandlerKey = "CLSID\{F7A9C6E0-EFF2-101A-8185-00DD01108C6B}\InprocHandler"
    Const strDAOInprocHandlerKeyVal = "ole2.dll"
    Const strDAOProgIDKey = "CLSID\{F7A9C6E0-EFF2-101A-8185-00DD01108C6B}\ProgID"
    Const strDAOProgIDKeyVal = "Access.OLE2Link"
    
    Dim hKey As Long
    
    If Not RegCreateKey(HKEY_CLASSES_ROOT, strDAOKey, vbNullString, hKey) Then
        '
        ' RegCreateKey displays an error if something goes wrong.
        '
        GoTo REGDAOError
    End If
    '
    ' Set the key's value
    '
    If Not RegSetStringValue(hKey, vbNullString, strDAOKeyVal, False) Then
        '
        ' RegSetStringValue displays an error if something goes wrong.
        '
        GoTo REGDAOError
    End If
    '
    ' Close the key
    '
    RegCloseKey hKey
    '
    ' Repeat the same process for the other two keys.
    '
    If Not RegCreateKey(HKEY_CLASSES_ROOT, strDAOInprocHandlerKey, vbNullString, hKey) Then GoTo REGDAOError
    If Not RegSetStringValue(hKey, vbNullString, strDAOInprocHandlerKeyVal, False) Then GoTo REGDAOError
    RegCloseKey hKey
    
    If Not RegCreateKey(HKEY_CLASSES_ROOT, strDAOProgIDKey, vbNullString, hKey) Then GoTo REGDAOError
    If Not RegSetStringValue(hKey, vbNullString, strDAOProgIDKeyVal, False) Then GoTo REGDAOError
    RegCloseKey hKey

    Exit Sub
        
REGDAOError:
    '
    ' Error messages should have already been displayed.
    '
    ExitSetup frmSetup1, gintRET_FATAL
        
End Sub
'-----------------------------------------------------------
' SUB: RegisterFiles
'
' Loop through the list (array) of files to register that
' was created in the CopySection function and register
' each file therein as required
'
' Notes: msRegInfo() array created by CopySection function
'-----------------------------------------------------------
'
Public Sub RegisterFiles()
    'This should remain uppercase.
    Const strEXT_EXE$ = "EXE"

    Dim intIdx As Integer
    Dim intLastIdx As Integer
    Dim strFilename As String
    Dim strMsg As String

    Dim intDllSelfRegRet As Integer
    Dim intErrRes As Integer
    Const FAIL_OLE = 2
    Const FAIL_LOAD = 3
    Const FAIL_ENTRY = 4
    Const FAIL_REG = 5

    On Error Resume Next

    '
    'Get number of items to register, if none then we can get out of here
    '
    intLastIdx = UBound(msRegInfo)
    If Err.Number <> 0 Then
        GoTo RFCleanup
    End If

    For intIdx = 0 To intLastIdx
        strFilename = msRegInfo(intIdx).strFilename

        If UCase$(Extension(msRegInfo(intIdx).strRegister)) = UCase$(gsEXT_REG) Then
            If UCase$(BaseName(msRegInfo(intIdx).strFilename)) = UCase$(BaseName(msRegInfo(intIdx).strRegister)) Then
                Kill msRegInfo(intIdx).strRegister
            End If
            GoTo GoodToGo
        End If
        Select Case UCase$(msRegInfo(intIdx).strRegister)
            Case mstrDLLSELFREGISTER
                NewAction gstrKEY_DLLSELFREGISTER, gstrQUOTE & strFilename & gstrQUOTE
                
RetryDllSelfReg:
                Err.Clear
                intErrRes = 0
                intDllSelfRegRet = DLLSelfRegister(strFilename)
                If (Err.Number <> 49) And (Err.Number <> 0) Then
                    intErrRes = resCOMMON_CANTREGUNEXPECTED
                Else
                    Select Case intDllSelfRegRet
                        Case 0
                            'Good - everything's okay
                        Case FAIL_OLE
                            intErrRes = resCOMMON_CANTREGOLE
                        Case FAIL_LOAD
                            intErrRes = resCOMMON_CANTREGLOAD
                        Case FAIL_ENTRY
                            intErrRes = resCOMMON_CANTREGENTRY
                        Case FAIL_REG
                            intErrRes = resCOMMON_CANTREGREG
                        Case Else
                            intErrRes = resCOMMON_CANTREGUNEXPECTED
                    End Select
                End If
                If intErrRes Then
                    'There was some kind of error
                    
                    'Log the more technical version of the error message -
                    'this would be too confusing to show to the end user
                    LogError ResolveResString(intErrRes, gstrPIPE1, strFilename)
                    
                    'Now show a general error message to the user
AskWhatToDo:
                    strMsg = ResolveResString(resCOMMON_CANTREG, gstrPIPE1, strFilename)
                    
                    Select Case MsgError(strMsg, vbExclamation Or vbAbortRetryIgnore, gstrTitle)
                        Case vbAbort:
                            ExitSetup frmSetup1, gintRET_ABORT
                            GoTo AskWhatToDo
                        Case vbRetry:
                            GoTo RetryDllSelfReg
                        Case vbIgnore:
                            AbortAction
                    End Select
                Else
                    CommitAction
                End If
            Case mstrEXESELFREGISTER
                '
                'Only self register EXE files
                '
                If UCase$(Extension(strFilename)) = strEXT_EXE Then 'strEXT_EXE is uppercase.
                    NewAction gstrKEY_EXESELFREGISTER, gstrQUOTE & strFilename & gstrQUOTE
                    Err.Clear
                    ExeSelfRegister strFilename
                    If Err.Number <> 0 Then
                        AbortAction
                    Else
                        CommitAction
                    End If
                End If
            Case mstrREMOTEREGISTER
                NewAction gstrKEY_REMOTEREGISTER, gstrQUOTE & strFilename & gstrQUOTE
                Err.Clear
                RemoteRegister strFilename, msRegInfo(intIdx)
                If Err.Number <> 0 Then
                    AbortAction
                Else
                    CommitAction
                End If
            Case mstrTLBREGISTER
                NewAction gstrKEY_TLBREGISTER, gstrQUOTE & strFilename & gstrQUOTE
                '
                ' Call vb6stkit.dll's RegisterTLB export which calls
                ' LoadTypeLib and RegisterTypeLib.
                '
RetryTLBReg:
                If Not RegisterTLB(strFilename) Then
                    '
                    ' Registration of the TLB file failed.
                    '
                    LogError ResolveResString(resCOMMON_CANTREGTLB, gstrPIPE1, strFilename)
TLBAskWhatToDo:
                    strMsg = ResolveResString(resCOMMON_CANTREGTLB, gstrPIPE1, strFilename)
                    
                    Select Case MsgError(strMsg, vbExclamation Or vbAbortRetryIgnore, gstrTitle)
                        Case vbAbort:
                            ExitSetup frmSetup1, gintRET_ABORT
                            GoTo TLBAskWhatToDo
                        Case vbRetry:
                            GoTo RetryTLBReg
                        Case vbIgnore:
                            AbortAction
                    End Select
                Else
                    CommitAction
                End If
            Case mstrVBLREGISTER
                '
                ' RegisterVBLFile takes care of logging, etc.
                '

                RegisterVBLFile strFilename
            Case Else
                RegEdit msRegInfo(intIdx).strRegister
        End Select
GoodToGo:
    Next


    Erase msRegInfo

RFCleanup:
    Err.Clear
End Sub
'-----------------------------------------------------------
' SUB: RegisterLicenses
'
' Find all the setup.lst license entries and register
' them.
'-----------------------------------------------------------
'
Public Sub RegisterLicenses()
    Const strINI_LICENSES = "Licenses"
    Const strREG_LICENSES = "Licenses"
    Dim iLic As Integer
    Dim strLine As String
    Dim strLicKey As String
    Dim strLicVal As String
    Dim iCommaPos As Integer
    Dim strMsg As String
    Dim hkeyLicenses As Long
    Const strCopyright$ = "Licensing: Copying the keys may be a violation of established copyrights."

    'Make sure the Licenses key exists
    If Not RegCreateKey(HKEY_CLASSES_ROOT, strREG_LICENSES, vbNullString, hkeyLicenses) Then
        'RegCreateKey will have already displayed an error message
        '  if something's wrong
        ExitSetup frmSetup1, gintRET_FATAL
    End If
    If Not RegSetStringValue(hkeyLicenses, vbNullString, strCopyright, False) Then
        RegCloseKey hkeyLicenses
        ExitSetup frmSetup1, gintRET_FATAL
    End If
    RegCloseKey hkeyLicenses
    
    iLic = 1
    Do
        strLine = ReadIniFile(gstrSetupInfoFile, strINI_LICENSES, gstrINI_LICENSE & iLic)
        If Len(strLine) = 0 Then
            '
            ' We've got all the licenses.
            '
            Exit Sub
        End If
        strLine = strUnQuoteString(strLine)
        '
        ' We have a license, parse it and register it.
        '
        iCommaPos = InStr(strLine, gstrCOMMA)
        If iCommaPos = 0 Then
            '
            ' Looks like the setup.lst file is corrupt.  There should
            ' always be a comma in the license information that separates
            ' the license key from the license value.
            '
            GoTo RLError
        End If
        strLicKey = Left$(strLine, iCommaPos - 1)
        strLicVal = Mid$(strLine, iCommaPos + 1)
        
        RegisterLicense strLicKey, strLicVal
        
        iLic = iLic + 1
    Loop While Len(strLine) > 0
    Exit Sub
        
RLError:
    strMsg = gstrSetupInfoFile & vbLf & vbLf & ResolveResString(resINVLINE) & vbLf & vbLf
    strMsg = strMsg & ResolveResString(resSECTNAME) & strINI_LICENSES & vbLf & strLine
    MsgError strMsg, vbCritical, gstrTitle
    ExitSetup frmSetup1, gintRET_FATAL
End Sub
'-----------------------------------------------------------
' SUB: RegisterLicense
'
' Register license information given the key and default
' value.  License information always goes into
' HKEY_CLASSES_ROOT\Licenses.
'-----------------------------------------------------------
'
Private Sub RegisterLicense(strLicKey As String, strLicVal As String)
    Const strREG_LICENSES = "Licenses"
    Dim hKey As Long
    '
    ' Create the key
    '
    If Not RegCreateKey(HKEY_CLASSES_ROOT, strREG_LICENSES, strLicKey, hKey) Then
        '
        ' RegCreateKey displays an error if something goes wrong.
        '
        GoTo REGError
    End If
    '
    ' Set the key's value
    '
    If Not RegSetStringValue(hKey, vbNullString, strLicVal, True) Then
        '
        ' RegSetStringValue displays an error if something goes wrong.
        '
        GoTo REGError
    End If
    '
    ' Close the key
    '
    RegCloseKey hKey

    Exit Sub
        
REGError:
    '
    ' Error messages should have already been displayed.
    '
    ExitSetup frmSetup1, gintRET_FATAL
End Sub
'-----------------------------------------------------------
' SUB: RegisterVBLFile
'
' Register license information in a VB License (vbl) file.
' Basically, parse out the license info and then call
' RegisterLicense.
'
' If strVBLFile is not a valid VBL file, nothing is
' registered.
'-----------------------------------------------------------
'
Private Sub RegisterVBLFile(strVBLFile As String)
    Dim strLicKey As String
    Dim strLicVal As String
    
    GetLicInfoFromVBL strVBLFile, strLicKey, strLicVal
    If Len(strLicKey) > 0 Then
        RegisterLicense strLicKey, strLicVal
    End If
End Sub

'----------------------------------------------------------
' SUB: RegisterAppRemovalEXE
'
' Registers the application removal program (Windows 95 only)
' or else places an icon for it in the application directory.
'
' Returns True on success, False otherwise.
'----------------------------------------------------------
'
Private Function RegisterAppRemovalEXE(ByVal strAppRemovalEXE As String, ByVal strAppRemovalLog As String, ByVal strGroupName As String) As Boolean
    On Error GoTo Err
    
    Const strREGSTR_VAL_AppRemoval_APPNAMELINE = "ApplicationName"
    Const strREGSTR_VAL_AppRemoval_DISPLAYNAME = "DisplayName"
    Const strREGSTR_VAL_AppRemoval_COMMANDLINE = "UninstallString"
    Const strREGSTR_VAL_AppRemoval_APPTOUNINSTALL = "AppToUninstall"

    Dim strREGSTR_PATH_UNINSTALL As String
    Dim strAppRemovalCmdLine As String
    Dim strMsg As String

    Dim iAppend As Integer
    Dim fOk As Boolean
    Dim hkeyAppRemoval As Long
    Dim hkeyOurs As Long
    Dim i As Integer

    Dim strAppRemovalKey As String
    Dim strAppRemovalKeyBase As String
    Dim hkeyTest As Long

    Dim strDisplayName As String
    Dim strDisplayNameBase As String

    strREGSTR_PATH_UNINSTALL = RegPathWinCurrentVersion() & "\Uninstall"
    
    'The command-line for the application removal executable is simply the path
    'for the installation logfile
    strAppRemovalCmdLine = GetAppRemovalCmdLine(strAppRemovalEXE, strAppRemovalLog, vbNullString, False, APPREMERR_NONE)
    '
    ' Make sure that the Removal command line (including path, filename, commandline args, etc.
    ' is not longer than the max allowed, which is _MAX_PATH.
    '
    If Not fCheckFNLength(strAppRemovalCmdLine) Then
        strMsg = ResolveResString(resCANTCREATEICONPATHTOOLONG) & vbLf & vbLf & ResolveResString(resCHOOSENEWDEST) & vbLf & vbLf & strAppRemovalCmdLine
        MsgError strMsg, vbOKOnly, gstrSETMSG
        ExitSetup frmCopy, gintRET_FATAL
    End If
    '
    ' Create registry entries to tell Windows where the app removal executable is,
    ' how it should be displayed to the user, and what the command-line arguments are
    '
    'Go ahead and create a key to the main Uninstall branch
    If Not RegCreateKey(HKEY_LOCAL_MACHINE, strREGSTR_PATH_UNINSTALL, vbNullString, hkeyAppRemoval) Then
        GoTo Err
    End If
    
    'We need a unique key.  This key is never shown to the end user.  We will use a key of
    'the form 'ST5UNST #xxx'
    strAppRemovalKeyBase = mstrFILE_APPREMOVALLOGBASE$ & " #"
    iAppend = 1
    
    Do
        strAppRemovalKey = strAppRemovalKeyBase & Format$(iAppend)
        If RegOpenKey(hkeyAppRemoval, strAppRemovalKey, hkeyTest) Then
            'This key already exists.  But we need a unique key.
            RegCloseKey hkeyTest
        Else
            'We've found a key that doesn't already exist.  Use it.
            Exit Do
        End If
        
        iAppend = iAppend + 1
    Loop
    
    '
    ' We also need a unique displayname.  This name is
    ' the only means the user has to identify the application
    ' to remove
    '
    strDisplayName = gstrAppName 'First try... Application name
    If Not IsDisplayNameUnique(hkeyAppRemoval, strDisplayName) Then
        'Second try... Add path
        strDisplayName = strDisplayName & " (" & gstrDestDir & ")"
        If Not IsDisplayNameUnique(hkeyAppRemoval, strDisplayName) Then
            'Subsequent tries... Append a unique integer
            strDisplayNameBase = strDisplayName
            iAppend = 3
            Do
                strDisplayName = strDisplayNameBase & " #" & Format$(iAppend)
                If IsDisplayNameUnique(hkeyAppRemoval, strDisplayName) Then
                    Exit Do
                Else
                    iAppend = iAppend + 1
                End If
            Loop
        End If
    End If
    
    'Go ahead and fill in entries for the app removal executable
    If Not RegCreateKey(hkeyAppRemoval, strAppRemovalKey, vbNullString, hkeyOurs) Then
        GoTo Err
    End If
    If Not RegSetStringValue(hkeyOurs, strREGSTR_VAL_AppRemoval_APPNAMELINE, gstrAppExe, False) Then
        GoTo Err
    End If
    If Not RegSetStringValue(hkeyOurs, strREGSTR_VAL_AppRemoval_DISPLAYNAME, strDisplayName, False) Then
        GoTo Err
    End If
    If Not RegSetStringValue(hkeyOurs, strREGSTR_VAL_AppRemoval_COMMANDLINE, strAppRemovalCmdLine, False) Then
        GoTo Err
    End If
    If Len(gstrAppToUninstall) = 0 Then gstrAppToUninstall = gstrAppExe
    If Not RegSetStringValue(hkeyOurs, strREGSTR_VAL_AppRemoval_APPTOUNINSTALL, gstrAppToUninstall, False) Then
        GoTo Err
    End If
    
    RegCloseKey hkeyAppRemoval
    RegCloseKey hkeyOurs
    
    RegisterAppRemovalEXE = True
    Exit Function
    
Err:
    If hkeyOurs Then
        RegCloseKey hkeyOurs
        RegDeleteKey hkeyAppRemoval, strAppRemovalKey
    End If
    If hkeyAppRemoval Then
        RegCloseKey hkeyAppRemoval
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: RegOpenKey
'
' Opens an existing key in the system registry.
'
' Returns: True if the key was opened OK, False otherwise
'   Upon success, phkResult is set to the handle of the key.
'-----------------------------------------------------------
'
Private Function RegOpenKey(ByVal hKey As Long, ByVal lpszSubKey As String, phkResult As Long) As Boolean
    Dim lResult As Long
    Dim strHkey As String

    strHkey = strGetHKEYString(hKey)

    lResult = OSRegOpenKey(hKey, lpszSubKey, phkResult)
    If lResult = ERROR_SUCCESS Then
        RegOpenKey = True
        AddHkeyToCache phkResult, strHkey & "\" & lpszSubKey
    End If
End Function

'----------------------------------------------------------
' FUNCTION: RegPathWinPrograms
'
' Returns the name of the registry key
' "\HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders"
'----------------------------------------------------------
'
Private Function RegPathWinPrograms() As String
    RegPathWinPrograms = RegPathWinCurrentVersion() & "\Explorer\Shell Folders"
End Function
 
'----------------------------------------------------------
' FUNCTION: RegPathWinCurrentVersion
'
' Returns the name of the registry key
' "\HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion"
'----------------------------------------------------------
'
Private Function RegPathWinCurrentVersion() As String
    RegPathWinCurrentVersion = "SOFTWARE\Microsoft\Windows\CurrentVersion"
End Function

'-----------------------------------------------------------
' FUNCTION: RegQueryStringValue
'
' Retrieves the string data for a named
' (strValueName = name) or unnamed (Len(strValueName) = 0)
' value within a registry key.  If the named value
' exists, but its data is not a string, this function
' fails.
'
' Returns: True on success, else False.
'   On success, strData is set to the string data value
'-----------------------------------------------------------
'
Private Function RegQueryStringValue(ByVal hKey As Long, ByVal strValueName As String, strData As String) As Boolean
    Dim lResult As Long
    Dim lValueType As Long
    Dim strBuf As String
    Dim lDataBufSize As Long
    
    ' Get length/data type
    lResult = OSRegQueryValueEx(hKey, strValueName, 0&, lValueType, ByVal 0&, lDataBufSize)
    If lResult = ERROR_SUCCESS Then
        If lValueType = REG_SZ Then
            strBuf = Space$(lDataBufSize)
            lResult = OSRegQueryValueEx(hKey, strValueName, 0&, 0&, ByVal strBuf, lDataBufSize)
            If lResult = ERROR_SUCCESS Then
                RegQueryStringValue = True
                strData = StringFromBuffer(strBuf)
            End If
        End If
    End If
End Function

'----------------------------------------------------------
' FUNCTION: RegQueryRefCount
'
' Retrieves the data inteded as a reference count for a
' particular value within a registry key.  Although
' REG_DWORD is the preferred way of storing reference
' counts, it is possible that some installation programs
' may incorrect use a string or binary value instead.
' This routine accepts the data whether it is a string,
' a binary value or a DWORD (Long).
'
' NOTE: There is no 16-bit version of this function.
'
' Returns: True on success, else False.
'   On success, lrefcount is set to the numeric data value
'-----------------------------------------------------------
'
Private Function RegQueryRefCount(ByVal hKey As Long, ByVal strValueName As String, lRefCount As Long) As Boolean
    Dim lResult As Long
    Dim lValueType As Long
    Dim lBuf As Long
    Dim lDataBufSize As Long
    Dim strRefCount As String

    ' Get length/data type
    lDataBufSize = 4

    lResult = OSRegQueryValueEx(hKey, strValueName, 0&, lValueType, lBuf, lDataBufSize)
    If lResult = ERROR_SUCCESS Then
        Select Case lValueType
            Case REG_DWORD
                lRefCount = lBuf
                RegQueryRefCount = True
            Case REG_BINARY
                If lDataBufSize = 4 Then
                    lRefCount = lBuf
                    RegQueryRefCount = True
                End If
            Case REG_SZ
                If RegQueryStringValue(hKey, strValueName, strRefCount) Then
                    lRefCount = Val(strRefCount)
                    RegQueryRefCount = True
                End If
        End Select
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: RegSetNumericValue
'
' Associates a named (strValueName = name) or unnamed (Len(strValueName) = 0)
'   value with a registry key.
'
' If fLog is missing or is True, then this action is logged in the logfile,
' and the value will be deleted by the application removal utility if the
' user choose to remove the installed application.
'
' NOTE: There is no 16-bit version of this function.
'
' Returns: True on success, else False.
'-----------------------------------------------------------
'
Private Function RegSetNumericValue(ByVal hKey As Long, ByVal strValueName As String, ByVal lData As Long, Optional ByVal fLog As Boolean = True) As Boolean
    Dim lResult As Long
    Dim strHkey As String

    strHkey = strGetHKEYString(hKey)
    
    If fLog Then
        NewAction _
          gstrKEY_REGVALUE, _
          gstrQUOTE & strHkey & gstrQUOTE _
            & ", " & gstrQUOTE & strValueName & gstrQUOTE
    End If

    lResult = OSRegSetValueNumEx(hKey, strValueName, 0, REG_DWORD, lData, 4)
    If lResult = ERROR_SUCCESS Then
        RegSetNumericValue = True
        If fLog Then
            CommitAction
        End If
    Else
        MsgError ResolveResString(resERR_REG), vbOKOnly Or vbExclamation, gstrTitle
        If fLog Then
            AbortAction
        End If
        If gfNoUserInput Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: RegSetStringValue
'
' Associates a named (strValueName = name) or unnamed (Len(strValueName) = 0)
'   value with a registry key.
'
' If fLog is missing or is True, then this action is logged in the
' logfile, and the value will be deleted by the application removal
' utility if the user choose to remove the installed application.
'
' Returns: True on success, else False.
'-----------------------------------------------------------
'
Private Function RegSetStringValue(ByVal hKey As Long, ByVal strValueName As String, ByVal strData As String, Optional ByVal fLog As Boolean = True) As Boolean
    Dim lResult As Long
    Dim strHkey As String
    
    If hKey = 0 Then
        Exit Function
    End If
    
    strHkey = strGetHKEYString(hKey)

    If fLog Then
        NewAction _
          gstrKEY_REGVALUE, _
          gstrQUOTE & strHkey & gstrQUOTE _
            & ", " & gstrQUOTE & strValueName & gstrQUOTE
    End If

    lResult = OSRegSetValueEx(hKey, strValueName, 0&, REG_SZ, ByVal strData, LenB(StrConv(strData, vbFromUnicode)) + 1)
    'lResult = OSRegSetValueEx(hKey, strValueName, 0&, REG_SZ, ByVal strData, Len(strData) + 1)
    
    If lResult = ERROR_SUCCESS Then
        RegSetStringValue = True
        If fLog Then
            CommitAction
        End If
    Else
        MsgError ResolveResString(resERR_REG), vbOKOnly Or vbExclamation, gstrTitle
        If fLog Then
            AbortAction
        End If
        If gfNoUserInput Then
            ExitSetup frmSetup1, gintRET_FATAL
        End If
    End If
End Function

'-----------------------------------------------------------
' SUB: RemoteRegister
'
' Synchronously run the client registration utility on the
' given remote server registration file in order to set it
' up properly in the registry.
'
' IN: [strFileName] - .EXE file to register
'-----------------------------------------------------------
'
Private Sub RemoteRegister(ByVal strFilename As String, rInfo As REGINFO)
    Const strClientRegistrationUtility$ = "CLIREG32.EXE"
    Const strAddressSwitch = " /s "
    Const strProtocolSwitch = " /p "
    Const strSilentSwitch = " /q "
    Const strNoLogoSwitch = " /nologo "
    Const strAuthenticationSwitch = " /a "
    Const strTypelibSwitch = " /t "
    Const strDCOMSwitch = " /d "
    Const strEXT_REMOTE$ = "VBR" 'This should remain uppercase
    Const strEXT_REMOTETLB$ = "TLB"

    Dim strAddress As String
    Dim strProtocol As String
    Dim intAuthentication As Integer
    Dim strCmdLine As String
    Dim fShell As Integer
    Dim strMatchingTLB As String
    Dim fDCOM As Boolean

    'Find the name of the matching typelib file.  This should have already
    'been installed to the same directory as the .VBR file.
    strMatchingTLB = strFilename
    If UCase$(Right$(strMatchingTLB, Len(strEXT_REMOTE))) = strEXT_REMOTE Then 'strEXT_REMOTE is uppercase.
        strMatchingTLB = Left$(strMatchingTLB, Len(strMatchingTLB) - Len(strEXT_REMOTE))
    End If
    strMatchingTLB = strMatchingTLB & strEXT_REMOTETLB

    strAddress = rInfo.strNetworkAddress
    strProtocol = rInfo.strNetworkProtocol
    intAuthentication = rInfo.intAuthentication
    fDCOM = rInfo.fDCOM
    frmRemoteServerDetails.GetServerDetails strFilename, strAddress, strProtocol, fDCOM
    frmMessage.Refresh
    strCmdLine = strClientRegistrationUtility & strAddressSwitch & _
                 gstrQUOTE & strAddress & gstrQUOTE & " "
    If Not fDCOM Then
        strCmdLine = strCmdLine & strProtocolSwitch & strProtocol & _
                     strAuthenticationSwitch & Format$(intAuthentication) & " "
    End If
    strCmdLine = strCmdLine & strNoLogoSwitch & _
                 strTypelibSwitch & gstrQUOTE & strMatchingTLB & gstrQUOTE & " "
    If fDCOM Then
        strCmdLine = strCmdLine & strDCOMSwitch
    End If
    If gfNoUserInput Then
        strCmdLine = strCmdLine & strSilentSwitch
    End If
    strCmdLine = strCmdLine & gstrQUOTE & strFilename & gstrQUOTE

    '
    'Synchronously shell out and run the utility with the correct switches
    '
    fShell = SyncShell(strCmdLine, INFINITE, , False)

    If Not fShell Then
        MsgError ResolveResString(resCANTRUNPROGRAM, gstrPIPE1, strClientRegistrationUtility), vbOKOnly Or vbExclamation, gstrTitle, gintRET_FATAL
        ExitSetup frmSetup1, gintRET_FATAL
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION: ResolveDestDir
'
' Given a destination directory string, equate any macro
' portions of the string to their runtime determined
' actual locations and return a string reflecting the
' actual path.
'
' IN: [strDestDir] - string containing directory macro info
'                    and/or actual dir path info
'
'     [fAssumeDir] - boolean that if true, causes this routine
'                    to assume that strDestDir contains a dir
'                    path.  If a directory isn't given it will
'                    make it the application path.  If false,
'                    this routine will return strDestDir as
'                    is after performing expansion.  Set this
'                    to False when you are not sure it is a
'                    directory but you want to expand macros
'                    if it contains any.  E.g., If this is a
'                    command line parameter, you can't be
'                    certain if it refers to a path.  In this
'                    case, set fAssumeDir = False.  Default
'                    is True.
'
' Return: A string containing the resolved dir name
'-----------------------------------------------------------
'
Public Function ResolveDestDir(ByVal strDestDir As String, Optional fAssumeDir As Boolean = True, Optional fRecognizedBySetupExe As Boolean = False) As String
    Const strMACROSTART$ = "$("
    Const strMACROEND$ = ")"

    Dim intPos As Integer
    Dim strResolved As String
    Dim hKey As Long
    Dim strPathsKey As String
    Dim fQuoted As Boolean
    
    Const strProgramFilesKey = "ProgramFilesDir"

    Dim strCommonFiles As String

    strPathsKey = RegPathWinCurrentVersion()
    strDestDir = Trim$(strDestDir)
    '
    ' If strDestDir is quoted when passed to this routine, it
    ' should be quoted when it's returned.  The quotes need
    ' to be temporarily removed, though, for processing.
    '
    If Left$(strDestDir, 1) = gstrQUOTE Then
        fQuoted = True
        strDestDir = strUnQuoteString(strDestDir)
    End If
    '
    ' We take the first part of destdir, and if its $( then we need to get the portion
    ' of destdir up to and including the last paren.  We then test against this for
    ' macro expansion.  If no ) is found after finding $(, then must assume that it's
    ' just a normal file name and do no processing.  Only enter the case statement
    ' if strDestDir starts with $(.
    '
    If Left$(strDestDir, 2) = strMACROSTART Then
        intPos = InStr(strDestDir, strMACROEND)

        Select Case UCase$(Left$(strDestDir, intPos))
            Case UCase$(gstrAPPDEST)
                If Len(gstrDestDir) > 0 Then
                    strResolved = gstrDestDir
                Else
                    strResolved = "?"
                End If
            Case UCase$(gstrWINDEST)
                strResolved = gstrWinDir
                fRecognizedBySetupExe = True
            Case UCase$(gstrFONTDEST)
                strResolved = gstrFontDir
            Case UCase$(gstrWINSYSDEST), UCase$(gstrWINSYSDESTSYSFILE)
                strResolved = gstrWinSysDir
                fRecognizedBySetupExe = True
            Case UCase$(gstrPROGRAMFILES)
                If RegOpenKey(HKEY_LOCAL_MACHINE, strPathsKey, hKey) Then
                    RegQueryStringValue hKey, strProgramFilesKey, strResolved
                    RegCloseKey hKey
                End If
    
                If Len(strResolved) = 0 Then
                    'If not otherwise set, let strResolved be the root of the first fixed disk
                    strResolved = strRootDrive()
                End If
            Case UCase$(gstrCOMMONFILES)
                'First determine the correct path of Program Files\Common Files, if under Win95
                strResolved = strGetCommonFilesPath()
                If Len(strResolved) = 0 Then
                    'If not otherwise set, let strResolved be the Windows directory
                    strResolved = gstrWinDir
                End If
            Case UCase$(gstrCOMMONFILESSYS)
                'First determine the correct path of Program Files\Common Files, if under Win95
                strCommonFiles = strGetCommonFilesPath()
                If Len(strCommonFiles) > 0 Then
                    'Okay, now just add \System, and we're done
                    strResolved = strCommonFiles & "System\"
                Else
                    'If Common Files isn't in the registry, then map the
                    'entire macro to the Windows\{system,system32} directory
                    strResolved = gstrWinSysDir
                End If
            Case UCase$(gstrDAODEST)
                strResolved = strGetDAOPath()
            Case Else
                intPos = 0
        End Select
    End If
    
    If intPos <> 0 Then
        AddDirSep strResolved
    End If

    If fAssumeDir Then
        If intPos = 0 Then
            '
            'if no drive spec, and doesn't begin with any root path indicator ("\"),
            'then we assume that this destination is relative to the app dest dir
            '
            If Mid$(strDestDir, 2, 1) <> gstrCOLON Then
                If Left$(strDestDir, 1) <> gstrSEP_DIR Then
                    strResolved = gstrDestDir
                End If
            End If
        Else
            If Mid$(strDestDir, intPos + 1, 1) = gstrSEP_DIR Then
                intPos = intPos + 1
            End If
        End If
    End If

    If fQuoted Then
        ResolveDestDir = strQuoteString(strResolved & Mid$(strDestDir, intPos + 1), True, False)
    Else
        ResolveDestDir = strResolved & Mid$(strDestDir, intPos + 1)
    End If
End Function
'-----------------------------------------------------------
' FUNCTION: ResolveDestDirs
'
' Given a space delimited string, this routine finds all
' Destination directory macros and expands them by making
' repeated calls to ResolveDestDir.  See ResolveDestDir.
'
' Note that the macro must immediately follow a space (or
' a space followed by a quote) delimiter or else it will
' be ignored.
'
' Note that this routine does not assume that each item
' in the delimited string is actually a directory path.
' Therefore, the last parameter in the call to ResolveDestDir,
' below, is false.
'
' IN: [str] - string containing directory macro(s) info
'             and/or actual dir path info
'
' Return: str with destdir macros expanded.
'-----------------------------------------------------------
'
Private Function ResolveDestDirs(str As String)
    Dim intAnchor As Integer
    Dim intOffset As Integer
    Dim strField As String
    Dim strExpField As String
    Dim strExpanded As String
    
    If Len(Trim$(strUnQuoteString(str))) = 0 Then
        ResolveDestDirs = str
        Exit Function
    End If

    intAnchor = 1

    Do
        intOffset = intGetNextFldOffset(intAnchor, str, " ")
        If intOffset = 0 Then intOffset = Len(str) + 1
        strField = Mid$(str, intAnchor, intOffset - intAnchor)
        strExpField = ResolveDestDir(strField, False)
        strExpanded = strExpanded & strExpField & " "
        intAnchor = intOffset + 1
    Loop While intAnchor < Len(str)
    
    ResolveDestDirs = Trim$(strExpanded)
End Function
'-----------------------------------------------------------
' FUNCTION: ResolveDir
'
' Given a pathname, resolve it to its smallest form.  If
' the pathname is invalid, then optionally warn the user.
'
' IN: [strPathName] - pathname to resolve
'     [fMustExist] - enforce that the path actually exists
'     [fWarn] - If True, warn user upon invalid path
'
' Return: A string containing the resolved dir name
'-----------------------------------------------------------
'
Public Function ResolveDir(ByVal strPathName As String, fMustExist As Integer, fWarn As Integer) As String
    Dim strMsg As String
    Dim fInValid As Integer
    Dim strUnResolvedPath As String
    Dim strResolvedPath As String
    Dim nIgnore As Long
    Dim cbResolved As Long

    Dim strDummy As String

    On Error Resume Next
    '
    'If the pathname is in actuality a file name, then it's invalid
    '
    If FileExists(strPathName) Then
        fInValid = True
        GoTo RDContinue
    End If

    strUnResolvedPath = strPathName

    If InStr(3, strUnResolvedPath, gstrSEP_DIR) > 0 Then

        strResolvedPath = Space$(gintMAX_PATH_LEN)
        cbResolved = GetFullPathName(strUnResolvedPath, gintMAX_PATH_LEN, strResolvedPath, nIgnore)
        If cbResolved = 0 Then
            '
            ' The path couldn't be resolved.  If we can actually
            ' switch to the directory we want, continue anyway.
            '
            AddDirSep strUnResolvedPath
            If DirExists(strUnResolvedPath) Then
                strResolvedPath = strUnResolvedPath
            Else
                fInValid = True
            End If
        Else
            '
            ' GetFullPathName returned us a NULL terminated string in
            ' strResolvedPath.  Remove the NULL.
            '
            strResolvedPath = StringFromBuffer(strResolvedPath)
            If Not CheckDrive(strResolvedPath, gstrTitle) Then
                fInValid = True
            Else
                AddDirSep strResolvedPath
                If fMustExist Then
                    Err.Clear
                    
                    strDummy = Dir$(strResolvedPath & "*.*")
                    
                    If Err.Number <> 0 Then
                        strMsg = ResolveResString(resNOTEXIST) & vbLf & vbLf
                        fInValid = True
                    End If
                End If
            End If
        End If
    Else
        fInValid = True
    End If

RDContinue:
    If fInValid Then
        If fWarn Then
            strMsg = strMsg & ResolveResString(resDIRSPECIFIED) & vbLf & vbLf & strPathName & vbLf & vbLf
            strMsg = strMsg & ResolveResString(resDIRINVALID)
            MsgError strMsg, vbOKOnly Or vbExclamation, ResolveResString(resDIRINVNAME)
            If gfNoUserInput Then
                ExitSetup frmSetup1, gintRET_FATAL
            End If
        End If

        ResolveDir = vbNullString
    Else
        ResolveDir = strResolvedPath
    End If

    Err.Clear
End Function

'-----------------------------------------------------------
' SUB: ShowPathDialog
'
' Display form to allow user to get either a source or
' destination path
'-----------------------------------------------------------
'
Public Sub ShowPathDialog()
    '
    'frmPath.Form_Load() reads frmSetup1.Tag to determine whether
    'this is a request for the source or destination path
    '
    frmPath.Show vbModal

    If gintRetVal = gintRET_CONT Then
        gstrDestDir = frmSetup1.Tag
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION: strExtractFilenameArg
'
' Extracts a quoted or unquoted filename from a string
'   containing command-line arguments
'
' IN: [str] - string containing a filename.  This filename
'             begins at the first character, and continues
'             to the end of the string or to the first space
'             or switch character, or, if the string begins
'             with a double quote, continues until the next
'             double quote
' OUT: Returns the filename, without quotes
'      str is set to be the remainder of the string after
'      the filename and quote (if any)
'-----------------------------------------------------------
'
Private Function strExtractFilenameArg(str As String, fErr As Boolean)
    Dim strFilename As String
    Dim iEndFilenamePos As Integer
    
    Dim iSpacePos As Integer
    Dim iSwitch1 As Integer
    Dim iSwitch2 As Integer
    Dim iQuote As Integer

    str = Trim$(str)
    
    If Left$(str, 1) = gstrQUOTE Then
        ' Filenames is surrounded by quotes
        iEndFilenamePos = InStr(2, str, gstrQUOTE) ' Find matching quote
        If iEndFilenamePos > 0 Then
            strFilename = Mid$(str, 2, iEndFilenamePos - 2)
            str = Right$(str, Len(str) - iEndFilenamePos)
        Else
            fErr = True
            Exit Function
        End If
    Else
        ' Filename continues until next switch or space or quote
        iSpacePos = InStr(str, " ")
        iSwitch2 = InStr(str, gstrSwitchPrefix2)
        iQuote = InStr(str, gstrQUOTE)
        
        If iSpacePos = 0 Then iSpacePos = Len(str) + 1
        If iSwitch1 = 0 Then iSwitch1 = Len(str) + 1
        If iSwitch2 = 0 Then iSwitch2 = Len(str) + 1
        If iQuote = 0 Then iQuote = Len(str) + 1
        
        iEndFilenamePos = iSpacePos
        If iSwitch2 < iEndFilenamePos Then iEndFilenamePos = iSwitch2
        If iQuote < iEndFilenamePos Then iEndFilenamePos = iQuote
        
        strFilename = Left$(str, iEndFilenamePos - 1)
        If iEndFilenamePos > Len(str) Then
            str = vbNullString
        Else
            str = Right$(str, Len(str) - iEndFilenamePos + 1)
        End If
    End If
    
    strFilename = Trim$(strFilename)
    If Len(strFilename) = 0 Then
        fErr = True
        Exit Function
    End If
    
    fErr = False
    strExtractFilenameArg = strFilename
    str = Trim$(str)
End Function



'-----------------------------------------------------------
' SUB: UpdateStatus
'
' "Fill" (by percentage) inside the PictureBox and also
' display the percentage filled
'
' IN: [pic] - PictureBox used to bound "fill" region
'     [sngPercent] - Percentage of the shape to fill
'     [fBorderCase] - Indicates whether the percentage
'        specified is a "border case", i.e. exactly 0%
'        or exactly 100%.  Unless fBorderCase is True,
'        the values 0% and 100% will be assumed to be
'        "close" to these values, and 1% and 99% will
'        be used instead.
'
' Notes: Set AutoRedraw property of the PictureBox to True
'        so that the status bar and percentage can be auto-
'        matically repainted if necessary
'-----------------------------------------------------------
'
Public Sub UpdateStatus(pic As PictureBox, ByVal sngPercent As Single, Optional ByVal fBorderCase As Boolean = False)
    Dim strPercent As String
    Dim intX As Integer
    Dim intY As Integer
    Dim intWidth As Integer
    Dim intHeight As Integer

    Dim intPercent As Integer

    'For this to work well, we need a white background and any color foreground (blue)
    Const colBackground = &HFFFFFF ' white
    Const colForeground = &H800000 ' dark blue

    pic.ForeColor = colForeground
    pic.BackColor = colBackground
    
    '
    'Format$ percentage and get attributes of text
    '
    intPercent = Int(100 * sngPercent + 0.5)
    
    'Never allow the percentage to be 0 or 100 unless it is exactly that value.  This
    'prevents, for instance, the status bar from reaching 100% until we are entirely done.
    If intPercent = 0 Then
        If Not fBorderCase Then
            intPercent = 1
        End If
    ElseIf intPercent = 100 Then
        If Not fBorderCase Then
            intPercent = 99
        End If
    End If
    
    strPercent = Format$(intPercent) & "%"
    intWidth = pic.TextWidth(strPercent)
    intHeight = pic.TextHeight(strPercent)

    '
    'Now set intX and intY to the starting location for printing the percentage
    '
    intX = pic.Width / 2 - intWidth / 2
    intY = pic.Height / 2 - intHeight / 2

    '
    'Need to draw a filled box with the pics background color to wipe out previous
    'percentage display (if any)
    '
    pic.DrawMode = 13 ' Copy Pen
    pic.Line (intX, intY)-Step(intWidth, intHeight), pic.BackColor, BF

    '
    'Back to the center print position and print the text
    '
    pic.CurrentX = intX
    pic.CurrentY = intY
    pic.Print strPercent

    '
    'Now fill in the box with the ribbon color to the desired percentage
    'If percentage is 0, fill the whole box with the background color to clear it
    'Use the "Not XOR" pen so that we change the color of the text to white
    'wherever we touch it, and change the color of the background to blue
    'wherever we touch it.
    '
    pic.DrawMode = 10 ' Not XOR Pen
    If sngPercent > 0 Then
        pic.Line (0, 0)-(pic.Width * sngPercent, pic.Height), pic.ForeColor, BF
    Else
        pic.Line (0, 0)-(pic.Width, pic.Height), pic.BackColor, BF
    End If

    pic.Refresh
End Sub

'-----------------------------------------------------------
' FUNCTION: WriteAccess
'
' Determines whether there is write access to the specified
' directory.
'
' IN: [strDirName] - directory to check for write access
'
' Returns: True if write access, False otherwise
'-----------------------------------------------------------
'
Public Function WriteAccess(ByVal strDirName As String) As Boolean
    Const strFILE$ = "VB6STTMP.CCT"

    Dim intFileNum As Integer

    On Error Resume Next

    AddDirSep strDirName

    intFileNum = FreeFile
    Open strDirName & strFILE For Output As intFileNum

    WriteAccess = (Err.Number = 0)

    Close intFileNum

    Kill strDirName & strFILE

    Err.Clear
End Function
'-----------------------------------------------------------
' FUNCTION: WriteMIF
'
' If this is a SMS install, this routine writes the
' failed MIF status file if something goes wrong or
' a successful MIF if everything installs correctly.
'
' The MIF file requires a special format specified
' by SMS.  Currently, this routine implements the
' minimum requirements.  The hardcoded strings below
' that are written to the MIF should be written
' character by character as they are; except that
' status message should change depending on the
' circumstances of the install.  DO NOT LOCALIZE
' anything except the status message.
'
' IN: [strMIFFilename] - The name of the MIF file.
'                        Passed in to setup1 by
'                        setup.exe.  It is probably
'                        named <appname>.mif where
'                        <appname> is the name of the
'                        application you are installing.
'
'     [fStatus] - False to write a failed MIF (i.e. setup
'                 failed); True to write a successful MIF.
'
'     [strSMSDescription] - This is the description string
'                           to be written to the MIF file.
'                           It cannot be longer than 255
'                           characters and cannot contain
'                           carriage returns and/or line
'                           feeds.  This routine will
'                           enforce these requirements.
'
' Note, when running in SMS mode, there is no other way
' to display a message to the user than to write it to
' the MIF file.  Displaying a MsgBox will cause the
' computer to appear as if it has hung.  Therefore, this
' routine makes no attempt to display an error message.
'-----------------------------------------------------------
'
#If SMS Then
Private Sub WriteMIF(ByVal strMIFFilename As String, ByVal fStatus As Boolean, ByVal strSMSDescription As String)
    Const strSUCCESS = """SUCCESS"""                 ' Cannot be localized as per SMS
    Const strFAILED = """FAILED"""                   ' Cannot be localized as per SMS
    
    Dim fn As Integer
    Dim intOffset As Integer
    Dim fOpened As Boolean
        
    On Error GoTo WMIFFAILED  ' If we fail, we just return without doing anything
                              ' because there is no way to inform the user while
                              ' in SMS mode.

    '
    ' If the description string is greater than 255 characters,
    ' truncate it.  Required my SMS.
    '
    strSMSDescription = Left$(strSMSDescription, MAX_SMS_DESCRIP)
    '
    ' Remove any carriage returns or line feeds and replace
    ' them with spaces.  The message must be a single line.
    '
    For intOffset = 1 To Len(strSMSDescription)
        If (Mid$(strSMSDescription, intOffset, 1) = vbLf) Or (Mid$(strSMSDescription, intOffset, 1) = vbCr) Then
            Mid$(strSMSDescription, intOffset, 1) = " "
        End If
    Next intOffset
    '
    ' Open the MIF file for append, but first delete any existing
    ' ones with the same name.  Note, that setup.exe passed a
    ' unique name so if there is one with this name already in
    ' on the disk, it was put there by setup.exe.
    '
    If FileExists(strMIFFilename) Then
        Kill strMIFFilename
    End If
    
    fn = FreeFile
    Open strMIFFilename For Append As fn
    fOpened = True
    '
    ' We are ready to write the actual MIF file
    ' Note, none of the string below are supposed
    ' to be localized.
    '
    Print #fn, "Start Component"
        Print #fn, Tab; "Name = ""Workstation"""
        Print #fn, Tab; "Start Group"
            Print #fn, Tab; Tab; "Name = ""InstallStatus"""
            Print #fn, Tab; Tab; "ID = 1"
            Print #fn, Tab; Tab; "Class = ""MICROSOFT|JOBSTATUS|1.0"""
            Print #fn, Tab; Tab; "Start Attribute"
                Print #fn, Tab; Tab; Tab; "Name = ""Status"""
                Print #fn, Tab; Tab; Tab; "ID = 1"
                Print #fn, Tab; Tab; Tab; "Type = String(16)"
                If fStatus Then
                    Print #fn, Tab; Tab; Tab; "Value = "; strSUCCESS
                Else
                    Print #fn, Tab; Tab; Tab; "Value = "; strFAILED
                End If
            Print #fn, Tab; Tab; "End Attribute"
            Print #fn, Tab; Tab; "Start Attribute"
                Print #fn, Tab; Tab; Tab; "Name = ""Description"""
                Print #fn, Tab; Tab; Tab; "ID = 2"
                Print #fn, Tab; Tab; Tab; "Type = String(256)"
                Print #fn, Tab; Tab; Tab; "Value = "; strSMSDescription
            Print #fn, Tab; Tab; "End Attribute"
        Print #fn, Tab; "End Group"
    Print #fn, "End Component"

    Close fn
    '
    ' Success
    '
    Exit Sub

WMIFFAILED:
    '
    ' At this point we are unable to create the MIF file.
    ' Since we are running under SMS there is no one to
    ' tell, so we don't generate an error message at all.
    '
    If fOpened Then
        Close fn
    End If
    Exit Sub
End Sub
#End If

'-----------------------------------------------------------
'Adds or replaces an HKEY to the list of HKEYs in cache.
'Note that it is not necessary to remove keys from
'this list.
'-----------------------------------------------------------
'
Private Sub AddHkeyToCache(ByVal hKey As Long, ByVal strHkey As String)
    Dim intIdx As Integer
    
    intIdx = intGetHKEYIndex(hKey)
    If intIdx < 0 Then
        'The key does not already exist.  Add it to the end.
        On Error Resume Next
        ReDim Preserve hkeyCache(0 To UBound(hkeyCache) + 1)
        If Err.Number <> 0 Then
            'If there was an error, it means the cache was empty.
            On Error GoTo 0
            ReDim hkeyCache(0 To 0)
        End If
        On Error GoTo 0

        intIdx = UBound(hkeyCache)
    Else
        'The key already exists.  It will be replaced.
    End If

    hkeyCache(intIdx).hKey = hKey
    hkeyCache(intIdx).strHkey = strHkey
End Sub

'-----------------------------------------------------------
'Given a predefined HKEY, return the text string representing that
'key, or else return vbNullString.
'-----------------------------------------------------------
'
Private Function strGetPredefinedHKEYString(ByVal hKey As Long) As String
    Select Case hKey
        Case HKEY_CLASSES_ROOT
            strGetPredefinedHKEYString = "HKEY_CLASSES_ROOT"
        Case HKEY_CURRENT_USER
            strGetPredefinedHKEYString = "HKEY_CURRENT_USER"
        Case HKEY_LOCAL_MACHINE
            strGetPredefinedHKEYString = "HKEY_LOCAL_MACHINE"
        Case HKEY_USERS
            strGetPredefinedHKEYString = "HKEY_USERS"
    End Select
End Function

'-----------------------------------------------------------
'Given an HKEY, return the text string representing that
'key.
'-----------------------------------------------------------
'
Private Function strGetHKEYString(ByVal hKey As Long) As String
    Dim strKey As String
    Dim intIdx As Integer

    'Is the hkey predefined?
    strKey = strGetPredefinedHKEYString(hKey)
    If Len(strKey) > 0 Then
        strGetHKEYString = strKey
        Exit Function
    End If
    
    'It is not predefined.  Look in the cache.
    intIdx = intGetHKEYIndex(hKey)
    If intIdx >= 0 Then
        strGetHKEYString = hkeyCache(intIdx).strHkey
    End If
End Function

'-----------------------------------------------------------
'Searches the cache for the index of the given HKEY.
'Returns the index if found, else returns -1.
'-----------------------------------------------------------
'
Private Function intGetHKEYIndex(ByVal hKey As Long) As Integer
    Dim intUBound As Integer
    Dim intIdx As Integer
    
    On Error Resume Next
    intUBound = UBound(hkeyCache)
    If Err.Number <> 0 Then
        'If there was an error accessing the ubound of the array,
        'then the cache is empty
        GoTo NotFound
    End If
    On Error GoTo 0

    For intIdx = 0 To intUBound
        If hkeyCache(intIdx).hKey = hKey Then
            intGetHKEYIndex = intIdx
            Exit Function
        End If
    Next intIdx
    
NotFound:
    intGetHKEYIndex = -1
End Function

'-----------------------------------------------------------
'Returns the location of the Program Files\Common Files path, if
'it is present in the registry.  Otherwise, returns vbNullString.
'-----------------------------------------------------------
'
Public Function strGetCommonFilesPath() As String
    Dim hKey As Long
    Dim strPath As String
    
    Const strCommonFilesKey = "CommonFilesDir"

    If RegOpenKey(HKEY_LOCAL_MACHINE, RegPathWinCurrentVersion(), hKey) Then
        RegQueryStringValue hKey, strCommonFilesKey, strPath
        RegCloseKey hKey
    End If

    If Len(strPath) > 0 Then
        AddDirSep strPath
    End If
    
    strGetCommonFilesPath = strPath
End Function

'-----------------------------------------------------------
'Returns the location of the "Windows\Start Menu\Programs" Files path, if
'it is present in the registry.  Otherwise, returns vbNullString.
'-----------------------------------------------------------
'
Public Function strGetProgramsFilesPath() As String
    Dim hKey As Long
    Dim strPath As String
    
    Const strProgramsKey = "Programs"

    If RegOpenKey(HKEY_CURRENT_USER, RegPathWinPrograms(), hKey) Then
        RegQueryStringValue hKey, strProgramsKey, strPath
        RegCloseKey hKey
    End If

    If Len(strPath) > 0 Then
        AddDirSep strPath
    End If
    
    strGetProgramsFilesPath = strPath
End Function

'-----------------------------------------------------------
'Returns the directory where DAO is or should be installed.  If the
'key does not exist in the registry, it is created.  For instance, under
'NT 3.51 this location is normally 'C:\WINDOWS\MSAPPS\DAO'
'-----------------------------------------------------------
'
Private Function strGetDAOPath() As String
    Const strMSAPPS$ = "MSAPPS\"
    Const strDAO3032$ = "DAO350.DLL"
    
    'first look in the registry
    Const strKey = "SOFTWARE\Microsoft\Shared Tools\DAO350"
    Const strValueName = "Path"
    Dim hKey As Long
    Dim strPath As String

    If RegOpenKey(HKEY_LOCAL_MACHINE, strKey, hKey) Then
        RegQueryStringValue hKey, strValueName, strPath
        RegCloseKey hKey
    End If

    If Len(strPath) > 0 Then
        SeparatePathAndFileName strPath, strPath
        AddDirSep strPath
        strGetDAOPath = strPath
        Exit Function
    End If
    
    'It's not yet in the registry, so we need to decide
    'where the directory should be, and then need to place
    'that location in the registry.

    'For Win95, use "Common Files\Microsoft Shared\DAO"
    strPath = strGetCommonFilesPath() & ResolveResString(resMICROSOFTSHARED) & "DAO\"
    
    'Place this information in the registry (note that we point to DAO3032.DLL
    'itself, not just to the directory)
    If RegCreateKey(HKEY_LOCAL_MACHINE, strKey, vbNullString, hKey) Then
        RegSetStringValue hKey, strValueName, strPath & strDAO3032, False
        RegCloseKey hKey
    End If

    strGetDAOPath = strPath
End Function

'-----------------------------------------------------------
' Replace all double quotes with single quotes
'-----------------------------------------------------------
'
Public Sub ReplaceDoubleQuotes(str As String)
    Dim i As Integer
    
    For i = 1 To Len(str)
        If Mid$(str, i, 1) = gstrQUOTE Then
            Mid$(str, i, 1) = "'"
        End If
    Next i
End Sub

'-----------------------------------------------------------
'Returns the path to the root of the first fixed disk
'-----------------------------------------------------------
'
Private Function strRootDrive() As String
    Dim intDriveNum As Integer
    
    For intDriveNum = 0 To Asc("Z") - Asc("A")
        If GetDriveType(intDriveNum) = intDRIVE_FIXED Then
            strRootDrive = Chr$(Asc("A") + intDriveNum) & gstrCOLON & gstrSEP_DIR
            Exit Function
        End If
    Next intDriveNum

    strRootDrive = "C:\"
End Function

'-----------------------------------------------------------
' This routine verifies that strFileName is a valid file name.
' It checks that its length is less than the max allowed
' and that it doesn't contain any invalid characters..
'-----------------------------------------------------------
'
Public Function fValidFilename(strFilename As String) As Boolean
    Dim iInvalidChar As Integer
    Dim iFilename As Integer
    Dim strInvalidChars As String

    If Not fCheckFNLength(strFilename) Then
        '
        ' Name is too long.
        '
        Exit Function
    End If
    '
    ' Search through the list of invalid filename characters and make
    ' sure none of them are in the string.
    '
    strInvalidChars = ResolveResString(resCOMMON_INVALIDFILECHARS)
    
    For iInvalidChar = 1 To Len(strInvalidChars)
        If InStr(strFilename, Mid$(strInvalidChars, iInvalidChar, 1)) <> 0 Then
            Exit Function
        End If
    Next iInvalidChar
    
    fValidFilename = True
End Function

'-----------------------------------------------------------
' SUB: CountGroups
'
' Determines how many groups must be installed by counting
' them in the setup information file (SETUP.LST)
'-----------------------------------------------------------
'
Public Function CountGroups(ByVal strSection As String) As Integer
    Dim intIdx As Integer
    Dim sGroup As String
    
    intIdx = 0
    Do
        sGroup = ReadIniFile(gstrSetupInfoFile, strSection, gsGROUP & CStr(intIdx))
        If Len(sGroup) > 0 Then 'Found a group
            intIdx = intIdx + 1
        Else
            Exit Do
        End If
    Loop
    CountGroups = intIdx
End Function
'-----------------------------------------------------------
' SUB: GetGroup
'
' Returns the Groupname specified by Index
'-----------------------------------------------------------
'
Public Function GetGroup(ByVal strSection As String, ByVal Index As Integer)
    GetGroup = ReadIniFile(gstrSetupInfoFile, strSection, gsGROUP & CStr(Index))
End Function

'-----------------------------------------------------------
' SUB: SetGroup
'
' Sets Groupname specified by Index
'-----------------------------------------------------------
'
Public Sub SetGroup(ByVal strSection As String, ByVal Index As Integer, ByVal sGroupName As String)
    Const iBuf As Integer = 2048
    Const sEQUAL As String * 1 = "="
    Dim sGroup As String
    Dim sNames As String
    Dim sChar As String
    Dim ret As Long
    Dim lCount As Long
    Dim sKEY As String
    Dim sValue As String
    Dim fKey As Boolean

    sGroup = ReadIniFile(gstrSetupInfoFile, strSection, gsGROUP & CStr(Index))
    sNames = Space$(iBuf)
    ret = GetPrivateProfileSection(sGroup, sNames, iBuf, gstrSetupInfoFile)
    If ret = 0 Then 'We have nothing in this section, just quit.
        Exit Sub
    End If
    sNames = Left$(sNames, InStr(sNames, vbNullChar & vbNullChar))
    'We now have the Group name, modify the icons in that group
    fKey = True
    For lCount = 1 To Len(sNames)
        sChar = Mid$(sNames, lCount, 1)
        If (sChar = sEQUAL) Then
            fKey = False
        ElseIf (Asc(sChar) = 0) Or (Len(sNames) = lCount) Then
            If Len(sNames) = lCount Then
                If fKey Then
                    sKEY = sKEY & sChar
                Else
                    sValue = sValue & sChar
                End If
            End If
            If Len(sKEY) <> 0 Then
                WritePrivateProfileString sGroupName, sKEY, sValue, gstrSetupInfoFile
            End If
            sKEY = vbNullString
            sValue = vbNullString
            fKey = True
        Else
            If fKey Then
                sKEY = sKEY & sChar
            Else
                sValue = sValue & sChar
            End If
        End If
    Next
    WritePrivateProfileString strSection, gsGROUP & CStr(Index), sGroupName, gstrSetupInfoFile
End Sub
'-----------------------------------------------------------
' SUB: GetPrivate
'
' Returns the the value of whether the group is private specified by Index
'-----------------------------------------------------------
'
Public Function GetPrivate(ByVal strSection As String, ByVal Index As Integer) As Boolean
    GetPrivate = CBool(ReadIniFile(gstrSetupInfoFile, strSection, gsPRIVATE & CStr(Index)))
End Function
Public Function GetStart(ByVal strSection As String, ByVal Index As Integer) As Boolean
    GetStart = UCase$(ReadIniFile(gstrSetupInfoFile, strSection, gsPARENT & CStr(Index))) = UCase$(gsSTARTMENUKEY)
End Function

'-----------------------------------------------------------
' SUB: CountIcons
'
' Determines how many icons must be installed by counting
' them in the setup information file (SETUP.LST)
'-----------------------------------------------------------
'
Public Function CountIcons(ByVal strSection As String) As Integer
    Dim intIdx As Integer
    Dim cIcons As Integer
    Dim sGroup As String
    Dim oCol As New Collection
    Dim sGName As String
    Dim vGroup As Variant

    intIdx = 0
    cIcons = 0
    Do
        sGroup = ReadIniFile(gstrSetupInfoFile, strSection, gsGROUP & CStr(intIdx))
        If Len(sGroup) > 0 Then 'Found a group
            oCol.Add sGroup
            intIdx = intIdx + 1
        Else
            Exit Do
        End If
    Loop
    For Each vGroup In oCol
        intIdx = 1
        Do
            sGName = ReadIniFile(gstrSetupInfoFile, vGroup, gsICON & CStr(intIdx))
            If Len(sGName) > 0 Then
                cIcons = cIcons + 1
                intIdx = intIdx + 1
            Else
                Exit Do
            End If
        Loop
    Next
    CountIcons = cIcons
    
End Function
'-----------------------------------------------------------
' SUB: CreateIcons
'
' Walks through the list of files in SETUP.LST and creates
' Icons in the Program Group for files needed it.
'-----------------------------------------------------------
'
Public Sub CreateIcons(ByVal strSection As String)
    Dim intIdx As Integer
    Dim sFile As FILEINFO
    Dim strProgramIconTitle As String
    Dim strProgramIconCmdLine As String
    Dim strProgramPath As String
    Dim strProgramArgs As String
    Dim intAnchor As Integer
    Dim intOffset As Integer
    Dim strGroup As String
    Dim sGroup As String
    Dim oCol As New Collection
    Const CompareBinary = 0
    Dim sGName As String
    Dim vGroup As Variant
    Dim fPrivate As Boolean
    Dim sParent As String
    Dim intIdx2 As Integer
    '
    'For each file in the specified section, read info from the setup info file
    '
    intIdx = 0
    Do
        sGroup = ReadIniFile(gstrSetupInfoFile, strSection, gsGROUP & CStr(intIdx))
        If Len(sGroup) > 0 Then  'Found a group
            oCol.Add sGroup
            intIdx = intIdx + 1
        Else
            Exit Do
        End If
    Loop
    For Each vGroup In oCol
        intIdx = 0
        Do
            intIdx = intIdx + 1
            sGName = ReadIniFile(gstrSetupInfoFile, vGroup, gsICON & CStr(intIdx))
            If Len(sGName) > 0 Then
                '
                ' Get the Icon's caption and command line
                '
                strProgramIconTitle = ReadIniFile(gstrSetupInfoFile, vGroup, gsTITLE & CStr(intIdx))
                strProgramIconCmdLine = ReadIniFile(gstrSetupInfoFile, vGroup, gsICON & CStr(intIdx))
                strGroup = vGroup
                '
                ' if the ProgramIcon is specified, then we create an icon,
                ' otherwise we don't.
                '
                If Len(Trim$(strUnQuoteString(strProgramIconTitle))) > 0 Then
                    '
                    ' If the command line is not specified in SETUP.LST and the icon
                    ' is, then use the files destination path as the command line.  In
                    ' this case there are no parameters.
                    '
                    If Len(Trim$(strUnQuoteString(strProgramIconCmdLine))) = 0 Then
                        strProgramPath = sFile.strDestDir & gstrSEP_DIR & sFile.strDestName
                        strProgramArgs = vbNullString
                    Else
                        '
                        ' Parse the command line, to determine what is the exe, etc. and what
                        ' are the parameters.  The first space that is not contained within
                        ' quotes, marks the end of the exe, etc..  Everything afterwards are
                        ' parameters/arguments for the exe.  NOTE: It is important that if
                        ' the exe is contained within quotes that the parameters not be
                        ' contained within the same quotes.  The arguments can themselves
                        ' each be inside quotes as long as they are not in the same quotes
                        ' with the exe.
                        '
                        intAnchor = 1
                        intOffset = intGetNextFldOffset(intAnchor, strProgramIconCmdLine, " ")
                        If intOffset = 0 Then intOffset = Len(strProgramIconCmdLine) + 1
                        strProgramPath = Trim$(Left$(strProgramIconCmdLine, intOffset - 1))
                        '
                        ' Got the exe, now the parameters.
                        '
                        strProgramArgs = Trim$(Mid$(strProgramIconCmdLine, intOffset + 1))
                    End If
                    '
                    ' Expand all the Destination Directory macros that are embedded in the
                    ' Program Path and the Arguments'
                    '
                    strProgramPath = ResolveDestDir(strProgramPath)
                    strProgramArgs = ResolveDestDirs(strProgramArgs)
                    '
                    ' Finally, we have everything we need, create the icon.
                    '
                    intIdx2 = 0
                    Do
                        sGroup = ReadIniFile(gstrSetupInfoFile, gsICONGROUP, gsGROUP & CStr(intIdx2))
                        If UCase$(sGroup) = UCase$(strGroup) Then 'Found the group
                            If IsWindows95 Then
                                fPrivate = True
                            Else
                                fPrivate = GetPrivate(gsICONGROUP, intIdx2)
                            End If
                            If GetStart(gsICONGROUP, intIdx2) Then
                                sParent = gsSTARTMENUKEY
                            Else
                                sParent = gsPROGMENUKEY
                            End If
                            Exit Do
                        End If
                        intIdx2 = intIdx2 + 1
                    Loop
                    CreateShellLink strProgramPath, strGroup, strProgramArgs, strProgramIconTitle, fPrivate, sParent
                ElseIf Len(Trim$(strUnQuoteString(strProgramIconCmdLine))) > 0 Then
                    '
                    ' This file contained specified a command line in SETUP.LST but no icon.
                    ' This is an error.  Let the user know and skip this icon or abort.
        
                    '
                    If gfNoUserInput Or MsgWarning(ResolveResString(resICONMISSING, gstrPIPE1, sFile.strDestName), vbYesNo Or vbExclamation, gstrSETMSG) = vbNo Then
                        ExitSetup frmSetup1, gintRET_FATAL
                    End If
                End If
            Else
                Exit Do
            End If
        Loop
    Next
End Sub

'-----------------------------------------------------------
' SUB: RebootSystem
'
' Initiates a reboot. Returns immediately while reboot
' proceeds. Returns True if reboot is successfully
' initiated. Returns False otherwise.
'-----------------------------------------------------------
'
Public Function RebootSystem() As Boolean
    Dim ret As Long
    Dim hToken As Long
    Dim tkp As TOKEN_PRIVILEGES
    Dim tkpOld As TOKEN_PRIVILEGES
    Dim fOkReboot As Boolean
    Const sSHUTDOWN As String = "SeShutdownPrivilege"

    'Check to see if we are running on Windows NT
    If IsWindowsNT() Then
        'We are running windows NT.  We need to do some security checks/modifications
        'to ensure we have the token that allows us to reboot.
        If OpenProcessToken(GetCurrentProcess(), _
                TOKEN_ADJUST_PRIVILEGES Or TOKEN_QUERY, hToken) Then
            ret = LookupPrivilegeValue(vbNullString, sSHUTDOWN, tkp.Privileges(0).pLuid)
            tkp.PrivilegeCount = 1
            tkp.Privileges(0).Attributes = SE_PRIVILEGE_ENABLED
            fOkReboot = AdjustTokenPrivileges(hToken, 0, tkp, LenB(tkpOld), tkpOld, ret)
        End If
    Else
        'We are running Win95/98.  Nothing needs to be done.
        fOkReboot = True
    End If
    If fOkReboot Then RebootSystem = (ExitWindowsEx(EWX_REBOOT, 0) <> 0)
End Function

Private Function GetFileTime(ByVal aDate As Date) As FileTime
    Dim lTemp As SYSTEMTIME
    Dim lTime As FileTime
    
    VariantTimeToSystemTime aDate, lTemp
    SystemTimeToFileTime lTemp, lTime
    LocalFileTimeToFileTime lTime, GetFileTime
End Function

'-----------------------------------------------------------
' SUB: HandleFormQueryUnload
'
' Consolidates processing of Form_QueryUnload events. When a
' user closes a form, we check and make sure they want to
' exit. If yes, we shut down. If no, we cancel the unload.
'-----------------------------------------------------------
'
Public Sub HandleFormQueryUnload(UnloadMode As Integer, ByRef Cancel As Integer, Form As Form)
    If UnloadMode <> vbFormCode Then
        ExitSetup Form, gintRET_EXIT
        'If ExitSetup is cancelled, then we need to cancel the form unload.
        Cancel = True
    End If
End Sub

'-----------------------------------------------------------
' SUB: CheckForAndInstallDirectX
'
' Checks for a current version of DirectX and if it isn't the
' latest, or not installed, then we should install our version.
' Check only happens if the directx redist folder is part of the
' package.
'-----------------------------------------------------------
'

Public Sub CheckForAndInstallDirectX(ByVal strSection As String, ByVal lhWnd As Long)

    
    Dim ret As Long, fFoundFiles As Boolean
    Dim lMajor As Long, lMinor As Long
    Dim ans As Long, fInstall As Boolean
    Dim sCurDir As String, sNewSrc As String
    Dim intIdx As Integer
    Dim sFile As FILEINFO
        
    'First we need to check to see if the DirectX redist files are in the package.
    fFoundFiles = False
    intIdx = 1
    Do While ReadSetupFileLine(strSection, intIdx, sFile) And Not fFoundFiles
        If IsFileADXRedistFile(sFile.strSrcName) Then
            'We found a dx redist file we're done
            fFoundFiles = True
        End If
        intIdx = intIdx + 1
    Loop
    
    If fFoundFiles Then
        If IsWindows95 Then 'Check and install DX (Only on 9x systems)
            If Not DirExists(gsTEMPDIR) Then 'Make sure the temp folder exists
                MkDir gsTEMPDIR
            End If
            'Extract all the rest of the files for the DX redist
            intIdx = 1
            Do While ReadSetupFileLine(strSection, intIdx, sFile)
                If IsFileADXRedistFile(sFile.strSrcName) Then
                    'Extract this file to the temp folder
                    sNewSrc = gsTEMPDIR & sFile.strDestName
                    ExtractFileFromCab gsCABFULLNAME, sFile.strSrcName, sNewSrc, gintCabs, gstrSrcPath
                End If
                intIdx = intIdx + 1
            Loop
            'Save the current Drive/Path information
            sCurDir = CurDir
            If (Left$(gsTEMPDIR, 2) <> Left$(CurDir, 2)) And (InStr(Left$(gsTEMPDIR, 2), ":") > 0) Then ChDrive Left$(gsTEMPDIR, 2)
            ChDir gsTEMPDIR
            fInstall = False 'Do not install by default
            ret = DirectXSetupGetVersion(lMajor, lMinor)
            If ret = 0 Then
                'The GetVersion call failed.
                'Here would be a good place to put a dialog
                'asking the end user if they want to install
                'DX7.  We will just install by default (if need be).
                fInstall = True
            Else
                lMajor = lMajor - (lMajor And DSETUP_VERSION)
                If lMajor < 7 Then
                    'Here would be a good place to put a dialog
                    'asking the end user if they want to install
                    'DX7.  We will just install by default (if need be).
                    fInstall = True
                End If
            End If
            If fInstall Then 'Install DX7
                ret = DirectXSetup(0, Left$(gsTEMPDIR, Len(gsTEMPDIR) - 1), DSETUP_DIRECTX)
                If ret = DSETUPERR_SUCCESS Then
                    'Success do nothing
                ElseIf ret = DSETUPERR_SUCCESS_RESTART Then
                    gfDXReboot = True
                Else
                    'There was an error installing DX7
                    'You may place any error handling (or information)
                    'routines you wish here.  We fail silently.
                End If
            End If
            If (Left$(sCurDir, 2) <> Left$(CurDir, 2)) And (InStr(Left$(sCurDir, 2), ":") > 0) Then ChDrive Left$(sCurDir, 2)
            ChDir sCurDir
        End If
    End If
End Sub

Private Function IsFileADXRedistFile(ByVal sFile As String) As Boolean
    IsFileADXRedistFile = False
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_DSETUP) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_DSETUP32) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_CFGMGR32) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_DIRECTXCAB) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_DIRECTXINF) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_DXSETUP) Then
        IsFileADXRedistFile = True
    End If
    If UCase$(sFile) = UCase$(gstrAT & gstrFILE_SETUPAPIDLL) Then
        IsFileADXRedistFile = True
    End If
    
End Function
