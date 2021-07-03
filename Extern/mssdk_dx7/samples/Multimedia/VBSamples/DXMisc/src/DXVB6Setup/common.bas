Attribute VB_Name = "basCommon"
Option Explicit

'
' Public Constants
'

Public Const gstrSEP_DIR$ = "\"                         ' Directory separator character
Public Const gstrAT$ = "@"
Public Const gstrSEP_DRIVE$ = ":"                       ' Driver separater character, e.g., C:\
Public Const gstrSEP_DIRALT$ = "/"                      ' Alternate directory separator character
Public Const gstrSEP_EXT$ = "."                         ' Filename extension separator character
Public Const gstrSEP_URLDIR$ = "/"                      ' Separator for dividing directories in URL addresses.

Public Const gstrCOLON$ = ":"
Public Const gstrSwitchPrefix2 = "/"
Public Const gstrCOMMA$ = ","
Public Const gstrDECIMAL$ = "."
Public Const gstrQUOTE$ = """"
Public Const gstrASSIGN$ = "="
Public Const gstrINI_PROTOCOL = "Protocol"
'This should remain uppercase
Public Const gstrDCOM = "DCOM"

Public Const gintMAX_SIZE% = 255                        'Maximum buffer size
Public Const gintMAX_PATH_LEN% = 260                    ' Maximum allowed path length including path, filename,
                                                        ' and command line arguments for NT (Intel) and Win95.

Public Const intDRIVE_REMOVABLE% = 2                    'Constants for GetDriveType
Public Const intDRIVE_FIXED% = 3
Public Const intDRIVE_REMOTE% = 4
Public Const intDRIVE_CDROM% = 5

Public Const gintNOVERINFO% = 32767                     'flag indicating no version info

'File names
Public Const gstrFILE_SETUP$ = "SETUP.LST"              'Name of setup information file

'Share type macros for files
Public Const mstrPRIVATEFILE = vbNullString
Public Const mstrSHAREDFILE = "$(Shared)"

'INI File keys
Public Const gstrINI_FILES$ = "Setup1 Files"                           'default section to install
Public Const gstrINI_SETUP$ = "Setup"
Public Const gstrINI_COLOR$ = "Color"
Public Const gstrINI_BOOT$ = "Bootstrap"
Public Const gstrINI_APPNAME$ = "Title"
Public Const gstrINI_CABS$ = "Cabs"
Public Const gstrINI_APPDIR$ = "DefaultDir"
Public Const gstrINI_APPEXE$ = "AppExe"
Public Const gstrINI_APPPATH$ = "AppPath"
Public Const gstrINI_FORCEUSEDEFDEST = "ForceUseDefDir"
Public Const gstrINI_CABNAME$ = "CabFile"
Public Const gsPRIVATE As String = "PrivateGroup"

'This should remain uppercase.
Public Const gstrEXT_DEP$ = "DEP"

'Setup information file macros
Public Const gstrAPPDEST$ = "$(AppPath)"
Public Const gstrWINDEST$ = "$(WinPath)"
Public Const gstrFONTDEST$ = "$(Font)"
Public Const gstrWINSYSDEST$ = "$(WinSysPath)"
Public Const gstrWINSYSDESTSYSFILE$ = "$(WinSysPathSysFile)"
Public Const gstrPROGRAMFILES$ = "$(ProgramFiles)"
Public Const gstrCOMMONFILES$ = "$(CommonFiles)"
Public Const gstrCOMMONFILESSYS$ = "$(CommonFilesSys)"
Public Const gstrDAODEST$ = "$(MSDAOPath)"

Public Const gsZERO As String = "0"

'MsgError() Constants
Public Const MSGERR_ERROR = 1
Public Const MSGERR_WARNING = 2

'Shell Constants
Public Const NORMAL_PRIORITY_CLASS      As Long = &H20&
Public Const INFINITE                   As Long = -1&

Public Const STATUS_WAIT_0              As Long = &H0
Public Const WAIT_OBJECT_0              As Long = STATUS_WAIT_0

'GetLocaleInfo constants
Public Const LOCALE_FONTSIGNATURE = &H58&           ' font signature

Public Const TCI_SRCFONTSIG = 3

Public Const LANG_CHINESE = &H4
Public Const SUBLANG_CHINESE_TRADITIONAL = &H1           ' Chinese (Taiwan)
Public Const SUBLANG_CHINESE_SIMPLIFIED = &H2            ' Chinese (PR China)
Public Const CHARSET_CHINESESIMPLIFIED = 134
Public Const CHARSET_CHINESEBIG5 = 136

Public Const LANG_JAPANESE = &H11
Public Const CHARSET_SHIFTJIS = 128

Public Const LANG_KOREAN = &H12
Public Const SUBLANG_KOREAN = &H1                        ' Korean (Extended Wansung)
Public Const SUBLANG_KOREAN_JOHAB = &H2                  ' Korean (Johab)
Public Const CHARSET_HANGEUL = 129

Public Type STARTUPINFO
    cb              As Long
    lpReserved      As Long
    lpDesktop       As Long
    lpTitle         As Long
    dwX             As Long
    dwY             As Long
    dwXSize         As Long
    dwYSize         As Long
    dwXCountChars   As Long
    dwYCountChars   As Long
    dwFillAttribute As Long
    dwFlags         As Long
    wShowWindow     As Integer
    cbReserved2     As Integer
    lpReserved2     As Long
    hStdInput       As Long
    hStdOutput      As Long
    hStdError       As Long
End Type

Public Type PROCESS_INFORMATION
    hProcess    As Long
    hThread     As Long
    dwProcessID As Long
    dwThreadID  As Long
End Type

Private Type OFSTRUCT
    cBytes As Byte
    fFixedDisk As Byte
    nErrCode As Integer
    nReserved1 As Integer
    nReserved2 As Integer
    szPathName As String * 256
End Type

Public Type VERINFO                                            'Version FIXEDFILEINFO
    'There is data in the following two dwords, but it is for Windows internal
    '   use and we should ignore it
    Ignore(1 To 8) As Byte
    'Signature As Long
    'StrucVersion As Long
    FileVerPart2 As Integer
    FileVerPart1 As Integer
    FileVerPart4 As Integer
    FileVerPart3 As Integer
    ProductVerPart2 As Integer
    ProductVerPart1 As Integer
    ProductVerPart4 As Integer
    ProductVerPart3 As Integer
    FileFlagsMask As Long 'VersionFileFlags
    FileFlags As Long 'VersionFileFlags
    FileOS As Long 'VersionOperatingSystemTypes
    FileType As Long
    FileSubtype As Long 'VersionFileSubTypes
    'I've never seen any data in the following two dwords, so I'll ignore them
    Ignored(1 To 8) As Byte 'DateHighPart As Long, DateLowPart As Long
End Type

Private Type PROTOCOL
    strName As String
    strFriendlyName As String
End Type

Private Type OSVERSIONINFO 'for GetVersionEx API call
    dwOSVersionInfoSize As Long
    dwMajorVersion As Long
    dwMinorVersion As Long
    dwBuildNumber As Long
    dwPlatformId As Long
    szCSDVersion As String * 128
End Type

Private Type LOCALESIGNATURE
    lsUsb(3)          As Long
    lsCsbDefault(1)   As Long
    lsCsbSupported(1) As Long
End Type
Private Type FONTSIGNATURE
    fsUsb(3) As Long
    fsCsb(1) As Long
End Type
Private Type CHARSETINFO
    ciCharset As Long
    ciACP     As Long
    fs        As FONTSIGNATURE
End Type

'
' Public variables used for silent and SMS installation
'
Public gfSilent As Boolean                              ' Whether or not we are doing a silent install
Public gstrSilentLog As String                          ' filename for output during silent install.
#If SMS Then
Public gfSMS As Boolean                                 ' Whether or not we are doing an SMS silent install
Public gstrMIFFile As String                            ' status output file for SMS
Public gfSMSStatus As Boolean                           ' status of SMS installation
Public gstrSMSDescription As String                     ' description string written to MIF file for SMS installation
Public gfDontLogSMS As Boolean                          ' Prevents MsgFunc from being logged to SMS (e.g., for confirmation messasges)
Public Const MAX_SMS_DESCRIP = 255                      ' SMS does not allow description strings longer than 255 chars.
#End If

'Note: Silent mode is untested and unsupported, but it's still there and still works.
Public gfNoUserInput As Boolean                         ' True if either gfSMS or gfSilent is True

'Variables for caching font values
Private msFont As String                   ' the cached name of the font
Private mnFont As Integer                  ' the cached size of the font
Private mnCharset As Integer               ' the cached charset of the font

Public Const gsSTARTMENUKEY As String = "$(Start Menu)"
Public Const gsPROGMENUKEY As String = "$(Programs)"
Public Const gsPARENT As String = "Parent"

'
'List of available protocols
'
Public gProtocol() As PROTOCOL
Public gcProtocols As Integer
'
' MDAC_TYP.exe is a self extracting exe
' that installs data access.
'
Public gfMDag As Boolean
Public Const gstrFILE_MDAG = "mdac_typ.exe"
Public Const gstrFILE_MDAGARGS = " /q:a /c:""setup.exe /QN1"""
Public gstrMDagInstallPath As String

' DirectX Redist File Names
Public Const gstrFILE_DSETUP As String = "dsetup.dll"
Public Const gstrFILE_DSETUP32 As String = "dsetup32.dll"
Public Const gstrFILE_CFGMGR32 As String = "cfgmgr32.dll"
Public Const gstrFILE_DIRECTXCAB As String = "DirectX.Cab"
Public Const gstrFILE_DIRECTXINF As String = "directx.inf"
Public Const gstrFILE_DXSETUP As String = "DXSetup.exe"
Public Const gstrFILE_SETUPAPIDLL As String = "setupapi.dll"

'
'API/DLL Declarations for 32 bit SetupToolkit
'
Public Declare Function DLLSelfRegister Lib "vb6stkit.dll" (ByVal lpDllName As String) As Integer
Public Declare Function RegisterTLB Lib "vb6stkit.dll" (ByVal lpTLBName As String) As Integer
Public Declare Function OSfCreateShellLink Lib "vb6stkit.dll" Alias "fCreateShellLink" (ByVal lpstrFolderName As String, ByVal lpstrLinkName As String, ByVal lpstrLinkPath As String, ByVal lpstrLinkArguments As String, ByVal fPrivate As Long, ByVal sParent As String) As Long

Private Declare Function GetLocaleInfoLS Lib "Kernel32" Alias "GetLocaleInfoA" (ByVal Locale As Long, ByVal LCType As Long, lpLCData As LOCALESIGNATURE, ByVal cchData As Long) As Long
Private Declare Function TranslateCharsetInfo Lib "gdi32" (lpSrc As Long, lpcs As CHARSETINFO, ByVal dwFlags As Long) As Long

Private Declare Sub CopyMemory Lib "Kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)
Private Declare Function WaitForSingleObject Lib "Kernel32" (ByVal hProcess As Long, ByVal dwMilliseconds As Long) As Long
Private Declare Function InputIdle Lib "user32" Alias "WaitForInputIdle" (ByVal hProcess As Long, ByVal dwMilliseconds As Long) As Long
Private Declare Function CreateProcessA Lib "Kernel32" (ByVal lpApplicationName As Long, ByVal lpCommandLine As String, ByVal lpProcessAttributes As Long, ByVal lpThreadAttributes As Long, ByVal bInheritHandles As Long, ByVal dwCreationFlags As Long, ByVal lpEnvironment As Long, ByVal lpCurrentDirectory As Long, lpStartupInfo As STARTUPINFO, lpProcessInformation As PROCESS_INFORMATION) As Long
Public Declare Function GetDiskFreeSpace Lib "Kernel32" Alias "GetDiskFreeSpaceA" (ByVal lpRootPathName As String, lpSectorsPerCluster As Long, lpBytesPerSector As Long, lpNumberOfFreeClusters As Long, lpTtoalNumberOfClusters As Long) As Long
Public Declare Function GetFullPathName Lib "Kernel32" Alias "GetFullPathNameA" (ByVal lpFilename As String, ByVal nBufferLength As Long, ByVal lpBuffer As String, ByRef lpFilePart As Long) As Long
Public Declare Function GetPrivateProfileString Lib "Kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal lSize As Long, ByVal lpFilename As String) As Long
Public Declare Function WritePrivateProfileString Lib "Kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As Any, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lplFilename As String) As Long
Public Declare Function GetPrivateProfileSection Lib "Kernel32" Alias "GetPrivateProfileSectionA" (ByVal lpAppName As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFilename As String) As Long
Private Declare Function GetWindowsDirectory Lib "Kernel32" Alias "GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
Private Declare Function GetSystemDirectory Lib "Kernel32" Alias "GetSystemDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long) As Long
Private Declare Function GetDriveType32 Lib "Kernel32" Alias "GetDriveTypeA" (ByVal strWhichDrive As String) As Long
Public Declare Function GetTempPath Lib "Kernel32" Alias "GetTempPathA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long
Public Declare Function SendMessageString Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As String) As Long
Public Const LB_FINDSTRINGEXACT = &H1A2
Public Const LB_ERR = (-1)

Private Declare Function GetUserDefaultLCID Lib "Kernel32" () As Long
Private Declare Function GetLocaleInfoA Lib "Kernel32" (ByVal lLCID As Long, ByVal lLCTYPE As Long, ByVal strLCData As String, ByVal lDataLen As Long) As Long

Public Declare Function VerInstallFile Lib "version.dll" Alias "VerInstallFileA" (ByVal Flags&, ByVal SrcName$, ByVal DestName$, ByVal SrcDir$, ByVal DestDir$, ByVal CurrDir As Any, ByVal TmpName$, lpTmpFileLen&) As Long
Private Declare Function GetFileVersionInfoSize Lib "version.dll" Alias "GetFileVersionInfoSizeA" (ByVal sFile As String, lpLen As Long) As Long
Private Declare Function GetFileVersionInfo Lib "version.dll" Alias "GetFileVersionInfoA" (ByVal sFile As String, ByVal lpIgnored As Long, ByVal lpSize As Long, ByVal lpBuf As Long) As Long
Private Declare Function VerQueryValue Lib "version.dll" Alias "VerQueryValueA" (ByVal lpBuf As Long, ByVal szReceive As String, lpBufPtr As Long, lLen As Long) As Long
Private Declare Function OSGetShortPathName Lib "Kernel32" Alias "GetShortPathNameA" (ByVal lpszLongPath As String, ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long
Private Declare Function GetVersionEx Lib "Kernel32" Alias "GetVersionExA" (lpVersionInformation As OSVERSIONINFO) As Long

' Reboot system code
Public Const EWX_REBOOT = 2
Public Declare Function ExitWindowsEx Lib "user32" (ByVal uFlags As Long, ByVal dwReserved As Long) As Long

'Public constants used for string replacements
Public Const gstrPIPE1 As String = "|1"
Public Const gstrPIPE2 As String = "|2"

'----------------------------------------------------------
' FUNCTION: GetWinPlatform
' Get the current windows platform.
' ---------------------------------------------------------
Public Function GetWinPlatform() As Long
    Dim osvi As OSVERSIONINFO

    osvi.dwOSVersionInfoSize = Len(osvi)
    If GetVersionEx(osvi) = 0 Then
        Exit Function
    End If
    GetWinPlatform = osvi.dwPlatformId
End Function

'-----------------------------------------------------------
' SUB: AddDirSep
' Add a trailing directory path separator (back slash) to the
' end of a pathname unless one already exists
'
' IN/OUT: [strPathName] - path to add separator to
'-----------------------------------------------------------
'
Public Sub AddDirSep(strPathName As String)
    strPathName = RTrim$(strPathName)
    If Right$(strPathName, Len(gstrSEP_URLDIR)) <> gstrSEP_URLDIR Then
        If Right$(strPathName, Len(gstrSEP_DIR)) <> gstrSEP_DIR Then
            strPathName = strPathName & gstrSEP_DIR
        End If
    End If
End Sub

'-----------------------------------------------------------
' SUB: RemoveDirSep
' Removes a trailing directory path separator (back slash)
' at the end of a pathname if one exists
'
' IN/OUT: [strPathName] - path to remove separator from
'-----------------------------------------------------------
'
Public Sub RemoveDirSep(strPathName As String)
    Select Case Right$(strPathName, 1)
    Case gstrSEP_DIR, gstrSEP_DIRALT
        strPathName = Left$(strPathName, Len(strPathName) - 1)
    End Select
End Sub

'-----------------------------------------------------------
' FUNCTION: FileExists
' Determines whether the specified file exists
'
' IN: [strPathName] - file to check for
'
' Returns: True if file exists, False otherwise
'-----------------------------------------------------------
'
Public Function FileExists(ByVal strPathName As String) As Boolean
    Dim intFileNum As Integer

    On Error Resume Next

    '
    ' If the string is quoted, remove the quotes.
    '
    strPathName = strUnQuoteString(strPathName)
    '
    'Remove any trailing directory separator character
    '
    If Right$(strPathName, 1) = gstrSEP_DIR Then
        strPathName = Left$(strPathName, Len(strPathName) - 1)
    End If

    '
    'Attempt to open the file, return value of this function is False
    'if an error occurs on open, True otherwise
    '
    intFileNum = FreeFile
    Open strPathName For Input As intFileNum

    FileExists = (Err.Number = 0)

    Close intFileNum

    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: FileInUse
' Determines whether the specified file is currently in use
'
' IN: [strPathName] - file to check for
'
' Returns: True if file exists and is in use, False otherwise
'-----------------------------------------------------------
'
Public Function FileInUse(ByVal strPathName As String) As Boolean
    Dim hFile As Long
    
    On Error Resume Next
    '
    ' If the string is quoted, remove the quotes.
    '
    strPathName = strUnQuoteString(strPathName)
    '
    'Remove any trailing directory separator character
    '
    If Right$(strPathName, 1) = gstrSEP_DIR Then
        strPathName = Left$(strPathName, Len(strPathName) - 1)
    End If

    hFile = CreateFile(strPathName, GENERIC_WRITE, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_WRITE_THROUGH, 0)
    
    If hFile = INVALID_HANDLE_VALUE Then
        FileInUse = Err.LastDllError = ERROR_SHARING_VIOLATION
    Else
        CloseHandle hFile
    End If
    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: DirExists
'
' Determines whether the specified directory name exists.
' This function is used (for example) to determine whether
' an installation floppy is in the drive by passing in
' something like 'A:\'.
'
' IN: [strDirName] - name of directory to check for
'
' Returns: True if the directory exists, False otherwise
'-----------------------------------------------------------
'
Public Function DirExists(ByVal strDirName As String) As Boolean
    On Error Resume Next

    DirExists = (GetAttr(strDirName) And vbDirectory) = vbDirectory

    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: GetDriveType
' Determine whether a disk is fixed, removable, etc. by
' calling Windows GetDriveType()
'-----------------------------------------------------------
'
Public Function GetDriveType(ByVal intDriveNum As Integer) As Integer
    '
    ' This function expects an integer drive number in Win16 or a string in Win32
    '
    Dim strDriveName As String

    strDriveName = Chr$(Asc("A") + intDriveNum) & gstrSEP_DRIVE & gstrSEP_DIR
    GetDriveType = CInt(GetDriveType32(strDriveName))
End Function

'-----------------------------------------------------------
' FUNCTION: ReadProtocols
' Reads the allowable protocols from the specified file.
'
' IN: [strInputFilename] - INI filename from which to read the protocols
'     [strINISection] - Name of the INI section
'-----------------------------------------------------------
Public Function ReadProtocols(ByVal strInputFilename As String, ByVal strINISection As String) As Boolean
    Dim intIdx As Integer
    Dim strInfo As String
    Dim intOffset As Integer

    Erase gProtocol
    gcProtocols = 0

    Do
        intIdx = intIdx + 1
        strInfo = ReadIniFile(strInputFilename, strINISection, gstrINI_PROTOCOL & CStr(intIdx))
        If Len(strInfo) = 0 Then
            ReadProtocols = True
            Exit Function
        End If
        intOffset = InStr(strInfo, gstrCOMMA)
        If intOffset = 0 Then
            'Something is invalid. Exit and return False.
            Exit Function
        End If
        'The "ugly" name will be first on the line
        ReDim Preserve gProtocol(intIdx)
        gcProtocols = intIdx
        gProtocol(intIdx).strName = Left$(strInfo, intOffset - 1)
        
        '... followed by the friendly name
        gProtocol(intIdx).strFriendlyName = Mid$(strInfo, intOffset + 1)
        If (Len(gProtocol(intIdx).strName) = 0) Or (Len(gProtocol(intIdx).strFriendlyName) = 0) Then
            'Something is invalid. Exit and return False.
            Exit Function
        End If
    Loop
End Function

'-----------------------------------------------------------
' FUNCTION: ResolveResString
' Reads resource and replaces given macros with given values
'
' Example, given a resource number 14:
'    "Could not read '|1' in drive |2"
'   The call
'     ResolveResString(14, gstrPIPE1, "TXTFILE.TXT", gstrPIPE2, "A:")
'   would return the string
'     "Could not read 'TXTFILE.TXT' in drive A:"
'
' IN: [resID] - resource identifier
'     [varReplacements] - pairs of macro/replacement value
'-----------------------------------------------------------
'
Public Function ResolveResString(ByVal resID As Integer, ParamArray varReplacements() As Variant) As String
    Dim intMacro As Integer
    Dim strResString As String
    Dim strMacro As String
    Dim strValue As String

    strResString = LoadResString(resID)

    ' For each macro/value pair passed in...
    For intMacro = LBound(varReplacements) To UBound(varReplacements) - 1 Step 2
        strMacro = varReplacements(intMacro)
        strValue = varReplacements(intMacro + 1)

        'Replace all occurrences of strMacro with strValue
        strResString = Replace$(strResString, strMacro, strValue)
    Next intMacro

    ResolveResString = strResString
End Function
'-----------------------------------------------------------
' SUB: GetLicInfoFromVBL
' Parses a VBL file name and extracts the license key for
' the registry and license information.
'
' IN: [strVBLFile] - must be a valid VBL.
'
' OUT: [strLicKey] - registry key to write license info to.
'                    This key will be added to
'                    HKEY_CLASSES_ROOT\Licenses.  It is a
'                    guid.
' OUT: [strLicVal] - license information.  Usually in the
'                    form of a string of cryptic characters.
'-----------------------------------------------------------
'
Public Sub GetLicInfoFromVBL(strVBLFile As String, strLicKey As String, strLicVal As String)
    Const strLICKEYBASE = "HKEY_CLASSES_ROOT\Licenses\"
    Dim fn As Integer
    Dim strTemp As String
    Dim posEqual As Integer
    Dim fLicFound As Boolean
    
    fn = FreeFile
    Open strVBLFile For Input Access Read Lock Read Write As #fn
    '
    ' Read through the file until we find a line that starts with strLICKEYBASE
    '
    Do Until EOF(fn)
        Line Input #fn, strTemp
        strTemp = Trim$(strTemp)
        ' Avoid Option Compare Text and use explicit UCase comparisons because there
        ' is a Unicode character (&H818F) which is equal to a path separator when
        ' using Option Compare Text.
        If InStr(1, UCase$(strTemp), UCase$(strLICKEYBASE)) = 1 Then
            '
            ' We've got the line we want.
            '
            fLicFound = True
            Exit Do
        End If
    Loop

    Close fn

    If fLicFound Then
        '
        ' Parse the data on this line to split out the
        ' key and the license info.  The line should be
        ' the form of:
        ' "HKEY_CLASSES_ROOT\Licenses\<lickey> = <licval>"
        '
        ' First, get rid of the HKEY_CLASSES_ROOT\Licenses\ part.
        strTemp = Mid$(strTemp, Len(strLICKEYBASE) + 1)
        
        posEqual = InStr(strTemp, gstrASSIGN)
        If posEqual > 0 Then
            strLicKey = Trim$(Left$(strTemp, posEqual - 1))
            strLicVal = Trim$(Mid$(strTemp, posEqual + 1))
        End If
    Else
        strLicKey = vbNullString
        strLicVal = vbNullString
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION GetLongPathName
'
' Retrieve the long pathname version of a path possibly
'   containing short subdirectory and/or file names
'-----------------------------------------------------------
'
Public Function GetLongPathName(ByVal strShortPath As String) As String
    MakeLongPath strShortPath
    GetLongPathName = strShortPath
End Function

'-----------------------------------------------------------
' FUNCTION GetShortPathName
'
' Retrieve the short pathname version of a path possibly
'   containing long subdirectory and/or file names
'-----------------------------------------------------------
'
Function GetShortPathName(ByVal strLongPath As String) As String
    Const cchBuffer = 300
    Dim strShortPath As String
    Dim lResult As Long

    strShortPath = String$(cchBuffer, 0)
    lResult = OSGetShortPathName(strLongPath, strShortPath, cchBuffer)
    If lResult = 0 Then
        'Just use the long name as this is usually good enough
        GetShortPathName = strLongPath
    Else
        GetShortPathName = StringFromBuffer(strShortPath)
    End If
End Function
 
'-----------------------------------------------------------
' FUNCTION: GetDefMsgBoxButton
' Decode the flags passed to the MsgBox function to
' determine what the default button is.  Use this
' for silent installs.
'
' IN: [intFlags] - Flags passed to MsgBox
'
' Returns: VB defined number for button
'               vbOK        1   OK button pressed.
'               vbCancel    2   Cancel button pressed.
'               vbAbort     3   Abort button pressed.
'               vbRetry     4   Retry button pressed.
'               vbIgnore    5   Ignore button pressed.
'               vbYes       6   Yes button pressed.
'               vbNo        7   No button pressed.
'-----------------------------------------------------------
'
Private Function GetDefMsgBoxButton(intFlags) As Integer
    '
    ' First determine the ordinal of the default
    ' button on the message box.
    '
    Dim intButtonNum As Integer

    If (intFlags And vbDefaultButton2) = vbDefaultButton2 Then
        intButtonNum = 2
    ElseIf (intFlags And vbDefaultButton3) = vbDefaultButton3 Then
        intButtonNum = 3
    Else
        intButtonNum = 1
    End If
    '
    ' Now determine the type of message box we are dealing
    ' with and return the default button.
    '
    If (intFlags And vbRetryCancel) = vbRetryCancel Then
        If intButtonNum = 1 Then
            GetDefMsgBoxButton = vbRetry
        Else
            GetDefMsgBoxButton = vbCancel
        End If
    ElseIf (intFlags And vbYesNoCancel) = vbYesNoCancel Then
        Select Case intButtonNum
            Case 1
                GetDefMsgBoxButton = vbYes
            Case 2
                GetDefMsgBoxButton = vbNo
            Case 3
                GetDefMsgBoxButton = vbCancel
        End Select
    ElseIf (intFlags And vbOKCancel) = vbOKCancel Then
        If intButtonNum = 1 Then
            GetDefMsgBoxButton = vbOK
        Else
            GetDefMsgBoxButton = vbCancel
        End If
    ElseIf (intFlags And vbAbortRetryIgnore) = vbAbortRetryIgnore Then
        Select Case intButtonNum
            Case 1
                GetDefMsgBoxButton = vbAbort
            Case 2
                GetDefMsgBoxButton = vbRetry
            Case 3
                GetDefMsgBoxButton = vbIgnore
        End Select
    ElseIf (intFlags And vbYesNo) = vbYesNo Then
        If intButtonNum = 1 Then
            GetDefMsgBoxButton = vbYes
        Else
            GetDefMsgBoxButton = vbNo
        End If
    Else
        GetDefMsgBoxButton = vbOK
    End If
End Function
'-----------------------------------------------------------
' FUNCTION: GetDiskSpaceFree
' Get the amount of free disk space for the specified drive
'
' IN: [strDrive] - drive to check space for
'
' Returns: Amount of free disk space, or -1 if an error occurs
'-----------------------------------------------------------
'
Public Function GetDiskSpaceFree(ByVal strDrive As String) As Long
    Dim lRet As Long
    Dim lBytes As Long
    Dim lSect As Long
    Dim lClust As Long
    Dim lTot As Long

    On Error Resume Next

    'Start by assuming failure
    GetDiskSpaceFree = -1
    If GetDrive(strDrive, strDrive) Then
        lRet = GetDiskFreeSpace(strDrive, lSect, lBytes, lClust, lTot)
        If Err.Number = 0 Then
            If lRet <> 0 Then
                'Return the total number of bytes
                GetDiskSpaceFree = lBytes * lSect * lClust
                If Err.Number <> 0 Then
                    'Assume we overflowed when multiplying. Return max long.
                    GetDiskSpaceFree = &H7FFFFFFF
                End If
            End If
        End If
    End If

    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: GetWindowsSysDir
'
' Calls the windows API to get the windows\SYSTEM directory
' and ensures that a trailing dir separator is present
'
' Returns: The windows\SYSTEM directory
'-----------------------------------------------------------
'
Public Function GetWindowsSysDir() As String
    Dim strBuf As String

    strBuf = Space$(gintMAX_SIZE)
    '
    'Get the system directory and then trim the buffer to the exact length
    'returned and add a dir sep (backslash) if the API didn't return one
    '
    If GetSystemDirectory(strBuf, gintMAX_SIZE) Then
        GetWindowsSysDir = StringFromBuffer(strBuf)
        AddDirSep GetWindowsSysDir
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: IsWindows95
'
' Returns true if this program is running under Windows 95
'   or successor
'-----------------------------------------------------------
'
Public Function IsWindows95() As Boolean
    Const dwMask95 = &H1&
    IsWindows95 = (GetWinPlatform() And dwMask95)
End Function

'-----------------------------------------------------------
' FUNCTION: IsWindowsNT
'
' Returns true if this program is running under Windows NT
'-----------------------------------------------------------
'
Public Function IsWindowsNT() As Boolean
    Const dwMaskNT = &H2&
    IsWindowsNT = (GetWinPlatform() And dwMaskNT)
End Function

'-----------------------------------------------------------
' FUNCTION: IsUNCName
'
' Determines whether the pathname specified is a UNC name.
' UNC (Universal Naming Convention) names are typically
' used to specify machine resources, such as remote network
' shares, named pipes, etc.  An example of a UNC name is
' "\\SERVER\SHARE\FILENAME.EXT".
'
' IN: [strPathName] - pathname to check
'
' Returns: True if pathname is a UNC name, False otherwise
'-----------------------------------------------------------
'
Public Function IsUNCName(ByVal strPathName As String) As Integer
    Const strUNCNAME$ = "\\//\"        'so can check for \\, //, \/, /\

    IsUNCName = ((InStr(strUNCNAME, Left$(strPathName, 2)) > 0) And _
                 (Len(strPathName) > 1))
End Function
'-----------------------------------------------------------
' FUNCTION: LogSilentMsg
'
' If this is a silent install, this routine writes
' a message to the gstrSilentLog file.
'
' IN: [strMsg] - The message
'
' Normally, this routine is called inlieu of displaying
' a MsgBox and strMsg is the same message that would
' have appeared in the MsgBox

'-----------------------------------------------------------
'
Public Sub LogSilentMsg(strMsg As String)
    Dim fn As Integer

    If Not gfSilent Then Exit Sub

    On Error Resume Next

    fn = FreeFile
    Open gstrSilentLog For Append As fn
    Print #fn, strMsg
    Close fn

    Err.Clear
End Sub
'-----------------------------------------------------------
' FUNCTION: LogSMSMsg
'
' If this is a SMS install, this routine appends
' a message to the gstrSMSDescription string.  This
' string will later be written to the SMS status
' file (*.MIF) when the installation completes (success
' or failure).
'
' Note that if gfSMS = False, not message will be logged.
' Therefore, to prevent some messages from being logged
' (e.g., confirmation only messages), temporarily set
' gfSMS = False.
'
' IN: [strMsg] - The message
'
' Normally, this routine is called inlieu of displaying
' a MsgBox and strMsg is the same message that would
' have appeared in the MsgBox
'-----------------------------------------------------------
'
#If SMS Then
Public Sub LogSMSMsg(strMsg As String)
    If gfDontLogSMS Then
        ' We were told to ignore this message. Do so, but clear the ignore
        ' flag.
        gfDontLogSMS = False
    Else
        If Not gfSMS Then Exit Sub
        '
        ' Append the message.  Note that the total
        ' length cannot be more than 255 characters, so
        ' truncate anything after that.
        '
        gstrSMSDescription = Left$(gstrSMSDescription & strMsg, MAX_SMS_DESCRIP)
    End If
End Sub
#End If
'-----------------------------------------------------------
' FUNCTION: MakePathAux
'
' Creates the specified directory path.
'
' IN: [strDirName] - name of the dir path to make
'
' Returns: True if successful, False if error.
'-----------------------------------------------------------
'
Public Function MakePathAux(ByVal strDirName As String) As Boolean
    Dim strPath As String
    Dim intOffset As Integer
    Dim intAnchor As Integer
    Dim strOldPath As String

    On Error Resume Next

    '
    'Add trailing backslash
    '
    AddDirSep strDirName

    strOldPath = CurDir$
    '
    'Loop and make each subdir of the path separately.
    '
    'Start with the first backslash after the drive portion. This is the last
    '   character of the output from GetDrive.
    GetDrive strDirName, strPath
    intAnchor = Len(strPath)
    Do
        intOffset = InStr(intAnchor + 1, strDirName, gstrSEP_DIR)
        intAnchor = intOffset

        If intAnchor > 0 Then
            strPath = Left$(strDirName, intOffset - 1)
            ' Determine if this directory already exists
            If Not DirExists(strPath) Then
                ' We must create this directory
                Err.Clear
#If LOGGING Then
                NewAction gstrKEY_CREATEDIR, gstrQUOTE & strPath & gstrQUOTE
#End If
                MkDir strPath
#If LOGGING Then
                If Err.Number <> 0 Then
                    LogError ResolveResString(resMAKEDIR, gstrPIPE1, strPath)
                    AbortAction
                    GoTo Done
                Else
                    CommitAction
                End If
#End If
            End If
        End If
    Loop Until intAnchor = 0

    MakePathAux = True
Done:
    Err.Clear
End Function

'-----------------------------------------------------------
' FUNCTION: MsgError
'
' Forces mouse pointer to default, calls VB's MsgBox
' function, and logs this error and (32-bit only)
' writes the message and the user's response to the
' logfile (32-bit only)
'
' IN: [strMsg] - message to display
'     [intFlags] - MsgBox function type flags
'     [strCaption] - caption to use for message box
'     [intLogType] (optional) - The type of logfile entry to make.
'                   By default, creates an error entry.  Use
'                   the MsgWarning() function to create a warning.
'                   Valid types as MSGERR_ERROR and MSGERR_WARNING
'
' Returns: Result of MsgBox function
'-----------------------------------------------------------
'
Public Function MsgError(ByVal strMsg As String, ByVal intFlags As Integer, ByVal strCaption As String, Optional ByVal intLogType As Integer = MSGERR_ERROR) As Integer
    Dim iRet As Integer
    Dim strID As String
    Dim strLogMsg As String

    iRet = MsgFunc(strMsg, intFlags, strCaption)
    MsgError = iRet
#If LOGGING Then
    ' We need to log this error and decode the user's response.
    Select Case iRet
        Case vbOK
            strID = ResolveResString(resLOG_vbok)
        Case vbCancel
            strID = ResolveResString(resLOG_vbCancel)
        Case vbAbort
            strID = ResolveResString(resLOG_vbabort)
        Case vbRetry
            strID = ResolveResString(resLOG_vbretry)
        Case vbIgnore
            strID = ResolveResString(resLOG_vbignore)
        Case vbYes
            strID = ResolveResString(resLOG_vbyes)
        Case vbNo
            strID = ResolveResString(resLOG_vbno)
        Case Else
            strID = ResolveResString(resLOG_IDUNKNOWN)
    End Select

    strLogMsg = strMsg & vbLf & "(" & ResolveResString(resLOG_USERRESPONDEDWITH, gstrPIPE1, strID) & ")"
    On Error Resume Next
    Select Case intLogType
        Case MSGERR_WARNING
            LogWarning strLogMsg
        Case MSGERR_ERROR
            LogError strLogMsg
        Case Else
            LogError strLogMsg
    End Select
    Err.Clear
#End If
End Function

'-----------------------------------------------------------
' FUNCTION: MsgFunc
'
' Forces mouse pointer to default and calls VB's MsgBox
' function.  See also MsgError.
'
' IN: [strMsg] - message to display
'     [intFlags] - MsgBox function type flags
'     [strCaption] - caption to use for message box
' Returns: Result of MsgBox function
'-----------------------------------------------------------
'
Public Function MsgFunc(ByVal strMsg As String, ByVal intFlags As Integer, ByVal strCaption As String) As Integer
    Dim intOldPointer As Integer

    If gfNoUserInput Then
        MsgFunc = GetDefMsgBoxButton(intFlags)
        If gfSilent Then
            LogSilentMsg strMsg
        End If
#If SMS Then
        If gfSMS Then
            LogSMSMsg strMsg
        End If
#End If
    Else
        intOldPointer = Screen.MousePointer
        Screen.MousePointer = vbDefault
        MsgFunc = MsgBox(strMsg, intFlags, strCaption)
        Screen.MousePointer = intOldPointer
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: MsgWarning
'
' Forces mouse pointer to default, calls VB's MsgBox
' function, and logs this error and (32-bit only)
' writes the message and the user's response to the
' logfile (32-bit only)
'
' IN: [strMsg] - message to display
'     [intFlags] - MsgBox function type flags
'     [strCaption] - caption to use for message box
'
' Returns: Result of MsgBox function
'-----------------------------------------------------------
'
Public Function MsgWarning(ByVal strMsg As String, ByVal intFlags As Integer, ByVal strCaption As String) As Integer
    MsgWarning = MsgError(strMsg, intFlags, strCaption, MSGERR_WARNING)
End Function

'-----------------------------------------------------------
' SUB: SetFormFont
'
' Walks through all controls on specified form and
' sets Font a font chosen according to the system locale
'
' IN: [frm] - Form whose control fonts need to be set.
'-----------------------------------------------------------
'
Public Sub SetFormFont(frm As Form)
    Dim ctl As Control
    Dim fntSize As Integer
    Dim fntName As String
    Dim fntCharset As Integer
    Dim oFont As StdFont
    
    ' some controls may fail, so we will do a resume next...
    '
    On Error Resume Next
    
    ' get the font name, size, and charset
    '
    GetFontInfo fntName, fntSize, fntCharset

    'Create a new font object
    Set oFont = New StdFont
    With oFont
        .Name = fntName
        .Size = fntSize
        .Charset = fntCharset
    End With
    ' Set the form's font
    Set frm.Font = oFont
    '
    ' loop through each control and try to set its font property
    ' this may fail, but our error handling is shut off
    '
    For Each ctl In frm.Controls
        Set ctl.Font = oFont
    Next
    '
    ' get out, reset error handling
    '
    Err.Clear
End Sub

'-----------------------------------------------------------
' SUB:  GetFontInfo
'
' Gets the best font to use according the current system's
' locale.
'
' OUT:  [sFont] - name of font
'       [nFont] - size of font
'       [nCharset] - character set of font to use
'-----------------------------------------------------------
Private Sub GetFontInfo(sFont As String, nFont As Integer, nCharSet As Integer)
    Dim LCID    As Integer
    Dim PLangId As Integer
    Dim SLangId As Integer

    ' if font is set, used the cached values
    If Len(msFont) > 0 Then
        sFont = msFont
        nFont = mnFont
        nCharSet = mnCharset
        Exit Sub
    End If

    ' font hasn't been set yet, need to get it now...
    LCID = GetUserDefaultLCID                   ' get current LCID
    PLangId = PRIMARYLANGID(LCID)               ' get LCID's Primary language id
    SLangId = SUBLANGID(LCID)                   ' get LCID's Sub language id

    Select Case PLangId                         ' determine primary language id
    Case LANG_CHINESE
        If (SLangId = SUBLANG_CHINESE_TRADITIONAL) Then
            sFont = ChrW$(&H65B0) & ChrW$(&H7D30) & ChrW$(&H660E) & ChrW$(&H9AD4)   ' New Ming-Li
            nFont = 9
            nCharSet = CHARSET_CHINESEBIG5
        ElseIf (SLangId = SUBLANG_CHINESE_SIMPLIFIED) Then
            sFont = ChrW$(&H5B8B) & ChrW$(&H4F53)
            nFont = 9
            nCharSet = CHARSET_CHINESESIMPLIFIED
        End If
    Case LANG_JAPANESE
        sFont = ChrW$(&HFF2D) & ChrW$(&HFF33) & ChrW$(&H20) & ChrW$(&HFF30) & _
                ChrW$(&H30B4) & ChrW$(&H30B7) & ChrW$(&H30C3) & ChrW$(&H30AF)
        nFont = 9
        nCharSet = CHARSET_SHIFTJIS
    Case LANG_KOREAN
        If (SLangId = SUBLANG_KOREAN) Then
            sFont = ChrW$(&HAD74) & ChrW$(&HB9BC)
        ElseIf (SLangId = SUBLANG_KOREAN_JOHAB) Then
            sFont = ChrW$(&HAD74) & ChrW$(&HB9BC)
        End If
        nFont = 9
        nCharSet = CHARSET_HANGEUL
    Case Else
        sFont = "Tahoma"
        If Not IsFontSupported(sFont) Then
            'Tahoma is not on this machine.  This condition is very probably since
            'this is a setup program that may be run on a clean machine
            'Try Arial
            sFont = "Arial"
            If Not IsFontSupported(sFont) Then
                'Arial isn't even on the machine.  This is an unusual situation that
                'is caused by deliberate removal
                'Try system
                sFont = "System"
                'If system isn't supported, allow the default font to be used
                IsFontSupported sFont
                'If "System" is not supported, "IsFontSupported" will have
                'output the default font in sFont
            End If
        End If
        nFont = 8
        ' set the charset for the users default system Locale
        nCharSet = GetUserCharset
    End Select
    msFont = sFont
    mnFont = nFont
    mnCharset = nCharSet
'-------------------------------------------------------
End Sub
'-------------------------------------------------------

'------------------------------------------------------------
'- Language Functions...
'------------------------------------------------------------
Private Function PRIMARYLANGID(ByVal LCID As Integer) As Integer
    PRIMARYLANGID = (LCID And &H3FF)
End Function
Private Function SUBLANGID(ByVal LCID As Integer) As Integer
    SUBLANGID = (LCID / (2 ^ 10))
End Function

'-----------------------------------------------------------
' Function: GetUserCharset
'
' Get's the default user character set
'
' OS: Win 95 & NT 4 or newer
'-----------------------------------------------------------
Private Function GetUserCharset() As Integer
    Dim ls  As LOCALESIGNATURE                              ' local signature struct.
    Dim ci  As CHARSETINFO                                  ' character set info struct.
    Dim rc  As Long                                         ' return code

    ' get locale signature based on the USER's Default LCID.
    rc = GetLocaleInfoLS(GetUserDefaultLCID, LOCALE_FONTSIGNATURE, ls, Len(ls))
    If (rc > 0) Then                                        ' if success
        ls.lsCsbDefault(1) = 0                              ' zero out bits

        ' translate charset info from locale fontsignature.
        rc = TranslateCharsetInfo(ls.lsCsbDefault(0), ci, TCI_SRCFONTSIG)
        If rc <> 0 Then GetUserCharset = ci.ciCharset       ' return charset
    End If
End Function

'-----------------------------------------------------------
' Function: IsFontSupported
'
' Validates a font name to make sure it is supported by
' on the current system.
'
' IN/OUT: [sFontName] - name of font to check, will also]
'         be set to the default font name if the provided
'         one is not supported.
'-----------------------------------------------------------
Private Function IsFontSupported(sFontName As String) As Boolean
    Dim oFont As StdFont

    On Error Resume Next

    Set oFont = New StdFont
    oFont.Name = sFontName
    ' Check to see whether the font name passed in was valid by seeing
    ' if the property got set.
    IsFontSupported = (UCase$(oFont.Name) = UCase$(sFontName))
    ' Whatever happens, return a valid font name in this ByRef parameter.
    sFontName = oFont.Name

    Err.Clear
End Function

'-----------------------------------------------------------
' SUB: SetMousePtr
'
' Provides a way to set the mouse pointer only when the
' pointer state changes.  For every HOURGLASS call, there
' should be a corresponding DEFAULT call.  Other types of
' mouse pointers are set explicitly.
'
' IN: [intMousePtr] - type of mouse pointer desired
'-----------------------------------------------------------
'
Public Sub SetMousePtr(intMousePtr As Integer)
    Static intPtrState As Integer

    Select Case intMousePtr
        Case vbHourglass
            intPtrState = intPtrState + 1
        Case vbDefault
            intPtrState = intPtrState - 1
            If intPtrState < 0 Then
                intPtrState = 0
            End If
        Case Else
            Screen.MousePointer = intMousePtr
            Exit Sub
    End Select

    If intPtrState > 0 Then
        Screen.MousePointer = vbHourglass
    Else
        Screen.MousePointer = vbDefault
    End If
End Sub

'-----------------------------------------------------------
' FUNCTION: GetFileVerStruct
'
' Gets the file version information into a VERINFO TYPE
' variable
'
' IN: [strFilename] - name of file to get version info for
'     [fIsRemoteServerSupportFile] - whether or not this file is
'          a remote ActiveX component support file (.VBR)
'          (Enterprise edition only).  If missing, False is assumed.
' OUT: [sVerInfo] - VERINFO Type to fill with version info
'
' Returns: True if version info found, False otherwise
'-----------------------------------------------------------
'
Public Function GetFileVerStruct(ByVal sFile As String, sVer As VERINFO, Optional ByVal fIsRemoteServerSupportFile As Boolean = False) As Boolean
    Const sEXE As String = "\"
    Dim lVerSize As Long
    Dim lTemp As Long
    Dim lRet As Long
    Dim bInfo() As Byte
    Dim lpBuffer As Long
    Dim fFoundVer As Boolean

    If fIsRemoteServerSupportFile Then
        GetFileVerStruct = GetRemoteSupportFileVerStruct(sFile, sVer)
        fFoundVer = True
    Else
        '
        'Get the size of the file version info, allocate a buffer for it, and get the
        'version info.  Next, we query the Fixed file info portion, where the internal
        'file version used by the Windows VerInstallFile API is kept.  We then copy
        'the fixed file info into a VERINFO structure.
        '
        lVerSize = GetFileVersionInfoSize(sFile, lTemp)
        ReDim bInfo(lVerSize)
        If lVerSize > 0 Then
            lRet = GetFileVersionInfo(sFile, lTemp, lVerSize, VarPtr(bInfo(0)))
            If lRet <> 0 Then
                lRet = VerQueryValue(VarPtr(bInfo(0)), sEXE, lpBuffer, lVerSize)
                If lRet <> 0 Then
                    CopyMemory sVer, ByVal lpBuffer, lVerSize
                    fFoundVer = True
                    GetFileVerStruct = True
                End If
            End If
        End If
    End If
    If Not fFoundVer Then
        '
        ' We were unsuccessful in finding the version info from the file.
        ' One possibility is that this is a dependency file.
        '
        If UCase$(Extension(sFile)) = gstrEXT_DEP Then 'gstrEXT_DEP is uppercase.
            GetFileVerStruct = GetDepFileVerStruct(sFile, sVer)
        End If
    End If
End Function
'-----------------------------------------------------------
' FUNCTION: GetFileDescription
'
' Gets the file description information.
'
' IN: [strFilename] - name of file to get description of.
'
' Returns: Description (vbNullString if not found)
'-----------------------------------------------------------
'
Public Function GetFileDescription(ByVal sFile As String) As String
    Const sEXE As String = "\FileDescription"
    Dim lVerSize As Long
    Dim lTemp As Long
    Dim lRet As Long
    Dim bInfo() As Byte
    Dim lpBuffer As Long
    Dim sDesc As String
    Dim sKEY As String
    '
    'Get the size of the file version info, allocate a buffer for it, and get the
    'version info.  Next, we query the Fixed file info portion, where the internal
    'file version used by the Windows VerInstallFile API is kept.  We then copy
    'the info into a string.
    '
    lVerSize = GetFileVersionInfoSize(sFile, lTemp)
    ReDim bInfo(lVerSize)
    If lVerSize > 0 Then
        lRet = GetFileVersionInfo(sFile, lTemp, lVerSize, VarPtr(bInfo(0)))
        If lRet <> 0 Then
            sKEY = GetNLSKey(bInfo)
            lRet = VerQueryValue(VarPtr(bInfo(0)), sKEY & sEXE, lpBuffer, lVerSize)
            If lRet <> 0 Then
                sDesc = Space$(lVerSize)
                lstrcpyn sDesc, lpBuffer, lVerSize
                GetFileDescription = sDesc
            End If
        End If
    End If
End Function
Private Function GetNLSKey(byteVerData() As Byte) As String
    Const strTRANSLATION$ = "\VarFileInfo\Translation"
    Const strSTRINGFILEINFO$ = "\StringFileInfo\"
    Const strDEFAULTNLSKEY$ = "040904E4"
    Const LOCALE_IDEFAULTLANGUAGE& = &H9&
    Const LOCALE_IDEFAULTCODEPAGE& = &HB&

    Static strLANGCP As String

    Dim lpBufPtr As Long
    Dim strNLSKey As String
    Dim fGotNLSKey As Integer
    Dim intOffset As Integer
    Dim lVerSize As Long
    Dim lTmp As Long
    Dim lBufLen As Long
    Dim lLCID As Long
    Dim strTmp As String

    On Error GoTo GNLSKCleanup

    If VerQueryValue(VarPtr(byteVerData(0)), strTRANSLATION, lpBufPtr, lVerSize) <> 0 Then ' (Pass byteVerData array via reference to first element)
        If Len(strLANGCP) = 0 Then
            lLCID = GetUserDefaultLCID()
            If lLCID > 0 Then
                strTmp = Space$(8)

                GetLocaleInfoA lLCID, LOCALE_IDEFAULTCODEPAGE, strTmp, 8
                strLANGCP = StringFromBuffer(strTmp)
                Do While Len(strLANGCP) < 4
                    strLANGCP = gsZERO & strLANGCP
                Loop

                GetLocaleInfoA lLCID, LOCALE_IDEFAULTLANGUAGE, strTmp, 8
                strLANGCP = StringFromBuffer(strTmp) & strLANGCP
                Do While Len(strLANGCP) < 8
                    strLANGCP = gsZERO & strLANGCP
                Loop
            End If
        End If

        If VerQueryValue(VarPtr(byteVerData(0)), strLANGCP, lTmp, lBufLen) <> 0 Then
            strNLSKey = strLANGCP
        Else
            For intOffset = 0 To lVerSize - 1 Step 4
                CopyMemory lTmp, ByVal lpBufPtr + intOffset, 4
                strTmp = Hex$(lTmp)
                Do While Len(strTmp) < 8
                    strTmp = gsZERO & strTmp
                Loop

                strNLSKey = strSTRINGFILEINFO & Right$(strTmp, 4) & Left$(strTmp, 4)

                If VerQueryValue(VarPtr(byteVerData(0)), strNLSKey, lTmp, lBufLen) <> 0 Then
                    fGotNLSKey = True
                    Exit For
                End If
            Next

            If Not fGotNLSKey Then
                strNLSKey = strSTRINGFILEINFO & strDEFAULTNLSKEY
                If VerQueryValue(VarPtr(byteVerData(0)), strNLSKey, lTmp, lBufLen) <> 0 Then
                    fGotNLSKey = True
                End If
            End If
        End If
    End If

GNLSKCleanup:
    If fGotNLSKey Then
        GetNLSKey = strNLSKey
    End If
End Function
'-----------------------------------------------------------
' FUNCTION: GetDepFileVerStruct
'
' Gets the file version information from a dependency
' file (*.dep).  Such files do not have a Windows version
' stamp, but they do have an internal version stamp that
' we can look for.
'
' IN: [strFilename] - name of dep file to get version info for
' OUT: [sVerInfo] - VERINFO Type to fill with version info
'
' Returns: True if version info found, False otherwise
'-----------------------------------------------------------
'
Private Function GetDepFileVerStruct(ByVal strFilename As String, sVerInfo As VERINFO) As Boolean
    Const strVersionKey = "Version="
    Const strVersionKeyUCase = "VERSION="
    Dim cchVersionKey As Integer
    Dim iFile As Integer
    Dim strLine As String
    Dim strVersion As String

    cchVersionKey = Len(strVersionKey)
    sVerInfo.FileVerPart1 = gintNOVERINFO

    On Error GoTo Failed

    iFile = FreeFile

    Open strFilename For Input Access Read Lock Read Write As #iFile

    ' Loop through each line, looking for the key
    Do Until EOF(iFile)
        Line Input #iFile, strLine
        strLine = UCase$(strLine)
        If Left$(strLine, cchVersionKey) = strVersionKeyUCase Then
            ' We've found the version key.  Copy everything after the equals sign
            strVersion = Mid$(strLine, cchVersionKey + 1)

            'Parse and store the version information
            PackVerInfo strVersion, sVerInfo

            GetDepFileVerStruct = True
            Exit Do
        End If
    Loop

Failed:
    Close iFile
End Function

'-----------------------------------------------------------
' FUNCTION: GetRemoteSupportFileVerStruct
'
' Gets the file version information of a remote ActiveX component
' support file into a VERINFO TYPE variable (Enterprise
' Edition only).  Such files do not have a Windows version
' stamp, but they do have an internal version stamp that
' we can look for.
'
' IN: [strFilename] - name of file to get version info for
' OUT: [sVerInfo] - VERINFO Type to fill with version info
'
' Returns: True if version info found, False otherwise
'-----------------------------------------------------------
'
Private Function GetRemoteSupportFileVerStruct(ByVal strFilename As String, sVerInfo As VERINFO) As Boolean
    Const strVersionKey = "Version="
    Const strVersionKeyUCase = "VERSION="
    Dim cchVersionKey As Integer
    Dim iFile As Integer
    Dim strLine As String
    Dim strVersion As String

    cchVersionKey = Len(strVersionKey)
    sVerInfo.FileVerPart1 = gintNOVERINFO
    
    On Error GoTo Failed
    
    iFile = FreeFile

    Open strFilename For Input Access Read Lock Read Write As #iFile

    ' Loop through each line, looking for the key
    Do Until EOF(iFile)
        Line Input #iFile, strLine
        strLine = UCase$(strLine)
        If Left$(strLine, cchVersionKey) = strVersionKeyUCase Then
            ' We've found the version key.  Copy everything after the equals sign
            strVersion = Mid$(strLine, cchVersionKey + 1)

            'Parse and store the version information
            PackVerInfo strVersion, sVerInfo

            'Convert the format 1.2.3 from the .VBR into
            '1.2.0.3, which is really want we want
            sVerInfo.FileVerPart4 = sVerInfo.FileVerPart3
            sVerInfo.FileVerPart3 = 0

            GetRemoteSupportFileVerStruct = True
            Exit Do
        End If
    Loop

Failed:
    Close iFile
End Function
'-----------------------------------------------------------
' FUNCTION: GetWindowsFontDir
'
' Calls the windows API to get the windows font directory
' and ensures that a trailing dir separator is present
'
' Returns: The windows font directory
'-----------------------------------------------------------
'
Public Function GetWindowsFontDir() As String
    Dim oMalloc As IVBMalloc
    Dim sPath   As String
    Dim IDL     As Long

    ' Fill the item id list with the pointer of each folder item, rtns 0 on success
    If SHGetSpecialFolderLocation(0, sfidFONTS, IDL) = NOERROR Then
        sPath = String$(gintMAX_PATH_LEN, 0)
        SHGetPathFromIDListA IDL, sPath
        SHGetMalloc oMalloc
        oMalloc.Free IDL
        GetWindowsFontDir = StringFromBuffer(sPath)
    End If
    AddDirSep GetWindowsFontDir
End Function

'-----------------------------------------------------------
' FUNCTION: GetWindowsDir
'
' Calls the windows API to get the windows directory and
' ensures that a trailing dir separator is present
'
' Returns: The windows directory
'-----------------------------------------------------------
'
Public Function GetWindowsDir() As String
    Dim strBuf As String

    strBuf = Space$(gintMAX_SIZE)
    '
    'Get the windows directory and then trim the buffer to the exact length
    'returned and add a dir sep (backslash) if the API didn't return one
    '
    If GetWindowsDirectory(strBuf, gintMAX_SIZE) Then
        GetWindowsDir = StringFromBuffer(strBuf)
        AddDirSep GetWindowsDir
    End If
End Function

'-----------------------------------------------------------
' FUNCTION: ExtractFilenameItem
'
' Extracts a quoted or unquoted filename from a string.
'
' IN: [str] - string to parse for a filename.
'     [intAnchor] - index in str at which the filename begins.
'             The filename continues to the end of the string
'             or up to the next comma in the string, or, if
'             the filename is enclosed in quotes, until the
'             next double quote.
' OUT: Returns the filename, without quotes.
'      [intAnchor] is set to the comma, or else one character
'             past the end of the string
'      [fErr] is set to True if a parsing error is discovered
'
'-----------------------------------------------------------
'
Public Function strExtractFilenameItem(ByVal str As String, intAnchor As Integer, fErr As Boolean) As String
    Dim iEndFilenamePos As Integer
    Dim strFilename As String
    Dim iCommaPos As Integer

    Do While Mid$(str, intAnchor, 1) = " "
        intAnchor = intAnchor + 1
    Loop

    If Mid$(str, intAnchor, 1) = gstrQUOTE Then
        ' Filename is surrounded by quotes
        iEndFilenamePos = InStr(intAnchor + 1, str, gstrQUOTE) ' Find matching quote
        If iEndFilenamePos > 0 Then
            strFilename = Mid$(str, intAnchor + 1, iEndFilenamePos - 1 - intAnchor)
            intAnchor = iEndFilenamePos + 1
            Do While Mid$(str, intAnchor, 1) = " "
                intAnchor = intAnchor + 1
            Loop
            If intAnchor <= Len(str) Then
                ' If there are any more characters, then the next character
                ' must be a comma.
                If Mid$(str, intAnchor, 1) <> gstrCOMMA Then
                    fErr = True
                    Exit Function
                End If
            End If
        Else
            fErr = True
            Exit Function
        End If
    Else
        ' Filename continues until next comma or end of string
        iCommaPos = InStr(intAnchor, str, gstrCOMMA)
        If iCommaPos = 0 Then
            iCommaPos = Len(str) + 1
        End If
        iEndFilenamePos = iCommaPos

        strFilename = Mid$(str, intAnchor, iEndFilenamePos - intAnchor)
        intAnchor = iCommaPos
    End If

    strFilename = Trim$(strFilename)
    If Len(strFilename) = 0 Then
        fErr = True
        Exit Function
    End If

    fErr = False
    strExtractFilenameItem = strFilename
End Function

'-----------------------------------------------------------
' FUNCTION: Extension
'
' Extracts the extension portion of a file/path name
'
' IN: [strFilename] - file/path to get the extension of
'
' Returns: The extension if one exists, else vbNullString
'-----------------------------------------------------------
'
Public Function Extension(ByVal strFilename As String) As String
    Dim intDotPos As Integer
    Dim intSepPos As Integer

    intDotPos = InStrRev(strFilename, gstrSEP_EXT)
    If intDotPos > 0 Then
        'We've found a dot. Now make sure there is no '\' after it.
        intSepPos = InStr(intDotPos + 1, strFilename, gstrSEP_DIR)
        If intSepPos = 0 Then
            'There is no '\' after the dot. Make sure there is also no '/'.
            intSepPos = InStr(intDotPos + 1, strFilename, gstrSEP_DIRALT)
            If intSepPos = 0 Then
                'The dot has no '\' or '/' after it, so it is good.
                Extension = Mid$(strFilename, intDotPos + 1)
            End If
        End If
    End If
End Function
Public Function BaseName(sPathAndFile As String) As String
    '
    ' Strip the path from the file name, and just return the FileName
    ' Wraps the SeparatePathAndFileName from DWTools
    '
    Dim sFile As String

    SeparatePathAndFileName sPathAndFile, , sFile

    BaseName = sFile
End Function
'Given a fully qualified filename, returns the path portion and the file
'   portion.
Public Sub SeparatePathAndFileName(FullPath As String, _
    Optional ByRef Path As String, _
    Optional ByRef FileName As String)

    Dim nSepPos As Long
    Dim nSepPos2 As Long
    Dim fUsingDriveSep As Boolean

    nSepPos = InStrRev(FullPath, gstrSEP_DIR)
    nSepPos2 = InStrRev(FullPath, gstrSEP_DIRALT)
    If nSepPos2 > nSepPos Then
        nSepPos = nSepPos2
    End If
    nSepPos2 = InStrRev(FullPath, gstrSEP_DRIVE)
    If nSepPos2 > nSepPos Then
        nSepPos = nSepPos2
        fUsingDriveSep = True
    End If

    If nSepPos = 0 Then
        'Separator was not found.
        Path = CurDir$
        FileName = FullPath
    Else
        If fUsingDriveSep Then
            Path = Left$(FullPath, nSepPos)
        Else
            Path = Left$(FullPath, nSepPos - 1)
        End If
        FileName = Mid$(FullPath, nSepPos + 1)
    End If
End Sub

'-----------------------------------------------------------
' SUB: PackVerInfo
'
' Parses a file version number string of the form
' x[.x[.x[.x]]] and assigns the extracted numbers to the
' appropriate elements of a VERINFO type variable.
' Examples of valid version strings are '3.11.0.102',
' '3.11', '3', etc.
'
' IN: [strVersion] - version number string
'
' OUT: [sVerInfo] - VERINFO type variable whose elements
'                   are assigned the appropriate numbers
'                   from the version number string
'-----------------------------------------------------------
'
Public Sub PackVerInfo(ByVal strVersion As String, sVerInfo As VERINFO)
    Dim intOffset As Integer
    Dim intAnchor As Integer

    On Error GoTo PVIError

    intOffset = InStr(strVersion, gstrDECIMAL)
    If intOffset = 0 Then
        sVerInfo.FileVerPart1 = Val(strVersion)
        GoTo PVIMSLo
    Else
        sVerInfo.FileVerPart1 = Val(Left$(strVersion, intOffset - 1))
        intAnchor = intOffset + 1
    End If

    intOffset = InStr(intAnchor, strVersion, gstrDECIMAL)
    If intOffset = 0 Then
        sVerInfo.FileVerPart2 = Val(Mid$(strVersion, intAnchor))
        GoTo PVILSHi
    Else
        sVerInfo.FileVerPart2 = Val(Mid$(strVersion, intAnchor, intOffset - intAnchor))
        intAnchor = intOffset + 1
    End If

    intOffset = InStr(intAnchor, strVersion, gstrDECIMAL)
    If intOffset = 0 Then
        sVerInfo.FileVerPart3 = Val(Mid$(strVersion, intAnchor))
        GoTo PVILSLo
    Else
        sVerInfo.FileVerPart3 = Val(Mid$(strVersion, intAnchor, intOffset - intAnchor))
        intAnchor = intOffset + 1
    End If

    intOffset = InStr(intAnchor, strVersion, gstrDECIMAL)
    If intOffset = 0 Then
        sVerInfo.FileVerPart4 = Val(Mid$(strVersion, intAnchor))
    Else
        sVerInfo.FileVerPart4 = Val(Mid$(strVersion, intAnchor, intOffset - intAnchor))
    End If

    Exit Sub

PVIError:
    sVerInfo.FileVerPart1 = 0
PVIMSLo:
    sVerInfo.FileVerPart2 = 0
PVILSHi:
    sVerInfo.FileVerPart3 = 0
PVILSLo:
    sVerInfo.FileVerPart4 = 0
End Sub

Public Function strQuoteString(strUnQuotedString As String, Optional vForce As Boolean = False, Optional vTrim As Boolean = True)
'
' This routine adds quotation marks around an unquoted string, by default.  If the string is already quoted
' it returns without making any changes unless vForce is set to True (vForce defaults to False) except that white
' space before and after the quotes will be removed unless vTrim is False.  If the string contains leading or
' trailing white space it is trimmed unless vTrim is set to False (vTrim defaults to True).
'
    Dim strQuotedString As String

    strQuotedString = strUnQuotedString
    '
    ' Trim$ the string if necessary
    '
    If vTrim Then
        strQuotedString = Trim$(strQuotedString)
    End If
    '
    ' See if the string is already quoted
    '
    If Not vForce Then
        If Left$(strQuotedString, 1) = gstrQUOTE Then
            If Right$(strQuotedString, 1) = gstrQUOTE Then
                '
                ' String is already quoted.  We are done.
                '
                GoTo DoneQuoteString
            End If
        End If
    End If
    '
    ' Add the quotes
    '
    strQuotedString = gstrQUOTE & strQuotedString & gstrQUOTE
DoneQuoteString:
    strQuoteString = strQuotedString
End Function
Public Function strUnQuoteString(ByVal strQuotedString As String)
'
' This routine tests to see if strQuotedString is wrapped in quotation
' marks, and, if so, remove them.
'
    strQuotedString = Trim$(strQuotedString)

    If Mid$(strQuotedString, 1, 1) = gstrQUOTE Then
        If Right$(strQuotedString, 1) = gstrQUOTE Then
            '
            ' It's quoted.  Get rid of the quotes.
            '
            strQuotedString = Mid$(strQuotedString, 2, Len(strQuotedString) - 2)
        End If
    End If
    strUnQuoteString = strQuotedString
End Function
Public Function fCheckFNLength(strFilename As String) As Boolean
'
' This routine verifies that the length of the filename strFilename is valid.
' Under NT (Intel) and Win95 it can be up to 259 (gintMAX_PATH_LEN-1) characters
' long.  This length must include the drive, path, filename, commandline
' arguments and quotes (if the string is quoted).
'
    fCheckFNLength = (Len(strFilename) < gintMAX_PATH_LEN)
End Function
Public Function intGetNextFldOffset(ByVal intAnchor As Integer, strList As String, strDelimit As String) As Integer
'
' This routine reads from a strDelimit separated list, strList, and locates the next
' item in the list following intAnchor.  Basically it finds the next
' occurance of strDelimit that is not inside quotes.  If strDelimit is not
' found the routine returns 0.  Note intAnchor must be outside of quotes
' or this routine will return incorrect results.
'
' strDelimit is typically a comma.
'
' If there is an error this routine returns -1.
'
    Dim intQuote As Integer
    Dim intDelimit As Integer

    If intAnchor = 0 Then intAnchor = 1

    intQuote = InStr(intAnchor, strList, gstrQUOTE)
    intDelimit = InStr(intAnchor, strList, strDelimit)
    
    If intQuote > 0 Then
        If intQuote < intDelimit Then
            '
            ' A quote appeared before the next delimiter.  This
            ' means we might be inside quotes.  We still need to check
            ' if the closing quote comes after the delmiter or not.
            '
            intAnchor = intQuote + 1
            intQuote = InStr(intAnchor, strList, gstrQUOTE)
            If intQuote > intDelimit Then
                '
                ' The delimiter was inside quotes.  Therefore, ignore it.
                ' The next delimiter after the closing quote must be outside
                ' of quotes or else we have a corrupt file.
                '
                intAnchor = intQuote + 1
                intDelimit = InStr(intAnchor, strList, strDelimit)
                '
                ' Sanity check.  Make sure there is not another quote before
                ' the delimiter we just found.
                '
                If intDelimit > 0 Then
                    intQuote = InStr(intAnchor, strList, gstrQUOTE)
                    If intQuote > 0 Then
                        If intQuote < intDelimit Then
                            '
                            ' Something is wrong.  We've encountered a stray
                            ' quote.  Means the string is probably corrupt.
                            '
                            intDelimit = -1 ' Error
                        End If
                    End If
                End If
            End If
        End If
    End If
    intGetNextFldOffset = intDelimit
End Function
Public Function LongPath(Path As String) As String
    Dim oDesktop As IVBShellFolder
    Dim nEaten As Long
    Dim pIdl As Long
    Dim sPath As String
    Dim oMalloc As IVBMalloc

    If Len(Path) > 0 Then
        SHGetDesktopFolder oDesktop
        oDesktop.ParseDisplayName 0, 0, Path, nEaten, pIdl, 0
        sPath = String$(gintMAX_PATH_LEN, 0)
        SHGetPathFromIDListA pIdl, sPath
        SHGetMalloc oMalloc
        oMalloc.Free pIdl
        LongPath = StringFromBuffer(sPath)
    End If
End Function

'Try to convert a path to its long filename equivalent, but leave it unaltered
'   if we fail.
Public Sub MakeLongPath(Path As String)
    On Error Resume Next
    Path = LongPath(Path)
End Sub

Public Function StringFromBuffer(Buffer As String) As String
    Dim nPos As Long

    nPos = InStr(Buffer, vbNullChar)
    If nPos > 0 Then
        StringFromBuffer = Left$(Buffer, nPos - 1)
    Else
        StringFromBuffer = Buffer
    End If
End Function

''==============================================================================
''Code flow routines:

Public Function SyncShell(CommandLine As String, Optional Timeout As Long, _
    Optional WaitForInputIdle As Boolean, Optional Hide As Boolean = False) As Boolean

    Dim hProcess As Long

    Dim ret As Long
    Dim nMilliseconds As Long

    If Timeout > 0 Then
        nMilliseconds = Timeout
    Else
        nMilliseconds = INFINITE
    End If

    hProcess = StartProcess(CommandLine, Hide)

    If WaitForInputIdle Then
        'Wait for the shelled application to finish setting up its UI:
        ret = InputIdle(hProcess, nMilliseconds)
    Else
        'Wait for the shelled application to terminate:
        ret = WaitForSingleObject(hProcess, nMilliseconds)
    End If

    CloseHandle hProcess

    'Return True if the application finished. Otherwise it timed out or erred.
    SyncShell = (ret = WAIT_OBJECT_0)
End Function

Public Function StartProcess(CommandLine As String, Optional Hide As Boolean = False) As Long
    Const STARTF_USESHOWWINDOW As Long = &H1
    Const SW_HIDE As Long = 0
    
    Dim proc As PROCESS_INFORMATION
    Dim Start As STARTUPINFO

    'Initialize the STARTUPINFO structure:
    Start.cb = Len(Start)
    If Hide Then
        Start.dwFlags = STARTF_USESHOWWINDOW
        Start.wShowWindow = SW_HIDE
    End If
    'Start the shelled application:
    CreateProcessA 0&, CommandLine, 0&, 0&, 1&, _
        NORMAL_PRIORITY_CLASS, 0&, 0&, Start, proc

    StartProcess = proc.hProcess
End Function
Public Function CheckDataAccess() As Boolean
    Dim i As Integer
    Dim udtFile As FILEINFO
    Dim sTarget As String

    sTarget = UCase$(gstrAT & gstrFILE_MDAG)
    i = 1
    Do While ReadSetupFileLine(gstrINI_FILES, i, udtFile)
        If UCase$(udtFile.strSrcName) = sTarget Then 'This is mdac_typ
            CheckDataAccess = True
            Exit Function
        End If
        i = i + 1
    Loop
End Function
Public Sub InstallDataAccess()
    Dim sTarget As String

    'Create the folder if it doesn't exist already.
    If Not (DirExists(gsTEMPDIR)) Then
        MkDir gsTEMPDIR
    End If
    sTarget = gsTEMPDIR & gstrFILE_MDAG
    ExtractFileFromCab gsCABFULLNAME, gstrAT & gstrFILE_MDAG, sTarget, gintCabs, gstrSrcPath
    If FileExists(sTarget) Then
        SyncShell sTarget & gstrFILE_MDAGARGS, INFINITE
    End If
End Sub
