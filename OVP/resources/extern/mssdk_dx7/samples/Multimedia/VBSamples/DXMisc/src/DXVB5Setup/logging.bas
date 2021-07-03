Attribute VB_Name = "basLogging"
Option Explicit
Option Compare Text

'
' Module basLogging (32-bit functionality only)
'
'The routines in this module are used for logging actions,
'warnings, notes and errors in an application removal
'logfile.  This logfile will be used by the application
'removal utility (ST5UNST.EXE) in the event that the user
'decides to remove the installed application (via a Program
'Manager icon under Windows NT or the Add/Remove Programs
'control panel applet under Windows 95).
'
'The functions are based on transaction-like "actions".
'Whenever the setup program starts to process a new action
'(an action is anything which the application removal
'utility must undo), the function NewAction() must be
'called with the appropriate parameters for that action
'(search for NewAction in this project to see how the
'correct parameters for various actions are formed).
'When the action has been successfully completed, the
'function CommitAction() is called, or, if the
'action was not successfully completed, AbortAction()
'must be called.  If CommitAction() is called, then the
'action is logged at that point, and the application
'removal utility will undo that action (example, delete
'a file which was copied by setup).
'
'Actions may be nested (for instance, a file copy
'action may have a nested direction creation action).
'Any errors, warnings or notes logged will note in
'the logfile the pending action (if any).  Even if
'an error is logged, the pending action must either
'be committed or canceled.  See comments for each
'function below for more specifics.
'

'Application removal is only supported for 32-bit projects

'Set this constant to FALSE if you do not want warnings to appear
'in the logfile
Global Const fLOG_WARNINGS = True

'Global Action Key constants
Global Const gstrKEY_PRIVATEFILE = "PrivateFile"
Global Const gstrKEY_TEMPFILE = "TempFile"
Global Const gstrKEY_SHAREDFILE = "SharedFile"
Global Const gstrKEY_SYSTEMFILE = "SystemFile"
Global Const gstrKEY_CREATEDIR = "CreateDir"
Global Const gstrKEY_PROGMANGROUP = "ProgManGroup"
Global Const gstrKEY_PROGMANITEM = "ProgManItem"
Global Const gstrKEY_SHELLFOLDER = "ShellFolder"
Global Const gstrKEY_SHELLLINK = "ShellLink"
Global Const gstrKEY_DLLSELFREGISTER = "DllSelfRegister"
Global Const gstrKEY_EXESELFREGISTER = "ExeSelfRegister"
Public Const gstrKEY_TLBREGISTER = "TLBRegister"
Global Const gstrKEY_REMOTEREGISTER = "RemoteRegister"
Global Const gstrKEY_REGKEY = "RegKey"
Global Const gstrKEY_REGVALUE = "RegValue"

'VB5STKIT.DLL logging errors
Private Const LOGERR_SUCCESS = 0
Private Const LOGERR_INVALIDARGS = 1
Private Const LOGERR_OUTOFMEMORY = 2
Private Const LOGERR_EXCEEDEDCAPACITY = 3
Private Const LOGERR_WRITEERROR = 4
Private Const LOGERR_NOCURRENTACTION = 5
Private Const LOGERR_UNEXPECTED = 6
Private Const LOGERR_FILENOTFOUND = 7

'Logging error Severities
Private Const LogErrOK = 1 ' OK to continue upon this error
Private Const LogErrFatal = 2 ' Must terminate install upon this error

'SKIT432.DLL interfaces
Private Declare Function DllAbortAction Lib "VB5STKIT.DLL" Alias "AbortAction" () As Long
Private Declare Function DllAddActionNote Lib "VB5STKIT.DLL" Alias "AddActionNote" (ByVal lpszNote As String) As Long
Private Declare Function DllChangeActionKey Lib "VB5STKIT.DLL" Alias "ChangeActionKey" (ByVal lpszNewKey As String) As Long
Private Declare Function DllCommitAction Lib "VB5STKIT.DLL" Alias "CommitAction" () As Long
Private Declare Function fDllWithinAction Lib "VB5STKIT.DLL" Alias "fWithinAction" () As Long
Private Declare Function DllLogError Lib "VB5STKIT.DLL" Alias "LogError" (ByVal lpszERROR As String, ByVal lpszDURINGACTION As String, ByVal lpszErrMsg As String) As Long
Private Declare Function DllLogNote Lib "VB5STKIT.DLL" Alias "LogNote" (ByVal lpszNote As String) As Long
Private Declare Function DllLogWarning Lib "VB5STKIT.DLL" Alias "LogWarning" (ByVal lpszWARNING As String, ByVal lpszDURINGACTION As String, ByVal lpszWarningMsg As String) As Long
Private Declare Function DllNewAction Lib "VB5STKIT.DLL" Alias "NewAction" (ByVal lpszKey As String, ByVal lpszData As String) As Long
Private Declare Function DllEnableLogging Lib "VB5STKIT.DLL" Alias "EnableLogging" (ByVal lpszFilename As String) As Long
Private Declare Function DllDisableLogging Lib "VB5STKIT.DLL" Alias "DisableLogging" () As Long

'-----------------------------------------------------------
' SUB: AbortAction
'
' Aborts the current action.
'-----------------------------------------------------------
'
Sub AbortAction()
    ShowLoggingError DllAbortAction(), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: AddActionNote
'
' Adds an note which will be written to the log file
' immediately following the current action
'-----------------------------------------------------------
'
Sub AddActionNote(ByVal strNote As String)
    ShowLoggingError DllAddActionNote(strNote), LogErrOK
End Sub

'-----------------------------------------------------------
' SUB: ChangeActionKey
'
' Changes the key of the current action.
'-----------------------------------------------------------
'
Sub ChangeActionKey(ByVal strNewKey As String)
    ShowLoggingError DllChangeActionKey(strNewKey), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: CommitAction
'
' Marks the successful completion of the current action.
' The action will be output to the log file.
'-----------------------------------------------------------
'
Sub CommitAction()
    ShowLoggingError DllCommitAction(), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: DisableLogging
'
' Disables application removal logging.  All logging
' functions can still be called, and must still be
' symentically correct, but no data will be written to disk.
'-----------------------------------------------------------
'
Sub DisableLogging()
    ShowLoggingError DllDisableLogging(), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: EnableLogging
'
' Enables application setup/removal logging to the specified logfile
'-----------------------------------------------------------
'
Sub EnableLogging(ByVal strLogFileName As String)
    ShowLoggingError DllEnableLogging(strLogFileName), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: LogError
'
' Logs an error to the logfile.  The action is NOT aborted.
'-----------------------------------------------------------
'
Sub LogError(ByVal strErr As String)
    ShowLoggingError DllLogError(ResolveResString(resLOG_ERROR), ResolveResString(resLOG_DURINGACTION), strErr), LogErrFatal
End Sub

'-----------------------------------------------------------
' SUB: LogWarning
'
' Logs a warning to the logfile.  The action is NOT aborted.
' Warnings are different from errors in that generally
' warnings are not brought to the end user's attention.
' Also, the bootstrapper does not ever log warnings.  It only
' logs errors.
'
' The logging of warnings can be turned off by changing the
' value of fLOG_WARNINGS in the declarations section of this
' module.
'-----------------------------------------------------------
'
Sub LogWarning(ByVal strWarning As String)
    If fLOG_WARNINGS Then
        ShowLoggingError DllLogWarning(ResolveResString(resLOG_WARNING), ResolveResString(resLOG_DURINGACTION), strWarning), LogErrFatal
    End If
End Sub

'-----------------------------------------------------------
' SUB: LogNote
'
' Logs a note to the logfile.  It is not necessary to have
' a current action in order to execute this subroutine.
'-----------------------------------------------------------
'
Sub LogNote(ByVal strNote As String)
    ShowLoggingError DllLogNote(strNote), LogErrOK
End Sub

'-----------------------------------------------------------
' SUB: NewAction
'
' Marks the start of a new action for logging.  If this
' routine is called before any current action is committed
' or aborted, the previous action will be placed
' on a stack.  Once the new action has been committed or
' aborted, the previous action will become active again.
' The reporting of errors, warnings, notes and action
' results are not printed until the action aborts or
' commits.
' Several actions may be stacked in a first-in-first-out
' manner by calling this routine repeatedly.
'-----------------------------------------------------------
'
Sub NewAction(ByVal strKey As String, ByVal strData As String)
    ShowLoggingError DllNewAction(strKey, strData), LogErrFatal
End Sub

Sub ShowLoggingError(ByVal lErr As Long, ByVal lErrSeverity As Long)
    If lErr = LOGERR_SUCCESS Then
        Exit Sub
    End If
    
    Dim strErrMsg As String
    Static fRecursive As Boolean
    
    If fRecursive Then
        'If we're getting called recursively, we're likely
        'getting errors while trying to write out errors to
        'the logfile.  Nothing to do but turn off logging
        'and abort setup.
        DisableLogging
        MsgError ResolveResString(resUNEXPECTED), vbExclamation Or vbOKOnly, gstrTitle
        ExitSetup frmSetup1, gintRET_FATAL
    End If

    fRecursive = True

    Select Case lErr
        Case LOGERR_OUTOFMEMORY, LOGERR_WRITEERROR, LOGERR_UNEXPECTED, LOGERR_FILENOTFOUND
            strErrMsg = ResolveResString(resUNEXPECTED)
        
        Case LOGERR_INVALIDARGS, LOGERR_EXCEEDEDCAPACITY, LOGERR_NOCURRENTACTION
            'Note: These errors are most likely the result of improper customization
            'of this project.  Make certain that any changes you have made to these
            'files are valid and bug-free.
            'LOGERR_INVALIDARGS -- some parameter to a logging function was invalid or improper
            'LOGERR_EXCEEDEDCAPACITY -- the stacking depth of actions has probably been
            '   exceeded.  This most likely means that CommitAction or AbortAction statements
            '   are missing from your code.
            'LOGERR_NOCURRENTACTION -- the logging function you tried to use requires that
            '   there be a current action, but there was none.  Check for a missing NewAction
            '   statement.
            strErrMsg = ResolveResString(resUNEXPECTED)
        Case Else
            strErrMsg = ResolveResString(resUNEXPECTED)
        'End Case
    End Select
    
    Dim iRet As Integer
    Dim fAbort As Boolean
    
    fAbort = False
    If lErrSeverity = LogErrOK Then
        ' User can select whether or not to continue
        iRet = MsgFunc(strErrMsg, MB_OKCANCEL Or MB_ICONEXCLAMATION, gstrTitle)
        If gfNoUserInput Then iRet = IDCANCEL ' can't continue if silent install.
        Select Case iRet
            Case IDOK
            Case IDCANCEL
                fAbort = True
            Case Else
                fAbort = True
            'End Case
        End Select
    Else
        ' Fatal
        MsgFunc strErrMsg, MB_OK Or MB_ICONEXCLAMATION, gstrTitle
        fAbort = True
    End If

    If fAbort Then
        ExitSetup frmCopy, gintRET_ABORT
    End If

    fRecursive = False

End Sub

'-----------------------------------------------------------
' FUNCTION: fWithinAction
'
' Returns TRUE iff there is a current Action
'-----------------------------------------------------------
'
Function fWithinAction() As Boolean
    fWithinAction = fDllWithinAction()
End Function

