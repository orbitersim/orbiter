VERSION 5.00
Begin VB.Form frmPath 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "#"
   ClientHeight    =   4710
   ClientLeft      =   150
   ClientTop       =   1530
   ClientWidth     =   5955
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "path.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4710
   ScaleWidth      =   5955
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   4170
      MaskColor       =   &H00000000&
      TabIndex        =   7
      Top             =   2640
      Width           =   1560
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "#"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   4170
      MaskColor       =   &H00000000&
      TabIndex        =   6
      Top             =   1890
      Width           =   1560
   End
   Begin VB.DriveListBox drvDrives 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Left            =   216
      TabIndex        =   5
      Top             =   4140
      Width           =   3510
   End
   Begin VB.DirListBox dirDirs 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1605
      Left            =   204
      TabIndex        =   3
      Top             =   1896
      Width           =   3510
   End
   Begin VB.TextBox txtPath 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   288
      Left            =   204
      MaxLength       =   240
      TabIndex        =   1
      Top             =   1056
      Width           =   5532
   End
   Begin VB.Label lblDrives 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   210
      TabIndex        =   4
      Top             =   3870
      Width           =   105
   End
   Begin VB.Label lblDirs 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   210
      TabIndex        =   2
      Top             =   1590
      Width           =   105
   End
   Begin VB.Label lblPath 
      AutoSize        =   -1  'True
      Caption         =   "#"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   210
      TabIndex        =   0
      Top             =   750
      Width           =   105
   End
   Begin VB.Label lblPrompt 
      AutoSize        =   -1  'True
      Caption         =   "*"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   204
      TabIndex        =   8
      Top             =   204
      Width           =   5532
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "frmPath"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_TemplateDerived = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'
' Form/Module Variables
'
Dim mfMustExist As Integer
Dim mfCancelExit As Integer

Private Sub cmdCancel_Click()
    If mfCancelExit = True Then
        ExitSetup Me, gintRET_EXIT
    Else
        gfRetVal = gintRET_CANCEL
        Unload Me
    End If
End Sub

Private Sub cmdOK_Click()
    Dim strPathName As String
    Dim strMsg As String
    Dim intRet As Integer

    SetMousePtr gintMOUSE_HOURGLASS

    strPathName = ResolveDir(txtPath.Text, mfMustExist, True)

    If strPathName <> gstrNULL Then
        If frmSetup1.Tag = gstrDIR_DEST And strPathName <> gstrDestDir Then
            If DirExists(strPathName) = False Then
                strMsg = ResolveResString(resDESTDIR) & LS$ & strPathName
                strMsg = strMsg & LS$ & ResolveResString(resCREATE)
                intRet = MsgFunc(strMsg, MB_YESNO Or MB_ICONQUESTION, gstrTitle)
                If gfNoUserInput = True Then
                    ExitSetup Me, gintRET_FATAL
                End If
                If intRet = IDNO Then
                    txtPath.SetFocus
                    SetMousePtr gintMOUSE_DEFAULT
                    Exit Sub
                End If
            End If

            If IsValidDestDir(strPathName) = False Then
                txtPath.SetFocus
                SetMousePtr gintMOUSE_DEFAULT
                Exit Sub
            End If
        End If

        frmSetup1.Tag = strPathName
        gfRetVal = gintRET_CONT
        Unload Me
    Else
        txtPath.SetFocus
    End If

    SetMousePtr gintMOUSE_DEFAULT
End Sub

Private Sub dirDirs_Change()
    Static intBusy As Integer

    On Error Resume Next

    If intBusy = False Then
        intBusy = True

        ChDir dirDirs.Path

        If Err = 0 Then
            txtPath.Text = dirDirs.Path
            drvDrives.Drive = Left$(dirDirs.Path, 2)
        Else
            Err = 0
        End If

        intBusy = False
    End If
End Sub

Private Sub drvDrives_Change()
    Static strOldDrive As String
    Static intBusy As Integer

    Dim strDrive As String

    If intBusy = False Then
        intBusy = True

        strDrive = drvDrives.Drive

        If CheckDrive(strDrive, Me.Caption) = True Then
            strOldDrive = strDrive
            dirDirs.Path = strDrive
        Else
            drvDrives.Drive = strOldDrive
        End If

        intBusy = False
    End If
End Sub

Private Sub Form_Load()
    On Error Resume Next

    SetMousePtr gintMOUSE_HOURGLASS

    SetFormFont Me
    cmdOK.Caption = ResolveResString(resBTNOK)
    lblDrives.Caption = ResolveResString(resLBLDRIVES)
    lblDirs.Caption = ResolveResString(resLBLDIRS)
    lblPath.Caption = ResolveResString(resLBLPATH)
    
    If frmSetup1.Tag = gstrDIR_SRC Then
        Caption = ResolveResString(resINSTFROM)
        lblPrompt.Caption = ResolveResString(resSRCPROMPT, "|1", gstrAppName)
        cmdCancel.Caption = ResolveResString(resBTNEXIT, "|1", gstrAppName)
        mfCancelExit = True
        dirDirs.Path = gstrSrcPath
        If Err > 0 Then
            dirDirs.Path = Left$(App.Path, 3)
        End If
        mfMustExist = True
    Else
        Caption = ResolveResString(resCHANGEDIR)
        lblPrompt.Caption = ResolveResString(resDESTPROMPT)
        cmdCancel.Caption = ResolveResString(resBTNCANCEL)
        mfCancelExit = False
        dirDirs.Path = gstrDestDir
        If Err > 0 Then
            'Next try root of destination drive
            If Len(gstrDestDir) >= 2 Then
                If Mid$(gstrDestDir, 2, 1) = gstrCOLON Then
                    Err = 0
                    dirDirs.Path = Left$(gstrDestDir, 2) & gstrSEP_DIR
                End If
            End If
        End If
        If Err > 0 Then
            dirDirs.Path = Left$(App.Path, 3)
        End If
        
        'Init txtPath.Text to gstrDestDir even if this
        '  directory does not (yet) exist.
        txtPath.Text = gstrDestDir
        mfMustExist = False
    End If

    If frmSetup1.Tag = gstrDIR_SRC Then
        txtPath.Text = dirDirs.Path
    End If

    drvDrives.Drive = Left$(dirDirs.Path, 2)
    drvDrives_Change

    SetMousePtr gintMOUSE_DEFAULT

    CenterForm Me

    'Highlight all of txtPath's text so that typing immediately overwrites it
    txtPath.SelStart = 0
    txtPath.SelLength = Len(txtPath.Text)
    
    Err = 0
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode <> 1 Then
        If mfCancelExit = True Then
            ExitSetup Me, gintRET_EXIT
            Cancel = 1
        Else
            gfRetVal = gintRET_CANCEL
            Unload Me
        End If
    End If
End Sub

