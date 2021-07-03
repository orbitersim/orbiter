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
   HasDC           =   0   'False
   Icon            =   "path.frx":0000
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   ScaleHeight     =   4710
   ScaleWidth      =   5955
   ShowInTaskbar   =   0   'False
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
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private mfMustExist As Integer
Private mfCancelExit As Integer

Private mfSinkEvents As Boolean

Private Sub cmdCancel_Click()
    If mfCancelExit Then
        ExitSetup Me, gintRET_EXIT
    Else
        gintRetVal = gintRET_CANCEL
        Unload Me
    End If
End Sub

Private Sub cmdOK_Click()
    Dim strPathName As String
    Dim strMsg As String
    Dim intRet As Integer

    SetMousePtr vbHourglass

    strPathName = ResolveDir(txtPath.Text, mfMustExist, True)

    If Len(strPathName) > 0 Then
        ' Avoid Option Compare Text and use explicit UCase comparisons because there
        ' is a Unicode character (&H818F) which is equal to a path separator when
        ' using Option Compare Text.
        If UCase$(strPathName) <> UCase$(gstrDestDir) Then
            If Not DirExists(strPathName) Then
                strMsg = ResolveResString(resDESTDIR) & vbLf & vbLf & strPathName
                strMsg = strMsg & vbLf & vbLf & ResolveResString(resCREATE)
                intRet = MsgFunc(strMsg, vbYesNo Or vbQuestion, gstrTitle)
                If gfNoUserInput Then
                    ExitSetup Me, gintRET_FATAL
                End If
                If intRet = vbNo Then
                    txtPath.SetFocus
                    SetMousePtr vbDefault
                    Exit Sub
                End If
            End If

            If Not IsValidDestDir(strPathName) Then
                txtPath.SetFocus
                SetMousePtr vbDefault
                Exit Sub
            End If
        End If

        frmSetup1.Tag = strPathName
        gintRetVal = gintRET_CONT
        Unload Me
    Else
        txtPath.SetFocus
    End If

    SetMousePtr vbDefault
End Sub

Private Sub dirDirs_Change()
    If mfSinkEvents Then
        mfSinkEvents = False
        txtPath.Text = dirDirs.Path
        drvDrives.Drive = dirDirs.Path
        mfSinkEvents = True
    End If
End Sub

Private Sub drvDrives_Change()
    Static strOldDrive As String
    Dim strDrive As String

    If mfSinkEvents Then
        mfSinkEvents = False
        If GetDrive(drvDrives.Drive, strDrive) Then
            If CheckDrive(strDrive, Caption) Then
                strOldDrive = strDrive
                dirDirs.Path = strDrive
                txtPath.Text = dirDirs.Path
            Else
                drvDrives.Drive = strOldDrive
            End If
        End If
        mfSinkEvents = True
    End If
End Sub

Private Sub Form_Load()
    Dim strDrive As String

    On Error Resume Next

    mfSinkEvents = False
    SetMousePtr vbHourglass

    SetFormFont Me
    cmdOK.Caption = ResolveResString(resBTNOK)
    lblDrives.Caption = ResolveResString(resLBLDRIVES)
    lblDirs.Caption = ResolveResString(resLBLDIRS)
    lblPath.Caption = ResolveResString(resLBLPATH)

    Caption = ResolveResString(resCHANGEDIR)
    lblPrompt.Caption = ResolveResString(resDESTPROMPT)
    cmdCancel.Caption = ResolveResString(resBTNCANCEL)
    mfCancelExit = False
    dirDirs.Path = gstrDestDir
    If Err.Number <> 0 Then
        'Next try root of destination drive
        If GetDrive(gstrDestDir, strDrive) Then
            Err.Clear
            dirDirs.Path = strDrive
        End If
    End If
    If Err.Number <> 0 Then
        If GetDrive(App.Path, strDrive) Then
            dirDirs.Path = strDrive
        End If
    End If

    GetDrive dirDirs.Path, strDrive
    drvDrives.Drive = strDrive

    mfSinkEvents = True

    'Init txtPath.Text to gstrDestDir even if this
    '  directory does not (yet) exist.
    txtPath.Text = gstrDestDir
    mfMustExist = False

    SetMousePtr vbDefault

    CenterForm Me

    'Highlight all of txtPath's text so that typing immediately overwrites it
    txtPath.SelStart = 0
    txtPath.SelLength = Len(txtPath.Text)
    
    Err.Clear
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode <> vbFormCode Then
        If mfCancelExit Then
            ExitSetup Me, gintRET_EXIT
            Cancel = True
        Else
            gintRetVal = gintRET_CANCEL
            Unload Me
        End If
    End If
End Sub
