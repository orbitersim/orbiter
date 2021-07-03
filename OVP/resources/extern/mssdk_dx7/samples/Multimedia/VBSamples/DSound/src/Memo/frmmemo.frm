VERSION 5.00
Begin VB.Form frmMemo 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Current Memo"
   ClientHeight    =   3765
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   4800
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3765
   ScaleWidth      =   4800
   ShowInTaskbar   =   1  'True
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame fraMemo 
      Height          =   3615
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   4575
      Begin VB.CommandButton cmdLister 
         Caption         =   "Listen to Record"
         Height          =   375
         Left            =   240
         TabIndex        =   8
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton cmdSave 
         Caption         =   "Save Memo"
         Height          =   375
         Left            =   240
         TabIndex        =   7
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton cmdDelete 
         Caption         =   "Delete Memo"
         Height          =   375
         Left            =   240
         TabIndex        =   6
         Top             =   2880
         Width           =   1575
      End
      Begin VB.CommandButton cmdRecord 
         Caption         =   "Record Memo"
         Height          =   375
         Left            =   240
         TabIndex        =   5
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play Memo"
         Height          =   375
         Left            =   240
         TabIndex        =   4
         Top             =   960
         Width           =   1575
      End
      Begin VB.ListBox lstMemos 
         Height          =   2400
         Left            =   2040
         TabIndex        =   3
         Top             =   960
         Width           =   2415
      End
      Begin VB.Label lblDate 
         Caption         =   "Label2"
         Height          =   375
         Left            =   720
         TabIndex        =   2
         Top             =   240
         Width           =   3735
      End
      Begin VB.Label lblDatelbl 
         AutoSize        =   -1  'True
         Caption         =   "Date:"
         Height          =   195
         Left            =   240
         TabIndex        =   1
         Top             =   240
         Width           =   390
      End
   End
End
Attribute VB_Name = "frmMemo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdDelete_Click()
    If lstMemos.Text <> "" And lstMemos.Text <> "NO CURRENT MEMOS" Then
        Call DEL_REC(lstMemos.Text)
        R_LST
    End If
End Sub

Private Sub cmdLister_Click()
    Call Play_Sound
End Sub

Private Sub cmdPlay_Click()
    If lstMemos.Text <> "" Then Play_Sound_From_db lstMemos.Text, hWnd
End Sub

Private Sub cmdRecord_Click()
    Call record_memo(hWnd)
End Sub

Private Sub cmdSave_Click()
    Call SaveToDB
    Call R_LST
End Sub
Private Sub R_LST()
    lstMemos.Clear
    ''''''''''''''''''''''' memos?
    If Not HAS_MEMO(M, D, Y) Then
        lstMemos.AddItem "NO CURRENT MEMOS"
    Else
            If rs Is Nothing Then INITDB
            
            rs.MoveFirst
            Do While Not rs.EOF
                If rs!Month = M Then
                    If rs!Day = D Then
                        If rs!Year = Y Then
                            lstMemos.AddItem rs!Name '& " :" & rs!tm
                        End If
                    End If
                End If
                rs.MoveNext
            Loop
            
    End If
End Sub
Private Sub Form_Load()
    ''''''''''''''''''''''' cal
    lblDate.Caption = M & "/" & D & "/" & Y
    
    Call R_LST

End Sub


