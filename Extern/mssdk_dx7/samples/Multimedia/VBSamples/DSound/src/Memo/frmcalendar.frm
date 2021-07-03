VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Begin VB.Form frmCalendar 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "VB MEMO"
   ClientHeight    =   2625
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   2940
   Icon            =   "frmCalendar.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2625
   ScaleWidth      =   2940
   StartUpPosition =   3  'Windows Default
   Begin MSComCtl2.MonthView cal 
      Height          =   2370
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2700
      _ExtentX        =   4763
      _ExtentY        =   4180
      _Version        =   393216
      ForeColor       =   -2147483630
      BackColor       =   -2147483633
      Appearance      =   1
      StartOfWeek     =   24510465
      CurrentDate     =   36320
   End
End
Attribute VB_Name = "frmCalendar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''---------------------------------------------------------------
''This application requires MDAC to be on the system, Please
''ensure MDAC is on the system or this sample will not run
''---------------------------------------------------------------


Private Sub cal_DateDblClick(ByVal DateDblClicked As Date)
    
    D = cal.Day
    M = cal.Month
    Y = cal.Year
    
    frmMemo.Show 1
    
End Sub


Private Sub cal_Mouseup(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Call BoldDays(cal)
End Sub



Private Sub Form_Activate()
    
    cal.ShowToday = True
    Call cal_Mouseup(1, 0, 0, 0)
    
End Sub

Private Sub Form_Load()
    
    If rs Is Nothing Then Call INITDB
    
    Show
    cal.Value = Date
    DoEvents
End Sub


Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Call CLEANUPSND
    End
End Sub

