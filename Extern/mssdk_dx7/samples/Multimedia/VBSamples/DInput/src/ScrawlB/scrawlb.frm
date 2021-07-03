VERSION 5.00
Begin VB.Form frmCanvas 
   AutoRedraw      =   -1  'True
   BackColor       =   &H80000005&
   Caption         =   "Visual Basic Scrawl Sample"
   ClientHeight    =   6150
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   9990
   Icon            =   "ScrawlB.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   410
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   666
   StartUpPosition =   2  'CenterScreen
   Begin VB.Image imgPencil 
      Appearance      =   0  'Flat
      Height          =   480
      Left            =   840
      Picture         =   "ScrawlB.frx":0442
      Top             =   600
      Width           =   480
   End
   Begin VB.Menu mnuContext 
      Caption         =   "none"
      Visible         =   0   'False
      Begin VB.Menu mnuSpeed1 
         Caption         =   "Speed 1"
      End
      Begin VB.Menu mnuSpeed2 
         Caption         =   "Speed 2"
      End
      Begin VB.Menu mnuSpeed3 
         Caption         =   "Speed 3"
      End
      Begin VB.Menu Sep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuClear 
         Caption         =   "Clear"
      End
      Begin VB.Menu Sep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSuspend 
         Caption         =   "Suspend"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "Exit"
      End
   End
End
Attribute VB_Name = "frmCanvas"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Implements DirectXEvent


Dim Suspended As Boolean

Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)
  Dim diDeviceData(1 To BufferSize) As DIDEVICEOBJECTDATA
  Dim NumItems As Integer
  Dim i As Integer
  Dim windowRect As RECT
  Static OldSequence As Long
    
    On Error GoTo INPUTLOST
    NumItems = objDIDev.GetDeviceData(diDeviceData, 0)
    For i = 1 To NumItems
      Select Case diDeviceData(i).lOfs
        Case DIMOFS_X
           g_cursorx = g_cursorx + diDeviceData(i).lData * g_Sensitivity
           
           ' We don't want to update the cursor or draw a line is response to
           ' separate axis movements, or we will get a staircase instead of diagonal lines.
           ' A diagonal movement of the mouse results in two events with the same sequence number.
           If OldSequence <> diDeviceData(i).lSequence Then
             UpdateCursor
           End If
           OldSequence = diDeviceData(i).lSequence
         
         Case DIMOFS_Y
           g_cursory = g_cursory + diDeviceData(i).lData * g_Sensitivity
           If OldSequence <> diDeviceData(i).lSequence Then
             UpdateCursor
           End If
           OldSequence = diDeviceData(i).lSequence
        
         Case DIMOFS_BUTTON0
           If diDeviceData(i).lData And &H80 Then
             Drawing = True
             CurrentX = g_cursorx
             CurrentY = g_cursory
           Else
             Drawing = False
           End If
           
         Case DIMOFS_BUTTON1
           If diDeviceData(i).lData = 0 Then  ' button up
             objDIDev.Unacquire
             
             ' Get the system cursor into the same position as the private cursor
             Call GetWindowRect(hwnd, windowRect)
             Call SetCursorPos(g_cursorx + windowRect.Left, g_cursory + windowRect.Top)
             
             ' Pop up menu at that position
             Call PopupMenu(mnuContext)
           End If
           
         
       End Select
    Next i
  Exit Sub
  
INPUTLOST:
' Since no events are signalled if the device is not acquired, this can only happen
' if the device is lost between signalling and retrieval.

  If Err.Number = DIERR_INPUTLOST Then
    objDIDev.Acquire
  Else
    Exit Sub
  End If
    
End Sub


Public Sub UpdateCursor()

  If g_cursorx < 1 Then g_cursorx = 1
  If g_cursorx >= Canvas.ScaleWidth Then g_cursorx = Canvas.ScaleWidth - 1
  If g_cursory < 1 Then g_cursory = 1
  If g_cursory >= Canvas.ScaleHeight Then g_cursory = Canvas.ScaleHeight - 1
  Canvas.imgPencil.Left = g_cursorx
  Canvas.imgPencil.Top = g_cursory
  If Drawing Then
    Line -(g_cursorx, g_cursory)
  
  End If
End Sub

Private Sub Form_Click()
' Allow user to resume drawing after suspending

  Suspended = False
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
' This is a bit of a kludge. We need a way to force acquisition of the mouse whenever
' the context menu is closed, whenever we switch back to the application, or in any other
' circumstance where Windows is finished with the cursor. If a MouseMove event happens,
' we know the cursor is in our app window and Windows is generating mouse messages, therefore
' it's time to reacquire.
' Note: this event appears to happen even when there's no mouse activity, e.g. we have just
' Alt+Tabbed back, or cancelled out of the context menu with the Esc key.

  If Suspended Then Exit Sub    ' Allow use of Windows cursor
  On Error Resume Next
  objDIDev.Acquire
End Sub

Private Sub mnuClear_Click()
   Cls
End Sub

Private Sub mnuExit_Click()
  End
End Sub

Private Sub mnuSpeed1_Click()
  g_Sensitivity = 1
  mnuSpeed1.Checked = True
  mnuSpeed2.Checked = False
  mnuSpeed3.Checked = False
  objDIDev.Acquire
End Sub

Private Sub mnuSpeed2_Click()
  g_Sensitivity = 2
  mnuSpeed2.Checked = True
  mnuSpeed1.Checked = False
  mnuSpeed3.Checked = False
  objDIDev.Acquire
End Sub

Private Sub mnuSpeed3_Click()
  g_Sensitivity = 3
  mnuSpeed3.Checked = True
  mnuSpeed1.Checked = False
  mnuSpeed2.Checked = False
  objDIDev.Acquire
End Sub

Private Sub mnuSuspend_Click()
  Suspended = True
  objDIDev.Unacquire
End Sub
