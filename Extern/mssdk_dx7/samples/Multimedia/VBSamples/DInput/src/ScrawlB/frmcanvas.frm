VERSION 5.00
Begin VB.Form frmCanvas 
   AutoRedraw      =   -1  'True
   BackColor       =   &H80000005&
   Caption         =   "Visual Basic Scrawl Sample"
   ClientHeight    =   6150
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   9990
   Icon            =   "frmCanvas.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   410
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   666
   StartUpPosition =   2  'CenterScreen
   Begin VB.Image imgPencil 
      Appearance      =   0  'Flat
      Height          =   480
      Left            =   0
      Picture         =   "frmCanvas.frx":0442
      Top             =   0
      Width           =   480
   End
   Begin VB.Menu mnuContext 
      Caption         =   "none"
      Visible         =   0   'False
      Begin VB.Menu mnuAbout 
         Caption         =   "About..."
      End
      Begin VB.Menu Sep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSpeed1 
         Caption         =   "Speed 1"
      End
      Begin VB.Menu mnuSpeed2 
         Caption         =   "Speed 2"
      End
      Begin VB.Menu mnuSpeed3 
         Caption         =   "Speed 3"
      End
      Begin VB.Menu Sep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuClear 
         Caption         =   "Clear"
      End
      Begin VB.Menu Sep3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuSuspend 
         Caption         =   "Release Mouse"
      End
   End
End
Attribute VB_Name = "frmCanvas"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements DirectXEvent

Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)

' This is where we respond to any change in mouse state. Usually this will be an axis movement
' or button press or release, but it could also mean we've lost acquisition.
' Note: no event is signalled if we voluntarily Unacquire. Normally loss of acquisition will
' mean that the app window has lost the focus.

  Dim diDeviceData(1 To BufferSize) As DIDEVICEOBJECTDATA
  Dim NumItems As Integer
  Dim i As Integer
  Static OldSequence As Long
  
  ' Get data
  On Error GoTo INPUTLOST
  NumItems = objDIDev.GetDeviceData(diDeviceData, 0)
  On Error GoTo 0
  
  ' Process data
  For i = 1 To NumItems
    Select Case diDeviceData(i).lOfs
      Case DIMOFS_X
        g_cursorx = g_cursorx + diDeviceData(i).lData * g_Sensitivity
           
        ' We don't want to update the cursor or draw a line is response to
        ' separate axis movements, or we will get a staircase instead of diagonal lines.
        ' A diagonal movement of the mouse results in two events with the same sequence number.
        ' In order to avoid postponing the last event till the mouse moves again, we always
        ' reset OldSequence after it's been tested once.
          
        If OldSequence <> diDeviceData(i).lSequence Then
          UpdateCursor
          OldSequence = diDeviceData(i).lSequence
        Else
          OldSequence = 0
        End If
         
      Case DIMOFS_Y
        g_cursory = g_cursory + diDeviceData(i).lData * g_Sensitivity
        If OldSequence <> diDeviceData(i).lSequence Then
          UpdateCursor
          OldSequence = diDeviceData(i).lSequence
        Else
          OldSequence = 0
        End If
        
      Case DIMOFS_BUTTON0
        If diDeviceData(i).lData And &H80 Then
          Drawing = True
           
          ' Keep record for Line function
          CurrentX = g_cursorx
          CurrentY = g_cursory
           
          ' Draw a point in case button-up follows immediately
          PSet (g_cursorx, g_cursory)
        Else
          Drawing = False
        End If
           
      Case DIMOFS_BUTTON1
        If diDeviceData(i).lData = 0 Then  ' button up
          Popup
        End If
        
    End Select
  Next i
  Exit Sub
  
INPUTLOST:
  ' Windows stole the mouse from us. DIERR_INPUTLOST is raised if the user switched to
  ' another app, but DIERR_NOTACQUIRED is raised if the Windows key was pressed.
  If (Err.Number = DIERR_INPUTLOST) Or (Err.Number = DIERR_NOTACQUIRED) Then
    SetSystemCursor
    Exit Sub
  End If
    
End Sub


Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  
  Select Case KeyCode
    Case 93         ' AppMenu key
      Popup
      
    End Select

End Sub

Private Sub Form_Unload(Cancel As Integer)

  ' Restore the default window procedure
  If procOld <> 0 Then
    Call SetWindowLong(hWnd, GWL_WNDPROC, procOld)
  End If

  If EventHandle <> 0 Then objDX.DestroyEvent EventHandle
  
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)

  Dim didevstate As DIMOUSESTATE
  
  ' We want to force acquisition of the mouse whenever the context menu is closed,
  ' whenever we switch back to the application, or in any other circumstance where
  ' Windows is finished with the cursor. If a MouseMove event happens,
  ' we know the cursor is in our app window and Windows is generating mouse messages, therefore
  ' it's time to reacquire.

  ' Note: this event is triggered whenever the window gets the mouse, even when there's no mouse
  ' activity -- for example, when we have just Alt+Tabbed back, or cancelled out of the context
  ' menu with the Esc key.

   If Suspended Then Exit Sub    ' Allow continued use of Windows cursor
  
  ' This event gets called again after we acquire the mouse. In order to prevent the cursor
  ' position being set to the middle of the window, we check to see if we've already acquired,
  ' and if so, we don't reposition our private cursor. The only way to find out if the mouse
  ' is acquired is to try to retrieve data.
  
  On Error GoTo NOTYETACQUIRED
  Call objDIDev.GetDeviceStateMouse(didevstate)
  On Error GoTo 0
  Exit Sub
  
NOTYETACQUIRED:
  Call AcquireMouse
End Sub

Sub AcquireMouse()

  Dim CursorPoint As POINTAPI
  
  ' Move private cursor to system cursor.
  Call GetCursorPos(CursorPoint)  ' Get position before Windows loses cursor
  Call ScreenToClient(hWnd, CursorPoint)
  
  On Error GoTo CANNOTACQUIRE
  objDIDev.Acquire
  g_cursorx = CursorPoint.x
  g_cursory = CursorPoint.y
  
  UpdateCursor
  frmCanvas.imgPencil.Visible = True
  On Error GoTo 0
  Exit Sub

CANNOTACQUIRE:
  Exit Sub
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)

  ' Allows user to resume by clicking on the canvas.
  If Button = 1 Then Suspended = False
  
End Sub

Private Sub mnuAbout_Click()

  Call frmAbout.Show(vbModal, Me)
  
End Sub

Private Sub mnuClear_Click()

   Cls
   
End Sub



Private Sub mnuSpeed1_Click()

  g_Sensitivity = 1
  mnuSpeed1.Checked = True
  mnuSpeed2.Checked = False
  mnuSpeed3.Checked = False
  
End Sub

Private Sub mnuSpeed2_Click()

  g_Sensitivity = 2
  mnuSpeed2.Checked = True
  mnuSpeed1.Checked = False
  mnuSpeed3.Checked = False
  
End Sub

Private Sub mnuSpeed3_Click()

  g_Sensitivity = 3
  mnuSpeed3.Checked = True
  mnuSpeed1.Checked = False
  mnuSpeed2.Checked = False
  
End Sub

Private Sub mnuSuspend_Click()

  Suspended = Not Suspended
  imgPencil.Visible = Not Suspended
  
End Sub

Public Sub UpdateCursor()

  ' Update the position of our private cursor
  If g_cursorx < 0 Then g_cursorx = 0
  If g_cursorx >= frmCanvas.ScaleWidth Then g_cursorx = frmCanvas.ScaleWidth - 1
  If g_cursory < 0 Then g_cursory = 0
  If g_cursory >= frmCanvas.ScaleHeight Then g_cursory = frmCanvas.ScaleHeight - 1
  frmCanvas.imgPencil.Left = g_cursorx
  frmCanvas.imgPencil.Top = g_cursory
  If Drawing Then
    Line -(g_cursorx, g_cursory)
  End If
  
End Sub

Public Sub Popup()

  objDIDev.Unacquire
  SetSystemCursor
  Call PopupMenu(mnuContext)
  
End Sub

Public Sub SetSystemCursor()

 ' Get the system cursor into the same position as the private cursor,
 ' and stop drawing
 
  Dim point As POINTAPI
  
  imgPencil.Visible = False
  Drawing = False
  point.x = g_cursorx
  point.y = g_cursory
  Call ClientToScreen(hWnd, point)
  Call SetCursorPos(point.x, point.y)

End Sub
