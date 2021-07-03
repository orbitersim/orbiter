Attribute VB_Name = "modMain"
' Scrawl
' DirectInput Sample
'
' This sample application demonstrates use of the mouse in exclusive mode and how to use
' event notification for retrieving input data.
'
' Hold down the left button to draw. Click the right button or press the AppMenu key
' to bring up a context menu.
'
' An important issue in using exclusive mode is being able to release and reacquire the mouse
' as needed, so that the system cursor can be used. Any exclusive-mode app is forced to release
' the mouse when the user switches to another window by Alt+Tab. In addition, Scrawl surrenders
' the mouse so that the user can navigate the context menu. Reacquisition occurs in the
' MouseMove event, which is called only when Windows has the mouse.
'
' The context menu allows the user to set the mouse sensitivity, since DirectInput ignores any
' such settings in Control Panel.
'
' Choosing Suspend from the menu releases the system cursor and prevents
' the application from reacquiring till the user clicks on the client area.
'
' The sample also demonstrates how to subclass a window in order to intercept Windows messages
' that are not otherwise available in a Visual Basic app. In this case, we want to get the
' WM_ENTERMENULOOP message, so that we can release the mouse and get the
' system cursor when the user opens the system menu by pressing Alt+Spacebar. Note that
' subclassing can make debugging difficult. If you want to play around with this code and debug it,
' comment out the indicated line in Sub Main.

Option Explicit

Public objDX As New DirectX7
Public objDXEvent As DirectXEvent
Public objDI As DirectInput
Public objDIDev As DirectInputDevice

Public g_cursorx As Long
Public g_cursory As Long
Public g_Sensitivity
Public Const BufferSize = 10

Public EventHandle As Long
Public Drawing As Boolean
Public Suspended As Boolean

Public procOld As Long

' Windows API declares and constants

Public Const GWL_WNDPROC = (-4)
Public Const WM_ENTERMENULOOP = &H211
Public Const WM_EXITMENULOOP = &H212
Public Const WM_SYSCOMMAND = &H112

Public Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Public Declare Function SetCursorPos Lib "user32" (ByVal x As Long, ByVal y As Long) As Long
Public Declare Function ScreenToClient Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long
Public Declare Function ClientToScreen Lib "user32" (ByVal hWnd As Long, lpPoint As POINTAPI) As Long
Public Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hWnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long

Public Type POINTAPI
        x As Long
        y As Long
End Type


Sub Main()

  ' Show the main form first so we can use its window handle
  frmCanvas.Show
  
  ' Comment out the following line if you want to do any debugging. It subclasses
  ' the drawing window so that we can intercept Windows messages.
  procOld = SetWindowLong(frmCanvas.hWnd, GWL_WNDPROC, AddressOf SysMenuProc)

  ' Initialize our private cursor
  g_cursorx = frmCanvas.ScaleWidth \ 2
  g_cursory = frmCanvas.ScaleHeight \ 2
  g_Sensitivity = 2
  frmCanvas.mnuSpeed2.Checked = True
  
  ' Create DirectInput and set up the mouse
  Set objDI = objDX.DirectInputCreate
  Set objDIDev = objDI.CreateDevice("guid_SysMouse")
  Call objDIDev.SetCommonDataFormat(DIFORMAT_MOUSE)
  Call objDIDev.SetCooperativeLevel(frmCanvas.hWnd, DISCL_FOREGROUND Or DISCL_EXCLUSIVE)
  
  ' Set the buffer size
  Dim diProp As DIPROPLONG
  diProp.lHow = DIPH_DEVICE
  diProp.lObj = 0
  diProp.lData = BufferSize
  diProp.lSize = Len(diProp)
  Call objDIDev.SetProperty("DIPROP_BUFFERSIZE", diProp)

  ' Ask for notifications
  
  EventHandle = objDX.CreateEvent(frmCanvas)
  Call objDIDev.SetEventNotification(EventHandle)
  
  ' Acquire the mouse
  frmCanvas.AcquireMouse
  
End Sub

Public Function SysMenuProc(ByVal hWnd As Long, ByVal iMsg As Long, _
        ByVal wParam As Long, ByVal lParam As Long) As Long

' This procedure intercepts Windows messages and looks for any that might encourage us
' to Unacquire the mouse.

  If iMsg = WM_ENTERMENULOOP Then
    objDIDev.Unacquire
    frmCanvas.SetSystemCursor
  End If
  
  ' Call the default window procedure
  SysMenuProc = CallWindowProc(procOld, hWnd, iMsg, wParam, lParam)

End Function
