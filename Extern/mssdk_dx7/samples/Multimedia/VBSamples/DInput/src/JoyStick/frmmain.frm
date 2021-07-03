VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "JoyStick Sample"
   ClientHeight    =   6240
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6000
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6240
   ScaleWidth      =   6000
   StartUpPosition =   3  'Windows Default
   Begin VB.ListBox lstHat 
      Enabled         =   0   'False
      Height          =   1230
      ItemData        =   "frmMain.frx":0442
      Left            =   4080
      List            =   "frmMain.frx":0444
      TabIndex        =   6
      Top             =   3240
      Width           =   1695
   End
   Begin VB.ListBox lstButton 
      Enabled         =   0   'False
      Height          =   2790
      ItemData        =   "frmMain.frx":0446
      Left            =   2160
      List            =   "frmMain.frx":0448
      TabIndex        =   2
      Top             =   3240
      Width           =   1695
   End
   Begin VB.ListBox lstJoyAxis 
      Enabled         =   0   'False
      Height          =   2790
      ItemData        =   "frmMain.frx":044A
      Left            =   240
      List            =   "frmMain.frx":044C
      TabIndex        =   1
      Top             =   3240
      Width           =   1695
   End
   Begin VB.ListBox lstJoySticks 
      Height          =   1815
      ItemData        =   "frmMain.frx":044E
      Left            =   240
      List            =   "frmMain.frx":0450
      TabIndex        =   0
      Top             =   720
      Width           =   5535
   End
   Begin VB.Label lblHats 
      Caption         =   "POVs"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   4080
      TabIndex        =   7
      Top             =   2760
      Width           =   1335
   End
   Begin VB.Label lblButtons 
      Caption         =   "Buttons"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   2160
      TabIndex        =   5
      Top             =   2760
      Width           =   1575
   End
   Begin VB.Label lblAxis 
      Caption         =   "Axes"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   240
      TabIndex        =   4
      Top             =   2760
      Width           =   1335
   End
   Begin VB.Label lblJoy 
      Caption         =   "Joysticks"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   240
      TabIndex        =   3
      Top             =   120
      Width           =   4215
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Implements DirectXEvent

Dim dx As New DirectX7
Dim di As DirectInput
Dim diDev As DirectInputDevice
Dim diDevEnum As DirectInputEnumDevices
Dim EventHandle As Long
Dim joyCaps As DIDEVCAPS
Dim js As DIJOYSTATE
Dim DiProp_Dead As DIPROPLONG
Dim DiProp_Range As DIPROPRANGE
Dim DiProp_Saturation As DIPROPLONG
Dim AxisPresent(1 To 8) As Boolean
Dim running As Boolean

Sub InitDirectInput()
    
    Set di = dx.DirectInputCreate()
    Set diDevEnum = di.GetDIEnumDevices(DIDEVTYPE_JOYSTICK, DIEDFL_ATTACHEDONLY)
    If diDevEnum.GetCount = 0 Then
      MsgBox "No joystick attached."
      Unload Me
    End If
    
    'Add attached joysticks to the listbox
    Dim i As Integer
    For i = 1 To diDevEnum.GetCount
        Call lstJoySticks.AddItem(diDevEnum.GetItem(i).GetInstanceName)
    Next
    
    ' Get an event handle to associate with the device
    EventHandle = dx.CreateEvent(Me)
    Exit Sub
    
Error_Out:
    MsgBox "Error initializing DirectInput."
    Unload Me
    
End Sub


Private Sub DirectXEvent_DXCallback(ByVal eventid As Long)

' This is called whenever there's a change in the joystick state.
' We check the new state and update the display.


    Dim i As Integer
    Dim ListPos As Integer
    Dim S As String
    
    If diDev Is Nothing Then Exit Sub
        
    '' Get the device info
    On Local Error Resume Next
    diDev.GetDeviceStateJoystick js
    If Err.Number = DIERR_NOTACQUIRED Or Err.Number = DIERR_INPUTLOST Then
        diDev.Acquire
        Exit Sub
    End If
    
    
    On Error GoTo err_out
    
    ' Display axis coordinates
    ListPos = 0
    For i = 1 To 8
        If AxisPresent(i) Then
           Select Case i
               Case 1
                   S = "X: " & js.x
               Case 2
                   S = "Y: " & js.y
               Case 3
                   S = "Z: " & js.z
               Case 4
                   S = "RX: " & js.rx
               Case 5
                   S = "RY: " & js.ry
               Case 6
                   S = "RZ: " & js.rz
               Case 7
                   S = "Slider0: " & js.slider(0)
               Case 8
                   S = "Slider1: " & js.slider(1)
      
           End Select
           lstJoyAxis.List(ListPos) = S
           ListPos = ListPos + 1
        
        End If
     Next
    
    ' Buttons
    
    For i = 0 To joyCaps.lButtons - 1
        Select Case js.buttons(i)
        Case 0
            lstButton.List(i) = "Button " + CStr(i + 1) + ": Up"
            
        Case Else
            lstButton.List(i) = "Button " + CStr(i + 1) + ": Down"
            
        End Select
    Next
        
     ' Hats
    For i = 0 To joyCaps.lPOVs - 1
        lstHat.List(i) = "POV " + CStr(i + 1) + ": " + CStr(js.POV(i))
    Next
    
    Me.Caption = "Joystick Sample: Available"

    Exit Sub
    
err_out:
    MsgBox Err.Description & " : " & Err.Number, vbApplicationModal
    End

End Sub

Private Sub Form_Load()
    running = True
    InitDirectInput
End Sub


Private Sub Form_Unload(cancel As Integer)
        If EventHandle <> 0 Then dx.DestroyEvent EventHandle
        running = False
        DoEvents
        End
End Sub

Private Sub lstJoySticks_Click()

    
    On Local Error Resume Next
    
    Call CLRLISTS
    
    'Create the joystick device
    Set diDev = Nothing
    Set diDev = di.CreateDevice(diDevEnum.GetItem(lstJoySticks.ListIndex + 1).GetGuidInstance)
    diDev.SetCommonDataFormat DIFORMAT_JOYSTICK
    diDev.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
    
    ' Find out what device objects it has
    diDev.GetCapabilities joyCaps
    Call IdentifyAxes(diDev)
    
    ' Ask for notification of events
    Call diDev.SetEventNotification(EventHandle)

    ' Set deadzone for X and Y axis to 10 percent of the range of travel
    With DiProp_Dead
        .lData = 1000
        .lObj = DIJOFS_X
        .lSize = Len(DiProp_Dead)
        .lHow = DIPH_BYOFFSET
        .lObj = DIJOFS_X
        diDev.SetProperty "DIPROP_DEADZONE", DiProp_Dead
        .lObj = DIJOFS_Y
        diDev.SetProperty "DIPROP_DEADZONE", DiProp_Dead
    End With
    
    ' Set saturation zones for X and Y axis to 5 percent of the range
    With DiProp_Saturation
        .lData = 9500
        .lHow = DIPH_BYOFFSET
        .lSize = Len(DiProp_Saturation)
        .lObj = DIJOFS_X
         diDev.SetProperty "DIPROP_SATURATION", DiProp_Saturation
        .lObj = DIJOFS_Y
         diDev.SetProperty "DIPROP_SATURATION", DiProp_Saturation
    End With
    
    SetProp
    
    
    diDev.Acquire
    Me.Caption = "Joystick Sample: Querying Properties"
    
    ' Get the list of current properties
    ' USB joysticks wont call this callback until you play with the joystick
    ' so we call the callback ourselves the first time
    DirectXEvent_DXCallback 0
    
    ' Poll the device so that events are sure to be signaled.
    ' Usually this would be done in Sub Main or in the game rendering loop.
    
    While running = True
        DoEvents
        diDev.Poll
    Wend
End Sub

Sub SetProp()
    ' Set range for all axes
    With DiProp_Range
        .lHow = DIPH_DEVICE
        .lSize = Len(DiProp_Range)
        .lMin = 0
        .lMax = 10000
    End With
    diDev.SetProperty "DIPROP_RANGE", DiProp_Range
End Sub

Sub CLRLISTS()
    lstJoyAxis.Clear
    lstButton.Clear
    lstHat.Clear
End Sub

Sub IdentifyAxes(diDev As DirectInputDevice)

   ' It's not enough to count axes; we need to know which in particular
   ' are present.
   
   Dim didoEnum As DirectInputEnumDeviceObjects
   Dim dido As DirectInputDeviceObjectInstance
   Dim i As Integer
   
   For i = 1 To 8
     AxisPresent(i) = False
   Next
   
   ' Enumerate the axes
   Set didoEnum = diDev.GetDeviceObjectsEnum(DIDFT_AXIS)
   
   ' Check data offset of each axis to learn what it is
   
   For i = 1 To didoEnum.GetCount
     Set dido = didoEnum.GetItem(i)
         Select Case dido.GetOfs
            Case DIJOFS_X
              AxisPresent(1) = True
            Case DIJOFS_Y
              AxisPresent(2) = True
            Case DIJOFS_Z
              AxisPresent(3) = True
            Case DIJOFS_RX
              AxisPresent(4) = True
            Case DIJOFS_RY
              AxisPresent(5) = True
            Case DIJOFS_RZ
              AxisPresent(6) = True
            Case DIJOFS_SLIDER0
              AxisPresent(7) = True
            Case DIJOFS_SLIDER1
              AxisPresent(8) = True
         End Select
 
   Next
End Sub


