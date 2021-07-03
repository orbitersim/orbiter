VERSION 5.00
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "DirectInput Keyboard Sample"
   ClientHeight    =   3435
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5880
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3435
   ScaleWidth      =   5880
   StartUpPosition =   3  'Windows Default
   Begin VB.Timer tmrKey 
      Left            =   0
      Top             =   0
   End
   Begin VB.ListBox lstKeys 
      Height          =   2595
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   5655
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'==========================================================
'This sample will show the use of Direct Input with
'the keyboard.
'==========================================================



Dim dx As New DirectX7
Dim di As DirectInput
Dim diDEV As DirectInputDevice
Dim diState As DIKEYBOARDSTATE
Dim iKeyCounter As Integer
Dim aKeys(255) As String


Private Sub Form_Load()

    Set di = dx.DirectInputCreate()
        
    If Err.Number <> 0 Then
        MsgBox "Error starting Direct Input, please make sure you have DirectX installed", vbApplicationModal
        End
    End If
        
        
    Set diDEV = di.CreateDevice("GUID_SysKeyboard")
    
    diDEV.SetCommonDataFormat DIFORMAT_KEYBOARD
    diDEV.SetCooperativeLevel Me.hWnd, DISCL_BACKGROUND Or DISCL_NONEXCLUSIVE
    
    Me.Show
    
    diDEV.Acquire
        
    tmrKey.Interval = 10
    tmrKey.Enabled = True
    

End Sub

Private Sub Form_Unload(Cancel As Integer)
    diDEV.Unacquire
End Sub

Private Sub tmrKey_Timer()
    lstKeys.Clear
    
    diDEV.GetDeviceStateKeyboard diState
    
    For iKeyCounter = 0 To 255
        If diState.Key(iKeyCounter) <> 0 Then
            lstKeys.AddItem KeyNames(iKeyCounter)
        End If
    Next
    DoEvents
End Sub





Function KeyNames(iNum As Integer) As String

    aKeys(1) = "DIK_ESCAPE"
    aKeys(2) = "DIK_1  On main keyboard"
    aKeys(3) = "DIK_2  On main keyboard"
    aKeys(4) = "DIK_3  On main keyboard"
    aKeys(5) = "DIK_4  On main keyboard"
    aKeys(6) = "DIK_5  On main keyboard"
    aKeys(7) = "DIK_6  On main keyboard"
    aKeys(8) = "DIK_7  On main keyboard"
    aKeys(9) = "DIK_8  On main keyboard"
    aKeys(10) = "DIK_9  On main keyboard"
    aKeys(11) = "DIK_0  On main keyboard"
    aKeys(12) = "DIK_MINUS  On main keyboard"
    aKeys(13) = "DIK_EQUALS  On main keyboard"
    aKeys(14) = "DIK_BACK BACKSPACE"
    aKeys(15) = "DIK_TAB"
    aKeys(16) = "DIK_Q"
    aKeys(17) = "DIK_W"
    aKeys(18) = "DIK_E"
    aKeys(19) = "DIK_R"
    aKeys(20) = "DIK_T"
    aKeys(21) = "DIK_Y"
    aKeys(22) = "DIK_U"
    aKeys(23) = "DIK_I"
    aKeys(24) = "DIK_O"
    aKeys(25) = "DIK_P"
    aKeys(26) = "DIK_LBRACKET  ["
    aKeys(27) = "DIK_RBRACKET  ]"
    aKeys(28) = "DIK_RETURN  ENTER on main keyboard"
    aKeys(29) = "DIK_LCONTROL  Left CTRL Key"
    aKeys(30) = "DIK_A"
    aKeys(31) = "DIK_S"
    aKeys(32) = "DIK_D"
    aKeys(33) = "DIK_F"
    aKeys(34) = "DIK_G"
    aKeys(35) = "DIK_H"
    aKeys(36) = "DIK_J"
    aKeys(37) = "DIK_K"
    aKeys(38) = "DIK_L"
    aKeys(39) = "DIK_SEMICOLON"
    aKeys(40) = "DIK_APOSTROPHE"
    aKeys(41) = "DIK_GRAVE  Grave accent (`)"
    aKeys(42) = "DIK_LSHIFT  Left SHIFT"
    aKeys(43) = "DIK_BACKSLASH"
    aKeys(44) = "DIK_Z"
    aKeys(45) = "DIK_X"
    aKeys(46) = "DIK_C"
    aKeys(47) = "DIK_V"
    aKeys(48) = "DIK_B"
    aKeys(49) = "DIK_N"
    aKeys(50) = "DIK_M"
    aKeys(51) = "DIK_COMMA"
    aKeys(52) = "DIK_PERIOD  On main keyboard"
    aKeys(53) = "DIK_SLASH  Forward slash (/)on main keyboard"
    aKeys(54) = "DIK_RSHIFT  Right SHIFT"
    aKeys(55) = "DIK_MULTIPLY  Asterisk on numeric keypad"
    aKeys(56) = "DIK_LMENU  Left ALT"
    aKeys(57) = "DIK_SPACE Spacebar"
    aKeys(58) = "DIK_CAPITAL  CAPS LOCK"
    aKeys(59) = "DIK_F1"
    aKeys(60) = "DIK_F2"
    aKeys(61) = "DIK_F3"
    aKeys(62) = "DIK_F4"
    aKeys(63) = "DIK_F5"
    aKeys(64) = "DIK_F6"
    aKeys(65) = "DIK_F7"
    aKeys(66) = "DIK_F8"
    aKeys(67) = "DIK_F9"
    aKeys(68) = "DIK_F10"
    aKeys(69) = "vDIK_NUMLOCK"
    aKeys(70) = "DIK_SCROLL  SCROLL LOCK"
    aKeys(71) = "DIK_NUMPAD7"
    aKeys(72) = "DIK_NUMPAD8"
    aKeys(73) = "DIK_NUMPAD9"
    aKeys(74) = "DIK_SUBTRACT  Hyphen (minus sign) on numeric keypad"
    aKeys(75) = "DIK_NUMPAD4"
    aKeys(76) = "DIK_NUMPAD5"
    aKeys(77) = "DIK_NUMPAD6"
    aKeys(78) = "DIK_ADD  Plus sign on numeric keypad"
    aKeys(79) = "DIK_NUMPAD1"
    aKeys(80) = "DIK_NUMPAD2"
    aKeys(81) = "DIK_NUMPAD3"
    aKeys(82) = "DIK_NUMPAD0"
    aKeys(83) = "DIK_DECIMAL  Period (decimal point) on numeric keypad"
    aKeys(87) = "DIK_F11"
    aKeys(88) = "DIK_F12"
    aKeys(86) = "DIK_F13"
    aKeys(84) = "DIK_F14"
    aKeys(85) = "DIK_F15"
    aKeys(156) = "DIK_NUMPADENTER"
    aKeys(157) = "DIK_RCONTROL  Right CTRL key"
    aKeys(91) = "DIK_NUMPADCOMMA Comma on NEC PC98 numeric keypad"
    aKeys(181) = "DIK_DIVIDE  Forward slash (/)on numeric keypad"
    aKeys(183) = "DIK_SYSRQ"
    aKeys(184) = "DIK_RMENU  Right ALT"
    aKeys(199) = "DIK_HOME"
    aKeys(200) = "DIK_UP  Up arrow"
    aKeys(201) = "DIK_PRIOR  PAGE UP"
    aKeys(203) = "DIK_LEFT  Left arrow"
    aKeys(205) = "DIK_RIGHT  Right arrow"
    aKeys(207) = "DIK_END"
    aKeys(208) = "DIK_DOWN  Down arrow"
    aKeys(209) = "DIK_NEXT  PAGE DOWN"
    aKeys(210) = "DIK_INSERT"
    aKeys(211) = "DIK_DELETE"
    aKeys(219) = "DIK_LWIN  Left Windows key"
    aKeys(220) = "DIK_RWIN  Right Windows key"
    aKeys(221) = "DIK_APPS  Application key"
    aKeys(116) = "DIK_PAUSE"

    KeyNames = aKeys(iNum)

End Function

