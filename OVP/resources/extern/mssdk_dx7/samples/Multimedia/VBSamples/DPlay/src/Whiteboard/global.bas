Attribute VB_Name = "global"
Public dx As New DirectX7
Public dp As DirectPlay4
Public dpa As DirectPlayAddress
Public dpl As DirectPlayLobby3
Public dpsD As DirectPlaySessionData
Public PlayerName As String
Public player_id As Long
Public playerfname As String
Public eS As DirectPlayEnumSessions
Public dpM As DirectPlayMessage
Public join As Boolean
Public create As Boolean
Public mD As Boolean
Public CAN(100, 100) As Boolean
Public gX As Byte
Public gY As Byte
Public sessionGUID As String
Public IsHost As Boolean

'Public MSG_TO As Long
Public PRI_FROM As Long
Public gIP As String

Public eP As DirectPlayEnumPlayers
Public sD As DirectPlaySessionData

Public Const APP_GUID = "{37732A07-1CBC-11d3-9F59-00A0C9EA75AB}"
Public Const SWP_NOSIZE = &H1
Public Const HWND_TOP = 0
Public Const HWND_TOPMOST = -1



Public Enum msg_
    PLAYER_X = 1
    PLAYER_Y = 2
    LOBBY_MSG = 3
    MOUSE_MOVE = 4
    PRIVATE_MSG = 68
End Enum

Public Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)


