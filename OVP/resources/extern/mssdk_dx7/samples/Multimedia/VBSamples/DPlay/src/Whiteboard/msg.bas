Attribute VB_Name = "msg"

Public Sub send_x(n As msg_, k() As Single)
    On Error Resume Next
    
    '''''''''''' send the mouse data
    Set dpM = dp.CreateMessage()
    
    
    dpM.WriteLong n
    dpM.WriteSingle k(0)
    dpM.WriteSingle k(1)
    
    dp.Send player_id, DPID_ALLPLAYERS, DPSEND_GUARANTEED, dpM

End Sub
Public Sub send_msg(n As msg_, ms As String)
    On Error Resume Next
    
    Select Case n
    Case LOBBY_MSG
        Set dpM = dp.CreateMessage()
        
        dpM.WriteLong n
        dpM.WriteString ms
        
        dp.Send player_id, DPID_ALLPLAYERS, DPSEND_GUARANTEED, dpM
    Case PRIVATE_MSG
        'send a private message
        Set dpM = dp.CreateMessage()
        
        dpM.WriteLong n
        dpM.WriteString ms
        
        dp.Send player_id, PRI_FROM, DPSEND_GUARANTEED, dpM
    End Select
    
End Sub

Public Sub get_msg()
    On Error Resume Next
    Dim msgTY As Long, msg_count As Long
    Dim nM As DirectPlayMessage
    
    'msg_count = dp.GetMessageCount(0)
    
    
        
        Set nM = dp.CreateMessage()
        Dim FROM_ID As Long
        Set nM = dp.Receive(FROM_ID, 0, DPRECEIVE_ALL)

        msgTY = nM.ReadLong()
        
        'system messages
        If FROM_ID = DPID_SYSMSG Then
            Select Case msgTY
                Case DPSYS_CREATEPLAYERORGROUP
                    Call getPlayers(frmMain.lstPlayers)
                Case DPSYS_DESTROYPLAYERORGROUP
                    Call getPlayers(frmMain.lstPlayers)
            End Select
        Else
            '''''''' app messages
            Select Case msgTY
                Case LOBBY_MSG
                    Dim FROM_PLAYER As String
                    FROM_PLAYER = dp.GetPlayerFriendlyName(FROM_ID)
                    frmMain.txtMsg = frmMain.txtMsg.Text & vbCrLf & FROM_PLAYER & ">" & nM.ReadString
                    
                Case MOUSE_MOVE
                    Dim h(1) As Single
                    
                    For g = 0 To UBound(h)
                        h(g) = nM.ReadSingle
                    Next g
                    
                    frmMain.pic.PSet (h(0), h(1)), QBColor(10)
                Case PRIVATE_MSG
                    Dim FROM_NAME As String
                    Dim sPrivateMessage As String
                    
                    PRI_FROM = FROM_ID
                    FROM_NAME = dp.GetPlayerFriendlyName(PRI_FROM)
                    sPrivateMessage = nM.ReadString
                    frmPrivateMessage.txtPriMsg = frmPrivateMessage.txtPriMsg & vbCrLf & FROM_NAME & ">" & sPrivateMessage
                    MSG_FROM = FROM_ID
                    frmPrivateMessage.Show
            End Select
        End If
End Sub

