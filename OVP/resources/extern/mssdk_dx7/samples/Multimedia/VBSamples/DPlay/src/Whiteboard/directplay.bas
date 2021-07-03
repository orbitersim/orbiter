Attribute VB_Name = "DirectPlay"
Public Sub InitDplay()
    Set dp = dx.DirectPlayCreate("")
    Set dpl = dx.DirectPlayLobbyCreate()
End Sub

Public Sub InitAddress()
    
    If gIP <> "" Then
        Set dpa = dpl.CreateINetAddress(gIP, 0)
    Else
        Set dpa = dpl.CreateINetAddress("", 0)
    End If
    
    dp.InitializeConnection dpa
End Sub

Public Sub JoinChat()

    If dp Is Nothing Then Call InitDplay 'init dplay
    If dpa Is Nothing Then Call InitAddress 'init the dplay address
    
    frmJoin.Show 1  ' show the join form
    
    
End Sub


Public Sub StartMessageEngine()
    frmMain.tmrMSG.Interval = 1
    frmMain.tmrMSG.Enabled = True
End Sub




Public Sub CleanUp()
    Set dp = Nothing
    Set dpa = Nothing
    Set dpM = Nothing
    Set dpl = Nothing
End Sub
