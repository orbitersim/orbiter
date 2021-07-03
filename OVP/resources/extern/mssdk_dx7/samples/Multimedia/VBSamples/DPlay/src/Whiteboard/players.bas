Attribute VB_Name = "players"
Public Sub getPlayers(lst As ListBox)
    On Error Resume Next
    
    
    lst.Clear
    
    Set eP = dp.GetDPEnumPlayers(sessionGUID, DPENUMPLAYERS_ALL)

    Dim i As Integer
    
    For i = 1 To eP.GetCount
        lst.AddItem eP.GetShortName(i)
    Next i
    
    'Set eP = Nothing

End Sub
