Attribute VB_Name = "db"
Public db As Database
Public rs As Recordset

'''''''''''''''''''''''''
'Initalize the database
'''''''''''''''''''''''''

Public Sub INITDB()
    
    Dim sSQL As String
    
    Set db = OpenDatabase(App.Path + "\memo.mdb")
    sSQL = "SELECT * FROM tblMain"
    
    Set rs = db.OpenRecordset(sSQL, dbOpenDynaset)
    
End Sub


'''''''''''''''''''''''''
'Does this date have a
'memo?
'''''''''''''''''''''''''

Public Function HAS_MEMO(mo As String, dy As Integer, yr As Integer) As Boolean
    If rs Is Nothing Then INITDB
    
    If rs.RecordCount > 0 Then rs.MoveFirst
    Do While Not rs.EOF
        If rs!Month = mo Then
            If rs!Day = dy Then
                If rs!Year = yr Then
                    HAS_MEMO = True
                    Exit Do
                End If
            End If
        End If
        rs.MoveNext
    Loop

End Function

'''''''''''''''''''''''''''
'Delete a memo
'''''''''''''''''''''''''''

Public Sub DEL_REC(nm As String)
        rs.MoveFirst
        Do While Not rs.EOF
            If rs!Month = M Then
                If rs!Day = D Then
                    If rs!Year = Y Then
                        If rs!Name = nm Then
                            rs.Delete
                        End If
                    End If
                End If
            End If
            rs.MoveNext
        Loop
End Sub

'''''''''''''''''''''''''''''''
'Save Memo to Database
'''''''''''''''''''''''''''''''
Public Sub SaveToDB()

    Dim WAVE_DATA() As Byte
    Dim CNT As Long
    Dim TITLE As String
    Dim z As Single
    Dim CP As DSCBCAPS
    Dim STATUS As Long
    
    Randomize Timer
    
    'rs.MoveFirst?
    
    If Not MEMO_RECORD Then Exit Sub
    ''''''''''''''' stop if we are capturing
    STATUS = dscb.GetStatus
    If (STATUS And DSCBSTATUS_CAPTURING) Then
        dscb.Stop
    End If
    
    dscb.GetCaps CP
    
    ReDim WAVE_DATA(CP.lBufferBytes)
    
    dscb.ReadBuffer 0, 0, WAVE_DATA(0), DSCBLOCK_ENTIREBUFFER
    
    TITLE = InputBox("Please enter title of your memo.", "MEMO TITLE")
        
    If TITLE = "" Then
        TITLE = "MEMO: " & Rnd * 65535 & "DATE: " & M & D & Y
    End If
    
        
    With rs
        .AddNew
        !wav = WAVE_DATA()
        !Name = TITLE
        !Month = M
        !Day = D
        !Year = Y
        !wl = CP.lBufferBytes
        '!tm = Time()
        .Update
    End With
    
    
End Sub

'''''''''''''''''''''''''''''
'Get the dates with memos
'''''''''''''''''''''''''''''

Public Sub BoldDays(cl As MonthView)
    Dim tmpDate As Date
    
        On Local Error Resume Next
        If rs Is Nothing Then Call INITDB
        rs.MoveFirst
        
        Do While Not rs.EOF
            DoEvents
            tmpDate = rs!Month & "/" & rs!Day & "/" & rs!Year
            tmpDate = CDate(tmpDate)
            cl.DayBold(tmpDate) = True
            rs.MoveNext
        Loop
End Sub
