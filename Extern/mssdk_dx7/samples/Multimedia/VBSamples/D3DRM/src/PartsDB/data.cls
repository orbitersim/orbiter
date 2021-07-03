VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Data"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'''''''''''''''''''''''''''''
' DATA MIDDLEWARE           '
'''''''''''''''''''''''''''''
' replace with your favorite
' database code
'

Private Type rec
    AssemblyId As Long
    ModelPart As String
    PartID As String
    Description As String
    Price As Currency
    CompatibleParts As String
    Stock As String
    PartMake As String
End Type
 
Dim rs() As rec
Dim index As Integer
Dim lastindex As Integer
Dim maxsize As Integer

Public Function MoveTop()
    index = 0
    MoveTop = True
End Function

Public Function IsEOF()
    If index = -1 Then IsEOF = True
End Function
Public Function MoveNext()
    If index = lastindex Then
        index = -1
        Exit Function
    End If
        
    index = index + 1
    MoveNext = True
End Function

Public Property Get ModelPart() As String
    ModelPart = rs(index).ModelPart
End Property

Public Property Get PartID() As String
    PartID = rs(index).PartID
End Property

Public Property Get Description() As String
    Description = rs(index).Description
End Property

Public Property Get Price() As Currency
    Price = rs(index).Price
End Property

Public Property Get CompatibleParts() As String
    CompatibleParts = rs(index).CompatibleParts
End Property

Public Property Get Stock() As String
    Stock = rs(index).Stock
End Property

Public Property Get PartMake() As String
    PartMake = rs(index).PartMake
End Property

Public Function MoveToModelPartRecord(sname As String) As Boolean
    
    For index = 0 To lastindex
        If (UCase(rs(index).ModelPart) = UCase(sname)) Then
            MoveToModelPartRecord = True
            Exit Function
        End If
    Next
    MoveToModelPartRecord = False
End Function

Function InitData(sFile As String) As Boolean
    Dim strData As String
    On Local Error GoTo errOut
    
    ReDim rs(100)
    maxsize = 100
    
    Dim fl As Long
    fl = FreeFile
    index = 0
    Open sFile For Input As #fl
    Line Input #fl, strData
    Do While Not EOF(fl)
        Line Input #fl, strData
    
        
        Dim j As Long, q As Long
        Dim r As rec
        
        'Assembly ID - what assembly does this belong to
        j = 1
        q = InStr(j, strData, Chr(9))
        r.AssemblyId = Mid$(strData, 1, q - 1)
        
        'Unique ID for all parts
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.PartID = Mid$(strData, j, q - j)
    
        'Model Part .. whats the name of the part in the xfile
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.ModelPart = Mid$(strData, j + 1, q - 2 - j)
        
        'Part Price
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.Price =val(Mid$(strData, j + 1, q - 1 - j))
        
        'Description
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.Description = Mid$(strData, j + 1, q - 2 - j)
        
        'Stock
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.Stock = Mid$(strData, j, q - j)
        
        'PartMake
        j = q + 1
        q = InStr(j, strData, Chr(9))
        r.PartMake = Mid$(strData, j + 1, q - j - 2)
                
        'CompatibleParts
        j = q + 1
        r.CompatibleParts = Mid$(strData, j + 1)
        q = Len(r.CompatibleParts) - 1
        r.CompatibleParts = Mid$(r.CompatibleParts, 1, q)
        
        If index > maxsize Then
            maxsize = maxsize + 100
            ReDim Preserve rs(maxsize)
        End If
        
        rs(index) = r
        lastindex = index
        index = index + 1
        
    Loop
    InitData = True
    Exit Function
errOut:
    InitData = False
End Function
   
   




