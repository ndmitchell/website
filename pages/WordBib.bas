Attribute VB_Name = "WordBib"
Option Explicit

Sub UpdateBib()
Dim Uses As New Collection
Dim Refs As New Collection
Dim Nums As New Collection
'first run through and see what items are added in which order

Dim c As Comment, s As String, col As Collection, i As Integer, v As Variant
For Each c In ActiveDocument.Comments
    s = c.Range.Text
    If Left$(s, 1) = "!" Then
        s = Mid$(s, 2)
        If InCol(Refs, s) Then
            MsgBox "Ref multiply defined, " & s
            Exit Sub
        Else
            Refs.Add s, s
        End If
    Else
        If Not InCol(Uses, s) Then
            Uses.Add s, s
        End If
    End If
Next

'now check for any references not defined
For Each v In Uses
    If Not InCol(Refs, CStr(v)) Then
        MsgBox "Ref used but not defined, " & v
        Exit Sub
    End If
Next

For i = 1 To Uses.Count
    Nums.Add i, Uses.Item(i)
Next

Dim Comments As New Collection
For Each c In ActiveDocument.Comments
    Comments.Add c
Next

For i = 1 To Comments.Count
    Set c = Comments(i)
    Dim Start As Long
    Start = c.Scope.Start
    Dim Text As String, Name As String
    Text = c.Scope.Text
    Name = c.Range.Text
    
    s = Name
    Dim Number As Integer
    If Left$(s, 1) = "!" Then
        s = Mid$(s, 2)
        If InCol(Nums, s) Then
            Number = Nums(s)
        Else
            Number = 0
        End If
    Else
        Number = Nums(s)
    End If
    
    s = SetNumber(Text, Number)
    c.Scope.Text = s
    c.Delete
    ActiveDocument.Comments.Add ActiveDocument.Range(Start, Start + Len(s)), Name
Next
End Sub

Function InCol(c As Collection, s As String) As Boolean
On Error GoTo Failure
c.Item s
On Error GoTo 0
InCol = True
Failure:
End Function

Function SetNumber(s As String, Value As Integer) As String
Dim s2 As String, char As String
s2 = s

Dim Pre As String, Post As String, i As Integer
For i = 1 To Len(s)
    char = Mid$(s, i, 1)
    If IsDigit(char) Then
        Exit For
    Else
        Pre = Pre + char
    End If
Next

Post = Mid$(s, Len(Pre) + 1)
Do While Len(Post) >= 1 And IsDigit(Left$(Post, 1))
    Post = Mid$(Post, 2)
Loop

SetNumber = Pre & Value & Post
End Function

Function IsDigit(s As String) As Boolean
IsDigit = s >= "0" And s <= "9"
End Function
