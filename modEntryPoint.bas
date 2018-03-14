Attribute VB_Name = "Module1"
Option Explicit

Sub Button1_Click()
    Dim envReader As IDataSet: Set envReader = New objDataSetExcelRange
    Dim envTable As ListObject: Set envTable = Worksheets("base").ListObjects("envTable")
    Dim dataRange As String: dataRange = "A2:A" & (envTable.DataBodyRange.Rows.count + 1)
    Dim envProcessor As IDataProcessor: Set envProcessor = New objEnvProcessor
    
    Dim matcher As New objMatcher: With matcher
        .addPattern patternName:="Atomic Type", pattern:="[A-Z][A-Za-z]*"
        .addPattern patternName:="Abstract Type", pattern:="([A-Z])\.(\(*[A-Z].+)"
        .addPattern patternName:="Type Abstract Term", pattern:="([A-Z])\.([a-z].+)"
    End With
    
    With envReader
        .read ("base\" & dataRange)
        While Not .EOF
            'Debug.Print TypeName(matcher)
            'Debug.Print TypeName(atomicTypeSet)
            
            MsgBox (matcher.match(envProcessor, .fieldValue("A")))
            .moveNext
        Wend
        .cl
    End With
    
End Sub


Public Sub testSimple()
    Dim obj As objDataStructure
    Dim record As iRecord
    
    Set record = New objSimpleRecord
    
    Set obj = New objDataStructure
    With obj
        'pattern: ^([0-9]{3})([a-zA-Z])([0-9]{4})$
        .addFieldDescriptor "[0-9]{3}"
        .addFieldDescriptor "[a-zA-Z]{2}"
        .addFieldDescriptor "[0-9]{4}"
        
        Call .match(record, "224ba4442")
    End With
End Sub
