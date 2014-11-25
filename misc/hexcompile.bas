#Include "file.bi"
Dim As Byte h1, h2, tmp, outp
Dim As Integer hn1, hn2, t1, t2
Dim As Integer I = 0, b = 1
Dim As String infile, outfile, ow
Dim As Single start
infile = Command(1)
If infile = "" Then
	Print "No input specified! Program aborts!"
	Sleep 5000
	end
EndIf
outfile = Left(infile,Len(infile)-4) & ".65"
If FileExists(outfile) Then
	Input "Output file exists, okay to overwrite? ", ow
	If UCase(ow) = "YES" Or UCase(ow) = "Y" Then GoTo okay
	Print
	Print "Process aborted!"
	Sleep 5000
	end
EndIf

okay:
start = timer
Open infile For Binary As #1
Open outfile For Binary As #2
Do
	i + = 1
	redo:
		If i > Lof(1) Then
		Cls
		Print "Compiled " & infile
		Print
		Print "to"
		Print
		Print outfile
		Print 
		Print b - 1 & " bytes processed!"
		Print
		Print "Took " & Timer - start & " seconds!"
		Sleep 7000
		end
	EndIf
	Get #1, i, h1
	i+=1
	If UCase(Chr(h1)) <> "A" And UCase(Chr(h1)) <> "B" And UCase(Chr(h1)) <> "C" And UCase(Chr(h1)) <> "D" And UCase(Chr(h1)) <> "E" And UCase(Chr(h1)) <> "F" And UCase(Chr(h1)) <> "1" And UCase(Chr(h1)) <> "2" And UCase(Chr(h1)) <> "3" And UCase(Chr(h1)) <> "4" And UCase(Chr(h1)) <> "5" And UCase(Chr(h1)) <> "6" And UCase(Chr(h1)) <> "7" And UCase(Chr(h1)) <> "8" And UCase(Chr(h1)) <> "9" And UCase(Chr(h1)) <> "0" then
	GoTo redo
	End If
	Get #1, i, h2
	
	
If Str(LCase(Chr(h1))) = "0" Then hn1 = 0
If Str(LCase(Chr(h1))) = "1" Then hn1 = 1
If Str(LCase(Chr(h1))) = "2" Then hn1 = 2
If Str(LCase(Chr(h1))) = "3" Then hn1 = 3
If Str(LCase(Chr(h1))) = "4" Then hn1 = 4
If Str(LCase(Chr(h1))) = "5" Then hn1 = 5
If Str(LCase(Chr(h1))) = "6" Then hn1 = 6
If Str(LCase(Chr(h1))) = "7" Then hn1 = 7
If Str(LCase(Chr(h1))) = "8" Then hn1 = 8
If Str(LCase(Chr(h1))) = "9" Then hn1 = 9
If Str(LCase(Chr(h1))) = "a" Then hn1 = 10
If Str(LCase(Chr(h1))) = "b" Then hn1 = 11
If Str(LCase(Chr(h1))) = "c" Then hn1 = 12
If Str(LCase(Chr(h1))) = "d" Then hn1 = 13
If Str(LCase(Chr(h1))) = "e" Then hn1 = 14
If Str(LCase(Chr(h1))) = "f" Then hn1 = 15

If Str(LCase(Chr(h2))) = "0" Then hn2 = 0
If Str(LCase(Chr(h2))) = "1" Then hn2 = 1
If Str(LCase(Chr(h2))) = "2" Then hn2 = 2
If Str(LCase(Chr(h2))) = "3" Then hn2 = 3
If Str(LCase(Chr(h2))) = "4" Then hn2 = 4
If Str(LCase(Chr(h2))) = "5" Then hn2 = 5
If Str(LCase(Chr(h2))) = "6" Then hn2 = 6
If Str(LCase(Chr(h2))) = "7" Then hn2 = 7
If Str(LCase(Chr(h2))) = "8" Then hn2 = 8
If Str(LCase(Chr(h2))) = "9" Then hn2 = 9
If Str(LCase(Chr(h2))) = "a" Then hn2 = 10
If Str(LCase(Chr(h2))) = "b" Then hn2 = 11
If Str(LCase(Chr(h2))) = "c" Then hn2 = 12
If Str(LCase(Chr(h2))) = "d" Then hn2 = 13
If Str(LCase(Chr(h2))) = "e" Then hn2 = 14
If Str(LCase(Chr(h2))) = "f" Then hn2 = 15
t1 = hn2
t2 = hn1 * 16
outp = t1+t2
Put #2, b, outp
b+=1
Loop
