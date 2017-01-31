Dim Shared As UByte shift
Dim Shared As UByte mData 'Mapper Data written(Used on MMC1)
Dim Shared As UByte R0, R1, R2, R3 '4 Data registers for MMC1


Declare Function bankSwap(ByVal bank As UInteger,ByVal addr As UShort) As ULongInt

Function bankswap(ByVal bank As UInteger,ByVal addr As UShort) As ULongInt
	
	Select Case mapper
		Case 1 'MMC1
		If shift = 4 Then 'Only the last write matters
			Select Case addr
				Case &h8000 To &h9FFF 'Control
 
				Case &hA000 To &hBFFF 'CHR Bank 0
				
				Case &hC000 To &hDFFF 'CHR Bank 1
					
				Case &hE000 To &hFFFF 'PRG Bank
			End Select
			shift = 0 
			Else
			shift += 1
			endif
		Case 2 'UNROM
		bank = bank And 7
		bank*=&h4000
		'UnROM can only swap the first bank 
		For i As Integer = 0 To &h4000
			cpu.memory(&h8000+i) = prgRom(bank+i) 
		Next
		Case 3 'CNROM
		bank = bank And 3 
		bank*= &h2000
		For i As Integer = 0 To &h2000
			ppu.vram(i) = chrRom(bank+i)
		Next
		Case Else
		'Unsupported mapper	
	End Select
End Function
