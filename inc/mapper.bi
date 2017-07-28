Dim Shared As UByte shift
Dim Shared As UByte mData 'Mapper Data written(Used on MMC1)
Dim Shared As UByte R0, R1, R2, R3 '4 Data registers for MMC1
Dim Shared As UInteger prgOffs, chrOffs
#Define mirror (R0 And 3)
#Define prgSwap ((R0 Shr 2)And 1)
#Define pSize ((R0 Shr 3)And 3)
'#Define chrSize ((R0 Shr 4)And 1)  
#Define chrBank0 (R1 And &hF)
#Define chrBank1 (R2 And &hF)
#Define prgBank (R3 And &hF)
#Define wRAM (( R3 Shr 4)And 1)
R0 = &hC

Declare Function bankSwap(ByVal bank As UInteger,ByVal addr As UShort) As ULongInt

Function bankswap(ByVal value As UInteger,ByVal addr As UShort) As ULongInt
	
	Select Case mapper
		Case 1 'MMC1
		'MMC1 does it's register writes with a 5-bit shift register 
		'It takes 5 writes to Program space in order to set the SR
		'On the 5th write the address written to determines which 
		'MMC1 register gets the value of the shift register ~ Nobbs66
		
		shift += 1
		'mdata Or= ((value And 1)and 4)
		mdata Or= ((value And 1) Shr shift)
		If value >= &h80 Then R0 = &hC 'Reset signal sets 2 and 3
		If shift = 5 Then 'The register is full, so let's push it
			Select Case addr
				Case &h8000 To &h9FFF 'Control
 					R0 = mData 
				Case &hA000 To &hBFFF 'CHR Bank 0
					R1 = mData
					If Bit(R0,4) Then
						chrOffs = R1 * &h1000
					 	For i As Integer = 0 To &hFFF
					 		ppu.vram(i) = chrRom(chrOffs+i)
					 	Next
					Else
						chrOffs = (R1 Shr 1) * &h2000
						For i As Integer = 0 To &h1FFF
							ppu.vram(i) = chrRom(chrOffs+i)
						Next
					EndIf
				Case &hC000 To &hDFFF 'CHR Bank 1
					R2 = mData
					If Bit(R0,4) Then 
						chrOffs = R2 * &h1000
					 	For i As Integer = 0 To &hFFF
					 	ppu.vram(&h1000+i) = chrRom(chrOffs+i)
					 	Next
					EndIf
				Case &hE000 To &hFFFF 'PRG Bank0
						R3 = mData
						Select Case pSize
							Case 0 To 1 '32KB Mode
								prgOffs = ((R3 Shr 2)And 7)*&h8000 
								For i As Integer = 0 To &h7FFF
									cpu.memory(&h8000+i) = prgRom(prgOffs+i)
								Next 
							Case 2
								Dim As Integer loadOffset
								prgOffs =((R3 Shr 1) And &hF) * &h4000
								loadOffset = header.prgSize*&h4000 'Number of banks x 16KB
								loadOffset -= &h4000 'Get start of last bank
								For i As Integer = 0 to &h3FFF
								'cpu.memory(&h8000+i) = prgRom(loadOffset+i)
								cpu.memory(&hC000+i) = prgRom(prgOffs+i)
								Next
							Case 3
								Dim As Integer loadOffset
								prgOffs =((R3 Shr 1) And &hF) * &h4000
								loadOffset = header.prgSize*&h4000 'Number of banks x 16KB
								loadOffset -= &h4000 'Get start of last bank
								For i As Integer = 0 to &h3FFF
								'cpu.memory(&h8000+i) = prgRom(prgOffs+i)
								cpu.memory(&hC000+i) = prgRom(loadOffset+i)
								next
						End Select
							
					
			End Select
			shift = 0 
			Else
			shift += 1
		EndIf
		Case 2 'UNROM
		value = value And 7
		value*=&h4000
		'UnROM can only swap the first bank 
		For i As Integer = 0 To &h3FFF
			cpu.memory(&h8000+i) = prgRom(value+i) 
		Next
		Case 3 'CNROM
		value = value And 3 
		value*= &h2000
		For i As Integer = 0 To &h2000
			ppu.vram(i) = chrRom(value+i)
		Next
		Case 7 'AOROM
					value = value And 7
		value*=&h8000 
		For i As Integer = 0 To &h7FFF
			cpu.memory(&h8000+i) = prgRom(value+i) 
		Next
		Case Else
		'Unsupported mapper	
	End Select
End Function
