Declare Sub decode_and_execute(ByVal opc As UByte)

Sub decode_and_execute(ByVal opc As UByte)
	Select Case opc
		'add with carry
		Case 105
			instruction = "ADC"
			amode = "IMM"
			ticks += 2
			INS_ADC
		Case 101
			instruction = "ADC"
			amode = "ZP"
			ticks += 3
			INS_ADC
		Case 117
			instruction = "ADC"
			amode = "ZPX"
			ticks += 4
			INS_ADC
		Case 109
			instruction = "ADC"
			amode = "ABS"
			ticks += 4
			INS_ADC
		Case 125
			instruction = "ADC"
			amode = "ABSX"
			ticks += 4
			INS_ADC
		Case 121
			instruction = "ADC"
			amode = "ABSY"
			ticks += 4
			INS_ADC
		Case 97
			instruction = "ADC"
			amode = "INDX"
			ticks += 6
			INS_ADC
		Case 113
			instruction = "ADC"
			amode = "INDY"
			ticks += 5
			'logical and
			INS_ADC
		Case 41
			instruction = "AND"
			amode =  "IMM"
			ticks += 2
			INS_AND
		Case 37
			instruction = "AND"
			amode = "ZP"
			ticks += 3
			INS_AND
		Case 53
			instruction = "AND"
			amode = "ZPX"
			ticks += 4
			INS_AND
		Case 45
			instruction = "AND"
			amode = "ABS"
			ticks += 4
			INS_AND
		Case 61
			instruction = "AND"
			amode = "ABSX"
			ticks += 4
			INS_AND
		Case 57
			instruction = "AND"
			amode = "ABSY"
			ticks += 4
			INS_AND
		Case 33
			instruction = "AND"
			amode = "INDX"
			ticks += 5
			INS_AND
		Case 49
			instruction = "AND"
			amode = "INDY"
			ticks += 6
			INS_AND
			'arithmetic shift left
		Case 10
			instruction = "ASL"
			amode = "ACC"
			ticks += 2
			INS_ASL
		Case 6
			instruction = "ASL"
			amode = "ZP"
			ticks += 5
			INS_ASL
		Case 22
			instruction = "ASL"
			amode = "ZPX"
			ticks += 6
			INS_ASL
		Case 14
			instruction = "ASL"
			amode = "ABS"
			ticks += 6
			INS_ASL
		Case 30
			instruction = "ASL"
			amode = "ABSX"
			ticks += 7
			INS_ASL
			'branch if carry clear
		Case 144
			instruction = "BCC"
			amode = "REL"
			ticks += 2
			INS_BCC
			'branch if carry set
		Case 176
			instruction = "BCS"
			amode = "REL"
			ticks += 2
			INS_BCS
			'branch if equal
		Case 240
			instruction = "BEQ"
			amode = "REL"
			ticks += 2
			INS_BEQ
			'Bit test
		Case 36
			instruction = "BIT"
			amode = "ZP"
			ticks += 3
			INS_BIT
		Case 44
			instruction = "BIT"
			amode = "ABS"
			ticks += 4
			INS_BIT
			'Branch if minus
		Case 48
			instruction = "BMI"
			amode = "REL"
			ticks += 2
			INS_BMI
			'Branch if not equal
		Case 208
			instruction = "BNE"
			amode = "REL"
			ticks += 2
			INS_BNE
			'Branch if positve
		Case 16
			instruction = "BPL"
			amode = "REL"
			ticks += 2
			INS_BPL
			'Break
		Case 0
			instruction = "BRK"
			amode = "IMP"
			ticks += 7
			INS_BRK
			'Branch if overflow clear
		Case 80
			instruction = "BVC"
			amode = "REL"
			ticks += 2
			INS_BVC
			'Branch if overflow set
		Case 112
			instruction = "BVS"
			amode = "REL"
			ticks += 2
			INS_BVS
			'clear carry flag
		Case 24
			instruction = "CLC"
			amode = "IMP"
			ticks += 2
			INS_CLC
			'clear decimal mode
		Case 216
			instruction = "CLD"
			amode = "IMP"
			ticks += 2
			INS_CLD
			'Clear interrupt disable
		Case 88
			instruction = "CLI"
			amode = "IMP"
			ticks += 2
			INS_CLI
			'clear overflow flag
		Case 184
			instruction = "CLV"
			amode = "IMP"
			ticks += 2
			INS_CLV
			'compare
		Case 201
			instruction = "CMP"
			amode = "IMM"
			ticks += 2
			INS_CMP
		Case 197
			instruction = "CMP"
			amode = "ZP"
			ticks += 3
			INS_CMP
		Case 213
			instruction = "CMP"
			amode = "ZPX"
			ticks += 4
			INS_CMP
		Case 205	
			instruction = "CMP"
			amode = "ABS"
			ticks += 4
			INS_CMP
		Case 221
			instruction = "CMP"
			amode = "ABSX"
			ticks += 4
			INS_CMP
		Case 217
			instruction = "CMP"
			amode ="ABSY"
			ticks += 4
			INS_CMP
		Case 193
			instruction = "CMP"
			amode = "INDX"
			ticks += 6
			INS_CMP
		Case 209
			instruction = "CMP"
			amode = "INDY"
			ticks += 4
			INS_CMP
			'compare x register
		Case 224
			instruction = "CPX"
			amode = "IMM"
			ticks += 2
			INS_CPX
		Case 228
			instruction = "CPX"
			amode = "ZP"
			ticks += 3
			INS_CPX
		Case 236
			instruction = "CPX"
			amode = "ABS"
			ticks += 4
			INS_CPX
			'compare Y register
		Case 192
			instruction = "CPY"
			amode = "IMM"
			ticks += 2
			INS_CPY
		Case 196
			instruction = "CPY"
			amode = "ZP"
			ticks += 3
			INS_CPY
		Case 204
			instruction = "CPY"
			amode = "ABS"
			ticks += 4
			INS_CPY
			'Decrement memory
		Case 198
			instruction = "DEC"
			amode = "ZP"
			ticks += 5
			INS_DEC
		Case 214
			instruction  = "DEC"
			amode = "ZPX"
			ticks += 6
			INS_DEC
		Case 206
			instruction = "DEC"
			amode = "ABS"
			ticks += 6
			INS_DEC
		Case 222
			instruction = "DEC"
			amode = "ABSX"
			ticks += 7
			INS_DEC
			'Decrement X register
		Case 202
			instruction = "DEX"
			amode = "IMP"
			ticks += 2
			INS_DEX
			'decrement Y register
		Case 136
			instruction = "DEY"
			amode = "IMP"
			ticks += 2
			INS_DEY
			'exclusive or
		Case 73
			instruction = "EOR"
			amode = "IMM"
			ticks += 2
			INS_EOR
		Case 69
			instruction = "EOR"
			amode = "ZP"
			ticks += 3
			INS_EOR
		Case 85
			instruction = "EOR"
			amode = "ZPX"
			ticks += 4
			INS_EOR
		Case 77
			instruction = "EOR"
			amode = "ABS"
			ticks += 4
			INS_EOR
		Case 93
			instruction = "EOR"
			amode = "ABSX"
			ticks += 4
			INS_EOR
		Case 89
			instruction = "EOR"
			amode = "ABSY"
			ticks += 4
			INS_EOR
		Case 65
			instruction = "EOR"
			amode = "INDX"
			ticks += 6
			INS_EOR
		Case 81
			instruction = "EOR"
			amode = "INDY"
			ticks += 5
			INS_EOR
			'increment memory
		Case 230
			instruction = "INC"
			amode = "ZP"
			ticks += 5
			INS_INC
		Case 246
			instruction = "INC"
			amode = "ZPX"
			ticks += 6
			INS_INC
		Case 238
			instruction = "INC"
			amode = "ABS"
			ticks += 6
			INS_INC
		Case 254
			instruction = "INC"
			amode = "ABSX"
			ticks += 7
			INS_INC
			'increment x register
		Case 232
			instruction = "INX"
			amode = "IMP"
			ticks += 2 
			INS_INX
			'increment y register
		Case 200
			instruction = "INY"
			amode = "IMP"
			ticks += 2
			INS_INY
			'jump
		Case 76
			instruction = "JMP"
			amode = "ABS"
			ticks += 3
			INS_JMP
		Case 108
			instruction = "JMP"
			amode = "IND"
			ticks += 5
			INS_JMP
			'jump to subroutine
		Case 32
			instruction = "JSR"
			amode = "ABS"
			ticks += 6
			INS_JSR
			'load accumulator
		Case 169
			instruction = "LDA"
			amode = "IMM"
			ticks += 2
			INS_LDA
		Case 165
			instruction = "LDA"
			amode = "ZP"
			ticks += 3
			INS_LDA
		Case 181
			instruction = "LDA"
			amode = "ZPX"
			ticks += 4
			INS_LDA
		Case 173
			instruction = "LDA"
			amode = "ABS"
			ticks += 4
			INS_LDA
		Case 189		
			instruction = "LDA"
			amode = "ABSX"
			ticks += 4
			INS_LDA
		Case 185
			instruction = "LDA"
			amode = "ABSY"
			ticks += 4
			INS_LDA
		Case 161
			instruction = "LDA"
			amode = "INDX"
			ticks +=+ 6
			INS_LDA
		Case 177
			instruction = "LDA"
			amode = "INDY"
			ticks += 5
			INS_LDA
			'Load X register
		Case 162
			instruction = "LDX"
			amode = "IMM"
			ticks += 2
			INS_LDX
		Case 166
			instruction = "LDX"
			amode = "ZP"
			ticks += 3
			INS_LDX
		Case 182
			instruction = "LDX"
			amode = "ZPY"
			ticks += 4
			INS_LDX
		Case 174
			instruction = "LDX"
			amode = "ABS"
			ticks += 4
			INS_LDX
		Case 190
			instruction = "LDX"
			amode = "ABSY"
			ticks += 4
			INS_LDX
			'load Y register
		Case 160
			instruction = "LDY"
			amode = "IMM"
			ticks += 2
			INS_LDY
		Case 164
			instruction = "LDY"
			amode = "ZP"
			ticks += 3
			INS_LDY
		Case 180
			instruction = "LDY"
			amode = "ZPX"
			ticks += 4
			INS_LDY
		Case 172		
			instruction = "LDY"
			amode = "ABS"
			ticks += 4
			INS_LDY
		Case 188
			instruction = "LDY"
			amode = "ABSX"
			ticks += 4
			INS_LDY
			'logical shift right
		Case 74
			instruction = "LSR"
			amode = "ACC"
			ticks += 2
			INS_LSR
		Case 70
			instruction = "LSR"
			amode = "ZP"
			ticks += 5
			INS_LSR
		Case 86
			instruction = "LSR"
			amode = "ZPX"
			ticks += 6
			INS_LSR
		Case 78
			instruction = "LSR"
			amode = "ABS"
			ticks += 6
			INS_LSR
		Case 94
			instruction = "LSR"
			amode = "ABSX"
			ticks += 7
			INS_LSR
			'no operation
		Case 234
			instruction = "NOP"
			amode = "IMP"
			ticks += 2
			INS_NOP
			'logical inclusive or
		Case 9
			instruction = "ORA"
			amode = "IMM"
			ticks += 2
			INS_ORA
		Case 5
			instruction = "ORA"
			amode = "ZP"
			ticks += 3
			INS_ORA
		Case 21
			instruction = "ORA"
			amode = "ZPX"
			ticks += 4
			INS_ORA
		Case 13
			instruction = "ORA"
			amode = "ABS"
			ticks += 4
			INS_ORA
		Case 29
			instruction = "ORA"
			amode = "ABSX"
			ticks += 4
			INS_ORA
		Case 25
			instruction = "ORA"
			amode = "ABSY"
			ticks += 4
			INS_ORA
		Case 1
			instruction = "ORA"
			amode = "INDX"
			ticks += 6
			INS_ORA
		Case 17
			instruction = "ORA"
			amode = "INDY"
			ticks += 5
			INS_ORA
			'push accumulator
		Case 72
			instruction = "PHA"
			amode = "IMP"
			ticks += 3
			INS_PHA
			'push processor status
		Case 8
			instruction = "PHP"
			amode = "IMP"
			ticks += 3
			INS_PHP
			'pull accumulator
		Case 104
			instruction = "PLA"
			amode = "IMP"
			ticks += 4
			INS_PLA
			'pull processor status
		Case 40
			instruction = "PLP"
			amode = "IMP"
			ticks += 4
			INS_PLP
			'Rotate left
		Case 42
			instruction = "ROL"
			amode = "ACC"
			ticks += 2
			INS_ROL
		Case 38
			instruction = "ROL"
			amode = "ZP"
			ticks += 5
			INS_ROL
		Case 54
			instruction = "ROL"
			amode = "ZPX"
			ticks += 6
			INS_ROL
		Case 46
			instruction = "ROL"
			amode = "ABS"
			ticks += 6
			INS_ROL
		Case 62
			instruction = "ROL"
			amode = "ABSX"
			ticks += 7
			INS_ROL
			'rotate right
		Case 106
			instruction = "ROR"
			amode = "ACC"
			ticks += 2
			INS_ROR
		Case 102
			instruction = "ROR"
			amode = "ZP"
			ticks += 5
			INS_ROR
		Case 118
			instruction = "ROR"
			amode = "ZPX"
			ticks += 6
			INS_ROR
		Case 110
			instruction = "ROR"
			amode = "ABS"
			ticks += 6
			INS_ROR
		Case 126
			instruction = "ROR"
			amode = "ABSX"
			ticks += 7
			INS_ROR
			'return from interrupt
		Case 64
			instruction = "RTI"
			amode = "IMP"
			ticks += 6
			INS_RTI
			'return from subroutine
		Case 96
			instruction = "RTS"
			amode= "IMP"
			ticks += 6
			INS_RTS
			'subtract with carry
		Case 233
			instruction = "SBC"
			amode = "IMM"
			ticks += 2
			INS_SBC
		Case 229
			instruction = "SBC"
			amode = "ZP"
			ticks += 3
			INS_SBC
		Case 245
			instruction = "SBC"
			amode = "ZPX"
			ticks += 4
			INS_SBC
		Case 237
			instruction = "SBC"
			amode = "ABS"
			ticks += 4
			INS_SBC
		Case 253
			instruction = "SBC"
			amode = "ABSX"
			ticks += 4
			INS_SBC
		Case 249
			instruction = "SBC"
			amode = "ABSY"
			ticks += 4
			INS_SBC
		Case 225
			instruction = "SBC"
			amode = "INDX"
			ticks += 6
			INS_SBC
		Case 241
			instruction = "SBC"
			amode = "INDY"
			ticks += 5
			INS_SBC
			'set carry flag
		Case 56
			instruction = "SEC"
			amode = "IMP"
			ticks += 2
			INS_SEC
			'set decimal flag
		Case 248
			instruction = "SED"
			amode = "IMP"
			ticks += 2
			INS_SED
			'set interrupt disable
		Case 120
			instruction = "SEI"
			amode = "IMP"
			ticks += 2
			INS_SEI
			'Store accumulator
		Case 133
			instruction = "STA"
			amode = "ZP"
			ticks += 3
			INS_STA
		Case 149
			instruction = "STA"
			amode = "ZPX"
			ticks += 4
			INS_STA
		Case 141
			instruction = "STA"
			amode = "ABS"
			ticks += 4
			INS_STA
		Case 157
			instruction = "STA"
			amode = "ABSX"
			ticks += 5
			INS_STA
		Case 153
			instruction = "STA"
			amode = "ABSY"
			ticks += 5
			INS_STA
		Case 129
			instruction = "STA"
			amode = "INDX"
			ticks += 6
			INS_STA
		Case 145
			instruction = "STA"
			amode = "INDY"
			ticks += 6
			INS_STA
			'store x register
		Case 134
			instruction = "STX"
			amode = "ZP"
			ticks += 3
			INS_STX
		Case 150
			instruction = "STX"
			amode = "ZPY"
			ticks += 4
			INS_STX
		Case 142
			instruction = "STX"
			amode = "ABS"
			ticks += 4
			INS_STX
			'store y register
		Case 132
			instruction = "STY"
			amode = "ZP"
			ticks += 3
			INS_STY
		Case 148
			instruction = "STY"
			amode = "ZPX"
			ticks += 4
			INS_STY
		Case 140
			instruction = "STY"
			amode = "ABS"
			ticks += 4
			INS_STY
			'transfer accumulator to X
		Case 170
			instruction = "TAX"
			amode = "IMP"
			ticks += 2
			INS_TAX
			'transfer accumulator to Y
		Case 168
			instruction = "TAY"
			amode = "IMP"
			ticks += 2
			INS_TAY
			'transfer stack pointer to X
		Case 186
			instruction = "TSX"
			amode = "IMP"
			ticks += 2
			INS_TSX
			'transfer X to accumulator
		Case 138
			instruction = "TXA"
			amode = "IMP"
			ticks += 2
			INS_TXA
			'transfer x to stack pointer
		Case 154
			instruction = "TXS"
			amode = "IMP"
			ticks += 2
			INS_TXS
			'transfer Y to accumulator
		Case 152
			instruction = "TYA"
			amode = "IMP"
			ticks += 2
			INS_TYA
		Case Else
			instruction = "Decoder error! " & Str(opc)
			amode = "Decoder error!"

	End Select
	#ifdef debugmode
	'======================================================ONLY INCLUDED IF DEBUGMODE IS DEFINED!======================================================================
	If trace_done = 0 Then
	For i As Integer = 255 To 0 Step -1
		opHistory(i) = opHistory(i-1)
	Next

	opHistory(0) = instruction & "(" & amode & ") "
	End If
	trace_done=0
	'==================================================================================================================================================================
	#EndIf
End Sub
