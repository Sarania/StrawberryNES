Declare Sub decode(ByVal opc As UByte)

Sub decode(ByVal opc As UByte)
	Select Case opc
		'add with carry
		Case 105
			instruction = "ADC"
			amode = "IMM"

		Case 101
			instruction = "ADC"
			amode = "ZP"

		Case 117
			instruction = "ADC"
			amode = "ZPX"

		Case 109
			instruction = "ADC"
			amode = "ABS"

		Case 125
			instruction = "ADC"
			amode = "ABSX"

		Case 121
			instruction = "ADC"
			amode = "ABSY"

		Case 97
			instruction = "ADC"
			amode = "INDX"

		Case 113
			instruction = "ADC"
			amode = "INDY"

			'logical and
		Case 41
			instruction = "AND"
			amode =  "IMM"

		Case 37
			instruction = "AND"
			amode = "ZP"

		Case 53
			instruction = "AND"
			amode = "ZPX"

		Case 45
			instruction = "AND"
			amode = "ABS"

		Case 61
			instruction = "AND"
			amode = "ABSX"

		Case 57
			instruction = "AND"
			amode = "ABSY"

		Case 33
			instruction = "AND"
			amode = "INDX"

		Case 49
			instruction = "AND"
			amode = "INDY"

			'arithmetic shift left
		Case 10
			instruction = "ASL"
			amode = "ACC"

		Case 6
			instruction = "ASL"
			amode = "ZP"

		Case 22
			instruction = "ASL"
			amode = "ZPX"

		Case 14
			instruction = "ASL"
			amode = "ABS"

		Case 30
			instruction = "ASL"
			amode = "ABSX"

			'branch if carry clear
		Case 144
			instruction = "BCC"
			amode = "REL"

			'branch if carry set
		Case 176
			instruction = "BCS"
			amode = "REL"

			'branch if equal
		Case 240
			instruction = "BEQ"
			amode = "REL"

			'Bit test
		Case 36
			instruction = "BIT"
			amode = "ZP"

		Case 44
			instruction = "BIT"
			amode = "ABS"

			'Branch if minus
		Case 48
			instruction = "BMI"
			amode = "REL"

			'Branch if not equal
		Case 208
			instruction = "BNE"
			amode = "REL"

			'Branch if positve
		Case 16
			instruction = "BPL"
			amode = "REL"

			'Break
		Case 0
			instruction = "BRK"
			amode = "IMP"

			'Branch if overflow clear
		Case 80
			instruction = "BVC"
			amode = "REL"

			'Branch if overflow set
		Case 112
			instruction = "BVS"
			amode = "REL"

			'clear carry flag
		Case 24
			instruction = "CLC"
			amode = "IMP"

			'clear decimal mode
		Case 216
			instruction = "CLD"
			amode = "IMP"

			'Clear interrupt disable
		Case 88
			instruction = "CLI"
			amode = "IMP"

			'clear overflow flag
		Case 184
			instruction = "CLV"
			amode = "IMP"

			'compare
		Case 201
			instruction = "CMP"
			amode = "IMM"

		Case 197
			instruction = "CMP"
			amode = "ZP"

		Case 213
			instruction = "CMP"
			amode = "ZPX"

		Case 205
			instruction = "CMP"
			amode = "ABS"

		Case 221
			instruction = "CMP"
			amode = "ABSX"

		Case 217
			instruction = "CMP"
			amode ="ABSY"

		Case 193
			instruction = "CMP"
			amode = "INDX"

		Case 209
			instruction = "CMP"
			amode = "INDY"

			'compare x register
		Case 224
			instruction = "CPX"
			amode = "IMM"

		Case 228
			instruction = "CPX"
			amode = "ZP"

		Case 236
			instruction = "CPX"
			amode = "ABS"

			'compare Y register
		Case 192
			instruction = "CPY"
			amode = "IMM"

		Case 196
			instruction = "CPY"
			amode = "ZP"

		Case 204
			instruction = "CPY"
			amode = "ABS"

			'Decrement memory
		Case 198
			instruction = "DEC"
			amode = "ZP"

		Case 214
			instruction  = "DEC"
			amode = "ZPX"

		Case 206
			instruction = "DEC"
			amode = "ABS"

		Case 222
			instruction = "DEC"
			amode = "ABSX"

			'Decrement X register
		Case 202
			instruction = "DEX"
			amode = "IMP"

			'decrement Y register
		Case 136
			instruction = "DEY"
			amode = "IMP"

			'exclusive or
		Case 73
			instruction = "EOR"
			amode = "IMM"

		Case 69
			instruction = "EOR"
			amode = "ZP"

		Case 85
			instruction = "EOR"
			amode = "ZPX"

		Case 77
			instruction = "EOR"
			amode = "ABS"

		Case 93
			instruction = "EOR"
			amode = "ABSX"

		Case 89
			instruction = "EOR"
			amode = "ABSY"

		Case 65
			instruction = "EOR"
			amode = "INDX"

		Case 81
			instruction = "EOR"
			amode = "INDY"

			'increment memory
		Case 230
			instruction = "INC"
			amode = "ZP"

		Case 246
			instruction = "INC"
			amode = "ZPX"

		Case 238
			instruction = "INC"
			amode = "ABS"

		Case 254
			instruction = "INC"
			amode = "ABSX"

			'increment x register
		Case 232
			instruction = "INX"
			amode = "IMP"

			'increment y register
		Case 200
			instruction = "INY"
			amode = "IMP"

			'jump
		Case 76
			instruction = "JMP"
			amode = "ABS"

		Case 108
			instruction = "JMP"
			amode = "IND"

			'jump to subroutine
		Case 32
			instruction = "JSR"
			amode = "ABS"

			'load accumulator
		Case 169
			instruction = "LDA"
			amode = "IMM"

		Case 165
			instruction = "LDA"
			amode = "ZP"

		Case 181
			instruction = "LDA"
			amode = "ZPX"

		Case 173
			instruction = "LDA"
			amode = "ABS"

		Case 189
			instruction = "LDA"
			amode = "ABSX"

		Case 185
			instruction = "LDA"
			amode = "ABSY"

		Case 161
			instruction = "LDA"
			amode = "INDX"

		Case 177
			instruction = "LDA"
			amode = "INDY"

			'Load X register
		Case 162
			instruction = "LDX"
			amode = "IMM"

		Case 166
			instruction = "LDX"
			amode = "ZP"

		Case 182
			instruction = "LDX"
			amode = "ZPY"

		Case 174
			instruction = "LDX"
			amode = "ABS"

		Case 190
			instruction = "LDX"
			amode = "ABSY"

			'load Y register
		Case 160
			instruction = "LDY"
			amode = "IMM"

		Case 164
			instruction = "LDY"
			amode = "ZP"

		Case 180
			instruction = "LDY"
			amode = "ZPX"

		Case 172
			instruction = "LDY"
			amode = "ABS"

		Case 188
			instruction = "LDY"
			amode = "ABSX"

			'logical shift right
		Case 74
			instruction = "LSR"
			amode = "ACC"

		Case 70
			instruction = "LSR"
			amode = "ZP"

		Case 86
			instruction = "LSR"
			amode = "ZPX"

		Case 78
			instruction = "LSR"
			amode = "ABS"

		Case 94
			instruction = "LSR"
			amode = "ABSX"

			'no operation
		Case 234
			instruction = "NOP"
			amode = "IMP"

			'logical inclusive or
		Case 9
			instruction = "ORA"
			amode = "IMM"

		Case 5
			instruction = "ORA"
			amode = "ZP"

		Case 21
			instruction = "ORA"
			amode = "ZPX"

		Case 13
			instruction = "ORA"
			amode = "ABS"

		Case 29
			instruction = "ORA"
			amode = "ABSX"

		Case 25
			instruction = "ORA"
			amode = "ABSY"

		Case 1
			instruction = "ORA"
			amode = "INDX"

		Case 17
			instruction = "ORA"
			amode = "INDY"

			'push accumulator
		Case 72
			instruction = "PHA"
			amode = "IMP"

			'push processor status
		Case 8
			instruction = "PHP"
			amode = "IMP"

			'pull accumulator
		Case 104
			instruction = "PLA"
			amode = "IMP"

			'pull processor status
		Case 40
			instruction = "PLP"
			amode = "IMP"

			'Rotate left
		Case 42
			instruction = "ROL"
			amode = "ACC"

		Case 38
			instruction = "ROL"
			amode = "ZP"

		Case 54
			instruction = "ROL"
			amode = "ZPX"

		Case 46
			instruction = "ROL"
			amode = "ABS"

		Case 62
			instruction = "ROL"
			amode = "ABSX"

			'rotate right
		Case 106
			instruction = "ROR"
			amode = "ACC"

		Case 102
			instruction = "ROR"
			amode = "ZP"

		Case 118
			instruction = "ROR"
			amode = "ZPX"

		Case 110
			instruction = "ROR"
			amode = "ABS"

		Case 126
			instruction = "ROR"
			amode = "ABSX"

			'return from interrupt
		Case 64
			instruction = "RTI"
			amode = "IMP"

			'return from subroutine
		Case 96
			instruction = "RTS"
			amode= "IMP"

			'subtract with carry
		Case 233
			instruction = "SBC"
			amode = "IMM"

		Case 229
			instruction = "SBC"
			amode = "ZP"

		Case 245
			instruction = "SBC"
			amode = "ZPX"

		Case 237
			instruction = "SBC"
			amode = "ABS"

		Case 253
			instruction = "SBC"
			amode = "ABSX"

		Case 249
			instruction = "SBC"
			amode = "ABSY"

		Case 225
			instruction = "SBC"
			amode = "INDX"

		Case 241
			instruction = "SBC"
			amode = "INDY"

			'set carry flag
		Case 56
			instruction = "SEC"
			amode = "IMP"

			'set decimal flag
		Case 248
			instruction = "SED"
			amode = "IMP"

			'set interrupt disable
		Case 120
			instruction = "SEI"
			amode = "IMP"

			'Store accumulator
		Case 133
			instruction = "STA"
			amode = "ZP"

		Case 149
			instruction = "STA"
			amode = "ZPX"

		Case 141
			instruction = "STA"
			amode = "ABS"

		Case 157
			instruction = "STA"
			amode = "ABSX"

		Case 153
			instruction = "STA"
			amode = "ABSY"

		Case 129
			instruction = "STA"
			amode = "INDX"

		Case 145
			instruction = "STA"
			amode = "INDY"

			'store x register
		Case 134
			instruction = "STX"
			amode = "ZP"

		Case 150
			instruction = "STX"
			amode = "ZPY"

		Case 142
			instruction = "STX"
			amode = "ABS"

			'store y register
		Case 132
			instruction = "STY"
			amode = "ZP"

		Case 148
			instruction = "STY"
			amode = "ZPX"

		Case 140
			instruction = "STY"
			amode = "ABS"

			'transfer accumulator to X
		Case 170
			instruction = "TAX"
			amode = "IMP"

			'transfer accumulator to Y
		Case 168
			instruction = "TAY"
			amode = "IMP"

			'transfer stack pointer to X
		Case 186
			instruction = "TSX"
			amode = "IMP"

			'transfer X to accumulator
		Case 138
			instruction = "TXA"
			amode = "IMP"

			'transfer x to stack pointer
		Case 154
			instruction = "TXS"
			amode = "IMP"

			'transfer Y to accumulator
		Case 152
			instruction = "TYA"
			amode = "IMP"

		Case Else
			instruction = "Decoder error! " & Str(opc)
			amode = "Decoder error!"

	End Select
	For i As Integer = 255 To 0 Step -1
		opHistory(i) = opHistory(i-1)
	Next

	opHistory(0) = instruction & "(" & amode & ")"
End Sub
