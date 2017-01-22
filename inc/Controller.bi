'Controller input reading by Nobbs66
'Controller input is read as a series of reads to $4016 and $4017 
Sub INS_ReadPad1
	'Read controller 1 with 20 consecutive reads to $4016
	'ToDo: Add support for Tool assisted controller inputs by putting binary string in text file
	'reads 1-8 are for(a, b, select, start up, down, left, right) reads 8-16 are the same but for controller 3
	'reads 17-20 give information as to the type of controller that's plugged in.
	ctrlRead1 += 1
End Sub
Sub INS_ReadPad2
	'Read controller 2 with 20 consectutive read to $4017
	'All reads are the same as controllers 1 and 3, but the address read is on the next byte in RAM
	ctrlRead2 += 1
End Sub
