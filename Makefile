M80PATH=D:/M80
PORT=COM5

.SUFFIXES: .ASM .REL .BIN

MonRk.REL: MonRK.asm
	$(M80PATH)/M80 '=$< /I/L'

clean:
	del *.REL
	del *.PRN
	del *.BIN

all: MonRk.rkl
send: MonRK.rkl
	MODE $(PORT): baud=115200 parity=N data=8 stop=1
	cmd /C copy /B  $< $(PORT)

MonRK.rkl: MonRK.BIN
	../makerk/Release/makerk.exe 100 $< $@


MonRk.bin: MonRk.REL
	$(M80PATH)/L80 /P:100,$<,$@/N/E

