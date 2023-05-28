M80PATH=D:/M80

.SUFFIXES: .ASM .REL .BIN

MonRk.REL: MonRK.asm
	$(M80PATH)/M80 '=$< /I/L'

clean:
	del *.REL
	del *.PRN
	del *.BIN

all: MonRk.rkl

MonRK.rkl: MonRK.BIN
	../makerk/Release/makerk.exe b000 $< $@


MonRk.bin: MonRk.REL
	$(M80PATH)/L80 /P:100,$<,$@/N/E

