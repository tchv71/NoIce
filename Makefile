EMUPATH=D:\Emu80qt_40444
M80PATH=D:/M80

.SUFFIXES: .ASM .REL .BIN

MonRk.REL: MonRK.asm
	$(M80PATH)/M80 '=$< /I/L'

clean:
	del *.REL
	del *.PRN
	del *.BIN

all: MonRk.bin

MonRk.bin: MonRk.REL
	$(M80PATH)/L80 /P:100,$<,$@/N/E

run: bin/ESC80_32k.rk
	$(EMUPATH)/Emu80Qt bin/ESC80_palmira.rk
