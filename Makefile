M80PATH=D:/M80
PORT=COM3

.SUFFIXES: .ASM .REL .BIN

.ASM.REL:
	$(M80PATH)/M80 '=$< /I/L'

MonRk.REL: MonRK.asm

MonZ80.REL: MonZ80.ASM

clean:
	del *.REL
	del *.PRN
	del *.BIN

all: MonRk.rkl MonZ80.rkl

send: MonRK.rkl
	MODE $(PORT): baud=115200 parity=N data=8 stop=1
	cmd /C copy /B  $< $(PORT)

send2: MonZ80.rkl
	MODE $(PORT): baud=115200 parity=N data=8 stop=1
	cmd /C copy /B  $< $(PORT)

MonRK.rkl: MonRK.BIN
	../makerk/Release/makerk.exe 100 $< $@

MonRk.bin: MonRk.REL
	$(M80PATH)/L80 /P:100,$<,$@/N/E

MonZ80.bin: MonZ80.REL
	$(M80PATH)/L80 /P:100,$<,$@/N/E

MonZ80.rkl: MonZ80.BIN
	../makerk/Release/makerk.exe 100 $< $@


