PROJECT = nitro

$(PROJECT).prg: $(PROJECT).asm bios.inc kernel.inc
	rcasm -l -v -x -d 1802 $(PROJECT) | tee $(PROJECT).lst
	hextobin $(PROJECT)

clean:
	-rm -f $(PROJECT).lst
	-rm -f $(PROJECT).prg
	-rm -f $(PROJECT).bin

