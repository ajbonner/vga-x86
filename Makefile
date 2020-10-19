all: clean boot.bin vga.bin disk_img

boot.bin:
	nasm -f bin -o boot.bin boot.s

vga.bin:
	nasm -f bin -o vga.bin vga.s

clean:
	rm -rf *.bin *.img

run: disk_img
	qemu-system-i386 -drive format=raw,file=disk.img,index=0,if=floppy -machine pc

run_vga: clean vga.bin
	qemu-system-i386 -drive format=raw,file=vga.bin,index=0,if=floppy -machine pc

run_bochs: all
	bochs -q -rc debug.rc

disk_img: clean boot.bin vga.bin
	cat boot.bin vga.bin > disk.img

dis_boot: boot.bin
	objdump -m i8086 -M intel -b binary -Dx boot.bin

dis_vga: vga.bin
	objdump -m i8086 -M intel -b binary -Dx vga.bin