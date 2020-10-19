# 16-bit x86 VGA Programming

This code is the result of some x86 VGA programming trial and error.

`boot.s` contains a basic bootloader implementation that reads the first 64k bytes off the bootdisk to memory location 1000h:0000h (0x10000), the first 512bytes (200h) are a copy of the master boot record (MBR) itself. Stage2 execution therefore starts at 1000h:0200h which is setup as the entry point of `vga.s`.


There are some basic VGA routines in vga.s, some linear (chunky graphics) memory mode 13h 320x200 256 color, and some planar graphics in mode 12h 640x480 16 color. I have a (very) graphics mode basic font rendering implementation here as well.

I am publishing this just in case anyone else is having fun figuring out old school PC graphics as well!

*last updated Monday, 19th October, 2020*