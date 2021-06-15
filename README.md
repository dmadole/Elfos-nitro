This module installs an alternative soft UART implementation for Elf/OS to replace the one in BIOS, but without actually modifying BIOS. The replacement UART is capable of running from 1900 to 21000 baud with a 4Mhz processor clock, and proportionally high or lower for other clocks. It can run at 9600 baud at 1.8Mhz.

It also has a replacement for the standard BIOS f_input calls that are used for reading lines from the console. The replacement function allows some basic editing of text as it is being input. This works at the Elf/OS command line, Rc/basic, Rc/forth, and theoretically everything else that uses these standard BIOS functions.

The module loads into high memory on Elf/OS and so requires kernel 0.3.1 which initializes the himem variable. It only runs in high memory, as it is too large to load into kernel memory like turbo optionally can, so it cannot coexist with applications not yet compatible with high memory, and there is no workaround. It does coexist with turbo.

PLEASE NOTE: This module is not compatible with "xr" and "xs" and very likely never will be. I have written a YMODEM and XMODEM receiver named "yr" which is compatible with this UART (as well as the standard one, for supported baud rates). At some point if there is interest, I ma write a "ys" also to go with it.

NOTE ALSO: Unlike turbo which should run anywhere Elf/OS can run, this one needs to be built for specific EF input pins and signal polarity. This copy is built for Pico/Elf-like hardware, but if someone would like to try it on something else, I can make a different build for them.

Note that when you run the program, you must press a key so that it can detect baud rate, so don’t think it’s dead or crashed This needs to be done since it uses an measurement incompatible with BIOS due to the higher range. You can select a new baud rate on your terminal before pressing the key to change speed.

As of build 6, you can specify a baud rate on the command line rather than using auto-baud. In this case the command would look like "nitro 9600" to set the baud rate to 9600. Note that nitro assumes a clock speed of 4 Mhz in calculating the baud rate delay. If this is not correct for your system, you can use the "-k" option to specify a different clock speed in kilohertz. For example, if you have a 1.79 Mhz clock, you could use "nitro -k 1790 9600" to set the baud rate to 9600. If the parameters specified are not valid, no error message is emitted, rather the speed is set by auto-baud instead.

Here are the editing commands for input line editing. These are based on pure ASCII and not any particular terminal type:

Control-D -- deletes character at cursor
Control-F -- inserts space at cursor
Control-H -- moves cursor to left (backspace)
Control-L -- moves cursor to right
Control-U -- Erases entire input
Delete -- Deletes character to left of cursor

Some additional ideas are adding command history so you can bring back prior input lines and edit and reuse them, and maybe recognize function key sequences so that arrow and other keys can be used.

