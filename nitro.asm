
;  Copyright 2021, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc


           ; Pin and polarity definitions

#ifdef INVERT
#define    SERP    b2
#define    SERN    bn2
#define    SERSEQ  seq
#define    SERREQ  req
#else
#define    SERP    bn2
#define    SERN    b2
#define    SERSEQ  req
#define    SERREQ  seq
#endif


           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     br      entry


           ; Build information

           db      8+80h              ; month
           db      6                  ; day
           dw      2021               ; year
           dw      0                  ; build

           db      'See github.com/dmadole/Elfos-nitro for more info',0


           ; Check if hook points have already been patched and do not
           ; install if so, since we don't know what it is or what the
           ; impact might be of disconnecting it.

entry:     ldi     high hooklist      ; Get point to table of patch points
           phi     rd
           ldi     low hooklist
           plo     rd

chekloop   lda     rd                 ; a zero marks end of the table
           lbz     chekvers

           phi     rf                 ; get pointer to patch point
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           ldn     rd                 ; if points into kernel then ok
           smi     20h
           lbdf    cheknext

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Input or output hooks already installed',13,10,0
           sep     sret

cheknext:  inc     rd                 ; skip target address in table
           inc     rd

           lbr     chekloop           ; repeat for all


           ; Check minimum needed kernel version 0.4.0 in order to have
           ; heap manager available.

chekvers:  ldi     high k_ver         ; pointer to installed kernel version
           phi     rd
           ldi     low k_ver
           plo     rd

           lda     rd                 ; if major is non-zero then good
           lbnz    allocmem

           lda     rd                 ; if minor is 4 or more then good
           smi     4
           lbdf    allocmem

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
           sep     sret


           ; Allocate a page-aligned block from the heap for storage of
           ; the persistent code module. Make it permanent so it will
           ; not get cleaned up at program exit.

allocmem:  ldi     high end-module     ; length of persistent module
           phi     rc
           ldi     low end-module
           plo     rc

           ldi     255                 ; page-aligned
           phi     r7
           ldi     4                   ; permanent
           plo     r7

           sep     scall               ; request memory block
           dw      o_alloc
           lbnf    gotalloc

           sep     scall               ; return with error
           dw      o_inmsg
           db      'ERROR: Could not allocate memeory from heap',13,10,0
           sep     sret

gotalloc:  ghi     rf                  ; Offset to adjust addresses with
           smi     high module
           stxd


           ; Copy module code into the permanent heap block

           ldi     high end-module     ; length of code to copy
           phi     rc
           ldi     low end-module
           plo     rc

           ldi     high module         ; get source address
           phi     rd
           ldi     low module
           plo     rd

copycode:  lda     rd                  ; copy code to destination address
           str     rf
           inc     rf
           dec     rc
           glo     rc
           lbnz    copycode
           ghi     rc
           lbnz    copycode


           ; Update kernel hooks to point to the copied module code

           ldi     high hooklist      ; Get point to table of patch points
           phi     rd
           ldi     low hooklist
           plo     rd

           inc     r2                 ; point to page offset on stack

hookloop:  lda     rd                 ; a zero marks end of the table
           lbz     hookdone

           phi     rf                 ; get pointer to vector to hook
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           lda     rd                 ; add offset to get copy address
           add                        ;  and update into vector
           str     rf
           inc     rf
           lda     rd
           str     rf

           lbr     hookloop           ; repeat for all


           ; At this point we are done, set the baud rate either from command
           ; line if supplied, or auto-baud if not or if supplied rate is not
           ; valid, then output a success message, and end.

hookdone:  ldi     20000.1            ; Default clock speed is 4000 KHz
           phi     rc                 ; times 5 to give baud rate factor,
           ldi     20000.0            ; which is clock in Hertz divded by 8
           plo     rc                 ; then divided by 25

skipspc1:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc1

           smi     '-'-'!'            ; if next character is not a dash,
           lbnz    getbaud            ; then no option

           lda     ra                 ; if option is not 'k' then it is
           smi     'k'                ; not valid
           lbnz    notvalid

skipspc2:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc2

           dec     ra                 ; back up to non-whitespace character

           ghi     ra                 ; move input pointer to rf
           phi     rf
           glo     ra
           plo     rf

           sep     r4                 ; parse input number
           dw      f_atoi
           lbdf    notvalid           ; if not a number then abort

           ghi     rf                 ; save updated pointer to just past
           phi     ra                 ; number back into ra
           glo     rf
           plo     ra

           ldi     0                  ; multiply clock in khz by 5
           phi     rf
           ldi     5
           plo     rf

           sep     r4                 ; do multiply
           dw      f_mul16

           ghi     rc                 ; if more than a word, then out of
           lbnz    notvalid           ; range and not valid
           glo     rc
           lbnz    notvalid

           ghi     rb
           phi     rc
           glo     rb
           plo     rc

skipspc3:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc3

getbaud:   dec     ra                 ; back up to non-whitespace character

           ghi     ra                 ; move input pointer to rf
           phi     rf
           glo     ra
           plo     rf

           sep     r4                 ; parse input number
           dw      f_atoi
           lbdf    notvalid           ; if not a number then abort

           ghi     rd                 ; transfer baud rate to rf
           phi     rf
           glo     rd
           plo     rf

           ldi     0                  ; divide baud rate by 25 to scale
           phi     rd                 ; values to be managable in 16 bits
           ldi     25
           plo     rd

           sep     r4                 ; do division
           dw      f_div16

           ghi     rb                 ; move quotient rb to divisor rd
           phi     rd
           glo     rb
           plo     rd

           ghi     rc                 ; get clock rate factor into
           phi     rf                 ; dividend rf
           glo     rc
           plo     rf

           sep     r4                 ; divide scaled clock rate by
           dw      f_div16            ; scaled baud rate

           ghi     rb                 ; if result is larger than 255
           lbnz    notvalid           ; then out of range

           glo     rb                 ; transfer baud rate factor to re
           phi     re

           sep     r4                 ; load compressed value into re.1
           dw      timedone           ; if this fails, it will auto-baud

           lbr     finished           ; all done, return

notvalid:  sep     scall              ; if any argument not valid, then
           dw      o_setbd            ; just auto-baud instead


finished:  sep     scall
           dw      o_inmsg
           db      'Nitro Soft UART Module Build 0 for Elf/OS',13,10,0

           sep     sret


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with offset from the start
           ; of the module in himem to repoint those to.

hooklist:  dw      o_type, type
           dw      o_tty, type
           dw      o_readkey, read
           dw      o_msg, msg
           dw      o_inmsg, inmsg
           dw      o_input, input
           dw      o_inputl, inputl
           dw      o_setbd, setbd
           db      0


           org     $ + 0ffh & 0ff00h

; Soft high-speed UART input and output routines
;
; This maintains the existing BIOS convention for the baud rate constant in
; register RE.1 although the actual timing values are different due to the
; higher resolution and baud rate supported.
;
; Only the high 7 bits of RE are used to hold the baud rate, and the lowest
; bit is used to indicate if local echo is enabled in the input routine.
;
; A baud rate of 0 is reserved for another meaning in BIOS so valid values
; are 1-127 after shifting right one bit to remove the echo flag.
;
; To increase the range of baud rates, this is normalized to a range of
; 0-252, with 1-64 representing 0-63 and 64-126 representing 67-252. This
; sacrifices resolution at lower baud rates where it doesn't matter since
; the percentage error becomes lower as the baud rate is lower.
;
; This supports baud rates of approximately 2000 to 21000 on a 4 Mhz clock,
; and proportionally higher or lower for other clocks. For a 1.8 Mhz PIXIE
; clock in particular, this supports approximately 900 to 9600 baud.
;
; Within this module, a simplified subroutine convention is used to save
; overhead of SCRT and also saving and restoring registers needlessly. The
; way a call is done is to load the return address within the page into the
; D register, and then do a short branch to the subroutine:
;
;          ldi     retaddr
;          br      subroutine
;
; The subroutine pushes the return address to the stack, does it's work,
; then returns by popping the return address and setting it into the low
; byte of the program counter, effecting a branch:
;
;          stxd
;          ...
;          irx
;          ldx
;          plo     r3
;
; This takes just 6 instructions as compared to SCRT which takes 32 as it is
; implemented in Elf/OS. Obviously this only works within a single page of
; memory. It is also not possible to pass values in the D register; this
; uses RE.0 to pass values which works well with the Elf/OS SCRT anyway.
;
; Because the return address is arbitrary and does not have to be in-line
; to execution, this can sometimes replace a branch instruction, so the
; overhead can be only 5 instructions rather than 6.
;
; This is derived from a technique described by Wayne Bowdish on page 22 of
; IPSO FACTO issue 12 (June 1979).


module:    ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.


           ; Read a character from input. This is an extremely thin
           ; wrapper around the recv "thin call" subroutine. If echo
           ; is enabled, it is done from here.

read:      ldi     low echo            ; get input character
           br      recv

echo:      ghi     re                  ; if echo flag clear, just return
           shr
           bnf     return


           ; Send a character to output. This is a thin wrapper around the
           ; send "thin call" subroutine. Note that the read subroutine
           ; above falls through to this when echo is needed.

type:      ldi     return              ; echo character back
           br      send

return:    glo     re                  ; return character
           sep     sret


           ; Output a string from memory pointed to by RF.

msglp:     ldi     low msg
           br      send

msg:       lda     rf
           plo     re
           bnz     msglp

           sep     sret


           ; Output a string from memory pointed to by R6. Since this is
           ; called from SCRT this has the effect of outputting an inline
           ; string immediately following the call instructions.

inmsglp:   ldi     low inmsg
           br      send

inmsg:     lda     r6
           plo     re
           bnz     inmsglp

           sep     sret


           ; This "thin call" call subroutine receives a character through
           ; the EF2 line at the baud rate specified in RE.1

recv:      stxd                        ; push return address

           ldi     0ffh                ; preload character with all 1's, we
           plo     re                  ;  will look for a 0 to know when done

           ghi     re                  ; uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; for values less than 63, leave as-is,
           bnf     recvcomp            ;  for 63-126, change range to to 0-63

           shl                         ; multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

recvcomp:  ldn     r2                  ; get half for delay to middle of start
           shr                         ;  bit where we will time other bits
           smi     4                   ;  from, do first delay loop subtract

recvwait:  sex     r2
           SERP    recvwait            ; wait here until start bit comes in,
           bnf     recvskip            ;  jump based on first delay subtract

           ; Loop over bits

recvloop:  smi     4                   ; delays for cycles equal to the next
           bdf     recvloop            ;  higher multiple of 4 of value of d

recvskip:  sdi     recvjump.0-1        ; calculate jump based on remainder of
           plo     r3                  ;  timing loop, which is -1 to -4

recvjump:  skp                         ; delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           SERP    recvspac            ; if not ef2 then bit is space, go set
           br      recvmark            ;  df, otherwise a mark, leave df clear

recvspac:  smi     0                   ; jumps here to set df if a 1 bit

recvmark:  glo     re                  ; get current received character and
           shrc                        ;  shift right to put new bit at msb,
           plo     re                  ;  move stack pointer to delay value

           ldn     r2                  ; get timing constant and move sp back
           bdf     recvloop            ;  to character, branch if not done

        ;; Done receiving bits

recvdone:  smi     8                   ; delay for half a bit to get to stop
           bdf     recvdone            ;  bit, fractional part unimportant

           inc     r2                  ; pop return address and jump to it
           ldn     r2
           plo     r3


           ; This "thin call" subroutine outputs a character through the
           ; Q line based on the baud rate given in RE.1.

send:      stxd                        ; push return address

           ghi     re                  ; Uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; For values less than 63, leave as-is,
           bnf     sendcomp            ;  for 63-126, change range to to 0-63

           shl                         ; Multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

sendcomp:  ldn     r2                  ; Delay for one bit time before start
senddly1:  smi     4                   ;  bit so we can be called back-to-
           bdf     senddly1            ;  back without a start bit violation

           ldn     r2                  ; Get delay value, advance stack to
           SERSEQ                      ;  point to output character, then
senddly2:  smi     4                   ;  start sending start bit and delay
           bdf     senddly2            ;  for one bit in 4 cycle chunks

           sdi     sendjmp2.0-1        ; Calculate jump for remainder left
           smi     0                   ;  from delay loop, set DF to shift a
           plo     r3                  ;  1 in to know when send is complete

           ; Main loop jumps back to here

sendjmp2:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           glo     re                  ; Get character value to send,
           shrc                        ;  shift out lowest bit, update stack
           plo     re                  ;  to bit time delay value

           bz      sendstop            ; If all bits clear, we are done, 
           bdf     send1bit            ;  otherwise jump if sending a 1 bit

           SERSEQ                      ; Send a zero bit
           ldn     r2                  ;  advance the stack pointer back
senddly3:  smi     4                   ;  to character value, then delay
           bdf     senddly3            ;  for one bit time

           sdi     sendjmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

send1bit:  SERREQ                      ; Send a one bit, this block of code
           ldn     r2                  ;  is the same as for the zero bit
senddly4:  smi     4                   ;  above but is factored out twice
           bdf     senddly4            ;  to meet timing constraints

           sdi     sendjmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

        ;; End of character

sendstop:  SERREQ                      ; We are done sending, set the stop bit

           inc     r2                  ; pop return address and jump to it
           ldn     r2
           plo     r3


           ; Input a line into a buffer, either of fixed length 256 for
           ; input or of a length given in RC in inputl. We do this a little
           ; better than the stock bios one because we don't echo every
           ; character so things like backspacing past the beginning of the
           ; line can't happen, or entry of random control characters.

input:     ldi     high 256            ; preset for fixed-size version
           phi     rc
           ldi     low 256
           plo     rc

inputl:    dec     rc                  ; space for terminating zero

           glo     rb                  ; use rb for counting input
           stxd
           ghi     rb
           stxd

           ldi     0                   ; zero input count
           phi     rb
           plo     rb

getchar:   ldi     gotchar             ; read input character
           br      recv

gotchar:   glo     re                  ; get character

           smi     127                 ; got backspace
           bz      gotbksp

           adi     127-32              ; printing character received
           bdf     gotprnt

           adi     32-8                ; backspace received
           bz      gotbksp

           adi     8-3                 ; control-c received
           bz      gotctlc

           adi     3-13               ; carriage return received
           bnz     getchar


           ; Return from input due to either return or control-c. When
           ; either entry point is called, D will be zero and DF will be
           ; set as a result of the subtraction used for comparison.

           shr                         ; clear df flag if return char

gotctlc:   str     rf                  ; zero-terminate input string

           inc     r2                  ; restore saved rb
           lda     r2
           phi     rb
           ldn     r2
           plo     rb

           shlc                        ; push df flag
           plo     re

           ghi     re                  ; if echo disabled, get next char
           shr
           bnf     notecho

           sep     scall
           dw      o_inmsg
           db      13,10,0

notecho:   glo     re                  ; restore saved df flag, return
           shr
           sep     sret


           ; If a printing character, see if there is any room left in
           ; the buffer, and append it if there is, ignore otherwise.

gotprnt:   glo     rc                  ; if any room for character
           bnz     addprnt
           ghi     rc                  ; if not any room for character
           bz      getchar

addprnt:   glo     re
           str     rf                  ; append character to buffer
           inc     rf

           dec     rc                  ; increment count, decrement space
           inc     rb

           ghi     re                  ; if echo disabled, get next char
           shr
           bnf     getchar

           ldi     getchar             ; echo char and get next
           br      send


           ; Process a backspace received: if not at beginning of buffer,
           ; decrement buffer and count, increment free space, and output
           ; a backspace-space-backspace sequence to erase character.

gotbksp:   glo     rb
           bnz     dobkspc
           ghi     rb
           bz      getchar

dobkspc:   dec     rf                  ; back up pointer

           dec     rb                  ; decrement count, increment space
           inc     rc

           ghi     re                  ; if echo disabled, get next char
           shr
           bnf     getchar

           sep     scall
           dw      o_inmsg
           db      8,32,8,0

           br      getchar



           org     $ + 0ffh & 0ff00h

           ; Perform baud rate detection by measuring the start bit to
           ; stop bit in units of nine machine cycles, which will take nine
           ; times the amount of time in a single bit, therefore, the 
           ; measurement is equal to the number of machine cycles in each
           ; bit. Use a simple compression method to fit the eight bit
           ; measurement into the the seven bits allowed in RE.1. Because
           ; this measures the entire character, it will work on any
           ; ASCII character as it just needs bit 7 clear.

setbd:     SERREQ                      ; make output in correct state

timersrt:  ldi     0                   ; wait to make sure the line is idle,
timeidle:  smi     1                   ;  so we don't try to measure in the
           nop                         ;  middle of a character, we need to
           SERN    timersrt            ;  get 256 consecutive loops without
           bnz     timeidle            ;  input asserted before this exits

timestrt:  SERP    timestrt            ; stall here until start bit begins

           nop                         ; burn a half a loop's time here so
           ldi     1                   ;  that result rounds up if closer

timecnt1:  phi     re                  ; count up in units of 9 machine cycles
timecnt2:  adi     1                   ;  per each loop, remembering the last
           lbz     timedone            ;  time that input is asserted, the
           SERN    timecnt1            ;  very last of these will be just
           br      timecnt2            ;  before the start of the stop bit

timedone:  ldi     63                  ; pre-load this value that we will 
           plo     re                  ;  need in the calculations later

           ghi     re                  ; get timing loop value, subtract
           smi     23                  ;  offset of 23 counts, if less than
           bnf     timersrt            ;  this, then too low, go try again

           bz      timegood            ; fold both 23 and 24 into zero, this
           smi     1                   ;  adj is needed for 9600 at 1.8 mhz

timegood:  phi     re                  ; got a good measurement, save it

           smi     63                  ; subtract 63 from time, if less than
           bnf     timekeep            ;  this, then keep the result as-is

timedivd:  smi     3                   ; otherwise, divide the excess part
           inc     re                  ;  by three, adding to the 63 we saved
           bdf     timedivd            ;  earlier so results are 64-126
        
           glo     re                  ; get result of division plus 63
           phi     re                  ;  and save over raw measurement

timekeep:  ghi     re                  ; get final result and shift left one
           shl                         ;  bit to make room for echo flag, then
           adi     2+1                 ;  add 1 to baud rate and set echo flag
           phi     re                  ;  then store formatted result and
           sep     sret                ;  return to caller


end:       ; That's all folks!


