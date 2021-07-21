; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.
;
; The author grants a license to Michael H. Riley to use this
; code for any purpose he sees fit, including commercial use,
; and without any need to include the above notice.


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

start:     org     2000h
           br      checkver

           ; Build information

           db      7+80h              ; month
           db      21                 ; day
           dw      2021               ; year
           dw      8                  ; build
           db      'Written by David S. Madole',0


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for himem variable to allocate
           ; memory for persistent module to use.

checkver:  ldi     high k_ver         ; pointer to running version
           phi     r8
           ldi     low k_ver
           plo     r8

           lda     r7
           smi     0
           lbnz    allocmem

           lda     r7
           smi     4
           lbnf    versfail


           ; Allocate memory below from the heap for the driver code block,
           ; leaving ; address to copy code into in register R8 and R9. This
           ; is a brute-force way of dealing with the heap manager inability
           ; to allocate aligned blocks, but it seems to work fine.

allocmem:  ldi     high end-module
           phi     rc
           ldi     low end-module
           plo     rc

           ldi     0
           phi     r7
           ldi     4
           plo     r7

           lbr     doalloc

alloclp:   sep     scall
           dw      o_dealloc

           inc     rc

doalloc:   sep     scall
           dw      o_alloc

           glo     rf
           lbnz    alloclp

           glo     rf
           plo     r8
           plo     r9
           ghi     rf
           phi     r8
           phi     r9


           ; Copy the code of the persistent module to the memory block that
           ; was just allocated. R8 and R9 both point to this block before
           ; the copy. R9 will be used but R8 will still point to it after.

           ldi     high end-module
           phi     rf
           ldi     low end-module
           plo     rf

           ldi     high module        ; get source address to copy from
           phi     rd
           ldi     low module
           plo     rd

copycode:  lda     rd                 ; copy code to destination address
           str     r9
           inc     r9
           dec     rf
           glo     rf
           lbnz    copycode
           ghi     rf
           lbnz    copycode


           ; Update kernel and BIOS hooks to point to our module code. At
           ; this point, R9 points to the new BIOS jump table in RAM, and
           ; R8 points to the base address of the module code in RAM.

           ldi     high kernhook      ; Get point to table of patch points
           phi     r7
           ldi     low kernhook
           plo     r7

           ghi     r8
           smi     high module
           str     r2

kernloop:  lda     r7                 ; a zero marks end of the table
           lbz     kerndone

           phi     rd
           lda     r7                 ; get lsb of patch address
           plo     rd
           inc     rd                 ; skip the lbr opcode

           lda     r7
           add
           str     rd

           inc     rd
           lda     r7                 ; add the offset in the table to the
           str     rd                 ; address at the patch point

           lbr     kernloop


           ; At this point we are done, set the baud rate either from command
           ; line if supplied, or auto-baud if not or if supplied rate is not
           ; valid, then output a success message, and end.

kerndone:  SERREQ                     ; Make output in correct state

           ldi     20000.1            ; Default clock speed is 4000 KHz
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

           lbr     return             ; all done, return

notvalid:  sep     scall              ; if any argument not valid, then
           dw      o_setbd            ; jsut auto-baud instead

return:    ldi     message.1
           phi     rf
           ldi     message.0
           plo     rf

           sep     scall
           dw      o_msg

           sep     sret

versfail:  sex     r2
           ldi     vermsg.1
           stxd
           ldi     vermsg.0
           stxd
           lbr     return

message:   db      'Nitro Soft UART Module Build 8 for Elf/OS',13,10,0
vermsg:    db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with offset from the start
           ; of the module in himem to repoint those to.

kernhook:  dw      o_type, type
           dw      o_tty, type
           dw      o_readkey, read
           dw      o_msg, msg
           dw      o_inmsg, inmsg
           dw      o_input, input
           dw      o_inputl, inputl
           dw      o_setbd, setbd
           db      0


; ---------------------------------------------------------------------------
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


; ---------------------------------------------------------------------------
; Baud rate detection
;
; This measures the number of machine cycles time per serial bit. It does so
; by measuring across the 9 bits from the lead of the start bit to the lead
; of the stop bit. It can do so on any ASCII character input since it only
; relies on the highest data bit being zero, which is true for any character.
; Since the measurement loop is 9 machine cycles long, the measurement of
; 9 bits in loop units is equal to the measurement in cycles of one bit.


           org     $ + 0ffh & 0ff00h

module:    ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.


setbd:     SERREQ                      ; Make output in correct state

timersrt:  ldi     0                   ; Wait to make sure the line is idle,
timeidle:  smi     1                   ;  so we don't try to measure in the
           nop                         ;  middle of a character, we need to
           SERN    timersrt            ;  get 256 consecutive loops without
           bnz     timeidle            ;  input asserted before this exits

timestrt:  SERP    timestrt            ; Stall here until start bit begins

           nop                         ; Burn a half a loop's time here so
           ldi     1                   ;  that result rounds up if closer

timecnt1:  phi     re                  ; Count up in units of 9 machine cycles
timecnt2:  adi     1                   ;  per each loop, remembering the last
           lbz     timedone            ;  time that input is asserted, the
           SERN    timecnt1            ;  very last of these will be just
           br      timecnt2            ;  before the start of the stop bit

timedone:  ldi     63                  ; Pre-load this value that we will 
           plo     re                  ;  need in the calculations later

           ghi     re                  ; Get timing loop value, subtract
           smi     23                  ;  offset of 23 counts, if less than
           bnf     timersrt            ;  this, then too low, go try again

           bz      timegood            ; Fold both 23 and 24 into zero, this
           smi     1                   ;  adj is needed for 9600 at 1.8 Mhz

timegood:  phi     re                  ; Got a good measurement, save it

           smi     63                  ; Subtract 63 from time, if less than
           bnf     timekeep            ;  this, then keep the result as-is

timedivd:  smi     3                   ; Otherwise, divide the excess part
           inc     re                  ;  by three, adding to the 63 we saved
           bdf     timedivd            ;  earlier so results are 64-126
        
           glo     re                  ; Get result of division plus 63
           phi     re                  ;  and save over raw measurement

timekeep:  ghi     re                  ; Get final result and shift left one
           shl                         ;  bit to make room for echo flag, then
           adi     2+1                 ;  add 1 to baud rate and set echo flag
           phi     re                  ;  then store formatted result and
           sep     sret                ;  return to caller

; ---------------------------------------------------------------------------
; Input

read:      ldi     0ffh                ; Preload character with all 1's, we
           plo     re                  ;  will look for a 0 to know when done

           ghi     re                  ; Uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; For values less than 63, leave as-is,
           bnf     readcomp            ;  for 63-126, change range to to 0-63

           shl                         ; Multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

readcomp:  ldn     r2                  ; Get half for delay to middle of start
           shr                         ;  bit where we will time other bits
           smi     4                   ;  from, do first delay loop subtract

readwait:  sex     r2
           SERP    readwait            ; Wait here until start bit comes in,
           bnf     readskip            ;  jump based on first delay subtract

           ; Loop over bits

readloop:  smi     4                   ; Delays for cycles equal to the next
           bdf     readloop            ;  higher multiple of 4 of value of D

readskip:  sdi     readjump.0-1        ; Calculate jump based on remainder of
           plo     r3                  ;  timing loop, which is -1 to -4

readjump:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           SERP    readspac            ; If not EF2 then bit is space, go set
           br      readmark            ;  DF, otherwise a mark, leave DF clear

readspac:  smi     0                   ; Jumps here to set DF if a 1 bit

readmark:  glo     re                  ; Get current received character and
           shrc                        ;  shift right to put new bit at MSB,
           plo     re                  ;  move stack pointer to delay value

           ldn     r2                  ; Get timing constant and move SP back
           bdf     readloop            ;  to character, branch if not done

        ;; Done receiving bits

readdone:  smi     8                   ; Delay for half a bit to get to stop
           bdf     readdone            ;  bit, fractional part unimportant

           ghi     re                  ; Get baud rate constant and check
           shr                         ;  the echo flag, if set, we need to
           glo     re
           bdf     type                ;  echo, jump into type

           sep     sret                ;  and return to caller



; ---------------------------------------------------------------------------
; Output

type:      plo     re                  ; Save output character to stack
           stxd 

           ghi     re                  ; Uncompress the stored delay value:
           shr                         ;  shift to remove the echo flag,
           smi     1                   ;  then subtract 1 since 0 is reserved,
           str     r2                  ;  range is now 0-126, save on stack

           smi     63                  ; For values less than 63, leave as-is,
           bnf     typecomp            ;  for 63-126, change range to to 0-63

           shl                         ; Multiply by 2 to get range of 0-126,
           add                         ;  then add saved 63-126, giving range
           str     r2                  ;  of 63-252, update value on stack

typecomp:  ldn     r2                  ; Delay for one bit time before start
typedly1:  smi     4                   ;  bit so we can be called back-to-
           bdf     typedly1            ;  back without a start bit violation

           ldn     r2                  ; Get delay value, advance stack to
           SERSEQ                      ;  point to output character, then
typedly2:  smi     4                   ;  start sending start bit and delay
           bdf     typedly2            ;  for one bit in 4 cycle chunks

           sdi     typejmp2.0-1        ; Calculate jump for remainder left
           smi     0                   ;  from delay loop, set DF to shift a
           plo     r3                  ;  1 in to know when send is complete

           ; Main loop jumps back to here

typejmp2:  skp                         ; Delay 5 cycles from here
           skp                         ;  4 cycles from here
           lskp                        ;  3 cycles from here
           ldi     0                   ;  2 cycles from here

           glo     re                  ; Get character value to send,
           shrc                        ;  shift out lowest bit, update stack
           plo     re                  ;  to bit time delay value

           bz      typestop            ; If all bits clear, we are done, 
           bdf     type1bit            ;  otherwise jump if sending a 1 bit

           SERSEQ                      ; Send a zero bit
           ldn     r2                  ;  advance the stack pointer back
typedly3:  smi     4                   ;  to character value, then delay
           bdf     typedly3            ;  for one bit time

           sdi     typejmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

type1bit:  SERREQ                      ; Send a one bit, this block of code
           ldn     r2                  ;  is the same as for the zero bit
typedly4:  smi     4                   ;  above but is factored out twice
           bdf     typedly4            ;  to meet timing constraints

           sdi     typejmp2.0-1        ; Calculate the delay jump, this also
           plo     r3                  ;  returns to the start of the loop

        ;; End of character

typestop:  SERREQ                      ; We are done sending, set the stop bit
           inc     r2                  ;  return input if this was an echo,
           ldn     r2

           sep     sret                ;  then return to the caller

           ;

msglp:     sep     scall
           dw      o_type

msg:       lda     rf
           bnz     msglp

           sep     sret

           ;

inmsglp:   sep     scall
           dw      o_type

inmsg:     lda     r6
           bnz     inmsglp

           sep     sret

           ;
           org     ($ + 0ffh & 0ff00h) - 16

input:     ldi     0                   ; allow 256 input bytes
           phi     rc
           ldi     255
           plo     rc

           ; Input:
           ;   RC - maximum input size
           ;   RF - pointer to buffer
           ; Output:
           ;   RC - modified
           ;   RF - points to terminating zero
           ;   DF - set if control-c pressed

inputl:    ghi     re                   ; disable input echo but save first
           stxd                         ; so we can put back later
           ani     0feh
           phi     re

           glo     ra                   ; free up as a working register
           stxd
           ghi     ra
           stxd

           ldi     0                    ; pre-populate terminating 0
           str     rf
           plo     ra                   ; zero cursor position counter

inptloop:  sep     scall                ; get next character
           dw      o_readkey
           plo     re                   ; save a copy

           smi     32                   ; is it a printable char?
           bdf     printchr

           smi     3-32                 ; is it control-c?
           bz      inptdone 

           smi     4-3                  ; is it control-d?
           bz      deletech 

           smi     6-4                  ; is it control-f?
           bz      insertch 

           smi     8-6                  ; is it control-h?
           bz      backspac

           smi     12-8                 ; is it control-l?
           bz      forespac

           smi     13-12                ; is it return?
           bz      inptdone

           smi     21-13                ; is it control-u?
           bz      wipeline

           br      inptloop             ; ignore anything else


           ; Add a character to input, appending to end and extending if we
           ; are at the end of existing input, or overwriting if we are in
           ; the middle somewhere. Either way output the character too.

printchr:  smi     127-32
           bz      delete2

           ldn     rf                   ; are we at the end of current input?
           stxd                         ; remember answer
           bnz     inptchar

           glo     rc                   ; if extending, check if theres room
           bz      inptloop

inptchar:  glo     re
           str     rf
           inc     rf

           sep     scall                ; and output
           dw      o_type

           inc     ra                   ; move cursor to right

           inc     r2
           ldn     r2                   ; recheck if we are extending
           bnz     inptloop

           str     rf                   ; if so, add new zero terminator
           dec     rc                   ; and decrease the input limit
           br      inptloop


           ; Control-H moves the cursor left without changing the buffer.

backspac:  glo     ra                   ; ignore if we are all the way left
           bz      inptloop

dobacksp:  glo     re                   ; output the backspace
           sep     scall
           dw      o_type

           dec     ra                   ; move the cursor and buffer pointer
           dec     rf
           br      inptloop


           ; Control-L moves the cursor right without changing the buffer.

forespac:  ldn     rf                   ; if at the end then do nothing
           bz      inptloop

           sep     scall                ; output char at cursor to move right
           dw      o_type

           inc     ra
           inc     rf
           br      inptloop


           ; Either a control-c or return has been typed so we are done.
           ; The only difference between the two is how DF is set at exit.

inptdone:  lda    rf                    ; make rf point to terminator at end
           bnz    inptdone
           dec    rf

           ldi    13                   ; output carriage return
           sep    scall
           dw     o_type

           glo    re                   ; set df=1 if it is a control-c
           sdi    3
           sdi    0

           inc    r2                    ; restore saved register
           lda    r2
           phi    ra
           lda    r2
           plo    ra

           ldn    r2                   ; restore echo flag and return
           phi    re
inptsret:  sep    sret


           ; Wipe out the entire input

wipeline:  glo    ra
           bz     wipespac

           ldi    8
           sep    scall
           dw     o_type

           dec    ra
           dec    rf
           br     wipeline

wipespac:  ldn    rf
           bz     wipeback

           ldi    32
           sep    scall
           dw     o_type

           inc    ra
           inc    rc
           inc    rf
           br     wipespac

wipeback:  glo    ra
           bz     wipedone

           ldi    8
           sep    scall
           dw     o_type

           dec    ra
           dec    rf
           br     wipeback

wipedone:  str    rf
           br     inptloop


           ; Insert space character into input at current location

insertch:  glo     ra
           stxd
           ldi     0
           plo     ra

           ldi     32
           plo     re

insloop1:  ldn     rf                   ; push characters out until end
           stxd
           glo     re
           str     rf
           bz      insloop2

           sep     scall                ; output character as we go
           dw      o_type

           inc     r2
           ldn     r2
           plo     re
           inc     rf
           inc     ra
           br      insloop1

insloop2:  glo     ra                  ; back to starting point outputting
           bz      instdone

           ldi     8
           sep     scall
           dw      o_type

           dec     ra
           dec     rf
           br      insloop2

instdone:  dec     rc
           inc     r2
           inc     r2
           ldn     r2
           plo     ra
           br      inptloop

           ; Deletecharacter from input at current location

delete2:   glo     ra
           bz      inptloop

           dec     ra
           dec     rf

           ldi     8
           sep     scall
           dw      o_type

deletech:  ldn     rf                   ; if at end then nothing to do
           bz      inptloop

           glo     ra
           stxd
           ldi     0
           plo     ra

delloop1:  inc     rf
           ldn     rf
           dec     rf
           str     rf
           bz      delspace

           sep     scall                ; output character as we go
           dw      o_type

           inc     rf
           inc     ra
           br      delloop1

delspace:  ldi     32
           sep     scall
           dw      o_type

delloop2:  ldi     8
           sep     scall
           dw      o_type

           glo     ra                  ; back to starting point outputting
           bz      deledone

           dec     ra
           dec     rf
           br      delloop2

deledone:  inc     rc
           inc     r2
           ldn     r2
           plo     ra
           br      inptloop

end:       ; That's all folks!


