

;---------------------------------------------------
; Name: Adam Coyte
; Email:
; OS Name: HempOS
; student number:
;
; Details
; ----------------------------------
; This file is part of a simple 16 bit operating system.
; It resides within the first boot sector.
;---------------------------------------------------
.386						            ; Compile for a 80386 CPU
 option segment:use16				; Force 16 bit segments instead of default 32 bit
.model tiny         				; Tiny memory model



.code		     				; Start of code segment
 org 07c00h					; Bootloader entry point


main:
jmp short start
nop



;---------------------------------------------------
start:
;
; Summary:  Start of the main operating system code.
;
;---------------------------------------------------
  ;don't use that

	cli
    xor ax, ax					; Set AX to zero
    mov ds, ax					; Set data segment to where we are loaded
    add ax, 20h					; Skip over the size of the bootloader divided by 16 (512 / 16)
    mov ss, ax					; Set segment register to current location (start of the stack)
    mov sp, 4096				; Set ss:sp to the top of the 4k stack
    sti

;--------------------------------------------------------------------------

Intro:
    mov si,OFFSET hello      ;place the memory address of hello into esi
                             ;offset is just making sure it's on an even boundary
    call PrintString         ;cals
mainloop:                  ;label for mainloop
    call Newloin          ;call for a newline

;---------------------------------------------------------------------------
;The way that the string compare works is that the proccessor will check if cx is zero
;if it is not if will perform the string operation once and increment SI and DI(Direction flag has to be clear for this)
;It will decrement cx without modifyinh any flag
;it will then check the zero flag
;if the repeat condition holds then it will loop back to step 1 otherwise it will break and proceep onto next instruction
;
;
;REPE repeats the instruction CX times while vlaues are equal this means that CX must
;be larger than the string because then it won't stop checking the moment that you write
;the command name in, it'll read past and make sure that nothing else is added on afterwards
;
;for example: it will accept 'help' but not 'helpeweqweqwe' because it checks to make sure
;nothing is address afterwards
;
;This is how i understand the string compare
;----------------------------------------------------------------------------



;--------------------------------------------------------------------------

    lea si,prompt           ;load` address of promt in SI
    call PrintString        ;call printsting to print the string

    mov di,buffer            ;place the address of the variable in DI
    call InputString        ;call the inputstring function
;----------------------------
    cld
    mov cx,6                ;place 6 into cx because colr string is 5 char long
    mov si,OFFSET colr      ;place the address of colr in SI
    mov di,buffer           ;place the address of buffer in DI
    repe cmpsb              ;make sure they are equal
    je screenc              ;if equal jump
;----------------------------
    cld
    mov cx,5
    mov si,OFFSET ree      ;place the address of hlp in SI
    mov di,buffer   ;place the address of buffer in DI
    repe cmpsb
    je printX
;----------------------------
    cld
     mov cx,5
     mov si,OFFSET bep      ;place the address of bep in SI
     mov di,buffer   ;place the address of buffer in DI
     repe cmpsb       ;comapre
     je bell          ;jump if euqual
;----------------------------
    cld
    mov cx,4
    mov si,OFFSET scrn      ;place the address of bep in SI
    mov di,buffer   ;place the address of buffer in DI
    repe cmpsb
    je clearing
;----------------------------
    cld
    mov cx,6
    mov si,OFFSET rando      ;place the address of bep in SI
    mov di,buffer   ;place the address of buffer in DI
    repe cmpsb
    je random
;---------------------------
    mov cx,5
    mov si,OFFSET art      ;place the address of bep in SI
    mov di,buffer   ;place the address of buffer in DI
    repe cmpsb
    je tree
;---------------------------
    mov di,buffer ;place the address of the buffer into di
    mov ax,[di]   ;move the contents of the buffer into ax
    cmp al,00h    ;if those contents are merely a enter
    je mainloop   ;jump to mainloop


    call badcommand  ;display error
    jmp mainloop


;------------------------------------------------------------------------------
;This is the Data
;-----------------------------------------------------------------------------
      hello db 'HempOS', 0Dh, 0Ah, 0 ;delaring string, this is the welcome promtp
      prompt db '@>',0      ;delaring string, this is the prompt
      bep db 'beep',0       ;delaring string, this is the command
      colr db 'backc',0     ;delaring string, this is the command
      ree db "help",0       ;delaring string, this is the command
      scrn db 'clr',0     ;delaring string, this is the command
      rando db 'randc',0    ;delaring string, this is the command
      art db "tree",0       ;delaring string, this is the command
      entrval db 'Val:',0 ;delaring string, this is fro prompting user for a value
      buffer equ 3000       ;delaring variable, this is where the string is
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
;This displays all the commands available
;by placing each string into SI register and calling the PrintString function
;------------------------------------------------------------------------------
printX:
      call Newloin      ;newline

      mov si,OFFSET bep   ;move the address of the strign into si
      call PrintString    ;print the string

      call Newloin        ;newline
;--
      mov si,OFFSET colr  ;move the address of the string into si
      call PrintString    ;print the string

      call Newloin      ;newline
;--
      mov si,OFFSET ree   ;move the address of the string into si
      call PrintString    ;print the strign

      call Newloin        ;print newline
;--
      mov si,OFFSET scrn    ;move the address of the string into si
      call PrintString      ;print the string

      call Newloin        ;new line

      mov si,OFFSET rando   ;move the address of the string into si
      call PrintString      ;print the string

      call Newloin        ;newline

      mov si,OFFSET art   ;move the address of the strinf into si
      call PrintString    ;print the string

      jmp mainloop        ;jump back to the main loop


;----------------------------------------------------------------------------
; This changed the backround and font color dependant on user input
;It'll create a newline then prompt for a value
; this value is taken in by the inputstring function with no parsing or conversion happening
; this means that the value you input is actually the ascii value.
;    ***not enough space was available for a full ascii conversion***
;------------------------------------------------------------------------------
screenc:

call Newloin
mov si,OFFSET entrval   ;move the address of the string into si
call PrintString    ;print the string

mov di,buffer           ;place the address of the variable in DI
call InputString        ;call the inputstring function


mov AH, 06h            ;Scroll up function
xor cx,cx          ;Upper left corner CH=row, CL=column
mov dx, 184FH     ;lower right corner DH=row, DL=
mov si,buffer    ;move the address of the variable int

mov BH,[si]               ;place what's in the address(the variable into BH)
                          ;This is the colour

int 10H                   ;call interupt

jmp mainloop              ;jump to mainloop






;------------------------------------------------------------------------
;This makes the computer ring a bell
; ASCII value 7 is the bell all you need to do is use interupt to call it
;------------------------------------------------------------------------------
bell:
      mov al,7          ;place the ascii number (7) into al
      call writechar    ; writethe char
                        ;(7) in ascii is the bell sound
      jmp mainloop       ;jump back to the mainloop






;----------------------------------------------------------------------------
;This clears the screen
;------------------------------------------------------------------------------
clearing:


mov ax,0003h       ;place 0003h into ax this clear the crean
int 10h           ;call interupt
jmp mainloop      ;jump back to mainloop




;-----------------------------------------------------------------------------
;This retrieves a random charicater and prints it
;interrupt 1ah has to do with the system timer and clock services
; when 00h is in AH and the 1ah interupt is called
; the clock count is placed into different registers
;DX is the low-order part of the clock count
; we place that value and print the ascii character attributed to that value
;-------------------------------------------------------------------------
random:
  call Newloin  ;newline
  mov ah,00h    ; move 00 into ah
  int 1ah       ; read system-timer counter
  mov ax,dx     ; move low order  part of clock int oax

  call writechar  ;write char
                  ;this write the al value placed in ax

  jmp mainloop    ;




;-------------------------------------------------------------
;This prints out a triangle. Row's are dependant on the users input
; User is prompted for input. then the algorithm prints out a triangle
;based on the value.
; ------------------
;The getval function converts the ascii value to decimal but only allows one digit numbers
;------------------------------------------------------------
tree:
call Newloin            ;get a newline
mov si,OFFSET entrval   ;move the address of the string into si
call PrintString    ;print the string
call getval             ;get a value (ONLY ACCEPTS VALUES FROM 0 TO 9)
jc mainloop             ;if the value entered is wrong(not from 0 to 9)
                        ;the carry flag is set and that condition is checked here
                        ;if it is set then jump to mainloop

mov bl,al               ;place the user input into bl

xor eax,eax             ;clear eax

call Newloin            ;call subroutine for a newline
mov dl,1                ;initialize dl with 1
mov cl,1                ;initialize cl with 1

loopie:
  cmp cl,bl                   ;compare cl to bl
  jbe inner                   ; if cl is less or eual to bl then jump to inner
  jmp fini                    ;otherwise jump to the end of the function
      inner:
            mov al,42         ;value 42 is the star in ascii
            call writechar    ;print the star

            cmp dl,cl         ;compare dl to cl
            jb bufferloop     ;if dl is below then jump to bufferloop

            jmp loopiesetup   ;if not jump to loopiesetup

            bufferloop:
            inc dl            ;increment dl
            jmp inner         ;jump back to inner

        loopiesetup:
          mov dl,1          ;mov dl back to 1
          call Newloin      ;print a newline
          inc cl            ; increment cl
          jmp loopie        ;loop back to loopie
;-----------------
fini:
 jmp mainloop         ;jump back to mainloop



;----------------------------------------------------------------------------------------------------------------------------
;============================================================================================================================
;                     SUBROUTINES START HERE
;============================================================================================================================
;-----------------------------------------------------------------------------------------------------------------------------



;---------------------------------PrintString-----------------------------------
;
;Prints string, memory address is specified by used
; The memory where the string is placed is specified in si
; this value is then placed into al and int 10h is called to print it to screen.
; the value is always compared to see if it's a null pointer and if so the subroutine ends
;-----------------------------------------------------------------------------
PrintString:
      mov ah,0Eh      ;Place it into scrolling teletype BIOS routine
      mov al,[si]    ;place whatever the value is at the memory in si into al
      cmp al,0        ;compare to see if the value is 0 (null pointer)
      je done         ;if it is then branch to done
    ;	call printf
      int 10h      ;call interupt and print
      add si,1     ;incremment esi by 1
      loop PrintString  ;loop back

done:
        ret         ;return subroutine



;----------------------InputString------------------

;Inputs string at a place in memory
; The place where the string is inputed is specified by the register di
;Wait for the user to enter a value, check to see if it's a backspace
;         if a backspace then jump to the backspace label, where it checks to see
;         if it's the beginingof the strign in which case nothing happens. But otherwise
;         decrement the memory address and strign counter, print a backspace then a space then once again a backspace.
;         Jump back to inputing the string
;Check to see if the value is an enter, if so jump to done1 label where a null pointer(this is done twice for later parts of the program)
;is placed onto the string and returns the subroutine
;If it's simply a character place into memory and incrementthe memory and counter
;[The counter is there so that the user can't input infinite number of chars], there's always a compare on the counter.
; No more then 64 chars are allowed
;
;----------------------------------------------------
InputString:
      xor cl,cl   ;clear cl
Inputing:
      mov ah,0
      int 16h     ;wait for keypress

      cmp al, 08
      je backspace

      mov ah,0Eh  ;put into scrolling teletype BIOS routing
      int 10h     ;print out character

      cmp al,0Dh  ;compare to see if it's enter
      je done1    ;if it's enter then jump to done


      mov [di],al    ;put character in memory
      inc edi

      cmp cl, 3Fh ;63 chars inputed?
      je done1

      inc cl
      jmp Inputing  ;loop back up

backspace:
      cmp cl,0        ;begining of string?
      je Inputing     ;ignore it

      dec di        ;decrement adrress
      dec cl		    ; decrement counter as well

      mov ah, 0Eh
      mov al, 08h
      int 10h	     	; backspace on the screen

      mov al, ' '
      int 10h		; blank character out

      mov al, 08h
      int 10h		; backspace again

      jmp Inputing; go to the main loop


done1:
      mov al,0  ;null terminator
      mov [edi],al
      inc edi
      mov [edi],al


      ret





;------------------writechar--------------------

;Displays the character that was placed in al
;--------------------------------------------------
writechar:
  mov ah,0Eh      ;place into teletype scrolling mode
  int 10h         ;call interrupt to print out

  ret


;---------------------Newloin----------------------

;Print a Newline
;Places a carriage return and newline feed and prints them onto the screen
;---------------------------------------------------
  Newloin:
  mov ah,0Eh  ;put into scrolling teletype
  mov al,0Dh  ;carriage return
  int 10h
  mov al,0Ah      ;newline feed
  int 10h         ;newline
  ret





  
  ;----------------------------getval---------------------------------
  ;
  ;Get the value, Intakes input from user and converts to decimal
  ;             (ONLY WORKS FOR VALUE 0 TO 9) SINGLE DIGIT ONLY
  ;------------------------------------------------------------------
  getval:
  mov ah,0
  int 16h     ;wait for keypress

  call writechar  ;echo the char that was inputed
  cmp al,57       ;compare t o see if the ascii value is above 57
  jg badI         ;if it is greater then jump to badI
  cmp al,48       ;compare to see if ascii value is below 48
  jb badI         ;if it is below than jump to badcommand

  sub al,48       ;The reason is subtract the ascii value that the user inputed by 48
                  ;is because all decimals numbers are offfset with their ascii value counterparts
                  ;by a value of 48
                  ;for example: the ascii value of 7 is 55
                  ;so to get the computer to use the actual value seven it must be subtracted by 48

  clc              ;clear the carry flag
  jmp Finisimo     ;jump to the end

  badI:
  call badcommand  ;call the badcommad subroutine
  stc              ;set the carry flag
  Finisimo:
  ret             ;return the subroutine




 
;-----------------------badcommand---------------------
;
;Displays ? as a bad or unknown command was inputed
;-------------------------------------------------------
  badcommand:
  call Newloin    ;print newline
  mov al,'?'      ;print ? mark to demonstrate an unknown or incorrect command
  call writechar
  ret             ;return the subroutine



;-------------------------------------------------------------------
;END OF THE PROGRAM
;--------------------------------------------------------------------
finished:
;Stop here, black magic
    byte 510 - ($ - main) dup (0)		; Pad remainder of boot sector with zeros
    dw 0aa55h					; Boot signature

;----------------------------------------------

END main
