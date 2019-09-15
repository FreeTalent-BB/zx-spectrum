CLS
goto music
end


music:
    ASM
        LD A,(MININOTE)
       ; Our integer notes are multiples of
                            ; (1/51)...
                            ; Add to this to speed up the Tempo!

            CALL $2D28; Push A onto Calc Stack via Rom routine
            RST $28 ; use the floating point calculator
            DEFB $A1; stk_one
            DEFB $01; exchange
            DEFB $05; division
            DEFB $C3; Store_M3
            DEFB $02; Delete
            DEFB $38 ; end-calc

            LD A,(PITCH)
                            ; Default is 60, but this can be
                            ; changed to move music into a
                            ; a different key, in case a note is
                            ; too high or low to keep music in range

            CALL $2D28; Push A onto Calc Stack via Rom routine
            RST $28 ; use the floating point calculator
            DEFB $C4; Store_M4
            DEFB $02; Delete
            DEFB $38 ; end-calc

            ; Pitch offset is now stored in M4

    restore_notes:

            LD HL,NOTES

    read_notes_loop:

            LD A,(HL)
            AND A   ; (Zero is the end marker for the note data)
            JP Z,restore_notes   ; If duration byte is 0, then do this! (Loop or Exit)...

            LD B,A
            INC HL

            LD C,(HL)
            INC HL

    CheckKey:
            xor a
            in a, ($fe)
            cpl
            and %00011111
            RET nz ; EXIT routine if key is pressed

            PUSH HL
            CALL BEEPIT ; BC is set, so now Play note...
            POP HL

            Jp read_notes_loop

    BEEPIT:
            ; Input BC = B=Duration, C=Pitch
            ; Output Action: Beeps note using the ROM beeper routine...
            ; Duration will be a multiple of our mini-unit
            ; (Default mini-unit is usually 5/255 = 1/51)...
            ; To convert a Basic BEEP duration into our assembled value,
            ; Assembled Duration = INT(Basic duration in Sec / (1/51) )
            ; Our pitch will be the same as BASIC but with 60 added
            ; This avoids messing with negative numbers while storing data


            ; now to push Our duration Value and multiple
            PUSH BC

            LD A,B
            CALL $2D28; Push A onto Calc Stack via Rom routine: Duraton (B)
            RST $28 ; use the floating point calculator
            DEFB $E3; Recall_M3
            DEFB $04; multiple
            DEFB $38 ; end-calc

            POP BC

            LD A,C
            CALL $2D28; Push A onto Calc Stack via Rom routine: Pitch (C)
            RST $28 ; use the floating point calculator
            DEFB $E4; Recall Pitch offset from M4 Memory
            DEFB $03; subtract
            DEFB $38 ; end-calc

            ; Currect duration & pitch value now on calc stack and ready

            JP $03F8        ; Entry point for BEEP
            RET

    PITCH:
            DEFB 60 + (0)
            ; The number in brackets shifts the music key up or down
            ; Add 12 to move music down an octive, -12 to move music up octive.
    MININOTE:
            DEFB 51 + (0)
            ; 1st Number above is MiniNote: durations are multiples of 1/51
            ; 2nd Number is Tempo offset, added while playing...
    NOTES:

	DEFB 1,48,1,62,1,81,3,86
	DEFB 1,81,5,86,1,48,5,62
	DEFB 1,48,1,62,1,79,3,84
	DEFB 1,50,5,62,1,79,11,83
	DEFB 1,48,5,62,1,48,11,62
	DEFB 1,48,5,62,1,48,1,62
	DEFB 1,74,3,74,1,50,5,62
	DEFB 1,62,1,74,4,76,1,48
	DEFB 1,62,1,72,3,77,1,48
	DEFB 1,62,1,72,3,79,1,48
	DEFB 1,58,1,74,9,81,1,48
	DEFB 5,58,1,48,1,58,1,70
	DEFB 3,74,1,50,11,70,1,74
	DEFB 5,81,6,48,1,60,1,72
	DEFB 4,79,1,48,1,60,1,79
	DEFB 3,84,1,50,5,72,6,60
	DEFB 1,50,5,60,1,50,5,60
	DEFB 1,50,5,60,6,60,1,48
	DEFB 1,62,1,81,3,86,1,81
	DEFB 5,86,1,48,5,62,1,48
	DEFB 1,62,1,79,3,84,1,50
	DEFB 5,62,1,79,11,83,1,48
	DEFB 5,62,1,48,11,62,1,48
	DEFB 5,62,1,48,1,62,1,74
	DEFB 3,74,1,50,5,62,1,62
	DEFB 1,74,4,76,1,48,1,62
	DEFB 1,72,3,77,1,48,1,62
	DEFB 1,72,3,79,1,48,1,58
	DEFB 1,74,9,81,1,48,5,58
	DEFB 1,48,1,58,1,70,3,74
	DEFB 1,50,11,70,1,74,5,81
	DEFB 6,48,1,60,1,72,4,79
	DEFB 1,48,1,60,1,72,3,79
	DEFB 1,50,5,72,6,60,1,50
	DEFB 5,60,1,50,5,60,1,50
	DEFB 5,60,6,60,1,48,11,62
	DEFB 1,48,1,62,1,81,3,86
	DEFB 1,48,5,62,1,50,5,62
	DEFB 1,86,11,93,1,48,5,62
	DEFB 1,48,1,62,1,85,3,92
	DEFB 1,86,5,93,1,48,1,62
	DEFB 1,85,3,92,1,48,1,62
	DEFB 1,84,3,89,1,50,5,62
	DEFB 6,62,1,48,5,62,1,48
	DEFB 5,62,1,48,1,58,10,93
	DEFB 1,48,1,58,1,89,3,93
	DEFB 1,48,1,58,1,86,3,92
	DEFB 1,50,5,70,1,58,5,92
	DEFB 1,77,5,91,1,48,1,58
	DEFB 4,74,6,93,1,48,1,58
	DEFB 4,89,1,50,1,70,1,86
	DEFB 3,93,1,89,5,92,1,50
	DEFB 1,70,4,86,1,89,5,92
	DEFB 1,50,1,70,1,86,3,91
	DEFB 6,89,1,48,11,62,1,48
	DEFB 1,62,1,81,3,86,1,48
	DEFB 5,62,1,50,5,62,1,86
	DEFB 11,93,1,48,5,62,1,48
	DEFB 1,62,1,85,3,92,1,86
	DEFB 5,93,1,48,1,62,1,85
	DEFB 3,92,1,48,1,62,1,84
	DEFB 3,89,1,50,5,62,6,62
	DEFB 1,48,5,62,1,48,5,62
	DEFB 1,48,1,58,10,93,1,48
	DEFB 1,58,1,89,3,93,1,48
	DEFB 1,58,1,86,3,92,1,50
	DEFB 5,70,1,58,5,92,1,77
	DEFB 5,91,1,48,1,58,4,74
	DEFB 6,93,1,48,1,58,4,89
	DEFB 1,50,1,70,1,86,3,93
	DEFB 1,89,5,92,1,50,1,70
	DEFB 4,86,1,89,5,92,1,50
	DEFB 1,70,1,86,3,91,6,89
	DEFB 1,48,1,61,1,82,9,85
	DEFB 6,48,1,48,1,85,4,88
	DEFB 6,48,6,48,1,50,1,88
	DEFB 10,94,1,48,1,62,1,86
	DEFB 9,93,6,48,1,48,1,81
	DEFB 4,89,6,48,6,48,1,50
	DEFB 1,77,10,86,1,48,1,64
	DEFB 1,82,9,85,6,48,1,48
	DEFB 1,85,4,88,6,48,6,48
	DEFB 1,50,1,88,10,94,1,48
	DEFB 1,62,1,86,9,93,6,48
	DEFB 1,48,1,81,4,86,6,48
	DEFB 6,48,12,50,1,48,1,61
	DEFB 1,82,9,85,6,48,1,48
	DEFB 1,85,4,88,6,48,6,48
	DEFB 1,50,1,88,10,94,1,48
	DEFB 1,62,1,86,9,93,6,48
	DEFB 1,48,1,81,4,89,6,48
	DEFB 6,48,1,50,1,77,10,86
	DEFB 1,48,1,64,1,82,9,88
	DEFB 6,48,1,48,1,85,4,91
	DEFB 6,48,6,48,1,50,1,88
	DEFB 10,94,1,50,1,57,1,85
	DEFB 9,93,6,50,1,50,1,88
	DEFB 10,95,6,50,1,50,1,91
	DEFB 4,97,6,50,1,50,1,62
	DEFB 1,93,3,98,1,93,5,98
	DEFB 1,50,1,62,1,81,3,86
	DEFB 1,81,11,86,1,48,5,62
	DEFB 1,50,11,60,1,50,11,62
	DEFB 6,48,6,48,6,48,6,48
	DEFB 6,48,6,48,1,50,1,58
	DEFB 1,65,9,70,1,50,1,58
	DEFB 1,65,9,70,1,48,1,70
	DEFB 4,74,1,48,1,74,4,77
	DEFB 12,48,1,50,1,60,1,67
	DEFB 9,72,1,50,1,60,1,67
	DEFB 9,72,1,48,1,72,4,76
	DEFB 1,48,1,76,4,79,12,48
	DEFB 1,50,1,62,1,93,3,98
	DEFB 1,93,5,98,1,50,1,62
	DEFB 1,81,3,86,1,81,11,86
	DEFB 1,48,5,62,1,50,11,60
	DEFB 1,50,11,62,6,48,6,48
	DEFB 6,48,6,48,6,48,6,48
	DEFB 1,50,1,58,1,65,9,70
	DEFB 1,50,1,58,1,65,9,70
	DEFB 1,70,5,74,1,48,1,74
	DEFB 4,77,6,50,6,50,1,50
	DEFB 1,60,1,67,9,72,1,50
	DEFB 1,60,10,67,1,50,1,57
	DEFB 4,58,1,50,1,60,4,60
	DEFB 12,50,1,48,1,62,1,81
	DEFB 3,86,1,81,5,86,1,48
	DEFB 5,62,1,48,1,62,1,79
	DEFB 3,84,1,50,5,62,1,79
	DEFB 11,83,1,48,5,62,1,48
	DEFB 11,62,1,48,5,62,1,48
	DEFB 1,62,1,74,3,74,1,50
	DEFB 5,62,1,62,1,74,4,76
	DEFB 1,48,1,62,1,72,3,77
	DEFB 1,48,1,62,1,72,3,79
	DEFB 1,48,1,58,1,74,9,81
	DEFB 1,48,5,58,1,48,1,58
	DEFB 1,70,3,74,1,50,11,70
	DEFB 1,74,5,81,6,48,1,60
	DEFB 1,72,4,79,1,48,1,60
	DEFB 1,79,3,84,1,50,5,72
	DEFB 6,60,1,50,5,60,1,50
	DEFB 5,60,1,50,5,60,6,60
	DEFB 1,48,1,62,1,81,3,86
	DEFB 1,81,5,86,1,48,5,62
	DEFB 1,48,1,62,1,79,3,84
	DEFB 1,50,5,62,1,79,11,83
	DEFB 1,48,5,62,1,48,11,62
	DEFB 1,48,5,62,1,48,1,62
	DEFB 1,74,3,74,1,50,5,62
	DEFB 1,62,1,74,4,76,1,48
	DEFB 1,62,1,72,3,77,1,48
	DEFB 1,62,1,72,3,79,1,48
	DEFB 1,58,1,74,9,81,1,48
	DEFB 5,58,1,48,1,58,1,70
	DEFB 3,74,1,50,11,70,1,74
	DEFB 5,81,6,48,1,60,1,72
	DEFB 4,79,1,48,1,60,1,72
	DEFB 3,79,1,50,5,72,6,60
	DEFB 1,50,5,60,1,50,5,60
	DEFB 1,50,5,60,6,60,1,48
	DEFB 11,62,1,48,1,62,1,81
	DEFB 3,86,1,48,5,62,1,50
	DEFB 5,62,1,86,11,93,1,48
	DEFB 5,62,1,48,1,62,1,85
	DEFB 3,92,1,86,5,93,1,48
	DEFB 1,62,1,85,3,92,1,48
	DEFB 1,62,1,84,3,89,1,50
	DEFB 5,62,6,62,1,48,5,62
	DEFB 1,48,5,62,1,48,1,58
	DEFB 10,93,1,48,1,58,1,89
	DEFB 3,93,1,48,1,58,1,86
	DEFB 3,92,1,50,5,70,1,58
	DEFB 5,92,1,77,5,91,1,48
	DEFB 1,58,4,74,6,93,1,48
	DEFB 1,58,4,89,1,50,1,70
	DEFB 1,86,3,93,1,89,5,92
	DEFB 1,50,1,70,4,86,1,89
	DEFB 5,92,1,50,1,70,1,86
	DEFB 3,91,6,89,1,48,11,62
	DEFB 1,48,1,62,1,81,3,86
	DEFB 1,48,5,62,1,50,5,62
	DEFB 1,86,11,93,1,48,5,62
	DEFB 1,48,1,62,1,85,3,92
	DEFB 1,86,5,93,1,48,1,62
	DEFB 1,85,3,92,1,48,1,62
	DEFB 1,84,3,89,1,50,5,62
	DEFB 6,62,1,48,5,62,1,48
	DEFB 5,62,1,48,1,58,10,93
	DEFB 1,48,1,58,1,89,3,93
	DEFB 1,48,1,58,1,86,3,92
	DEFB 1,50,5,70,1,58,5,92
	DEFB 1,77,5,91,1,48,1,58
	DEFB 4,74,6,93,1,48,1,58
	DEFB 4,89,1,50,1,70,1,86
	DEFB 3,93,1,89,5,92,1,50
	DEFB 1,70,4,86,1,89,5,92
	DEFB 1,50,1,70,1,86,3,91
	DEFB 6,89,1,48,1,61,1,82
	DEFB 9,85,6,48,1,48,1,85
	DEFB 4,88,6,48,6,48,1,50
	DEFB 1,88,10,94,1,48,1,62
	DEFB 1,86,9,93,6,48,1,48
	DEFB 1,81,4,89,6,48,6,48
	DEFB 1,50,1,77,10,86,1,48
	DEFB 1,64,1,82,9,85,6,48
	DEFB 1,48,1,85,4,88,6,48
	DEFB 6,48,1,50,1,88,10,94
	DEFB 1,48,1,62,1,86,9,93
	DEFB 6,48,1,48,1,81,4,86
	DEFB 6,48,6,48,12,50,1,48
	DEFB 1,61,1,82,9,85,6,48
	DEFB 1,48,1,85,4,88,6,48
	DEFB 6,48,1,50,1,88,10,94
	DEFB 1,48,1,62,1,86,9,93
	DEFB 6,48,1,48,1,81,4,89
	DEFB 6,48,6,48,1,50,1,77
	DEFB 10,86,1,48,1,64,1,82
	DEFB 9,88,6,48,1,48,1,85
	DEFB 4,91,6,48,6,48,1,50
	DEFB 1,88,10,94,1,50,1,57
	DEFB 1,85,9,93,6,50,1,50
	DEFB 1,88,10,95,6,50,1,50
	DEFB 1,91,4,97,6,50,1,50
	DEFB 1,62,1,93,3,98,1,93
	DEFB 5,98,1,50,1,62,1,81
	DEFB 3,86,1,81,11,86,1,48
	DEFB 5,62,1,50,11,60,1,50
	DEFB 11,62,6,48,6,48,6,48
	DEFB 6,48,6,48,6,48,1,50
	DEFB 1,58,1,65,9,70,1,50
	DEFB 1,58,1,65,9,70,1,48
	DEFB 1,70,4,74,1,48,1,74
	DEFB 4,77,12,48,1,50,1,60
	DEFB 1,67,9,72,1,50,1,60
	DEFB 1,67,9,72,1,48,1,72
	DEFB 4,76,1,48,1,76,4,79
	DEFB 12,48,1,50,1,62,1,93
	DEFB 3,98,1,93,5,98,1,50
	DEFB 1,62,1,81,3,86,1,81
	DEFB 11,86,1,48,5,62,1,50
	DEFB 11,60,1,50,11,62,6,48
	DEFB 6,48,6,48,6,48,6,48
	DEFB 6,48,1,50,1,58,1,65
	DEFB 9,70,1,50,1,58,1,65
	DEFB 9,70,1,70,5,74,1,48
	DEFB 1,74,4,77,6,50,6,50
	DEFB 1,50,1,60,1,67,9,72
	DEFB 1,50,1,60,10,67,1,50
	DEFB 1,57,4,58,1,50,1,60
	DEFB 4,60,12,50,0

END ASM
