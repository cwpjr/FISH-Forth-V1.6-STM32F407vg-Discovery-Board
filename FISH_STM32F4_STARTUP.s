// FISH_STM32F4_STARTUP.s
//----------------------------------------------------------------------
#ifdef USE_CMAIN
 PUBLIC RET2c
#endif

 SECTION .text : CODE (2)
 ALIGNROM 2,0xFFFFFFFF

 PUBLIC STM32Fx_COLD_FISH
 PUBLIC  __iar_program_start
__iar_program_start
// MAIN() entry point defined by #Defines in FISH_STM32F4_CONFIG_DEFINES.h
// :NONAME FM0_COLD ( -- ) Reset Vector entry point. Setup FISH Virtual Machine.
STM32Fx_COLD_FISH:
// Initialize DICT RAM segment

	ldr	n, = 0x11111111	        // fill pattern
	ldr	t, = RAM_START          // START OF RAM WHERE DICT IS ALLOCATED
                                        // IN MEMMAP segment
	ldr	y, = RAM_INIT_END       // BEFORE UNINT SECTION IN MEMMAP
_fillRAM:
	str	n, [t]
	adds	t, t, #4
	cmp	t, y
	blo	_fillRAM
        
#ifdef USE_CMAIN
//	PUSH lr to sp for BYE
//	SUB	sp, sp, #4
//	MOV	t, lr
//	STR	t, [sp]
#endif

	LDR  	p, =PINIT
	LDR  	r, =RINIT
	ADR	i, FM4_WARM
	PUSHi2r
	NEXT

 LTORG

//---------------------------------FM4_WARM-------------------------------------
// :NONAME FM4_WARM ( -- ) Start of FISH interpretation PRE-EXECUTION. then QUIT
 SECTION .text : CONST (2)
 ALIGNROM 2,0xFFFFFFFF
FM4_WARM:
#ifdef TESTRAM
        DC32    flogRAM
#endif
        DC32	FWARM		        // FISH Init for WTEST code
        DC32	SoCinit		        // SYSCLK, systick, MS
        DC32    UART3_INIT    // NOT COMPLETE

// TEST CODE GOES HERE - Pre FISH Execution Environment
WTEST:

// COPIES CODE TO RAM - CHECK FISH CODE FOR THIS KIND OF RELOCATION!
// RELEATIVELY LINKED PRIOR DEFNITIONS!
//        DC32    FLASH_TEST_CODE_IN_RAM
// FLASH OPS CAN BE PERFORMED FROM FLASH WITH POTENTIAL WAIT STATES
//        DC32    FLASH_TEST_CODE_IN_FLASH      // EXECUTES IN FLASH

// TEST CODE END
FWARM_STARTING_UP:
//        DC32    DOTS            // SHOW ANY STACK ARTIFACTS HERE
        DC32	CR
	DC32	LIT, 0xFB, EMIT // ANSI ASCII CheckMark
	DC32	LIT, 0xF7, EMIT // ANSI ASCII 2 wavy's
	DC32	CR
	DC32	COLD	        // WARM ABORT THEN QUIT
#ifdef USE_CMAIN
	DC32	RET2c	// shouldnt get here, return to c main and restart
#endif
//------------------------ for meta-single-stepping ----------------------------
//:NONAME ssNEXT ( -- ) System Internal hi level breakpoint.
 SECTION .text : CODE (2)
 ALIGNROM 2,0xFFFFFFFF
ssNEXT1:
	LDM	w!, {x} // contents of cfa, (pfa), -> x, bump w to cfa+4
	MOV     pc, x	// w preserves cfa+4 (pfa) for DOCOL's benefit

//---------------------------- SYSTICK ISR -------------------------------------
// STI_ON: 7 E000E010h !  STI_OFF: 5 E000E010h !
// If BX not ued here and in SUBR's OR LDR t, 7 vs LDR t, =7 CAUSES:
// Error[Lp002]: relocation failed: value out of range or illegal: 0xfffff457 

 PUBLIC FMx_SYSTICK_ISR
 SECTION .text : CODE (2)
 ALIGNROM 2,0xFFFFFFFF
FMx_SYSTICK_ISR:
// save what you use
        PUSH    { t, n, lr}
        LDR     n, = STICKER
        LDR     t, [n]
        ADDS    t, t, #1
        STR     t, [n]
// restore what was being used
        POP     { t, n, pc }
//        BX      lr
 LTORG

/*
//-------------------------- ISR's and inline ASM ------------------------------
//:NONAME ISR_FISH Hi level call of FISH word from ISA
//      Save all regs and index a table of CFA's = execute if non-zero

//:NONAME ILA Inline Assembly code execution
 SECTION .text : CONST (2)
 ALIGNROM 2,0xFFFFFFFF
ILA:
	DC32	DOCOL
        DC32    NOOP
//	DC32	ASM_START	// HW ISSUE
//	NOP
//      DC32    ASM_END
	DC32	SEMIS
*/
