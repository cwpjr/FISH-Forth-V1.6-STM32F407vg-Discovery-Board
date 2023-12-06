        NAME    main
        
        SECTION .intvec : CODE (2)
//        CODE32
        SECTION .intvec:CODE:NOROOT(2)

//        EXTERN  __iar_program_start
        PUBLIC  __vector_table

        DATA
__vector_table
        DCD     0x20000000 ; 0x0 ; 0x2001FFFF ; SDRAM_TRUE_END  ; sfe(CSTACK)
        DCD     __iar_program_start ; Reset_Handler
        
        SECTION .text : CODE (2)
       PUBLIC  __iar_program_start
__iar_program_start
        B main

        
        SECTION .text : CODE (2)
//        CODE32

main    NOP
        B main

        END
