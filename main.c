// FISH STM32F4 DISCO "int main();


#define STM32F40XX      `// for stm32f4xx.h usage
//Without this:
// Fatal Error[Pe035]: #error directive: 
//      "Please select first the target STM32F4xx device
//      used in your application (in stm32f4xx.h file)" 

// For CMSIS function usage
#include "stm32f4xx.h"
#include "core_cm4.h"     // For CMSIS NVIC_SystemReset function

//extern inline void NVIC_SystemReset();


//#pragma call_graph_root = "interrupt"
//#pragma weak HardFault_Handler=NVIC_SystemReset
/*
If NMI_Handler is not defined elsewhere in the program, all references to
NMI_Handler will refer to Default_Handler.
*/
__weak void HardFault_Handler( void ) { NVIC_SystemReset(); }   // HW reset for stm32f4xx.h

//__weak void HardFault_Handler( void ) { // for stm32f4xx.h usage }

void C_CMSIS_DISABLE_IRQS() {
  __disable_irq();
}

void C_CMSIS_ENABLE_IRQS() {
  __enable_irq();
}

/*
void C_CMSIS_REV(unsigned long word)
{
// prototype is:
// unsigned long __REV(unsigned long);
// This works if word is passed in r0
  __REV(word);
// Results is returned in r0
}
*/


//#ifdef USE_CMAIN // defined in FISH_STM32F4_CONFIG_DEFINES.h
// the prototype of main needs to be defined as int main() or int main(int argc , char *argv)
int main() {

 extern void STM32Fx_COLD_FISH();
 
	volatile static int i = 0 ;
	while(1) {
		STM32Fx_COLD_FISH();		// ret2c returns here
		i++ ;
	}

	return 0 ;
}
//#endif