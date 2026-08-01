#include <stdlib.h>
void *__stack_chk_guard = (void *)0x00000aff0d0a0000ULL;
void __stack_chk_fail(void) { abort(); }
