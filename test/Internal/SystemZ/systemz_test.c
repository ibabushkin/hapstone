/* An attempt to map all struct sizes to account for the shitty C11 support
 * C2HS exposes. We just print them in a structured fashion to account for
 * the anonymous unions present in most of the code. That way, currently
 * broken Storable instances can be fixed. - This is what happens when you
 * start writing proper property-based tests ;)
 */

#include <stdio.h>

#include <capstone/systemz.h>

// print all struct sizes
void print_sizes(void){
    puts("systemz.h");
    printf("[*] sysz_op_mem: %d\n", sizeof(sysz_op_mem));
    printf("[*] cs_sysz_op: %d\n", sizeof(cs_sysz_op));
    printf("[*] cs_sysz: %d\n", sizeof(cs_sysz));
}

// print all offsets and member sizes
void print_alignment(void){
    cs_sysz_op test = {0, 1};
    void *base = &test;

    printf("cs_sysz_op: %d\n", sizeof(cs_sysz_op));
    puts("cs_sysz_op\toffset\tsize");
    printf("type:\t\t%d\t%d\n", (void *)&test.type - base,
            sizeof(sysz_op_type));
    printf("union.reg:\t%d\t%d\n", (void *)&test.reg - base,
            sizeof(unsigned int));
    printf("union.imm:\t%d\t%d\n", (void *)&test.imm - base,
            sizeof(int64_t));
    printf("union.mem:\t%d\t%d\n", (void *)&test.mem - base,
            sizeof(sysz_op_mem));
}

int main(void){
    print_sizes();
    print_alignment();

    return 0;
}
