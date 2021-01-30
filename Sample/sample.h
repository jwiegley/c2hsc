#ifndef C2HSC_SAMPLE_H
#define C2HSC_SAMPLE_H

#include <stdio.h>
#include <stdint.h>
#include <string.h>

struct test_info;
typedef struct test_info* test_info_ptr;
typedef const char* an_pchar;

int32_t test_init(int32_t flag, test_info_ptr p_test_info);
int32_t test_process(test_info_ptr p_test_info, an_pchar buf);
void test_uninit(test_info_ptr p_test_info);

#endif
