
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */


/* You should add your own constant definitions here */
#define ERROR_MASK 0x100
#define MAX_ALLOWED_POSITIVE_VALUE SHRT_MAX

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFF9
#define SET_EOB  0x2
#define RESET_EOB ~(0x2)
#define CHECK_EOB 
#define SET_R_FLAG 0x4
#define RESET_R_FLAG ~(0x4)
#define CHECK_R_FLAG 

/* user data type declarations */
typedef struct BufferDescriptor {
    char* cb_head;            /* pointer to the beginning of character array (character buffer) */
    short capacity;           /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;        /* the offset (in chars) to the add-character location */
    short getc_offset;        /* the offset (in chars) to the get-character location */
    short markc_offset;       /* the offset (in chars) to the mark location */
    char  inc_factor;         /* character array increment factor */
    char  mode;               /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, * pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
/*
Place your function declarations here.
Do not include the function header comments here.
Place them in the buffer.c file
*/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc(pBuffer const pBD, char symbol);
int b_clear(Buffer* const pBD);
void b_free(Buffer* const pBD);
int b_isfull(Buffer* const pBD);
short b_addcoffset(Buffer* const pBD);
short b_capacity(Buffer* const pBD);
short b_markc(Buffer* const pBD, short mark);
int b_mode(Buffer* const pBD);
size_t b_incfactor(Buffer* const pBD);
int b_load(FILE* const fi, Buffer* const pBD);
int b_isempty(Buffer* const pBD);
char b_getc(Buffer* const pBD);
int b_eob(Buffer* const pBD);
int b_print(Buffer* const pBD, char nl);
Buffer* b_compact(Buffer* const pBD, char symbol);
char b_rflag(Buffer* const pBD);
short b_retract(Buffer* const pBD);
short b_reset(Buffer* const pBD);
short b_getcoffset(Buffer* const pBD);
int b_rewind(Buffer* const pBD);
char* b_location(Buffer* const pBD, short loc_offset);

#endif
