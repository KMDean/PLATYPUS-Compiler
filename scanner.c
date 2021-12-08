
/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "token.h"
#include "table.h"
#include "buffer.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /* keywords lookup function */


/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
    if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
    /* in case the buffer has been read previously  */
    b_rewind(psc_buf);
    b_clear(str_LTBL);
    line = 1;
    sc_buf = psc_buf;
    return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

Token malar_next_token(void)
{
    Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
    unsigned char c; /* input symbol */
    int state = 0; /* initial state of the FSM */
    short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
    short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

    /* DECLARE YOUR LOCAL VARIABLES HERE IF NEEDED */

    int t_state = NOAS;
    char c0, c1, c2, c3; /* controls b_getc calls */
    int charCount = 0;

    while (1) { /* endless loop broken by token returns it will generate a warning */

        /* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

        c = b_getc(sc_buf);

        /* Part 1: Implementation of token driven scanner */

        if (c == '\n') {
            /* new line */
            ++line; /* counting the number of lines */
            continue;
        }
        if (c == '\r') {
            c = b_getc(sc_buf);
            if (c != '\n')
                b_retract(sc_buf);
            ++line;
            continue;
        }
        if (c == '\t' || c == ' ' || c == '\v') {
            /* tab, space, return */
            continue;
        }
        if (c == '(') {
            /* open parenthesis */
            t.code = LPR_T; /* left */
            return t;
        }
        if (c == ')') {
            /* close parenthesis */
            t.code = RPR_T; /* right */
            return t;
        }

        if (c == '{') {
            /* open curly */
            t.code = LBR_T; /* left */
            return t;
        }
        if (c == '}') {
            /* close curly */
            t.code = RBR_T; /* right */
            return t;
        }

        if (c == ',') {
            /* comma */
            t.code = COM_T;
            return t;
        }
        if (c == ';') {
            /* semi */
            t.code = EOS_T;
            return t;
        }
        if (c == '#') {
            /* pound */
            c0 = b_getc(sc_buf);
            if (c0 == '#') {
                t.code = SCC_OP_T;
                return t;
            }
            b_retract(sc_buf);
        }

        /* arithmetic operators */
        if (c == '+') {
            /* adding */
            t.attribute.arr_op = PLUS;
            t.code = ART_OP_T;
            return t;
        }
        if (c == '-') {
            /* subtracting */
            t.attribute.arr_op = MINUS;
            t.code = ART_OP_T;
            return t;
        }
        if (c == '*') {
            /* multiplying */
            t.attribute.arr_op = MULT;
            t.code = ART_OP_T;
            return t;
        }
        if (c == '/') {
            /* dividing */
            t.attribute.arr_op = DIV;
            t.code = ART_OP_T;
            return t;
        }
        if (c == '=') {
            /* equals */
            c = b_getc(sc_buf);
            if (c == '=') {
                t.attribute.rel_op = EQ;
                t.code = REL_OP_T;
                return t;
            }
            else {
                b_retract(sc_buf);
                t.code = ASS_OP_T;
                return t;
            }
        }

        /* AND/OR */
        if (c == '.') {
            /* check for .AND. and .OR. */
            c0 = b_getc(sc_buf);
            c1 = b_getc(sc_buf);
            c2 = b_getc(sc_buf);
            c3 = b_getc(sc_buf);

            if (c0 == 'A' && c1 == 'N' && c2 == 'D' && c3 == '.') {
                /* AND */
                t.attribute.log_op = AND;
                t.code = LOG_OP_T;
                return t;
            }
            b_retract(sc_buf);
            if (c0 == 'O' && c1 == 'R' && c2 == '.') {
                /* OR */
                t.attribute.log_op = OR;
                t.code = LOG_OP_T;
                return t;
            }

            /* error because not OR and not AND */
            sprintf(t.attribute.err_lex, ".");
            t.code = ERR_T;

            for (int i = 0; i < 3; ++i) {
                b_retract(sc_buf);
            }
            return t;
        }

        if (c == '<') {
            c = b_getc(sc_buf);

            if (c == '<') {
                t.code = SCC_OP_T;
                return t;
            }
            else if (c == '>') {
                t.attribute.rel_op = NE;
                t.code = REL_OP_T;
                return t;
            }
            t.attribute.rel_op = LT;
            t.code = REL_OP_T;
            b_retract(sc_buf);
            return t;
        }
        if (c == '>') {
            t.attribute.rel_op = GT;
            t.code = REL_OP_T;
            return t;
        }

        if (c == '\"') {
            short stringAttr = str_LTBL->addc_offset;
            do {
                ++charCount;
                c = b_getc(sc_buf);
            } while (c != '"' && c != '\0' && c != 255);
            for (int i = 0; i < charCount; ++i) {
                b_retract(sc_buf);
            }
            if (c == '\"') {
                c = b_getc(sc_buf);
                while (c != '\"') {
                    if (c == '\r' || c == '\n') {
                        ++line;
                    }
                    b_addc(str_LTBL, c);
                    c = b_getc(sc_buf);
                }
                b_addc(str_LTBL, '\0');
                t.code = STR_T;
                t.attribute.str_offset = stringAttr;
                return t;
            }
            else {
                b_retract(sc_buf);
                if (charCount > ERR_LEN) {
                    char max_chars = ERR_LEN - 3;
                    int i = 0;
                    for (i = 0; i < max_chars; ++i) {
                        c = b_getc(sc_buf);
                        t.attribute.err_lex[i] = c;
                    }
                    for (i; i < ERR_LEN; ++i) {
                        t.attribute.err_lex[i] = '.';
                    }
                    t.attribute.err_lex[ERR_LEN] = '\0';
                }
                while (c != '\0' && c != 255) {
                    c = b_getc(sc_buf);
                }
                b_retract(sc_buf);
                t.code = ERR_T;
                return t;
            }
        }

        /* comments */
        if (c == '!') {
            /* comment */

            c0 = b_getc(sc_buf);
            if (c0 == '<') {
                while (c0 != '\r' && c0 != '\n' && c0 != '\0' && c0 != 255) {
                    c0 = b_getc(sc_buf);
                }
                b_retract(sc_buf);
                continue;
            }
            /* If there's an error, assume that the line is still meant to be a comment and skip it, but also report an error. */
            else {
                if (c0 != '!') {
                    sprintf(t.attribute.err_lex, "!%c", c0);
                    t.code = ERR_T;
                    while (c0 != '\r' && c0 != '\n' && c0 != '\0' && c0 != 255) {
                        c0 = b_getc(sc_buf);
                    }
                    b_retract(sc_buf);
                    return t;
                }
                while (c0 != '\r' && c0 != '\n' && c0 != '\0' && c0 != 255) {
                    c0 = b_getc(sc_buf);
                }
                b_retract(sc_buf);
                continue;
            }
        }

        /* errors */
        /* SEOF(255) */
        if (SEOF == c) {
            /* SEOF */
            t.attribute.seof = SEOF_0;
            t.code = SEOF_T;
            return t;
        }
        if (SEOF_255 == c) {
            /* EOF */
            t.attribute.seof = SEOF_EOF;
            t.code = SEOF_T;
            return t;
        }
        if (RT_FAIL_2 == c) {
            strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
            t.code = RTE_T;
            scerrnum = 1;
            return t;
        }

        /* Part 2: Implementation of Finite State Machine (DFA)
                    or Transition Table driven Scanner
                    Note: Part 2 must follow Part 1 to catch the illegal symbols
        */

        lexstart = b_getcoffset(sc_buf);
        b_markc(sc_buf, lexstart - 1);
        state = get_next_state(state, c);
        t_state = as_table[state];
        while (NOAS == t_state) {
            c = b_getc(sc_buf);
            state = get_next_state(state, c);
            t_state = as_table[state];
        }
        if (ASWR == t_state) {
            b_retract(sc_buf);
        }

        lexend = b_getcoffset(sc_buf);
        lex_buf = b_allocate(200, 15, 'a');
        b_reset(sc_buf);
        for (int i = lexstart; i <= lexend; ++i) {
            c = b_getc(sc_buf);
            b_addc(lex_buf, c);
        }
        b_addc(lex_buf, '\0');
        t = aa_table[state](lex_buf->cb_head);
        b_free(lex_buf);
        return t;
    } //end while(1)
}


/* DO NOT MODIFY THE CODE OF THIS FUNCTION YOU CAN REMOVE THE COMMENTS ONLY */
int get_next_state(int state, char c)
{
    int col;
    int next;
    col = char_class(c);
    next = st_table[state][col];
#ifdef DEBUG
    printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
    assert(next != IS);

#ifdef DEBUG
    if (next == IS) {
        printf("Scanner Error: Illegal state:\n");
        printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
        exit(1);
    }
#endif
    return next;
}

int char_class(char c)
{
    int val;

    /* THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
        TABLE st_table FOR THE INPUT CHARACTER c.
        SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
        FOR EXAMPLE IF COLUMN 2 REPRESENTS[A - Za - z]
        THE FUNCTION RETURNS 2 EVERY TIME c IS ONE
        OF THE LETTERS A, B, ..., Z, a, b...z.
        PAY ATTENTION THAT THE FIRST COLOMN IN THE TT IS 0 (has index 0) */

    if (isalpha(c)) {
        /* alphabetic */
        val = 0;
    }
    else if (isdigit(c)) {
        /* numeric */
        if (c == '0') {
            /* 0 */
            val = 1;
        }
        else {
            /* 1-9 */
            val = 2;
        }
    }
    else if (c == '.') {
        val = 3;
    }
    else if (c == '#') {
        val = 4;
    }
    else if (c == '"') {
        val = 5;
    }
    else if (SEOF == c || EOF == c) {
        val = 6;
    }
    else {
        val = 7;
    }
    return val;
}



/*  HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
    ************************************************************

    ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords(VID - AVID / KW)
    REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER */

Token aa_func02(char lexeme[]) {

    /* WHEN CALLED THE FUNCTION MUST
        1. CHECK IF THE LEXEME IS A KEYWORD.
        IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
        FOR THE KEYWORD.THE ATTRIBUTE CODE FOR THE KEYWORD
        IS ITS INDEX IN THE KEYWORD LOOKUP TABLE(kw_table in table.h).
        IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.

        2. SET a AVID TOKEN.
        IF THE lexeme IS LONGER than VID_LEN(see token.h) CHARACTERS,
        ONLY FIRST VID_LEN CHARACTERS ARE STORED
        INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
        ADD \0 AT THE END TO MAKE A C - type STRING. */

    Token t;

    int keyword = -1;
    keyword = iskeyword(lexeme);

    if (keyword >= -0) {
        /* if keyword */
        t.attribute.kwt_idx = keyword;
        t.code = KW_T;
    }
    else {
        if (strlen(lexeme) < VID_LEN) {
            /* lexeme is shorter than vid_len */
            strcpy(t.attribute.vid_lex, lexeme);
            t.attribute.vid_lex[strlen(lexeme) + 1] = '\0';
        }
        else {
            strncpy(t.attribute.vid_lex, lexeme, VID_LEN);
            t.attribute.vid_lex[VID_LEN] = '\0';
        }
        t.code = AVID_T;
    }
    return t;
}

/* ACCEPTING FUNCTION FOR THE string variable identifier(VID - SVID)
    REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER */

Token aa_func03(char lexeme[]) {

    /*WHEN CALLED THE FUNCTION MUST
        1. SET a SVID TOKEN.
        IF THE lexeme IS LONGER than VID_LEN characters,
        ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
        INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
        AND THEN THE # CHARACTER IS APPENDED TO THE NAME.
        \0 AT THE END TO MAKE A C - type STRING. */

    Token t;
    if (strlen(lexeme) < VID_LEN) {
        /* lexeme is shorter than vid_len */
        strncpy(t.attribute.vid_lex, lexeme, strlen(lexeme));
        t.attribute.err_lex[strlen(lexeme)] = SEOF;
    }
    else {
        /* lexeme is longer than vid_lex */
        strncpy(t.attribute.vid_lex, lexeme, (VID_LEN - 1));
        for (int i = 0; i < VID_LEN; ++i) {
            t.attribute.vid_lex[i] = lexeme[i];
        }
        t.attribute.vid_lex[VID_LEN - 1] = '#';
        t.attribute.vid_lex[VID_LEN] = SEOF;
    }
    t.code = SVID_T;
    return t;
}

/* ACCEPTING FUNCTION FOR THE floating - point literal(FPL) */

Token aa_func08(char lexeme[]) {

    /* THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
        WHICH IS THE ATTRIBUTE FOR THE TOKEN.
        THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
        IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
        THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
        than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
        STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
        err_lex C - type string.
        BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE */

    Token t = { 0 };

    double floatingPointLiteral = atof(lexeme);
    if ((floatingPointLiteral > FLT_MIN && floatingPointLiteral < FLT_MAX) || floatingPointLiteral == 0) {
        /* 2 byte range */
        t.attribute.flt_value = (float)floatingPointLiteral;
        t.code = FPL_T;
    }
    else {
        if (strlen(lexeme) > ERR_LEN) {
            unsigned int i = 0;
            for (i = 0; i < ERR_LEN - 3; ++i) {
                if (lexeme[i] == '\n') {
                    ++line;
                }
                t.attribute.err_lex[i] = lexeme[i];
            }
            t.attribute.err_lex[ERR_LEN - 3] = '.';
            t.attribute.err_lex[ERR_LEN - 2] = '.';
            t.attribute.err_lex[ERR_LEN - 1] = '.';
            t.attribute.err_lex[ERR_LEN] = '\0';
        }
        else {
            unsigned int i = 0;
            for (i = 0; i < strlen(lexeme); ++i) {
                if (lexeme[i] == '\n') {
                    ++line;
                }
                t.attribute.err_lex[i] = lexeme[i];
            }
            t.attribute.err_lex[i] = SEOF;
        }
        t.code = ERR_T;
    }
    return t;
}

/* ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant(DIL) */

Token aa_func05(char lexeme[]) {

    /* THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
        TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
        THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.
        IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
        THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
        than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
        STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
        err_lex C - type string.
        BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE */

    Token t = { 0 };
    int lex_dec = atoi(lexeme); // change to int



    if ((lex_dec >= SHRT_MIN && lex_dec <= SHRT_MAX) || lex_dec == 0) {
        t.attribute.int_value = lex_dec;
        t.code = INL_T;
    }
    else {
        if (strlen(lexeme) > ERR_LEN) {
            unsigned int i = 0;
            for (i = 0; i < ERR_LEN - 3; ++i) {
                if (lexeme[i] == '\n') {
                    ++line;
                }
                t.attribute.err_lex[i] = lexeme[i];
            }
            t.attribute.err_lex[ERR_LEN - 3] = '.';
            t.attribute.err_lex[ERR_LEN - 2] = '.';
            t.attribute.err_lex[ERR_LEN - 1] = '.';
            t.attribute.err_lex[ERR_LEN] = '\0';
        }
        else {
            unsigned int i = 0;
            for (i = 0; i < strlen(lexeme); ++i) {
                if (lexeme[i] == '\n') {
                    ++line;
                }
                t.attribute.err_lex[i] = lexeme[i];
            }
            t.attribute.err_lex[i] = SEOF;
        }
    }

    return t;
}

Token aa_func10(char lexeme[]) {
   
   Token currentToken = { 0 };
   int i;
 
   if (currentToken.attribute.str_offset > 0) {
      currentToken.code = STR_T;
    
    		for (i = 0; i < strlen(lexeme); i++) {
	     		if (lexeme[i] == '\n') {
				      ++line;
			     } 
       
        if ((i == 0 || i == strlen(lexeme) - 1) && lexeme[i] == "\"") {
			
        } else {
          b_addc(str_LTBL, lexeme[i]);
        }
		      bufferAddChar(stringLiteralTable, '\0');
	   }
	
	   return currentToken;
}

/* ACCEPTING FUNCTION FOR THE ERROR TOKEN */

Token aa_func11(char lexeme[]) {

    /* THE FUNCTION SETS THE ERROR TOKEN.lexeme[] CONTAINS THE ERROR
        THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
        AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
        than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
        STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
        err_lex C - type string.
        IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
        BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE */

    Token t = { 0 };

    if (strlen(lexeme) > ERR_LEN) {
        unsigned int i = 0;
        for (i = 0; i < ERR_LEN - 3; ++i) {
            if (lexeme[i] == '\n') {
                ++line;
            }
            t.attribute.err_lex[i] = lexeme[i];
        }
        t.attribute.err_lex[ERR_LEN - 3] = '.';
        t.attribute.err_lex[ERR_LEN - 2] = '.';
        t.attribute.err_lex[ERR_LEN - 1] = '.';
        t.attribute.err_lex[ERR_LEN] = '\0';
    }
    else {
        unsigned int i = 0;
        for (i = 0; i < strlen(lexeme); ++i) {
            if (lexeme[i] == '\n') {
                ++line;
            }
            t.attribute.err_lex[i] = lexeme[i];
        }
        t.attribute.err_lex[i] = SEOF;
    }

    return t;
}

Token aa_func12(char lexeme[]) {
    Token t;

    strncpy(t.attribute.err_lex, lexeme, ERR_LEN);
    t.attribute.err_lex[ERR_LEN] = '\0';
    t.code = ERR_T;

    return t;
}

/* HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS(IF ANY). */

int iskeyword(char* kw_lexeme) {
    if (NULL == kw_lexeme) {
        return RT_FAIL_2;
    }
    for (int i = 0; i < KWT_SIZE; ++i) {
        if (strcmp(kw_lexeme, kw_table[i]) == 0) {
            /* if it matches */
            return i;
        }
    }
    return RT_FAIL_2;
}
