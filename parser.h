
#ifndef PARSER_H_
#define PARSER_H_

#ifndef BUFFER_H_
#include "buffer.h"
#endif // !BUFFER_H_

#ifndef TOKEN_H_
#include "token.h"
#endif // !TOKEN_H_

#define NO_ATTR -1

typedef enum Keywords {
	ELSE,
	FALSE,
	IF,
	PLATYPUS,
	READ,
	REPEAT,
	THEN,
	TRUE,
	WHILE,
	WRITE
} Keywords_;

/* globals */
static Token lookahead; // nextToken
int synerrno;

/* externals */
extern pBuffer str_LTBL;
extern Token malar_next_token(void);
extern int line;
extern char* kw_table[];

/* functions */
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char*);
void program(void);
void opt_statements(void);
void statements(void);
void statements_p(void);
void statement(void);
void assignment(void);
void assignment_expression(void);
void selection(void);
void iteration(void);
void pre_condition(void);
void input(void);
void variable_list(void);
void variable_list_p(void);
void variable_identifier(void);
void output(void);
void output_list(void);
void opt_variable_list(void);
void arithmetic(void);
void unary_arithmetic(void);
void additive_arithmetic(void);
void additive_arithmetic_p(void);
void multiplicative_arithmetic(void);
void multiplicative_arithmetic_p(void);
void primary_arithmetic(void);
void string(void);
void string_p(void);
void primary_string(void);
void conditional(void);
void logical_or(void);
void logical_or_p(void);
void logical_and(void);
void logical_and_p(void);
void relational(void);
void primary_a_relational_p(void);
void primary_s_relational_p(void);
void primary_a_relational(void);
void primary_s_relational(void);

#endif // !PARSER_H_