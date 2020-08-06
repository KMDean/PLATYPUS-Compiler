
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

#include "parser.h"

/*
* Purpose: Run the parser
* Parameters: int sync_token_code
* Return value: void
*/
void parser(void) { // from assignment description
	lookahead = malar_next_token();
	program(); 
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
* Purpose:  Match attribute to code
* Parameters: int pr_token_code, int pr_token_attribute
* Return value: void
*/
void match(int pr_token_code, int pr_token_attribute) {

	// the attribute code is used only when the token code is one of the following codes : KW_T, LOG_OP_T, ART_OP_T, REL_OP_T.
	// else if the match is successful and the lookahead is SEOF_T, the function returns.
	// if the match is successful and the lookahead is not SEOF_T, the function advances

	if (lookahead.code != pr_token_code) { 
		syn_eh(pr_token_code); 
		return; 
	}
	if (lookahead.code == SEOF_T) {
		return;
	}

	/* set attribute from code */
	switch (pr_token_code) {
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx) { 
			syn_eh(pr_token_code); 
			return; 
		}
		break;
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.log_op) { 
			syn_eh(pr_token_code); 
			return; 
		}
		break;
	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.arr_op) { 
			syn_eh(pr_token_code); 
			return; 
		}
		break;
	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.rel_op) { 
			syn_eh(pr_token_code); 
			return; 
		}
		break;
	default:
		break;
	}

	// if the new lookahead token is ERR_T, the function calls the error printing function syn_printe()
	// then advances to the next input token by calling malar_next_token() again
	// increments the error counter synerrno, and returns 

	lookahead = malar_next_token();
	if (lookahead.code == ERR_T) { /* if error */
		syn_printe(); // print token
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

/*
* Purpose:  This function implements an error recovery.
* Parameters: int sync_token_code
* Return value: void
*/
void syn_eh(int sync_token_code) {

	// the function calls syn_printe() and increments the error counter
	// then the function advances the input token(lookahead) until it finds a token code matching the one required by the parser
	// (pr_token_code passed to the function as sync_token_code)

	syn_printe();
	synerrno++;
	while (sync_token_code != lookahead.code) { // panic mode recovery
		lookahead = malar_next_token(); 
		if (sync_token_code == lookahead.code) { /* if the input parametre matches the next token */
			if (sync_token_code != SEOF_T) { /* if not end of file */
				lookahead = malar_next_token();
				return;
			}
		}
		if (lookahead.code == SEOF_T) { /* if end of file */
			exit(synerrno); // prevents overrunning input buffer
			return;
		}
	}
}

/*
* Purpose:  Print syntax errors
* Parameters: int sync_token_code
* Return value: void
*/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);

	// If the offending token is a keyword, variable identifier or string literal you must use the corresponding token attribute to access and print the lexeme 
	// (keyword name, variable name, or string)

	switch (t.code) {
	case ERR_T: /* error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case SEOF_T: /* End of File */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case AVID_T: /* Arithmetic Variable Identifier */
		// doesnt get printed
	case SVID_T: /* String Variable Identifier */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case FPL_T: /* Floating Point Literal */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* Integer Literal */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T: /* String Literal */
		printf("%d\n", b_markc(str_LTBL, t.attribute.str_offset));
		break;
	case SCC_OP_T: /* String Concatenation Operator */
		printf("NA\n");
		break;
	case ASS_OP_T: /* Assignment Operator */
		printf("NA\n");
		break;
	case ART_OP_T: /* Arithmetic Operator */
		printf("%d\n", t.attribute.get_int);
		break;
	case REL_OP_T: /* Relational Operator */
		printf("%d\n", t.attribute.get_int);
		break;
	case LOG_OP_T: /* Logical Operator */
		printf("%d\n", t.attribute.get_int);
		break;
	case LPR_T: /* Left Parenthesis */
		printf("NA\n");
		break;
	case RPR_T: /* Right Parenthesis */
		printf("NA\n");
		break;
	case LBR_T: /* Left (curly) Bracket */
		printf("NA\n");
		break;
	case RBR_T: /* Right (curly) bracket */
		printf("NA\n");
		break;
	case KW_T: /* Keyword */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;
	case COM_T: /* Comma */
		printf("NA\n");
		break;
	case EOS_T: /* End of Statement (semicolon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	} /* End switch */
} /* End syn_printe() */

/*
* Purpose:  Prints the char pointer
* Parameters: char* message
* Return value: void
*/
void gen_incode(char* out) { // called when production is recognized
	printf("%s", out);
}

/*
* Purpose:  Main proram
* Parameters: void
* Return value: void
*/
void program(void) { // from assignment description
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

/*
* Purpose:  Main proram
* Parameters:
* Return value: void
*/
void opt_statements() { // from assignment description
	switch (lookahead.code) {
	case AVID_T: 
		// do nothing
	case SVID_T:
		statements(); 
		break;
	case KW_T:
		if (lookahead.attribute.get_int == IF
			|| lookahead.attribute.get_int == WHILE
			|| lookahead.attribute.get_int == READ
			|| lookahead.attribute.get_int == WRITE) {
			statements();
		}
		break;
	default: /* optional statements */;
		gen_incode("PLATY: Opt_statements parsed");
	}
}

/*
* Purpose:  <statements> -> <statement><statements'>
* Parameters: void
* Return value: void
*/
void statements(void) {
	statement();
	statements_p();
}

/*
* Purpose: <statements'> -> <statement><statements'> | E
* Parameters: void
* Return value: void
*/
void statements_p(void) {
	switch (lookahead.code) {
	case KW_T: 
		switch (lookahead.attribute.kwt_idx) { 
		case PLATYPUS: 
		
		case ELSE: 
		
		case THEN: 
		
		case REPEAT:
			return;
		default: 
			break;
		}
	case AVID_T: 
	
	case SVID_T: /* avid_t or svid_t */
		statement();
		statements_p();
		break;
	}
}

/*
* Purpose: <statement> -> <assignment> | <selection> | <iteration> | <input> | <output>
* Parameters: void
* Return value: void
*/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T: 
	
	case SVID_T: /* avid_t or svid_t */
		assignment();
		break;
	case KW_T: 
		switch (lookahead.attribute.kwt_idx) {
		case IF: 
			selection();
			break;
		case WHILE:
			iteration();
			break;
		case READ:
			input();
			break;
		case WRITE:
			output();
			break;
		default: 
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*
* Purpose: <assignment> -> <assignment expression>;
* Parameters: void
* Return value: void
*/
void assignment(void) {
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*
* Parameters: void
* Return value: void
*/
void assignment_expression(void) {
	if (lookahead.code == AVID_T) {
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
		return;
	}
	if (lookahead.code == SVID_T) {
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string();
		gen_incode("PLATY: Assignment expression (string) parsed\n");
		return;
	}
	syn_printe(); /* print error */
}

/*
* Purpose: <selection> -> IF <pre-condition> {<conditonal expression>}	 THEN {<opt_statments>}		ELSE {<opt_statements>}; -- if statement setup
* Parameters: void
* Return value: void
*/
void selection(void) {
	match(KW_T, IF); /* matches IF */
	pre_condition();
	match(LPR_T, NO_ATTR); /* matches left bracket*/
	conditional();
	match(RPR_T, NO_ATTR);	/* matches right brakcet*/
	match(KW_T, THEN);	/* matches THEN */
	match(LBR_T, NO_ATTR);	/* matches left curly */
	opt_statements();
	match(RBR_T, NO_ATTR); /* matches right curly */
	match(KW_T, ELSE); /* matches ELSE */
	match(LBR_T, NO_ATTR);/* matches left curly */
	opt_statements(); 
	match(RBR_T, NO_ATTR); /* matches right curly */
	match(EOS_T, NO_ATTR); /* matches semi colon */
	gen_incode("PLATY: Selection statement parsed\n");
}

/*
* Purpose: <iteration> -> WHILE <pre-condition> (<conditional>)		REPEAT { <statements> }; -- loop setup
* Parameters: void
* Return value: void
*/
void iteration() {
	match(KW_T, WHILE); /* matches WHILE */
	pre_condition();
	match(LPR_T, NO_ATTR);	/* matches left bracket */
	conditional();
	match(RPR_T, NO_ATTR);	/* matches right bracket or end loop */
	match(KW_T, REPEAT); /* matches REPEAT */
	match(LBR_T, NO_ATTR); /* matches left curly */
	statements(); 
	match(RBR_T, NO_ATTR); /* matches right curly */
	match(EOS_T, NO_ATTR); /* matches semicolon */
	gen_incode("PLATY: Iteration statement parsed\n");
}

/*
* <pre condition> -> TRUE | FALSE -- true/false setup
* Parameters: void
* Return value: void
*/
void pre_condition(void) {
	if (KW_T == lookahead.code) {
		if (lookahead.attribute.kwt_idx == TRUE) { 
			match(KW_T, TRUE); 
			return; 
		}
		if (lookahead.attribute.kwt_idx == FALSE) { 
			match(KW_T, FALSE); 
			return; 
		} else { 
			syn_printe(); 
			return; 
		}
		syn_printe();
	}
}

/*
* Purpose: <input> -> READ(<variable list>); -- read setup
* Parameters: void
* Return value: void
*/
void input(void) {
	match(KW_T, READ); /* matches keyword to READ */
	match(LPR_T, NO_ATTR); /* matches left bracket */
	variable_list(); 
	match(RPR_T, NO_ATTR);	/* matches right bracket */
	match(EOS_T, NO_ATTR); /* matches semicolon */
	gen_incode("PLATY: Input statement parsed\n");
}

/*
* Purpose: <variable list> -> <variable identifier><variable list'>
* Parameters: void
* Return value: void
*/
void variable_list(void) {
	variable_identifier();
	variable_list_p();
	gen_incode("PLATY: Variable list parsed\n");
}

/*
* Purpose: <variable list'> -> ,<variable identifier><variable list'> | E
* Parameters: void
* Return value: void
*/
void variable_list_p(void) {
	if (lookahead.code == COM_T) { // comma
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_p();
	}
}

/*
* Parameters: void
* Return value: void
*/
void variable_identifier(void) {
	if (lookahead.code == AVID_T) { 
		match(AVID_T, NO_ATTR); 
		return; 
	}
	if (lookahead.code == SVID_T) { 
		match(SVID_T, NO_ATTR); 
		return; 
	}
	syn_printe();
}

/*
* Purpose: <output> -> WRITE(<output list>);
* Parameters: void
* Return value: void
*/
void output(void) {
	match(KW_T, WRITE);	/* matches WRITE*/
	match(LPR_T, NO_ATTR); /* matches left bracket */
	output_list(); 
	match(RPR_T, NO_ATTR); /* matches right bracket */
	match(EOS_T, NO_ATTR); /* matches semicolon */
	gen_incode("PLATY: Output statement parsed\n");
}

/*
* Parameters: void
* Return value: void
*/
void output_list(void) {
	if (lookahead.code == STR_T) {
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		return;
	}
	opt_variable_list();
}

/*
* Parameters: void
* Return value: void
*/
void opt_variable_list(void) {
	if (lookahead.code == AVID_T) { 
		variable_list(); 
		return; 
	}
	if (lookahead.code == SVID_T) { 
		variable_list(); 
		return; 
	}
	gen_incode("PLATY: Output list (empty) parsed\n");
}

/*
* Purpose: <arithmetic> -> <unary arithmetic> | <additive arithmetic>
* Parameters: void
* Return value: void
*/
void arithmetic(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.code) {
		case ART_OP_T:
			switch (lookahead.attribute.arr_op) {
			case MULT:

			case DIV:
				multiplicative_arithmetic_p();
				additive_arithmetic_p();
				return;
			default:
				break;
			}
			unary_arithmetic();
			break;
		}
		break;
	case AVID_T: 
	
	case FPL_T: 
	
	case INL_T: 
	
	case LPR_T:
		additive_arithmetic();
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Arithmetic expression parsed\n");
}

/*
* Purpose: <unary arithmetic> -> - <primary arithemtic> | + <primary arithmetic>
* Parameters: void
* Return value: void
*/
void unary_arithmetic(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		match(ART_OP_T, lookahead.attribute.arr_op);
		primary_arithmetic();
		gen_incode("PLATY: Unary arithmetic expression parsed\n");
		break;
	default:
		syn_printe();
		return;
	}
}

/*
* Purpose: <additive arithmetic> -> <multiplicative arithmetic><additive arithmetic'>
* Parameters: void
* Return value: void
*/
void additive_arithmetic(void) {
	multiplicative_arithmetic();
	additive_arithmetic_p();
}

/*
* Purpose: <additive arithmetic'> -> + <multiplicative arithmetic><additive arithmetic'> | - <multiplicative arithmetic><additive arithmetic'> | E
* Parameters: void
* Return value: void
*/
void additive_arithmetic_p(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case PLUS:
			match(ART_OP_T, PLUS);
			multiplicative_arithmetic();
			additive_arithmetic_p();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			multiplicative_arithmetic();
			additive_arithmetic_p();
			break;
		default:
			return;
		}
		break;
	default:
		return;
	}
	gen_incode("PLATY: Additive arithmetic expression parsed\n");
}

/*
* Purpose: <multiplicative arithemtic> -> <priamary arithmetic><multiplicative arithmetic'>
* Parameters: void
* Return value: void
*/
void multiplicative_arithmetic(void) {
	primary_arithmetic();
	multiplicative_arithmetic_p();
}

/*
* Purpose: <multiplicative arithmetic'> -> * <priamry arithmetic><multiplicative arithmetic'> | / <priamry arithmetic><multiplicative arithmetic'> | E 
* Parameters: void
* Return value: void
*/
void multiplicative_arithmetic_p(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		switch (lookahead.attribute.arr_op) {
		case DIV:
			match(ART_OP_T, DIV); /* division */
			primary_arithmetic();
			multiplicative_arithmetic_p();
			break;
		case MULT:
			match(ART_OP_T, MULT); /* multiplication */
			primary_arithmetic();
			multiplicative_arithmetic_p();
			break;
		default:
			return;
		}
		break;
	default:
		return;
	}
	gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
}

/*
* Purpose: <primary arithmetic> -> AVID_T | FPL_T | INL_T | LPR_T
* Parameters: void
* Return value: void
*/
void primary_arithmetic(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed\n");
}

/*
* Purpose: <string> -> <primary string><string'> 
* Parameters: void
* Return value: void
*/
void string(void) {
	primary_string();
	string_p();
}

/*
* Purpose: <string'> -> << <primary string><string'> | E 
* Parameters: void
* Return value: void
*/
void string_p(void) {
	if (lookahead.code == SCC_OP_T) {
		match(SCC_OP_T, NO_ATTR);
		primary_string();
		string_p();
		return;
	}
	gen_incode("PLATY: String expression parsed\n");
	return;
}

/*
* Purpose: <primary string> -> SVID_T | STR_T
* Parameters: void
* Return value: void
*/
void primary_string(void) {
	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		return;
	}
	gen_incode("PLATY: Primary string expression parsed\n");
}

/*
* Purpose: <conditional> -> <logical or>
* Parameters: void
* Return value: void
*/
void conditional(void) {
	logical_or();
	gen_incode("PLATY: Conditional expression parsed\n");
}

/*
* Purpose: <logical or> -> <logical and> <logical or'>
* Parameters: void
* Return value: void
*/
void logical_or(void) {
	logical_and();
	logical_or_p();
}

/*
* Purpose: <logical or'> -> .OR. <logical and> <logical or'> | E
* Parameters: void
* Return value: void
*/
void logical_or_p(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case AND:
			return;
		default:
			break;
		}
		match(LOG_OP_T, OR);
		logical_and();
		logical_or_p();
		gen_incode("PLATY: Logical OR expression parsed\n");
	default:
		break;
	}
}

/*
* Purpose: <logical and> -> <relational> <logical and'>
* Parameters: void
* Return value: void
*/
void logical_and(void) {
	relational();
	logical_and_p();
}

/*
* Purpose: <logical and'> -> .AND. <relational> <logical and> | E
* Parameters: void
* Return value: void
*/
void logical_and_p(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		switch (lookahead.attribute.log_op) {
		case OR:
			return;
		default:
			break;
		}
		match(LOG_OP_T, AND);
		relational();
		logical_and_p();
		gen_incode("PLATY: Logical AND expression parsed\n");
		break;
	default:
		break;
	}
}

/*
* Purpose: <relational> -> <primary a_relational> <primary a_relational'> | <primary s_relational> <primary s_relational'>
* Parameters: void
* Return value: void
*/
void relational(void) {
	switch (lookahead.code) {
	case AVID_T: 
	
	case FPL_T: 
	
	case INL_T:
		primary_a_relational();
		primary_a_relational_p();
		break;
	case SVID_T: 
	
	case STR_T:
		primary_s_relational();
		primary_s_relational_p();
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Relational expression parsed\n");
}

/*
* Purpose: <primary a_relational'> -> == <primary a_relational> | <> <primary a_relational> | > <primary a_relational> | < <primary a_relational>
* Parameters: void
* Return value: void
*/
void primary_a_relational_p(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		switch (lookahead.attribute.rel_op) {
		case EQ: 
			match(REL_OP_T, EQ);
			primary_a_relational();
			break;
		case NE: 
			match(REL_OP_T, NE);
			primary_a_relational();
			break;
		case LT: 
			match(REL_OP_T, LT);
			primary_a_relational();
			break;
		case GT: 
			match(REL_OP_T, GT);
			primary_a_relational();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*
* Purpose: <primary s_relational'> -> == <primary s_relational> | <> <primary s_relational> | > <primary s_relational> | < <primary s_relational>
* Parameters: void
* Return value: void
*/
void primary_s_relational_p() {
	switch (lookahead.code) {
	case REL_OP_T: 
		switch (lookahead.attribute.rel_op) {
		case EQ: 
			match(REL_OP_T, EQ);
			primary_s_relational();
			break;
		case NE: 
			match(REL_OP_T, NE);
			primary_s_relational();
			break;
		case LT: 
			match(REL_OP_T, LT);
			primary_s_relational();
			break;
		case GT: 
			match(REL_OP_T, GT);
			primary_s_relational();
			break;
		default:
			syn_printe();
		}
		break;
	default:
		syn_printe();
	}
}

/*
* Purpose: <primary a_relational> -> AVID_T | FPL_T | INL_T
* Parameters: void
* Return value: void
*/
void primary_a_relational() {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	default:
		syn_printe();
	}
	gen_incode("PLATY: Primary a_relational expression parsed\n");
}

/*
* Purpose: <primary s_relational> -> STR_T, SVID_T
* Parameters: void
* Return value: void
*/
void primary_s_relational(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		break;
	default:
		syn_printe();
	}
	primary_string();
	gen_incode("PLATY: Primary s_relational expression parsed\n");
}
