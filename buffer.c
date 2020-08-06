
#include "buffer.h"

/* only use constants */

Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/*
	creates new buffer
	tries to allocate for one buffer using calloc
	tries to allocate one dynamic character buffer using malloc with init_capacity,
		range of 0 and max 1 inclusive, if init=0 set to 200def and ignore inc_fact to 15 in mode a and m or 0 in mode f,
		pointer returned by malloc is cb_head
	sets buffer mode and inc_factor, if o_mode is f then both = 0, if inc = 0 and init !=0 then mode and inc = 0,
		if o_mode is a and inc is 1-255inclusive then mode =1 and inc_factor=inc_factor,
		if o_mode is m and inc_fact 1-100 inclusive then mode = -1 and inc_fact=inc_fact
	copies init_cap into buffer capacity variable
	sets flags to def which is FFF9 hex
	returns pointer to buffer, return null on error, if error - return immediately
	no memory leaks, dangling, bad param
	*/
	Buffer* pBD;

	if (init_capacity < 0 || init_capacity >(MAX_ALLOWED_POSITIVE_VALUE - 1)) {
		return NULL;
	}
	pBD = (Buffer*)calloc(1, sizeof(Buffer));
	if (NULL == pBD) {
		return NULL;
	}
	if (init_capacity == 0) {
		init_capacity = DEFAULT_INIT_CAPACITY;
		inc_factor = DEFAULT_INC_FACTOR;
		if (o_mode == 'f') {
			inc_factor = 0;
		}
	}
	pBD->cb_head = malloc(init_capacity * sizeof(char*));
	if (NULL == pBD->cb_head) {
		return NULL;
	}
	if (o_mode == 'f' || inc_factor == 0) {
		pBD->mode = 0;
		pBD->inc_factor = 0;
	}
	if (o_mode == 'f' && inc_factor != 0) {
		pBD->mode = 0;
		pBD->inc_factor = 0;
	}
	else if (o_mode == 'a' && (inc_factor >= 1 && inc_factor <= 255)) {
		pBD->mode = 1;
		pBD->inc_factor = inc_factor;
	}
	else if (o_mode == 'm' && (inc_factor >= 1 && inc_factor <= 100)) {
		pBD->mode = -1;
		pBD->inc_factor = inc_factor;
	}
	pBD->capacity = init_capacity;
	pBD->flags = DEFAULT_FLAGS;
	return pBD;
}

pBuffer b_addc(pBuffer const pBD, char symbol) {
	/*
	using bitwise operation reset flags r_flag bit to 0
	tries to add symbol to character array pointed by pBD
	if buffer is operational and not full then symbol can be stored
	function adds character to content of character buffer, increments addc_offset by 1 and returns
	if full then try to resize buffer by increasing current capacity to new capacity, depends on operational mode
	if operational = 0 then return NULL
	if 1 then try increase by adding inc_fact(converted to bytes) to capacity,
		if result is positive and doesnt exceed MAX-1 then assigns MAX-1 to new capacity,
		max value determined by datatype of variable which contains buffer capacity
		if result negative return NULL
	if -1 then tries increase by:
		max return NULL
		formula:
			availableSpace=maxCap-currentCap
			newInc=availableSpace*inc_fact/100
			newCap=currentCap+newInc
		if currentCap cant incr but current < max, set max to newCap
		if cap incr mode is 1 or -1 then
			tries to expand using realloc with new cap, if fail return NULL
			if location change then set r_flag to 1 using bitwise
			add symbol to buffer content
			change val of addc_offset by 1 and save new cap to buffer cap
			returns pointer to buffer
		must not detroy buffer or contents, change in platform must not lead to bad behaviour
	*/
	short availableSpace = 0;
	short newCapacity = 0;
	short newIncrement = 0;

	if (NULL == pBD) {
		return NULL;
	}
	pBD->flags &= RESET_R_FLAG;
	if (!b_isfull(pBD)) {
		pBD->cb_head[pBD->addc_offset++] = symbol;
		return pBD;
	}
	if (pBD->mode == 0) {
		return NULL;
	}
	if (pBD->mode == 1) {
		newCapacity = pBD->capacity + pBD->inc_factor;
		if ((pBD->capacity > 0) && (pBD->capacity > (MAX_ALLOWED_POSITIVE_VALUE - 1))) {
			newCapacity = MAX_ALLOWED_POSITIVE_VALUE - 1;
		}
		else if (newCapacity < 0) {
			return NULL;
		}
	}
	if (pBD->mode == -1) {
		if (pBD->capacity == (MAX_ALLOWED_POSITIVE_VALUE - 1)) {
			return NULL;
		}
		availableSpace = MAX_ALLOWED_POSITIVE_VALUE - 1 - pBD->capacity;
		newIncrement = availableSpace * pBD->inc_factor / 100;
		newCapacity += (pBD->capacity + pBD->inc_factor);
		if (newIncrement == 0 || newIncrement >= MAX_ALLOWED_POSITIVE_VALUE) {
			newCapacity = MAX_ALLOWED_POSITIVE_VALUE - 1;
		}
		else
		{
			newCapacity = pBD->capacity + newIncrement;
		}
	}
	/* if pBD->mode == 1 || pBD->mode == -1 */
	char* updatedBuffer = realloc(pBD->cb_head, newCapacity * sizeof(char));
	if (NULL == updatedBuffer) {
		return NULL;
	}
	if (updatedBuffer != pBD->cb_head) {
		pBD->cb_head = updatedBuffer;
		pBD->flags = pBD->flags | SET_R_FLAG;
	}


	pBD->capacity = newCapacity;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	return pBD;
}

int b_clear(Buffer* const pBD) {
	/*
	return -1 on error
	retains memory space but re-initializes all data members of Buffer
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	pBD->capacity = DEFAULT_INIT_CAPACITY;
	pBD->inc_factor = DEFAULT_INC_FACTOR;
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = DEFAULT_FLAGS;
	return 0; /* was 1 */
}

void b_free(Buffer* const pBD) {
	/*
	frees memory for character buffer and buffer structure (buffer descriptor)
	*/
	if (NULL == pBD) {
		return;
	}
	if (NULL != pBD->cb_head) {
		free(pBD->cb_head);
	}
	free(pBD);
}

int b_isfull(Buffer* const pBD) {
	/*
	returns 1 if full else 0
	if error -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	if (pBD->addc_offset == pBD->capacity) {
		return 1;
	}
	return 0;
}

short b_addcoffset(Buffer* const pBD) {
	/*
	returns current addc_offset
	-1 if error
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->addc_offset;
}

short b_capacity(Buffer* const pBD) {
	/*
	returns capacity
	-1 if error
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->capacity;
}

short b_markc(Buffer* const pBD, short mark) {
	/*
	sets markc_offset to mark
	mark be between 0 and addc_offset inclusive
	returns markc_offset or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	if (mark >= 0 && mark <= pBD->addc_offset) {
		pBD->markc_offset = mark;
		return pBD->markc_offset;
	}
	return RT_FAIL_1;
}

int b_mode(Buffer* const pBD) {
	/*
	returns mode or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->mode;
}

size_t b_incfactor(Buffer* const pBD) {
	/*
	returns non neg inc or error of 0x100
	*/
	if (NULL == pBD) {
		return ERROR_MASK;
	}
	if (pBD->inc_factor == 255) {
		return ERROR_MASK;
	}
	return pBD->inc_factor;
}

int b_load(FILE* const fi, Buffer* const pBD) {
	/*
	reads a file using fgetc(fi) char by char and use b_addc()
	if char cant be added then function returns char to file stream (file buffer)
		using ungetc() lib funct then returns -2 using LOAD_FAIL const
	do this until standard macro feof(fi) detects end of file which mustnt be added to buffer
	only feof to detect end of file, using other is not allowed
	if other errors return -1 else return number of chars added
	*/
	char bufferChar;
	int count = 0;
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	if (NULL == fi) {
		return RT_FAIL_1;
	}
	while (1)
	{
		bufferChar = (char)fgetc(fi);
		if (feof(fi)) {
			break;
		}
		if (NULL == b_addc(pBD, bufferChar)) {
			ungetc(bufferChar, fi);
			return LOAD_FAIL;
		}
		count++;
	}
	return count;
}

int b_isempty(Buffer* const pBD) {
	/*
	if addc_offset is 0 then return 1 else 0
	error -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->addc_offset == 0;
}

char b_getc(Buffer* const pBD) {
	/*
	reads buffer
	checks argument for valid if not then return -2
	if getc_offset = addc_offset then bitwise to set flags eob to 1 and returns 0 else set eob to 0
	incr getc_offset by 1 and returns char located at getc_offset
	*/
	if (NULL == pBD) {
		return RT_FAIL_2;
	}
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags = pBD->flags | SET_EOB;
		return 0;
	}
	pBD->flags &= RESET_EOB;
	return pBD->cb_head[pBD->getc_offset++];
}

int b_eob(Buffer* const pBD) {
	/*
	returns val of flags field determined only by eob bit
	bitwise used to return val of flags or return -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->flags & SET_EOB;
}

int b_print(Buffer* const pBD, char nl) {
	/*
	diagnostic purposes
	use printf to print char by char of charbuff to stdout
	in loop print using b_getc() and b_eob() to find end
	after loop check nl and if not 0 then print new line
	returns num of chars printed else -1
	*/
	char bufferChar;
	int i = 0;
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	while (1) {
		bufferChar = b_getc(pBD);
		if (b_eob(pBD)) {
			break;
		}
		i++;
		printf("%c", bufferChar);
	}
	if (nl) {
		printf("\n");
	}
	return i;
}

Buffer* b_compact(Buffer* const pBD, char symbol) {
	/*
	for operational modes, function shrinks or expands new capacity
	new cap is current limit plus space for one more char
	new cap is addc_offset + 1 converted to bytes
	uses realloc to adjust new cap and updates all necessary members
	adds symbol to end of buffer using addc_offset and not b_addc and incr addc_offset then returns pointer to buffer
	must set r_flag correctly and return NULL for errors
	*/
	short newCapacity;
	char* updatedBuffer;
	if (NULL == pBD) {
		return NULL;
	}
	newCapacity = (pBD->addc_offset + 1) * sizeof(char);
	if (newCapacity < 0) {
		return NULL;
	}
	updatedBuffer = realloc(pBD->cb_head, newCapacity);
	if (NULL == updatedBuffer) {
		return NULL;
	}
	if (updatedBuffer != pBD->cb_head) {
		pBD->cb_head = updatedBuffer;
		pBD->flags &= RESET_R_FLAG;
	}
	pBD->capacity = newCapacity;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	return pBD;
}

char b_rflag(Buffer* const pBD) {
	/*
	returns flags determined by rflag bit
	bitwise to return flags else -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->flags &= SET_R_FLAG;
}

short b_retract(Buffer* const pBD) {
	/*
	decr getc_offset by 1
	return getc_offset or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	if (pBD->getc_offset <= 0) { /* avoids getc_offset being below zero */
		return pBD->getc_offset;
	}
	return pBD->getc_offset--;
}

short b_reset(Buffer* const pBD) {
	/*
	sets getc_offset to markc_offset
	returns getc_offset or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

short b_getcoffset(Buffer* const pBD) {
	/*
	returns getc_offset or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	return pBD->getc_offset;
}

int b_rewind(Buffer* const pBD) {
	/*
	sets getc_offset and markc_offset to 0
	returns 0 or -1
	*/
	if (NULL == pBD) {
		return RT_FAIL_1;
	}
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

char* b_location(Buffer* const pBD, short loc_offset) {
	/*
	returns pointer to location of charbuff indicated by loc_offset
	loc_offset is distance measured in chars from beginning (cb_head)
	else return NULL
	*/
	if (NULL == pBD) {
		return NULL;
	}
	return pBD->cb_head + loc_offset;
}