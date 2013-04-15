// GASM Source Code, Gareth Owen, gaz@athene.co.uk, http://gasm.cjb.net
// YOU MUST READ LICENSE.TXT BEFORE USE OF THE SOURCE CODE IS PERMITTED, 
// IF YOU DO NOT HAVE LICENSE.TXT THEN EITHER GET A COPY OF 
// DELETE THIS SOURCE CODE

/*
GASM Assembler
Copyright (C) 1999  Gareth Owen <gaz@athene.co.uk>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "gasm.h"

// Prototypes

int gstrnicmp(char *string1, char *string2, unsigned int n);
int gstricmp(char *string1, char *string2);
void gstrlwr(char *string);

t_instruction Parseline(char *orig_line);
t_dynamic_instruction FindDynamicInstruction(char *ins_name, unsigned int parm1, unsigned int parm2);
unsigned long Eval2(char *e_string, char brackets, unsigned int evaluated);
unsigned char FindModRM(unsigned long &modrm_evaluated);
void GetDisplacement(char *d_string, char *displacement);

void fatal_error(char *error_msg);
t_instruction Handledesc(char *line);
char isHex(char *string);

t_dynamic_instruction AutoModify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p);
t_dynamic_instruction Parm1_Modify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p);
t_dynamic_instruction Parm2_Modify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p);

signed long GetRelative(char *address, unsigned long instructionend);
unsigned char CheckValidLabel(char *label_name);
unsigned int GetSegment(char *s_string);
unsigned long GetOffset(char *s_string);
char Contains(char *c_string, char c);	// Does the character contain c?
t_fixed_instruction FindFixedInstruction(char *instruction);
unsigned long SolveEquation(char *equation);
char isInstruction(char *i_string);
char isRelativeInstruction(char *instruction);
void Command(char *line);
void Preprocessor(char *line);

t_instruction Handletimes(char *line);
t_instruction Handletss(char *line);

char isSegmentRegister(char *seg_reg);
char SegmentRegister(char *seg_reg);
char isControlRegister(char *control_reg);
char ControlRegister(char *control_reg);
char isDebugRegister(char *debug_reg);
char DebugRegister(char *debug_reg);

void HandleEQU(char *line);
int remstring(char *str, char *rem);

unsigned char FindRegister(char *reg);

void ExtractBrackets(char *string);

t_instruction DataType(char *line);
t_instruction popSeg(char *seg_reg);
t_instruction pushSeg(char *seg_reg);
void Parsefile(char writeoutput);


unsigned long line_number = 0;  // Current line number of current file
unsigned long offset = 0;       // Binary offset (of output file)
unsigned char pass_number = 1, BITS = 16;  // Current pass
unsigned int prefixes = 0;
unsigned char address_override_disable = 0, address_override_force = 0, size_override_force = 0;

char *temp_p=0;			// Stores instruction, sorry, un-imaginative name :)
char filename[255];
FILE *out;

//**************************************
// MAIN FUNCTION -=- PROGRAM ENTRY POINT
//**************************************
void main(int argc, char **argv)
{
	char tempstring[500], *newline;
	int loop=0;

        tempstring[0] = '\0';

        // Initialise label array
        for(loop=0; loop<=MAX_LABELS; loop++) {
		labels[loop].active = FALSE;
		labels[loop].pass_number = 0;
	}
	
	memcpy(&labels, &predefines, sizeof(predefines));

        printf("GASM  --- Gaz's Assembler v0.55, by Gareth Owen.\n(C)Copyright 1998 by Gareth Owen. All rights reserved\n");
	printf("http://gasm.cjb.net -=- Contact Gareth Owen at gaz@athene.co.uk\n");

	if(argc != 2)
	{
		printf("\n\nUsage: gasm <source.asm>\n");
		exit(1);
	}
	
	for(loop=0; argv[1][loop]!='.' && argv[1][loop]!=(char)NULL; loop++)
		tempstring[loop] = argv[1][loop];

	tempstring[loop] = (char)NULL;

	strcat(tempstring, ".bin");

      // Open output file
	if((out = fopen(tempstring, "wb"))==(char)NULL)
	{
		perror(tempstring);
		exit(1);
	}
	
	 // Initialize variables for Pass 1
	pass_number = 1;
	offset = 0;
	line_number = 0;

	for(;pass_number <= 3; pass_number++)
	{
		strcpy(filename, argv[1]);

		if(pass_number!=3) Parsefile(FALSE);
		else Parsefile(TRUE);
	
		offset = 0;
		line_number = 0;
		BITS = 16;
	}

	fclose(out);
}

void Parsefile(char writeoutput)
{
	char line[500], *newline=0;
        FILE *in;
        t_instruction instruction;      // Stores assembled/compiled instruction

        line[0] = '\0';
        instruction.length = 0;

	  // Open source file
	if((in = fopen(filename, "rt"))==(char)NULL)
	{
		printf("Error opening %s\n", filename);
		perror(filename);
		exit(1);
	}

	while(!feof(in))
	{
		instruction.length = 0;         // Clear instruction length
                line[0] = '\0';
                fgets(line, 500, in);   // Read in Line
                line_number++;          // Increase line number count
                // Remove any \r's and \n's and make ;s the line terminator
		for(int loop=0; line[loop]; loop++) if(line[loop]=='\r' || line[loop]=='\n' || line[loop]==';') line[loop]=(char)(char)NULL;
		newline = line;
		for(; newline[0]==' ' || newline[0]=='\t'; newline++);	

            instruction = Parseline(newline);  // Parse line to assembler
		prefixes = 0;
		
            // Write assembled/compiled instruction to out file
		if(writeoutput == TRUE && instruction.length!=0) for(int loop=0; loop<instruction.length; loop++) fputc(instruction.i[loop], out);

                offset += instruction.length;   // Add instruction length to current offset
                free(instruction.i);            // Free instruction data
      }
	fclose (in);
}

//***************************************************
// Assembles/compiles instruction from text to binary
//***************************************************

t_instruction Parseline(char *line)
{
        // Just a load of variables that are going to be used
	t_instruction instruction;
	t_fixed_instruction finstruction;
	char *parm1=0, *parm2=0, temp_char=0, temp_modrm=0, temp_string[255], *orig_line;
	unsigned int parm1_evaluated=0, parm2_evaluated=0;
	unsigned int loop = 0, temp_count=0, temp_int=0;
	unsigned long temp_long = 0, modrm_eval;

	temp_p = 0;
	address_override_disable = 0;
	address_override_force = 0;
	size_override_force = 0;


	signed char temp_schar = 0;	// for the relative instructions
	signed int temp_sint = 0;
	signed long temp_slong = 0;

	t_dynamic_instruction find_dyn_instruction;     // Used searching for dynamic instruction

      find_dyn_instruction.opcode_length = 0;         // Set to zero..
	
	orig_line = (char *) malloc(strlen(line) + 1);
	strcpy(orig_line, line);
	
	temp_count = 0;

        // Initialize final instruction variables
        instruction.length = 0;
        instruction.i = (unsigned char *) malloc(100);

	if(strstr(line, "s:"))
	{
		if(strstr(line, "cs:")) { prefixes |= PREFIX_CS; remstring(line, "cs:"); instruction.i[temp_count] = 0x2E; temp_count++; parm1_evaluated = Evaluator(line);}
		if(strstr(line, "ds:")) { prefixes |= PREFIX_DS; remstring(line, "ds:"); instruction.i[temp_count] = 0x3E; temp_count++; parm1_evaluated = Evaluator(line);}
		if(strstr(line, "es:")) { prefixes |= PREFIX_ES; remstring(line, "es:"); instruction.i[temp_count] = 0x26; temp_count++; parm1_evaluated = Evaluator(line);}
		if(strstr(line, "fs:")) { prefixes |= PREFIX_FS; remstring(line, "fs:"); instruction.i[temp_count] = 0x64; temp_count++; parm1_evaluated = Evaluator(line);}
		if(strstr(line, "gs:")) { prefixes |= PREFIX_GS; remstring(line, "gs:"); instruction.i[temp_count] = 0x65; temp_count++; parm1_evaluated = Evaluator(line);}
		if(strstr(line, "ss:")) { prefixes |= PREFIX_SS; remstring(line, "ss:"); instruction.i[temp_count] = 0x36; temp_count++; parm1_evaluated = Evaluator(line);}
	}


	// override prefixes
	if(prefixes & PREFIX_REPNZ) { instruction.i[temp_count] = 0xF2; temp_count++; }
	if(prefixes & PREFIX_REPZ) { instruction.i[temp_count] = 0xF3; temp_count++; }
	if(prefixes & PREFIX_LOCK) { instruction.i[temp_count] = 0xF0; temp_count++; }
	if(prefixes & PREFIX_REP) { instruction.i[temp_count] = 0xF3; temp_count++; }

	       temp_p = strtok(line, " \t");     // temp_p stores instruction name, i couldn't think of a more imaginative name, sorry :-)
        if(!temp_p) return instruction; 

	        // Get parameter one to the instruction
	parm1 = strtok((char)NULL, ",");

	if(parm1) while((parm1[0] == ' ' || parm1[0] == '\t') && parm1[0]!=(char)NULL) if (parm1[0]==' ' || parm1[0]=='\t') parm1++;
	if(parm1) for(loop=(strlen(parm1)-1); loop!=0 && (parm1[loop]==' ' || parm1[loop]=='\t'); loop--) if(parm1[loop] == ' ' || parm1[loop] == '\t') parm1[loop]=(char)NULL;

        if(!gstrnicmp(parm1, "equ", 3)) { HandleEQU(orig_line); return instruction; }
        if(!gstrnicmp(parm1, "tss", 3))  return Handletss(orig_line);

        if(isInstruction(temp_p)==FALSE || !gstrnicmp(temp_p, "rep", 3)) goto notinstruction;

	if(parm1) 
	{          
	    // Process any size overrides
            if(strstr(parm1, "byte"))
		{
			remstring(parm1, "byte");	
			parm1_evaluated = Evaluator(parm1);

			if(parm1_evaluated == EVALUATE_REGM32) { parm1_evaluated = EVALUATE_REGM8; address_override_force = TRUE; }
			if(parm1_evaluated == EVALUATE_REGM16) parm1_evaluated = EVALUATE_REGM8;
		}
		else if(strstr(parm1, "word") && !strstr(parm1, "dword"))
		{		
			remstring(parm1, "word");
			parm1_evaluated = Evaluator(parm1);

			if(parm1_evaluated == EVALUATE_IMM8) parm1_evaluated = EVALUATE_IMM16;
		}
		else if(strstr(parm1, "dword"))
		{
			remstring(parm1, "dword");
			parm1_evaluated = Evaluator(parm1);
			
			if(parm1_evaluated == EVALUATE_REGM16) { parm1_evaluated = EVALUATE_REGM32; address_override_disable = TRUE; }
			if(parm1_evaluated == EVALUATE_PTR16_PTR16) parm1_evaluated =  EVALUATE_PTR16_PTR32;
		}
		else parm1_evaluated = Evaluator(parm1);
	} else parm1_evaluated = EVALUATE_NONE;

	if(parm1) while((parm1[0] == ' ' || parm1[0] == '\t') && parm1[0]!=(char)NULL) if (parm1[0]==' ' || parm1[0]=='\t') parm1++;
	if(parm1) for(loop=(strlen(parm1)-1); loop!=0 && (parm1[loop]==' ' || parm1[loop]=='\t'); loop--) if(parm1[loop] == ' ' || parm1[loop] == '\t') parm1[loop]=(char)NULL;


        // Get parameter two of instruction
	parm2 = strtok((char)NULL, ",");

	if(parm2) 
	{
                // Process any size overrides
                if(strstr(parm2, "byte"))
		{
			remstring(parm2, "byte");
			parm2_evaluated = Evaluator(parm2);

			if(parm2_evaluated == EVALUATE_REGM16) parm2_evaluated = EVALUATE_REGM8;
			if(parm2_evaluated == EVALUATE_REGM32) { parm2_evaluated = EVALUATE_REGM8; address_override_force = TRUE; }
		}
		else if(strstr(parm2, "word") && !strstr(parm2, "dword"))
		{
			remstring(parm2, "word");
			parm2_evaluated = Evaluator(parm2);

			if(parm2_evaluated == EVALUATE_IMM8) parm2_evaluated = EVALUATE_IMM16;
		}
		else if(strstr(parm2, "dword"))
		{
			remstring(parm2, "dword");
			parm2_evaluated = Evaluator(parm2);

			if(parm2_evaluated == EVALUATE_REGM16) { parm2_evaluated = EVALUATE_REGM32; address_override_disable = TRUE; }
			if(parm1_evaluated == EVALUATE_PTR16_PTR16) parm1_evaluated =  EVALUATE_PTR16_PTR32;
		}
		else parm2_evaluated = Evaluator(parm2);
	} else parm2_evaluated = EVALUATE_NONE;

	if(parm2) while((parm2[0] == ' ' || parm2[0] == '\t') && parm2[0]!=(char)NULL) if(parm2[0]==' ' || parm2[0]=='\t') parm2++;
	if(parm2) for(loop=(strlen(parm2)-1); loop!=0 && (parm2[loop]==' ' || parm2[loop]=='\t'); loop--) if(parm2[loop] == ' ' || parm2[loop] == '\t') parm2[loop]=(char)NULL;

	loop = 0;

	 // If the instruction has no parameters, then its a fixed instruction
	if(parm1_evaluated == EVALUATE_NONE && parm2_evaluated == EVALUATE_NONE)
	{
fixed_instruction:                
		finstruction = FindFixedInstruction(temp_p);    // Try and find it...

		if((BITS == 16 && finstruction.bits == 32) || (BITS == 32 && finstruction.bits == 16)) { instruction.i[temp_count] = 0x66; instruction.i[temp_count+1] = 0x67; temp_count+=2; }
		
                // Return it if it was found
		if(finstruction.length!=0)
		{
			instruction.length = finstruction.length + temp_count;

			instruction.i[temp_count] = finstruction.c1; temp_count++;
			if(finstruction.length == 2 || finstruction.length == 3) { instruction.i[temp_count] = finstruction.c2; temp_count++; }
			if(finstruction.length == 3) { instruction.i[temp_count] = finstruction.c3; temp_count++; }

			return instruction;
		}
	}
        // Use some intelligence to realise that if the parameter is
        // an IMM8, and the instruction only supports IMM16, then we can still
        // do it....(this function does a lot more too!)
	find_dyn_instruction = AutoModify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);

        // Looks like that variation of the instruction wasn't found, possibly because
        // of invalid parameters, or something
notinstruction:
	if(find_dyn_instruction.opcode_length == 0)
	{
		    if(!gstricmp(temp_p, "pop") && parm1_evaluated == EVALUATE_SREG) return popSeg(parm1);
                if(!gstricmp(temp_p, "push") && parm1_evaluated == EVALUATE_SREG) return pushSeg(parm1);
		    if(!gstricmp(temp_p, "desc")) return Handledesc(orig_line);

                if(!gstricmp(temp_p, "repne") || !gstricmp(temp_p, "repnz")) prefixes |= PREFIX_REPNZ;
                if(!gstricmp(temp_p, "repe") || !gstricmp(temp_p, "repz")) prefixes |= PREFIX_REPZ;
                if(!gstricmp(temp_p, "rep")) prefixes |= PREFIX_REP;
                if(!gstricmp(temp_p, "lock")) prefixes |= PREFIX_LOCK;
                if(!gstricmp(temp_p, "repne") || !gstricmp(temp_p, "repnz") || !gstricmp(temp_p, "repz") || !gstricmp(temp_p, "repe") || !gstricmp(temp_p, "rep") || !gstricmp(temp_p, "lock"))
		    {
				for(;orig_line[0]!=(char)' ' && orig_line[0]!=(char)'\t' && orig_line[0]!=(char)(char)NULL; orig_line++);
				for(;(orig_line[0]==(char)' ' || orig_line[0]==(char)'\t') && orig_line[0]!=(char)(char)NULL; orig_line++);

				if(orig_line[0] == (char)NULL) goto fixed_instruction;

				return Parseline(orig_line);
		    }
		
		if(isInstruction(temp_p)==TRUE) // Is there an instruction by that name?
                	fatal_error("You have entered arguments not possible for the instruction");
			

                if(!gstricmp(temp_p, "db") || !gstricmp(temp_p, "dd") || !gstricmp(temp_p, "dw")) return DataType(orig_line);
                if(!gstricmp(temp_p, "times")) return Handletimes(orig_line);
		if(temp_p[0]=='[') { Command(orig_line); return instruction; }
		if(temp_p[0]=='%') { Preprocessor(orig_line); return instruction; }

                // If the instruction wasn't found as dynamic or fixed, then we
                // must assume its a label

		if(temp_p[strlen(temp_p)-1]==':') 
		{
			temp_p[strlen(temp_p)-1]=(char)(char)NULL;
			Addlabel(temp_p, offset);
			temp_p[strlen(temp_p)] = ':';
		}
		else Addlabel(temp_p, offset);

		for(;orig_line[0]!=(char)' ' && orig_line[0]!=(char)'\t' && orig_line[0]!=(char)(char)NULL; orig_line++);
		for(;(orig_line[0]==(char)' ' || orig_line[0]==(char)'\t') && orig_line[0]!=(char)(char)NULL; orig_line++);

		if(orig_line[0]==(char)(char)NULL)
		{
			instruction.length = 0;
			return instruction;
		}

		return Parseline(orig_line);	
	}

	  // By now, we have found the instruction in our database, now to assemble it
        // with operands etc..

        // Does the instruction need the 32bit overrides?
      if((parm1_evaluated == EVALUATE_IMM32 || parm1_evaluated == EVALUATE_REG32 || parm2_evaluated == EVALUATE_IMM32 || parm2_evaluated == EVALUATE_REG32 || parm1_evaluated == EVALUATE_EAX || parm2_evaluated == EVALUATE_EAX || Evaluator(parm1)==EVALUATE_REG32 || Evaluator(parm2)==EVALUATE_REG32 || parm1_evaluated == EVALUATE_PTR16_PTR32 || parm2_evaluated == EVALUATE_PTR16_PTR32) && BITS == 16)
	{
		if(parm1_evaluated!=EVALUATE_CREG && parm2_evaluated!=EVALUATE_CREG)
		{
			instruction.i[temp_count] = 0x66;
			temp_count++;
		}
	}
        if((((parm1_evaluated == EVALUATE_REGM32 || parm2_evaluated == EVALUATE_REGM32 || (find_dyn_instruction.format & FORMAT_REL32)) && address_override_disable == FALSE) && BITS == 16) || address_override_force == TRUE)
	{
		if(parm1_evaluated!=EVALUATE_CREG && parm2_evaluated!=EVALUATE_CREG)
		{
			instruction.i[temp_count] = 0x67;
			temp_count++;
			address_override_force = 0;
		}
	}
	if(!(parm1_evaluated == EVALUATE_IMM32 || parm1_evaluated == EVALUATE_REG32 || parm2_evaluated == EVALUATE_IMM32 || parm2_evaluated == EVALUATE_REG32 || parm1_evaluated == EVALUATE_EAX || parm2_evaluated == EVALUATE_EAX || Evaluator(parm1)==EVALUATE_REG32 || Evaluator(parm2)==EVALUATE_REG32 || parm1_evaluated == EVALUATE_PTR16_PTR32 || parm2_evaluated == EVALUATE_PTR16_PTR32) && BITS == 32)
	{
		if(parm1_evaluated!=EVALUATE_CREG && parm2_evaluated!=EVALUATE_CREG)
		{
			instruction.i[temp_count] = 0x66;
			temp_count++;
		}
	}
        if((!((parm1_evaluated == EVALUATE_REGM32 || parm2_evaluated == EVALUATE_REGM32 || (find_dyn_instruction.format & FORMAT_REL32)) && address_override_disable == FALSE) && BITS == 32) || address_override_force == TRUE)
	{
		if(parm1_evaluated!=EVALUATE_CREG && parm2_evaluated!=EVALUATE_CREG)
		{
			instruction.i[temp_count] = 0x67;
			temp_count++;
			address_override_force = 0;
		}
	}	


        // Now put the opcode in..
	instruction.i[temp_count] = find_dyn_instruction.c1;
	temp_count++;
	if(find_dyn_instruction.opcode_length == 2) { instruction.i[temp_count] = find_dyn_instruction.c2; temp_count++; }
	if(find_dyn_instruction.opcode_length == 3) { instruction.i[temp_count] = find_dyn_instruction.c3; temp_count+=2; }
	
	if(find_dyn_instruction.format & FORMAT_REGPLUSOP)	// Do we have to add the register to the opcode ?
	{
		if(parm1_evaluated == EVALUATE_REG8 || parm1_evaluated == EVALUATE_REG16 || parm1_evaluated == EVALUATE_REG32) instruction.i[temp_count-1] += FindRegister(parm1);
		if(parm2_evaluated == EVALUATE_REG8 || parm2_evaluated == EVALUATE_REG16 || parm2_evaluated == EVALUATE_REG32) instruction.i[temp_count-1] += FindRegister(parm2);
	}

        // If we need a MOD-RM byte, then calculate what it is....
// *********************** MOD-RM BYTE CALCULATOR ***************************
	if(find_dyn_instruction.format & FORMAT_MODRM)
	{
                // We cannot have two REGMs for one instruction...
                if((parm1_evaluated == EVALUATE_REGM8 || parm1_evaluated == EVALUATE_REGM16) && (parm2_evaluated == EVALUATE_REGM8 || parm2_evaluated == EVALUATE_REGM16))
				fatal_error("You cannot have two memory operands per instruction!");

                // We cannot have two REGs for one instruction... or can we?
                // Yes we can, we'll use the REGM part of the MODRM byte
		if(parm1_evaluated == EVALUATE_REG8 && (parm2_evaluated == EVALUATE_REG8 || parm2_evaluated == EVALUATE_REG16))
			parm1_evaluated = EVALUATE_REGM8;
		if(parm1_evaluated == EVALUATE_REG16 && (parm2_evaluated == EVALUATE_REG8 || parm2_evaluated == EVALUATE_REG16))
			parm1_evaluated = EVALUATE_REGM16;
	
		modrm_eval = 0;
		temp_modrm = 0;

                // Now calculate the MODRM part for the REGM (if applicable)
        	if(parm1_evaluated == EVALUATE_REGM16 || parm1_evaluated == EVALUATE_REGM8 || parm1_evaluated == EVALUATE_REGM32)
		{
			modrm_eval = Eval2(parm1, 0, parm1_evaluated);
			temp_modrm += FindModRM(modrm_eval);
		}
                // Now to the registers..
		if(parm1_evaluated == EVALUATE_REG8 || parm1_evaluated == EVALUATE_REG16 || parm1_evaluated == EVALUATE_REG32)
		{
                        if(!gstricmp("ax", parm1) || !gstricmp("al", parm1) || !gstricmp("eax", parm1)) temp_modrm |= 0x00;
                        if(!gstricmp("bx", parm1) || !gstricmp("bl", parm1) || !gstricmp("ebx", parm1)) temp_modrm |= 0x18;
                        if(!gstricmp("cx", parm1) || !gstricmp("cl", parm1) || !gstricmp("ecx", parm1)) temp_modrm |= 0x08;
                        if(!gstricmp("dx", parm1) || !gstricmp("dl", parm1) || !gstricmp("edx", parm1)) temp_modrm |= 0x10;
                        if(!gstricmp("si", parm1) || !gstricmp("dh", parm1) || !gstricmp("esi", parm1)) temp_modrm |= 0x30;
                        if(!gstricmp("di", parm1) || !gstricmp("bh", parm1) || !gstricmp("edi", parm1)) temp_modrm |= 0x38;
                        if(!gstricmp("bp", parm1) || !gstricmp("ch", parm1) || !gstricmp("ebp", parm1)) temp_modrm |= 0x28;
                        if(!gstricmp("sp", parm1) || !gstricmp("ah", parm1) || !gstricmp("esp", parm1)) temp_modrm |= 0x20;
		}
                // Now do the REGM part of the MODRM for parameter two (if applicable) 
		if(parm2_evaluated == EVALUATE_REGM16 || parm2_evaluated == EVALUATE_REGM8 || parm2_evaluated == EVALUATE_REGM32)
		{
			modrm_eval = Eval2(parm2, 0, parm2_evaluated);
			temp_modrm += FindModRM(modrm_eval);
		}
                // Now do the REGs
		if(parm2_evaluated == EVALUATE_REG8 || parm2_evaluated == EVALUATE_REG16 || parm2_evaluated == EVALUATE_REG32)
		{
                        if(!gstricmp("ax", parm2) || !gstricmp("al", parm2) || !gstricmp("eax", parm2)) temp_modrm |= 0x00;
                        if(!gstricmp("bx", parm2) || !gstricmp("bl", parm2) || !gstricmp("ebx", parm2)) temp_modrm |= 0x18;
                        if(!gstricmp("cx", parm2) || !gstricmp("cl", parm2) || !gstricmp("ecx", parm2)) temp_modrm |= 0x08;
                        if(!gstricmp("dx", parm2) || !gstricmp("dl", parm2) || !gstricmp("edx", parm2)) temp_modrm |= 0x10;
                        if(!gstricmp("si", parm2) || !gstricmp("dh", parm2) || !gstricmp("esi", parm2)) temp_modrm |= 0x30;
                        if(!gstricmp("di", parm2) || !gstricmp("bh", parm2) || !gstricmp("edi", parm2)) temp_modrm |= 0x38;
                        if(!gstricmp("bp", parm2) || !gstricmp("ch", parm2) || !gstricmp("ebp", parm2)) temp_modrm |= 0x28;
                        if(!gstricmp("sp", parm2) || !gstricmp("ah", parm2) || !gstricmp("esp", parm2)) temp_modrm |= 0x20;
		}
		if(parm1_evaluated == EVALUATE_SREG)
			temp_modrm += SegmentRegister(parm1);
		if(parm2_evaluated == EVALUATE_SREG)
			temp_modrm += SegmentRegister(parm2);
		if(parm1_evaluated == EVALUATE_CREG)
			temp_modrm += ControlRegister(parm1);
		if(parm2_evaluated == EVALUATE_CREG)
			temp_modrm += ControlRegister(parm2);
		if(parm1_evaluated == EVALUATE_DREG)
			temp_modrm += DebugRegister(parm1);
		if(parm2_evaluated == EVALUATE_DREG)
			temp_modrm += DebugRegister(parm2);

                // Add the initial modrm byte to the calculated one
		    temp_modrm += find_dyn_instruction.initial_modrm;
                instruction.i[temp_count] = temp_modrm;         // put it into the instruction
                temp_count++;           // Increase our instruction size count
		
                // Get any displacement that might exist (REGM only)
		if((modrm_eval & MODRM_FORMAT_DISP8) || (modrm_eval & MODRM_FORMAT_DISP16) || (modrm_eval & MODRM_FORMAT_DISP32)) GetDisplacement(parm1, temp_string);
                if(((modrm_eval & MODRM_FORMAT_DISP8) || (modrm_eval & MODRM_FORMAT_DISP16) || (modrm_eval & MODRM_FORMAT_DISP32)) && !gstricmp(temp_string, "")) GetDisplacement(parm2, temp_string);

             // Put them into the instruction
		if(modrm_eval & MODRM_FORMAT_DISP8)
		{
			temp_char = StringtoChar(temp_string);
			instruction.i[temp_count] = temp_char;
			temp_count++;
		}
		else if(modrm_eval & MODRM_FORMAT_DISP16)
		{
			temp_int = StringtoInt(temp_string);
			memcpy(&instruction.i[temp_count], &temp_int, 2);
			temp_count+=2;
		}
		else if(modrm_eval & MODRM_FORMAT_DISP32)
		{
			temp_long = StringtoLong(temp_string);
			memcpy(&instruction.i[temp_count], &temp_long, 4);
			temp_count+=4;
		}
	}
//*************************** MOD-RM BYTE CALCULATOR END *********************************
// Now calculate any parameters that are NOT relative to itself
if(!(find_dyn_instruction.format & FORMAT_REL8 || find_dyn_instruction.format & FORMAT_REL16 || find_dyn_instruction.format & FORMAT_REL32))
{
	switch(parm1_evaluated)
	{
		case EVALUATE_IMM8: 	temp_char = StringtoChar(parm1);
					instruction.i[temp_count] = temp_char;
					temp_count++;
					break;
			
		case EVALUATE_IMM16:	temp_int = StringtoInt(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;
	
		case EVALUATE_IMM32:	temp_long = StringtoLong(parm1);
					memcpy(&instruction.i[temp_count], &temp_long, 4);
					temp_count+=4;
					break;
		
		case EVALUATE_PTR16_PTR16:
					temp_int = (unsigned int)GetOffset(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					temp_int = (unsigned int) GetSegment(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;
		
		case EVALUATE_PTR16_PTR32:
					temp_long = GetOffset(parm1);
					memcpy(&instruction.i[temp_count], &temp_long, 4);
					temp_count+=4;
					temp_int = GetSegment(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;

		default: break;
	}
	switch(parm2_evaluated)
	{
		case EVALUATE_IMM8: 	temp_char = StringtoChar(parm2);
					instruction.i[temp_count] = temp_char;
					temp_count++;
					break;
		
		case EVALUATE_IMM16:	temp_int = StringtoInt(parm2);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;
	
		case EVALUATE_IMM32:	temp_long = StringtoLong(parm2);
					memcpy(&instruction.i[temp_count], &temp_long, 4);
					temp_count+=4;
					break;

		case EVALUATE_PTR16_PTR16:
					temp_int = (unsigned int)GetOffset(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					temp_int = (unsigned int) GetSegment(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;
		
		case EVALUATE_PTR16_PTR32:
					temp_long = GetOffset(parm1);
					memcpy(&instruction.i[temp_count], &temp_long, 4);
					temp_count+=4;
					temp_int = GetSegment(parm1);
					memcpy(&instruction.i[temp_count], &temp_int, 2);
					temp_count+=2;
					break;
		
		default: break;
	}
}
// Now add any RELATIVES
if(find_dyn_instruction.format & FORMAT_REL8 || find_dyn_instruction.format & FORMAT_REL16 || find_dyn_instruction.format & FORMAT_REL32)
{
	switch(parm1_evaluated)
	{
            	case EVALUATE_IMM8:
						temp_char = StringtoChar(parm1);
						temp_schar = (offset+temp_count+1) - temp_char;
						temp_schar = 0 - temp_schar;
						instruction.i[temp_count] = temp_schar;
						temp_count++;
						break;
		
		case EVALUATE_IMM16:		temp_int = StringtoInt(parm1);
						temp_sint = (offset+temp_count+2) - temp_int;	// relative to end of instruction...
						temp_sint = 0 - temp_sint;
						memcpy(&instruction.i[temp_count], &temp_sint, 2);
						temp_count += 2;
						break;
		
		case EVALUATE_IMM32:		temp_long = StringtoLong(parm1);
						temp_slong = (offset+temp_count+4) - temp_long;	// relative to end of instruction...
						temp_slong = 0 - temp_slong;
						memcpy(&instruction.i[temp_count], &temp_slong, 4);
						temp_count += 4;
						break;
	}
	switch(parm2_evaluated)
	{
            	case EVALUATE_IMM8: 		temp_char = StringtoChar(parm2);
						temp_schar = (offset+temp_count+1) - temp_char;
						temp_schar = 0 - temp_schar;
						instruction.i[temp_count] = temp_schar;
						temp_count++;
						break;
		
		case EVALUATE_IMM16:		temp_int = StringtoInt(parm2);
						temp_sint = (offset+temp_count+2) - temp_int;	// relative to end of instruction...
						temp_sint = 0 - temp_sint;
						memcpy(&instruction.i[temp_count], &temp_sint, 2);
						temp_count += 2;
						break;
		
		case EVALUATE_IMM32:		temp_long = StringtoLong(parm2);
						temp_slong = (offset+temp_count+4) - temp_long;	// relative to end of instruction...
						temp_slong = 0 - temp_slong;
						memcpy(&instruction.i[temp_count], &temp_slong, 4);
						temp_count += 4;
						break;
	}
}                 
	
        // Now just return the data
	instruction.length = temp_count;
	return instruction;
}

//**********************************************
// StringtoInt: Converts a string to an integer.
// Can be a label, equation, decimal, hex, etc..
//**********************************************
unsigned int StringtoInt(char *string)
{
	char *endp;	
	char temp_string[255];
	t_label label;
	int loop=0;

	while((string[0]==' ' || string[0]=='\t') && string[0]!=(char)NULL) if(string[0]==' ' || string[0]=='\t') string++;
	for(loop=(strlen(string)-1); loop!=0 && (string[loop]==' ' || string[loop]=='\t'); loop--) if(string[loop] == ' ' || string[loop] == '\t') string[loop]=(char)NULL;
        if(!gstricmp(string, "")) return 0;
        if(!gstricmp(string, "$")) return (int) offset;
	
	if(string[0]=='-')
	{
		strcpy(temp_string, string+1);
		return 0 - StringtoInt(temp_string);
	}

	if(Contains(string, '+') || Contains(string, '-') || Contains(string, '*') || Contains(string, '/'))
		return (unsigned int) SolveEquation(string);

      if(!gstrnicmp(string, "0x", 2))
	{
		return (unsigned int) strtol(string, &endp, 0);
	}
	if(string[strlen(string)-1]=='h' && isHex(string))
	{
		strcpy(temp_string, "0x");
		string[strlen(string)-1]=(char)(char)NULL;
		strcat(temp_string, string);
		string[strlen(string)]='h';	// NOTE: string length has decreased

		return (unsigned int) strtol(temp_string, &endp, 0);
	}
	if(string[0]==39 && string[2]==39)	// 'a', 'b', etc
	{
		return (unsigned int) string[1];
	}	
	if(isdigit(string[0]))
	{
		return atoi(string);
	}

	// Not normal number, most probably label..
        gstrlwr(string);
	label = Findlabel(string);
	if(label.active == TRUE) return (int) label.m_offset;
	if(pass_number > 1) 
		fatal_error("Undefined label/identifier"); 
	
	return 0x1;	// Return smallest possible value instead if not found... not 0 to avoid divide by 0
}

//**********************************************
// StringtoChar: Converts a string to an char.
// Can be a label, equation, decimal, hex, etc..
//**********************************************
unsigned char StringtoChar(char *string)
{
	char *endp;	
	char temp_string[255];
	t_label label;
	int loop=0;	

	while((string[0]==' ' || string[0]=='\t') && string[0]!=(char)NULL) if(string[0]==' ' || string[0]=='\t') string++;
	for(loop=(strlen(string)-1); loop!=0 && (string[loop]==' ' || string[loop]=='\t'); loop--) if(string[loop] == ' ' || string[loop] == '\t') string[loop]=(char)NULL;

        if(!gstricmp(string, "")) return 0;
        if(!gstricmp(string, "$")) return (char) offset;

	if(string[0]=='-')
	{
		strcpy(temp_string, string+1);
		return 0 - StringtoChar(temp_string);
	}

	if(Contains(string, '+') || Contains(string, '-') || Contains(string, '*') || Contains(string, '/'))
		return (unsigned char) SolveEquation(string);

        if(!gstrnicmp(string, "0x", 2))
	{
		return (unsigned char) strtol(string, &endp, 0);
	}
	if(string[strlen(string)-1]=='h' && isHex(string))
	{
		strcpy(temp_string, "0x");
		string[strlen(string)-1]=(char)(char)NULL;
		strcat(temp_string, string);
		string[strlen(string)]='h';	// NOTE: string length has decreased

		return (unsigned char) strtol(temp_string, &endp, 0);
	}
	if(string[0]==39 && string[2]==39)	// 'a', 'b', etc
	{
		return string[1];
	}
	if(isdigit(string[0]))
	{
		return (unsigned char) atoi(string);
	}

	// Not normal number, most probably label..
        gstrlwr(string);
	label = Findlabel(string);
	if(label.active == TRUE) return (char) label.m_offset;
	if(pass_number > 1) 
		fatal_error("Undefined label/identifier"); 


	return 0x1;	// Return smallest possible value instead if not found... not 0 to avoid divide by 0
}

//**********************************************
// StringtoLong: Converts a string to an long.
// Can be a label, equation, decimal, hex, etc..
//**********************************************
unsigned long StringtoLong(char *string)
{
	char *endp;	
	char temp_string[255];
	t_label label;
	int loop=0;

	while((string[0]==' ' || string[0]=='\t') && string[0]!=(char)NULL)  if(string[0]==' ' || string[0]=='\t') string++;
	for(loop=(strlen(string)-1); loop!=0 && (string[loop]==' ' || string[loop]=='\t'); loop--) if(string[loop] == ' ' || string[loop] == '\t') string[loop]=(char)NULL;

        if(!gstricmp(string, "")) return 0;
        if(!gstricmp(string, "$")) return offset;

	if(string[0]=='-')
	{
		strcpy(temp_string, string+1);
		return 0 - StringtoLong(temp_string);
	}

	if(Contains(string, '(') || Contains(string, ')') || Contains(string, '+') || Contains(string, '-') || Contains(string, '*') || Contains(string, '/'))
		return SolveEquation(string);

        if(!gstrnicmp(string, "0x", 2))
	{
		return strtol(string, &endp, 0);
	}
	if(string[strlen(string)-1]=='h' && isHex(string))
	{
		strcpy(temp_string, "0x");
		string[strlen(string)-1]=(char)(char)NULL;
		strcat(temp_string, string);
		string[strlen(string)]='h';		// NOTE: string length has decreased

		return strtol(temp_string, &endp, 0);
	}
	if(string[0]==39 && string[2]==39)	// 'a', 'b', etc
	{
		return (unsigned long) string[1];
	}
	if(isdigit(string[0]))
	{
		return atol(string);
	}

	// Not normal number, most probably label..
        gstrlwr(string);
	label = Findlabel(string);
	if(label.active == TRUE) return label.m_offset;
	if(pass_number > 1) 
		fatal_error("Undefined label/identifier");

	return 0x01;	// Return smallest possible value instead if not found... not 0 to avoid divide by 0
}

//**********************************************************
// Evaluator: Evaluates a parameter
// Finds out whether it is an immediate, REGM, pointer, etc.
//**********************************************************
unsigned int Evaluator(char *e_string)
{
        unsigned int loop=0, loop2=0, temp=0;
	t_label labell;
	char temp_string[255], *endp;
	unsigned long temp_long=0;
	signed long temp_slong = 0;
        char temp_bracket_string[255], eval1=0;

        // Just make sure that the string isn't (char)NULL, crashes otherwise :-/
	if(!e_string) return EVALUATE_NONE;
	for(loop=0; e_string[loop]!=(char)(char)NULL && (e_string[loop]==(char)' ' || e_string[loop]==(char)'\t'); loop++);
	if(e_string[loop]==(char)(char)NULL) return EVALUATE_ERROR;
	loop=0;

	while((e_string[0] == ' ' || e_string[0] == '\t') && e_string[0]!=(char)NULL) if(e_string[0]==' ' || e_string[0]=='\t') e_string++;
	for(loop=(strlen(e_string)-1); loop!=0 && (e_string[loop]==' ' || e_string[loop]=='\t'); loop--) if(e_string[loop] == ' ' || e_string[loop] == '\t') e_string[loop]=(char)NULL;

        if(!gstricmp(e_string, "ax") || !gstricmp(e_string, "bx") || !gstricmp(e_string, "cx") || !gstricmp(e_string, "dx") ||
                        !gstricmp(e_string, "sp") || !gstricmp(e_string, "bp") || !gstricmp(e_string, "si") || !gstricmp(e_string, "di"))
		return EVALUATE_REG16;
	
        if(!gstricmp(e_string, "al") || !gstricmp(e_string, "cl") || !gstricmp(e_string, "dl") || !gstricmp(e_string, "bl") ||
                        !gstricmp(e_string, "ah") || !gstricmp(e_string, "ch") || !gstricmp(e_string, "dh") || !gstricmp(e_string, "bh"))
		return EVALUATE_REG8;
        if(!gstricmp(e_string, "eax") || !gstricmp(e_string, "ecx") || !gstricmp(e_string, "edx") || !gstricmp(e_string, "ebx") ||
                        !gstricmp(e_string, "esp") || !gstricmp(e_string, "ebp") || !gstricmp(e_string, "esi") || !gstricmp(e_string, "edi"))
		return EVALUATE_REG32;
	if(isSegmentRegister(e_string)==TRUE) return EVALUATE_SREG;
	if(isControlRegister(e_string)==TRUE) return EVALUATE_CREG;
	if(isDebugRegister(e_string)==TRUE) return EVALUATE_DREG;

	
        if((!gstrnicmp(e_string, "0x", 2) || e_string[strlen(e_string)-1]=='h' || (e_string[0]==39 && e_string[2]==39)  || isdigit(e_string[0])) && (Contains(e_string, ':')==FALSE) && isRelativeInstruction(temp_p)==TRUE)
	{
		temp_long = StringtoLong(e_string);
		temp_slong = (offset+5) - temp_long;
		temp_slong = 0 - temp_slong;

		if(temp_slong>-128 && temp_slong<128) return EVALUATE_IMM8;	
		else if(temp_slong>-65536 && temp_slong<65536) return EVALUATE_IMM16;
		else if(temp_slong<-65536 || temp_slong>65536) return EVALUATE_IMM32;
	}
        if((e_string[0]=='{' || !gstrnicmp(e_string, "0x", 2) || e_string[strlen(e_string)-1]=='h' || (e_string[0]==39 && e_string[2]==39) || isdigit(e_string[0]) || strchr(e_string, '+') || strchr(e_string, '-') || strchr(e_string, '*') || strchr(e_string, '/')) && ((Contains(e_string, ':')==FALSE) && !strchr(e_string, ']') && !strchr(e_string, '[')))
	{
		/*temp_long = StringtoLong(e_string);
						
                if(temp_long <=0xFF) return EVALUATE_IMM8;
		if(temp_long >=0x100 && temp_long <=0xFFFF) return EVALUATE_IMM16;
		if(temp_long >=0x10000) return EVALUATE_IMM32;*/

		temp_slong = StringtoLong(e_string);
		
		if(temp_slong >=-127 && temp_slong <=127) return EVALUATE_IMM8;
		if(temp_slong >= -65535 && temp_slong <= 65535) return EVALUATE_IMM16;
		if(temp_slong < -65535 || temp_slong > 65535) return EVALUATE_IMM32;
                
                return EVALUATE_ERROR;
	}
	
      if(e_string[0]=='[')
	{
		for(loop=1;e_string[loop]!=']' && e_string[loop]!=(char)NULL;loop++)
		{
			for(loop2=0; e_string[loop]!='+' && e_string[loop]!='*' && e_string[loop]!='-' && e_string[loop]!=(char)NULL && e_string[loop]!=']'; loop2++)
			{
				temp_string[loop2] = e_string[loop];
				loop++;
			}
			temp_string[loop2] = (char)NULL;
					
			temp = Evaluator(temp_string);
			
			if(temp == EVALUATE_REG8) return EVALUATE_REGM8;
			if(temp == EVALUATE_REG16) return EVALUATE_REGM16;
			if(temp == EVALUATE_REG32) return EVALUATE_REGM32;
		}
		if(temp == EVALUATE_IMM8) return EVALUATE_REGM16;
		if(temp == EVALUATE_IMM16) return EVALUATE_REGM16;
		if(temp == EVALUATE_IMM32) return EVALUATE_REGM32;
	}
	
        if(Contains(e_string, ':')==TRUE)
	{
		// Segment offset something..
		int anotherloop = 0;	
		
		for(loop=0; e_string[loop]!=':' && e_string[loop]!=(char)(char)NULL; loop++);		
		loop++;
		
		for(anotherloop=0;e_string[loop]!=(char)(char)NULL; loop++)
		{
			temp_bracket_string[anotherloop] = e_string[loop];
			anotherloop++;
		}
		temp_bracket_string[anotherloop] = (char)(char)NULL;

		eval1 = 0;
		eval1 = Evaluator(temp_bracket_string);
		if((eval1==EVALUATE_REG8 || eval1==EVALUATE_IMM8) && e_string[0]=='[') 
			return EVALUATE_REGM8;
		if((eval1==EVALUATE_REG16 || eval1==EVALUATE_IMM16) && e_string[0]=='[')
			return EVALUATE_REGM16;
		if((eval1==EVALUATE_REG32 || eval1==EVALUATE_IMM32) && e_string[0]=='[')
			return EVALUATE_REGM32;
		if(eval1==EVALUATE_REG8 || eval1==EVALUATE_IMM8)
			return EVALUATE_PTR16_PTR16;
		if(eval1==EVALUATE_REG16 || eval1==EVALUATE_IMM16)
			return EVALUATE_PTR16_PTR16;
		if(eval1==EVALUATE_REG32 || eval1==EVALUATE_IMM32)
			return EVALUATE_PTR16_PTR32;
	
		return EVALUATE_ERROR;
	}
	
	if(labell.active!=FALSE)
	{
		if(isRelativeInstruction(temp_p)==FALSE)
		{
			if(labell.m_offset >= 0 && labell.m_offset <=255) return EVALUATE_IMM8;
			if(labell.m_offset >=256 && labell.m_offset <=65535) return EVALUATE_IMM16;
			if(labell.m_offset >=65536) return EVALUATE_IMM32;
		}
		else
		{
			temp_long = StringtoLong(e_string);
			temp_slong = (offset+4) - temp_long;
			temp_slong = 0 - temp_slong;

			if(temp_slong>-128 && temp_slong<128) return EVALUATE_IMM8;	
			else if(temp_slong>-65536 && temp_slong<65536) return EVALUATE_IMM16;
			else if(temp_slong<-65536 || temp_slong>65536) return EVALUATE_IMM32;
	

		}
	}
	if(CheckValidLabel(e_string)==TRUE)
	{
		temp_long = StringtoLong(e_string);
		
                if(temp_long <=0xFF) return EVALUATE_IMM8;
		if(temp_long >=0x100 && temp_long <=0xFFFF) return EVALUATE_IMM16;
		if(temp_long >=0x10000) return EVALUATE_IMM32;
	}
	
	return EVALUATE_ERROR;
}

//*************************
// Findlabel: Finds a label
// in the label array
//*************************
t_label Findlabel(char *label_name)
{
	int loop=0;	
	t_label label;

        for(loop=0; loop<MAX_LABELS; loop++)
                if(!gstricmp(label_name, labels[loop].l_name))
			return labels[loop];

	// Not found..
	label.active = FALSE;
	return label;
}

//******************************************
// Addlabel: Adds a label to the label array
// Overrites if previous entry was entered
// on a different pass
//******************************************
void Addlabel(char *label_name, long moffset)
{
	int loop=0;

	if(CheckValidLabel(label_name)==FALSE)
		fatal_error("Invalid label name");
	
        for(loop=0; gstricmp(label_name, labels[loop].l_name) && loop<MAX_LABELS; loop++);

        if(!gstricmp(label_name, labels[loop].l_name) && pass_number == labels[loop].pass_number) 
		fatal_error("Duplicate label name found");
        else if(!gstricmp(label_name, labels[loop].l_name)) { labels[loop].m_offset = moffset; labels[loop].pass_number = pass_number; return; }
	
        for(loop=0; labels[loop].active!=FALSE && loop<MAX_LABELS; loop++);

	strcpy(labels[loop].l_name, label_name);
	labels[loop].m_offset = moffset;
	labels[loop].pass_number = pass_number;
	labels[loop].active = TRUE;
}
		
//*********************************************
// FindDynamicInstruction: Finds an instruction
// with any parameters
//*********************************************
t_dynamic_instruction FindDynamicInstruction(char *ins_name, unsigned int parm1, unsigned int parm2)
{
	int loop=0;
	t_dynamic_instruction dyn_instr;

	for(loop=0; dynamic_lookup[loop].opcode_length!=0; loop++)
                if(!gstricmp(ins_name, dynamic_lookup[loop].instruction) && dynamic_lookup[loop].parm1 == parm1 && dynamic_lookup[loop].parm2 == parm2)
			return dynamic_lookup[loop];
			
	dyn_instr.opcode_length = 0;	// Set length to zero, to indicate nothing found ...
	return dyn_instr;
}

//********************************
// Eval2: Evaluates a REGM and
// returns the evaluated value for
// lookup in the REGM table
//********************************
unsigned long Eval2(char *e_string, char inbrackets, unsigned int evaluated)
{
	unsigned int temp_modrm=0, loop=0, loop2=0;	
	signed long temp_slong = 0;
	char temp_string[255];

	if(!e_string) return 0;

	if(e_string[0]=='[')
	{
		temp_modrm = MODRM_FORMAT_INBRACK;		

		for(loop=1;e_string[loop]!=']' && e_string[loop]!=(char)NULL;loop++)
		{
			for(loop2=0; e_string[loop]!='+' && e_string[loop]!='*' && e_string[loop]!='-' && e_string[loop]!=(char)NULL && e_string[loop]!=']'; loop2++)
			{
				temp_string[loop2] = e_string[loop];
				loop++;
			}
			temp_string[loop2] = (char)NULL;
					
                        if(gstricmp(temp_string, "")) temp_modrm |= Eval2(temp_string, 1, evaluated);
		}
	}
        if(!gstricmp("al", e_string)) return MODRM_FORMAT_AX;
      if(!gstricmp("bl", e_string)) return MODRM_FORMAT_BX;
      if(!gstricmp("cl", e_string)) return MODRM_FORMAT_CX;
      if(!gstricmp("dl", e_string)) return MODRM_FORMAT_DX;
      if(!gstricmp("dh", e_string)) return MODRM_FORMAT_SI;
      if(!gstricmp("bh", e_string)) return MODRM_FORMAT_DI;
      if(!gstricmp("ch", e_string)) return MODRM_FORMAT_BP;
      if(!gstricmp("ah", e_string)) return MODRM_FORMAT_SP;

      if(!gstricmp("ax", e_string)) return MODRM_FORMAT_AX;
      if(!gstricmp("bx", e_string)) return MODRM_FORMAT_BX;
      if(!gstricmp("cx", e_string)) return MODRM_FORMAT_CX;
      if(!gstricmp("dx", e_string)) return MODRM_FORMAT_DX;
      if(!gstricmp("si", e_string)) return MODRM_FORMAT_SI;
      if(!gstricmp("di", e_string)) return MODRM_FORMAT_DI;
      if(!gstricmp("bp", e_string)) return MODRM_FORMAT_BP;
      if(!gstricmp("sp", e_string)) return MODRM_FORMAT_SP;

        if(!gstricmp("eax", e_string)) return MODRM_FORMAT_EAX;
      if(!gstricmp("ebx", e_string)) return MODRM_FORMAT_EBX;
      if(!gstricmp("ecx", e_string)) return MODRM_FORMAT_ECX;
      if(!gstricmp("edx", e_string)) return MODRM_FORMAT_EDX;
      if(!gstricmp("esi", e_string)) return MODRM_FORMAT_ESI;
      if(!gstricmp("edi", e_string)) return MODRM_FORMAT_EDI;
      if(!gstricmp("ebp", e_string)) return MODRM_FORMAT_EBP;
      if(!gstricmp("esp", e_string)) return MODRM_FORMAT_ESP;

	if(inbrackets == 0)
	{
		GetDisplacement(e_string, temp_string);

		temp_slong = StringtoLong(temp_string);
		
		if(temp_slong != 0)
		{
			if(temp_slong >= -127 && temp_slong <= 127) temp_modrm |= MODRM_FORMAT_DISP8;
			else if(temp_slong >= -65535 && temp_slong <= 65535) temp_modrm |= MODRM_FORMAT_DISP16;
			else if(temp_slong < 65535 || temp_slong > 65535) temp_modrm |= MODRM_FORMAT_DISP32;
		}
	}

	return temp_modrm;
}

//**************************
// GetDisplacement: Gets any
// displacement in a REGM
//**************************
void GetDisplacement(char *d_string, char *displacement)
{
	unsigned int temp_eval=0, loop=0, loop2=0, w_operator=0;
	char temp_string[255];

        temp_string[0] = '\0';
        displacement[0] = '\0';

	if(!d_string) return;

	if(d_string[0]=='[')
	{
		for(loop=1; d_string[loop]!=']' && d_string[loop]!=(char)NULL && d_string[loop]!='+' && d_string[loop]!='-' && d_string[loop]!='*'; loop++)
		{
			temp_string[loop2] = d_string[loop];
			loop2++;
		}
		temp_string[loop2] = (char)NULL;
		w_operator = '+';

		temp_eval = Evaluator(temp_string);
		if(temp_eval==EVALUATE_IMM8 || temp_eval == EVALUATE_IMM16 || temp_eval == EVALUATE_IMM32)
			strcpy(displacement, temp_string);

		w_operator = d_string[loop];
		loop++;
		for(;d_string[loop]!=']' && d_string[loop]!=(char)NULL; loop++)
		{
			for(loop2=0; d_string[loop]!=']' && d_string[loop]!=(char)NULL && d_string[loop]!='+' && d_string[loop]!='-' && d_string[loop]!='*'; loop2++)
			{
				temp_string[loop2] = d_string[loop];
				loop++;
			}
			temp_string[loop2] = (char)NULL;

			temp_eval = Evaluator(temp_string);		

			if(temp_eval == EVALUATE_IMM8 || temp_eval == EVALUATE_IMM16 || temp_eval == EVALUATE_IMM32)
			{
                                if(w_operator == '+' && gstricmp(displacement, "")) strcat(displacement, "+");
				if(w_operator == '-') strcat(displacement, "-");
				if(w_operator == '*') strcat(displacement, "*");

				strcat(displacement, temp_string);
			}
			w_operator = d_string[loop];
		}
	}
}

//*************************************
// FindModRM: Takes the evaluated MODRM
// and finds it in the lookup table
//*************************************
unsigned char FindModRM(unsigned long &modrm_evaluated)
{
	int loop=0;

	for(loop=0; modrm_lookup[loop].format != 0; loop++)
		if(modrm_lookup[loop].format == modrm_evaluated) return modrm_lookup[loop].modrm_byte;

	if(modrm_evaluated & MODRM_FORMAT_DISP8)
	{
		modrm_evaluated &= (modrm_evaluated - MODRM_FORMAT_DISP8);
		modrm_evaluated |= MODRM_FORMAT_DISP16;
	
		for(loop=0; modrm_lookup[loop].format != 0; loop++)
			if(modrm_lookup[loop].format == modrm_evaluated) return modrm_lookup[loop].modrm_byte;
	}

	if(modrm_evaluated & MODRM_FORMAT_DISP16)
	{
		modrm_evaluated &= (modrm_evaluated - MODRM_FORMAT_DISP16);
		modrm_evaluated |= MODRM_FORMAT_DISP32;
	
		for(loop=0; modrm_lookup[loop].format != 0; loop++)
			if(modrm_lookup[loop].format == modrm_evaluated) return modrm_lookup[loop].modrm_byte;
	}

	fatal_error("Invalid arguments in memory operand");
        return 0;
}

//***********************************************
// AutoModify: Automatically modifys
// parameters to be found in the lookup
// tables... eg. IMM8 can also be IMM16.
// Of course, this function does more than that..
//***********************************************
t_dynamic_instruction AutoModify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p)
{
	t_dynamic_instruction find_dyn_instruction;

	find_dyn_instruction.opcode_length = 0;

	unsigned int temp_parm1=0, temp_parm2=0;

	temp_parm1 = parm1_evaluated;
	temp_parm2 = parm2_evaluated;

	find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	if(find_dyn_instruction.opcode_length != 0) return find_dyn_instruction;

	find_dyn_instruction = Parm1_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	if(find_dyn_instruction.opcode_length != 0) return find_dyn_instruction;

	parm1_evaluated = temp_parm1;

	find_dyn_instruction = Parm2_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	if(find_dyn_instruction.opcode_length != 0) return find_dyn_instruction;

	parm2_evaluated = temp_parm2;

	find_dyn_instruction = Parm1_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	find_dyn_instruction = Parm2_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	if(find_dyn_instruction.opcode_length != 0) return find_dyn_instruction;

	parm1_evaluated = temp_parm1;
	parm2_evaluated = temp_parm2;

	find_dyn_instruction = Parm2_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	find_dyn_instruction = Parm1_Modify(parm1_evaluated, parm2_evaluated, parm1, parm2, temp_p);
	if(find_dyn_instruction.opcode_length != 0) return find_dyn_instruction;

	find_dyn_instruction.opcode_length = 0;
	return find_dyn_instruction;

}

// See AutoModify description
t_dynamic_instruction Parm1_Modify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p)
{
	t_dynamic_instruction find_dyn_instruction;	
	int parm2_evaluated_temp = parm2_evaluated;

	find_dyn_instruction.opcode_length = 0;

	if(parm1_evaluated!=EVALUATE_NONE)
      {     
                if(!gstricmp(parm1, "al")) { parm1_evaluated = EVALUATE_AL; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm1, "ax")) { parm1_evaluated = EVALUATE_AX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm1, "eax")) { parm1_evaluated = EVALUATE_EAX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm1, "dx")) { parm1_evaluated = EVALUATE_DX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm1, "cl")) { parm1_evaluated = EVALUATE_CL; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }

                if(!gstricmp(parm2, "dx")) { parm2_evaluated = EVALUATE_DX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
	}
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_AL) parm1_evaluated = EVALUATE_REG8;
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_AX) parm1_evaluated = EVALUATE_REG16;
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_EAX) parm1_evaluated = EVALUATE_REG32;
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_DX) parm1_evaluated = EVALUATE_REG16;
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_DX) parm2_evaluated = parm2_evaluated_temp;
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_CL) parm1_evaluated = EVALUATE_REG8;

	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_IMM16 && StringtoLong(parm1)<256)
	{
		parm1_evaluated = EVALUATE_IMM8;

		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		if(find_dyn_instruction.opcode_length == 0) parm1_evaluated = EVALUATE_IMM16;
	}
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_IMM32 && StringtoLong(parm1)<=65535)
	{
		parm1_evaluated = EVALUATE_IMM16;

		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		if(find_dyn_instruction.opcode_length == 0) parm1_evaluated = EVALUATE_IMM32;
	}


	if(find_dyn_instruction.opcode_length == 0 && (parm1_evaluated == EVALUATE_IMM8 || parm1_evaluated == EVALUATE_IMM16))
	{
		parm1_evaluated = EVALUATE_IMM16;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		if(find_dyn_instruction.opcode_length == 0)
		{
			parm1_evaluated = EVALUATE_IMM32;
			find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		}
	}
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_REG8)
	{
		parm1_evaluated = EVALUATE_REGM8;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_REG16)
	{
		parm1_evaluated = EVALUATE_REGM16;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_REG32)
	{
		parm1_evaluated = EVALUATE_REGM32;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}
	if(find_dyn_instruction.opcode_length == 0 && (parm1_evaluated == EVALUATE_REG16 || parm1_evaluated == EVALUATE_REG32) && parm2_evaluated == EVALUATE_NONE)
	{
		switch(parm1_evaluated)
		{
			case EVALUATE_REG16: parm1_evaluated = EVALUATE_REGM16; break;
			case EVALUATE_REG32: parm1_evaluated = EVALUATE_REGM32; break;
		}
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}

	return find_dyn_instruction;
}

// See AutoModify description
t_dynamic_instruction Parm2_Modify(unsigned int &parm1_evaluated, unsigned int &parm2_evaluated, char *parm1, char *parm2, char *temp_p)
{
	t_dynamic_instruction find_dyn_instruction;	
	int parm1_evaluated_temp = parm1_evaluated;
	
	find_dyn_instruction.opcode_length = 0;

	if(parm2_evaluated!=EVALUATE_NONE)
     	{
                if(!gstricmp(parm2, "al")) { parm2_evaluated = EVALUATE_AL; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm2, "ax")) { parm2_evaluated = EVALUATE_AX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm2, "eax")) { parm2_evaluated = EVALUATE_EAX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm2, "dx")) { parm2_evaluated = EVALUATE_DX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
                else if(!gstricmp(parm2, "cl")) { parm2_evaluated = EVALUATE_CL; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }

                if(!gstricmp(parm1, "dx")) { parm1_evaluated = EVALUATE_DX; find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated); }
	}
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_AL) parm2_evaluated = EVALUATE_REG8;
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_AX) parm2_evaluated = EVALUATE_REG16;
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_EAX) parm2_evaluated = EVALUATE_REG32;
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_DX) parm2_evaluated = EVALUATE_REG16;
	if(find_dyn_instruction.opcode_length == 0 && parm1_evaluated == EVALUATE_DX) parm1_evaluated = parm1_evaluated_temp;
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_CL) parm2_evaluated = EVALUATE_REG8;

	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_IMM16 && StringtoLong(parm2)<256)
	{
		parm2_evaluated = EVALUATE_IMM8;

		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		if(find_dyn_instruction.opcode_length == 0) parm2_evaluated = EVALUATE_IMM16;
	}
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_IMM32 && StringtoLong(parm2)<=65535)
	{
		parm2_evaluated = EVALUATE_IMM16;

		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		if(find_dyn_instruction.opcode_length == 0) parm2_evaluated = EVALUATE_IMM32;
	}

	if(find_dyn_instruction.opcode_length == 0 && (parm2_evaluated == EVALUATE_IMM8 || parm2_evaluated == EVALUATE_IMM16))
	{
		parm2_evaluated = EVALUATE_IMM16;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);

		if(find_dyn_instruction.opcode_length == 0)
		{
			parm2_evaluated = EVALUATE_IMM32;
			find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
		}
	}
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_REG8)
	{
		parm2_evaluated = EVALUATE_REGM8;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_REG16)
	{
		parm2_evaluated = EVALUATE_REGM16;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}
	if(find_dyn_instruction.opcode_length == 0 && parm2_evaluated == EVALUATE_REG32)
	{
		parm2_evaluated = EVALUATE_REGM32;
		find_dyn_instruction = FindDynamicInstruction(temp_p, parm1_evaluated, parm2_evaluated);
	}

	
	return find_dyn_instruction;
}

//************************************
// GetRelative:
// Gets the relative address in a REGM
//************************************
signed long GetRelative(char *address, unsigned long instructionend)
{
	unsigned long temp_long=0;
	signed long temp_slong=0;	

	temp_long = StringtoLong(address);
	temp_slong = (instructionend) - temp_long;	// relative to end of instruction...
	temp_slong = 0 - temp_slong;
	
	return temp_slong;
}

//****************************
// CheckValidLabel:
// Does the string comply with
// label rules?
//****************************
unsigned char CheckValidLabel(char *label_name)
{
	int loop=0;
	
	for(loop=0; loop<strlen(label_name); loop++)
		if(isalnum(label_name[loop])==0 && label_name[loop]!='_' && label_name[loop]!='.') { return FALSE; }

        if(!gstrnicmp(label_name, "0x", 2)) return FALSE;        // Cannot start "0x" !

	return TRUE;
}

//**********************************
// GetSegment: Gets the segment
// in a 0x03:0x04, where the segment
// is 0x03 in this case
//**********************************
unsigned int GetSegment(char *s_string)
{
	int loop=0;
	char temp_string[255];

	for(loop=0; s_string[loop]!=(char)':' && s_string[loop]!=(char)(char)NULL; loop++)
		temp_string[loop] = s_string[loop];

	temp_string[loop] = (char)(char)NULL;

	return StringtoInt(temp_string);
}

//*********************************
// GetOffset: Gets the offset
// in a 0x03:0x04, where the offset
// is 0x04 in this case
//*********************************
unsigned long GetOffset(char *s_string)
{
	int loop=0, loop2=0;
	char temp_string[255];

	for(loop=0; s_string[loop]!=(char)':' && s_string[loop]!=(char)(char)NULL; loop++);	

	loop++;

	for(; s_string[loop]!=(char)(char)NULL; loop++)
	{
		temp_string[loop2] = s_string[loop];
		loop2++;
	}

	temp_string[loop2] = (char)(char)NULL;

	return StringtoLong(temp_string);
}

//***********************************
// Contains: Does c_string contain c?
//***********************************
char Contains(char *c_string, char c)
{
	int loop = 0;

	for(loop=0; loop<strlen(c_string); loop++)
		if(c_string[loop]==c) return TRUE;

	return FALSE;
}

//***********************************
// FindFixedInstruction: Finds an
// instruction without any parameters
//***********************************
t_fixed_instruction FindFixedInstruction(char *instruction)
{
	int loop = 0;
	t_fixed_instruction fi_temp;

	for(loop=0; fixed_instruction_lookup[loop].length!=0; loop++)
                if(!gstricmp(instruction, fixed_instruction_lookup[loop].i_name)) return fixed_instruction_lookup[loop];

	fi_temp.length = 0;
	return fi_temp;
}	

//**********************************
// SolveEquation: Solves an equation
// eg. 3 + 4 = 7
//**********************************
unsigned long SolveEquation(char *equation)
{
	unsigned int loop=0, loop2=0;
	unsigned long result=0;
	char temp_string[255];

	while((equation[0]==' ' || equation[0]=='\t') && equation[0]!=(char)NULL) if(equation[0]==' ' || equation[0]=='\t') equation++;
        if(!gstricmp(equation, "")) return 0;

	if(equation[loop]!='(')
        {
                	for(loop2=0; equation[loop]!=(char)(char)NULL && equation[loop]!='+' && equation[loop]!='-' && equation[loop]!='*' && equation[loop]!='/'; loop++)
                	{       temp_string[loop2] = equation[loop]; loop2++;   }
	
                	temp_string[loop2] = (char)(char)NULL;
			result = StringtoLong(temp_string);
        }
        else
        {
                	strcpy(temp_string, equation+loop);
                	ExtractBrackets(temp_string);
			loop+= (strlen(temp_string) + 2);
		    	result = SolveEquation(temp_string);
        }
	
	while(loop<strlen(equation))
	{
		if(equation[loop]=='+')
		{
			loop++;
			while((equation[loop]==' ' || equation[loop]=='\t') && equation[loop]!=(char)NULL) if(equation[loop]==' ' || equation[loop]=='\t') loop++;
			
			if(equation[loop]!='(') {
				for(loop2=0; equation[loop]!=(char)(char)NULL && equation[loop]!='+' && equation[loop]!='-' && equation[loop]!='*' && equation[loop]!='/'; loop++)
				{	temp_string[loop2] = equation[loop]; loop2++;	}
	
				temp_string[loop2] = (char)(char)NULL;
				result += StringtoLong(temp_string);
			}
			else
			{
				strcpy(temp_string, equation+loop);
				ExtractBrackets(temp_string);
				loop+= (strlen(temp_string)+ 2);
			    	result += SolveEquation(temp_string);
			}
		}
		if(equation[loop]=='-')
		{
			loop++;
			while((equation[loop]==' ' || equation[loop]=='\t') && equation[loop]!=(char)NULL) if(equation[loop]==' ' || equation[loop]=='\t') loop++;

			if(equation[loop]!='(') {
				for(loop2=0; equation[loop]!=(char)(char)NULL && equation[loop]!='+' && equation[loop]!='-' && equation[loop]!='*' && equation[loop]!='/'; loop++)
				{	temp_string[loop2] = equation[loop]; loop2++;	}
	
				temp_string[loop2] = (char)(char)NULL;
				result -= StringtoLong(temp_string);
			}
			else
			{
				strcpy(temp_string, equation+loop);
				ExtractBrackets(temp_string);
				loop+= (strlen(temp_string)+2);
			    	result -= SolveEquation(temp_string);
			}
		}
		if(equation[loop]=='*')
		{
			loop++;
			while((equation[loop]==' ' || equation[loop]=='\t') && equation[loop]!=(char)NULL) if(equation[loop]==' ' || equation[loop]=='\t') loop++;

			if(equation[loop]!='(') {
				for(loop2=0; equation[loop]!=(char)(char)NULL && equation[loop]!='+' && equation[loop]!='-' && equation[loop]!='*' && equation[loop]!='/'; loop++)
				{	temp_string[loop2] = equation[loop]; loop2++;	}
	
				temp_string[loop2] = (char)(char)NULL;
				result *= StringtoLong(temp_string);
			}
			else
			{
				strcpy(temp_string, equation+loop);
				ExtractBrackets(temp_string);
				loop+= (strlen(temp_string)+2);
			    	result *= SolveEquation(temp_string);
			}
		}
		if(equation[loop]=='/')
		{
			loop++;
			while((equation[loop]==' ' || equation[loop]=='\t') && equation[loop]!=(char)NULL) if(equation[loop]==' ' || equation[loop]=='\t') loop++;

			if(equation[loop]!='(') {
				for(loop2=0; equation[loop]!=(char)(char)NULL && equation[loop]!='+' && equation[loop]!='-' && equation[loop]!='*' && equation[loop]!='/'; loop++)
				{	temp_string[loop2] = equation[loop]; loop2++;	}
	
				temp_string[loop2] = (char)(char)NULL;
				
				if(StringtoLong(temp_string)==0)
					fatal_error("Attempted divide by 0");
					
				result /= StringtoLong(temp_string);
			}
			else
			{
				strcpy(temp_string, equation+loop);
				ExtractBrackets(temp_string);
				loop += (strlen(temp_string)+2);
				if(SolveEquation(temp_string)==0)
					fatal_error("Attempted divide by 0");
				
			    	result /= SolveEquation(temp_string);
			}
		}
		if(equation[loop]==' ' || equation[loop]=='\t') loop++;
		if(equation[loop] == (char)NULL) break;
	}

	return result;
}

// If there are brackets then this will extract the contents until the finish
// Parse string in string, string is returned in string aswell

void ExtractBrackets(char *string)
{
	unsigned int loop=0, loop2 = 0, brackets = 0;
	char temp_string[255];
	if(string[0]!='(') return;

        temp_string[0] = '\0';

	while((string[0]==' ' || string[0]=='\t') && string[0]!=(char)NULL)  if(string[0]==' ' || string[0]=='\t') string++;
        if(!gstricmp(string, "")) return;

	loop++; // Get past first bracket..
	brackets = 1; // one open bracket

	for(; (!(string[loop]==')' && brackets == 0)) && string[loop]!=(char)NULL; loop++)
	{
		if(brackets!=0) 
		{
			temp_string[loop2] = string[loop];
			if(string[loop]=='(') brackets++;
			if(string[loop]==')') brackets--;
			loop2++;
		} 
	}
	loop2--;	// Clear ending bracket
	temp_string[loop2] = (char)NULL;

	strcpy(string, temp_string);
}

//**************************************
// isInstruction: Is the string an
// instruction (either fixed or dynamic)
//**************************************
char isInstruction(char *i_string)
{
	int loop=0;

	for(loop=0; fixed_instruction_lookup[loop].length!=0; loop++)
                if(!gstricmp(i_string, fixed_instruction_lookup[loop].i_name)) return TRUE;

	for(loop=0; dynamic_lookup[loop].opcode_length!=0; loop++)
                if(!gstricmp(i_string, dynamic_lookup[loop].instruction)) return TRUE;

	return FALSE;
}

char isSegmentRegister(char *seg_reg)
{
        if(!gstricmp(seg_reg, "es") || !gstricmp(seg_reg, "cs") || !gstricmp(seg_reg, "ss") || !gstricmp(seg_reg, "ds") || 
           !gstricmp(seg_reg, "fs") || !gstricmp(seg_reg, "gs")) return TRUE;

	return FALSE;
}

char SegmentRegister(char *seg_reg)
{
        if(!gstricmp(seg_reg, "es")) return 0x0;
        if(!gstricmp(seg_reg, "cs")) return 0x8;
        if(!gstricmp(seg_reg, "ss")) return 0x10;
        if(!gstricmp(seg_reg, "ds")) return 0x18;
        if(!gstricmp(seg_reg, "fs")) return 0x20;
        if(!gstricmp(seg_reg, "gs")) return 0x28;

	fatal_error("Invalid segment register, internal error\n");
        return 0; // Keep Borland happy
}

char isControlRegister(char *control_reg)
{
        if(!gstricmp(control_reg, "cr0") || !gstricmp(control_reg, "cr2") || !gstricmp(control_reg, "cr3") || !gstricmp(control_reg, "cr4")) return TRUE;

	return FALSE;
}

char ControlRegister(char *control_reg)
{
        if(!gstricmp(control_reg, "cr0")) return 0x0;
        if(!gstricmp(control_reg, "cr2")) return 0x10;
        if(!gstricmp(control_reg, "cr3")) return 0x18;
        if(!gstricmp(control_reg, "cr4")) return 0x20;
	
	fatal_error("Invalid control register, internal error");
        return 0;
}

char isDebugRegister(char *debug_reg)
{
        if(!gstricmp(debug_reg, "dr0") || !gstricmp(debug_reg, "dr1") || !gstricmp(debug_reg, "dr2") || !gstricmp(debug_reg, "dr3") || 
           !gstricmp(debug_reg, "dr4") || !gstricmp(debug_reg, "dr5") || !gstricmp(debug_reg, "dr6") || !gstricmp(debug_reg, "dr7")) return TRUE;

	return FALSE;
}

char DebugRegister(char *debug_reg)
{
        if(!gstricmp(debug_reg, "dr0")) return 0x0;
        if(!gstricmp(debug_reg, "dr1")) return 0x8;
        if(!gstricmp(debug_reg, "dr2")) return 0x10;
        if(!gstricmp(debug_reg, "dr3")) return 0x18;
        if(!gstricmp(debug_reg, "dr4")) return 0x20;
        if(!gstricmp(debug_reg, "dr5")) return 0x28;
        if(!gstricmp(debug_reg, "dr6")) return 0x30;
        if(!gstricmp(debug_reg, "dr7")) return 0x38;

	fatal_error("Invalid debug register, internal error");
        return 0;
}

t_instruction DataType(char *line)
{
	char *dtype, *alldata, data[255], firsttime = 0;
	t_instruction instruction;
	unsigned int count = 0, loop=0, dataloop=0, temp_int=0;
	unsigned long temp_long=0;

        instruction.length = 0;
        if(!line) return instruction;
	instruction.i = (unsigned char *)malloc(500);

	dtype = strtok(line, " \t");
	alldata = strtok(NULL, "\n");

        if(!dtype || !alldata) return instruction;

        gstrlwr(dtype);

        for(dataloop=0; (alldata[dataloop]==' ' || alldata[dataloop]=='\t') && alldata[dataloop]!='\0'; dataloop++);
	loop=0;
	data[0] = (char)NULL;
	firsttime = 1;

	while(data[0]!=(char)NULL || firsttime==1)
	{
                if(!gstricmp(dtype, "db"))
		{
			if(data[0]==39)
			{
				for(loop=1; data[loop]!=39 && data[loop]!=(char)NULL; loop++)
				{
					instruction.i[count] = data[loop];
					count++;
				}
			}
			else if(data[0] == 34)
			{
				for(loop=1; data[loop]!=34 && data[loop]!=(char)NULL; loop++)
				{
					instruction.i[count] = data[loop];
					count++;
				}
			}
			else if(data[0]!=(char)NULL)
			{
				instruction.i[count] = StringtoChar(data);
				count++;
			}
		}
                if(!gstricmp(dtype, "dw") && data[0]!=(char)NULL)
		{
			temp_int = StringtoInt(data);
			memcpy(&instruction.i[count], &temp_int, 2);
			count += 2;
		}
                if(!gstricmp(dtype, "dd") && data[0]!=(char)NULL)
		{
			temp_long = StringtoLong(data);
			memcpy(&instruction.i[count], &temp_long, 4);
			count+=4;
		}
		loop=0; firsttime = 0;
		if(alldata[dataloop]==39)
		{
			loop=1; data[0] = 39; dataloop++;
			for(;alldata[dataloop]!=(char)NULL && alldata[dataloop]!=39; dataloop++)
			{
				data[loop] = alldata[dataloop];
				loop++;
			}
			data[loop] = 39;
			loop++; dataloop++;
			data[loop] = (char)NULL;
			loop++;
		}
		else if(alldata[dataloop]==34)
		{
			loop=1; data[0] = 34; dataloop++;
			for(;alldata[dataloop]!=(char)NULL && alldata[dataloop]!=34; dataloop++)
			{
				data[loop] = alldata[dataloop];
				loop++;
			}
			data[loop]=34;
			loop++; dataloop++;
			data[loop]=(char)NULL;
			loop++;
		}
		else
		{
			for(;alldata[dataloop]!=(char)NULL && alldata[dataloop]!=','; dataloop++)
			{
				data[loop] = alldata[dataloop];
				loop++;
			}
			data[loop] = (char)NULL;
			loop++;
		}
		for(; alldata[dataloop]!=',' && alldata[dataloop]!=(char)NULL; dataloop++);
		if(alldata[dataloop]==',') dataloop++;
                for(; (alldata[dataloop]==' ' || alldata[dataloop]=='\t') && alldata[dataloop]!=(char)(char)NULL; dataloop++);
	}
	instruction.length = count;

	return instruction;
}

void Command(char *line)
{
	char *command, *parameters;

	remstring(line, "["); remstring(line, "]");

	command = strtok(line, " \n");
	parameters = strtok((char)NULL, "\n");

        if(!gstricmp(command, "ORG")) offset+=StringtoLong(parameters);
        if(!gstricmp(command, "BITS"))
	{
		if(StringtoLong(parameters) == 16) BITS = 16;
		else if(StringtoLong(parameters) == 32) BITS = 32;
		else fatal_error("Invalid BITS operand, valid operands are 16 and 32 only");
		
	}
}

void Preprocessor(char *line)
{
	char *command, *parameters;
	char filename_backup[255];
	unsigned line_number_backup=0;

	command = strtok(line, " \n");
	parameters = strtok((char)NULL, "\n");

        if(!gstricmp(command, "%include"))
	{
		remstring(parameters, "\"");
		remstring(parameters, "\"");

		line_number_backup = line_number;
		line_number = 0;
		strcpy(filename_backup, filename);
		strcpy(filename, parameters);
	
		if(pass_number != 3) Parsefile(FALSE);
		else Parsefile(TRUE);

		line_number = line_number_backup;
		strcpy(filename, filename_backup);
	}
}

t_instruction popSeg(char *seg_reg)
{
	t_instruction instruction;

	instruction.length = 0;
	instruction.i = (unsigned char *)malloc(3);

        if(!gstricmp(seg_reg, "ds"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x1F;
		return instruction;
	}
        if(!gstricmp(seg_reg, "es"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x07;
		return instruction;
	}
        if(!gstricmp(seg_reg, "ss"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x17;
		return instruction;
	}
        if(!gstricmp(seg_reg, "fs"))
	{
                instruction.length = 2;
		instruction.i[0] = 0x0F;
		instruction.i[1] = 0xA1;
		return instruction;
	}
        if(!gstricmp(seg_reg, "gs"))
	{
                instruction.length = 2;
		instruction.i[0] = 0x0F;
		instruction.i[1] = 0xA9;
		return instruction;
	}
	
	fatal_error("Invalid segment register, internal error");
        return instruction;
}
t_instruction pushSeg(char *seg_reg)
{
	t_instruction instruction;

	instruction.length = 0;
	instruction.i = (unsigned char *)malloc(3);

        if(!gstricmp(seg_reg, "cs"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x0E;
		return instruction;
	}
        if(!gstricmp(seg_reg, "ss"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x16;
		return instruction;
	}
        if(!gstricmp(seg_reg, "ds"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x1E;
		return instruction;
	}
        if(!gstricmp(seg_reg, "es"))
	{
                instruction.length = 1;
		instruction.i[0] = 0x06;
		return instruction;
	}
        if(!gstricmp(seg_reg, "fs"))
	{
                instruction.length = 2;
		instruction.i[0] = 0x0F;
		instruction.i[1] = 0xA0;
		return instruction;
	}
        if(!gstricmp(seg_reg, "gs"))
	{
                instruction.length = 2;
		instruction.i[0] = 0x0F;
		instruction.i[1] = 0xA8;
		return instruction;
	}
	
	fatal_error("Invalid segment register\n");
        return instruction;
}

char isRelativeInstruction(char *instruction)
{
	int loop = 0;

	for(loop=0; dynamic_lookup[loop].opcode_length!=0; loop++)
                if(!gstricmp(instruction, dynamic_lookup[loop].instruction) && ((dynamic_lookup[loop].format & FORMAT_REL8) || (dynamic_lookup[loop].format & FORMAT_REL16) || (dynamic_lookup[loop].format & FORMAT_REL32))) return TRUE;
	
	return FALSE;
}

void HandleEQU(char *line)
{
	char *label=0, *value=0, *equ_cmd=0;

	label = strtok(line, " \t\n");
	equ_cmd = strtok((char)NULL, " \t\n");
	value = strtok((char)NULL, "\n");

	if(!equ_cmd || !value || !label)
		fatal_error("Unable to split the line up into three parts, label, equ, value");

        if(gstricmp(equ_cmd, "equ") && gstricmp(equ_cmd, "EQU"))
		fatal_error("Internal error.. HandleEQU()\n");
	
        gstrlwr(label);

	Addlabel(label, StringtoLong(value));
}

unsigned char FindRegister(char *reg)
{
	int loop=0;

        for(loop=0; gstricmp(reg_lookup[loop].reg, ""); loop++)
                if(!gstricmp(reg_lookup[loop].reg, reg)) return reg_lookup[loop].value;

	fatal_error("Unable to find register, internal error");
        return 0;
}


// remsting, removes a substring from a string
// Thanks to intShredR for this one!
int remstring(char *str, char *rem)
{
        char *n, *occurence=strstr(str, rem);
                             
        int rlen=strlen(rem);
                               
        if (!occurence) return 0;
                                 
        for (n=occurence+rlen; *n!='\0'; n++) *(n-rlen)=*n;
        str[strlen(str)-rlen]='\0';
                                         
        return 1;
}

t_instruction Handletimes(char *line)
{
        char *timescommand, *number, *stuff;
	t_instruction instruction, temp_instruction;
	unsigned int loop=0, loop2=0;
	
	timescommand = strtok(line, " \t\n");
	number = strtok((char)NULL, " \t\n");
	stuff = strtok((char)NULL, "\n");

	temp_instruction.length = 0;
        if(!timescommand || !number || !stuff)
                fatal_error("3 parameters not supplied");

	temp_instruction = Parseline(stuff);

	instruction.length = 0;
      instruction.i = (unsigned char *)malloc((temp_instruction.length * StringtoLong(number)));

	if(!instruction.i) {
		printf("Unable to allocate %d bytes of memory\n", temp_instruction.length * StringtoLong(number));
		fatal_error("Out of memory");
	}

	if(temp_instruction.length != 0)
	{
            for(loop=0; loop<StringtoLong(number); loop++)
		{
			for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
			instruction.length += temp_instruction.length;
		}
	}

	return instruction;
}

void fatal_error(char *error_msg)
{
	printf("%s:%d: %s\n", filename, line_number, error_msg);
	exit(1);
}


t_instruction Handletss(char *line)
{
	char *label=0, *tss_cmd=0, tempstring[255];

	label = strtok(line, " \t\n");
	tss_cmd = strtok((char)NULL, "\n");

	if(!tss_cmd || !label)
		fatal_error("Unable to split the line up into two parts, label, tss");

        if(gstricmp(tss_cmd, "tss"))
		fatal_error("Internal error.. Handletss()\n");
	
        gstrlwr(label);
	
	Addlabel(label, offset);
	sprintf(tempstring, "%s.back", label);
	Addlabel(tempstring, offset);
	sprintf(tempstring, "%s.esp0", label);
	Addlabel(tempstring, offset+4);
	sprintf(tempstring, "%s.ss0", label);
	Addlabel(tempstring, offset+8);
	sprintf(tempstring, "%s.esp1", label);
	Addlabel(tempstring, offset+12);
	sprintf(tempstring, "%s.ss1", label);
	Addlabel(tempstring, offset+16);
	sprintf(tempstring, "%s.esp2", label);
	Addlabel(tempstring, offset+20);
	sprintf(tempstring, "%s.ss2", label);
	Addlabel(tempstring, offset+24);
	sprintf(tempstring, "%s.cr3", label);
	Addlabel(tempstring, offset+28);
	sprintf(tempstring, "%s.eip", label);
	Addlabel(tempstring, offset+32);
	sprintf(tempstring, "%s.eflags", label);
	Addlabel(tempstring, offset+36);
	sprintf(tempstring, "%s.eax", label);
	Addlabel(tempstring, offset+40);
	sprintf(tempstring, "%s.ecx", label);
	Addlabel(tempstring, offset+44);
	sprintf(tempstring, "%s.edx", label);
	Addlabel(tempstring, offset+48);
	sprintf(tempstring, "%s.ebx", label);
	Addlabel(tempstring, offset+52);
	sprintf(tempstring, "%s.esp", label);
	Addlabel(tempstring, offset+56);
	sprintf(tempstring, "%s.ebp", label);
	Addlabel(tempstring, offset+60);
	sprintf(tempstring, "%s.esi", label);
	Addlabel(tempstring, offset+64);
	sprintf(tempstring, "%s.edi", label);
	Addlabel(tempstring, offset+68);
	sprintf(tempstring, "%s.es", label);
	Addlabel(tempstring, offset+72);
	sprintf(tempstring, "%s.cs", label);
	Addlabel(tempstring, offset+76);
	sprintf(tempstring, "%s.ss", label);
	Addlabel(tempstring, offset+80);
	sprintf(tempstring, "%s.ds", label);
	Addlabel(tempstring, offset+84);
	sprintf(tempstring, "%s.fs", label);
	Addlabel(tempstring, offset+88);
	sprintf(tempstring, "%s.gs", label);
	Addlabel(tempstring, offset+92);
	sprintf(tempstring, "%s.ldt", label);
	Addlabel(tempstring, offset+96);
	sprintf(tempstring, "%s.trap", label);
	Addlabel(tempstring, offset+100);
	sprintf(tempstring, "%s.io", label);
	Addlabel(tempstring, offset+102);

	strcpy(tempstring, "times 104 db 0");

      return Parseline(tempstring);
}

int gstricmp(char *string1, char *string2)
{
	int loop=0;
	
	if(!string1 || !string2) return 1;

	while(*string1 && *string2)
	if(tolower(*(string1++)) != tolower(*(string2++))) return 1;

	if(tolower(*(string1++)) != tolower(*(string2++))) return 1;

	return 0;
}

int gstrnicmp(char *string1, char *string2, unsigned int n)
{
	int loop=0;

	if(!string1 || !string2) return 1;	

	for(loop=0; loop<n; loop++)
		if(tolower(*string1++) != tolower(*string2++)) return 1;
	return 0;
}

void gstrlwr(char *string)
{
	if(!string) return;
	while(string[0]) { string[0] = tolower(string[0]); string++;} 
}
 
t_instruction Handledesc(char *line)
{
      char *desccommand=0, *stuff=0, *dparm1=0, *dparm2=0, *dparm3=0, tempstring[255];
	unsigned long vdparm1=0, vdparm2=0, vdparm3=0;
	t_instruction instruction, temp_instruction;
	unsigned int loop=0, loop2=0;
	
	desccommand = strtok(line, " \t\n");
	stuff = strtok((char)NULL, "\n");

	dparm1 = strtok(stuff, ",");
	dparm2 = strtok(NULL, ",");	
	dparm3 = strtok(NULL, "\n");

	temp_instruction.length = 0;
        if(!desccommand || !stuff || !dparm1 || !dparm2 || !dparm3)
                fatal_error("Invalid format");

	
	vdparm1 = StringtoLong(dparm1);
	vdparm2 = StringtoLong(dparm2);
	vdparm3 = StringtoLong(dparm3);

	instruction.length = 0;
      instruction.i = (unsigned char *)malloc(10);

	if(!instruction.i)
		fatal_error("Out of memory");


	if(vdparm3 & ((~vdparm3)>>2) & 0x400)
	{
		sprintf(tempstring, "dw %d", vdparm1);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "dw %d", vdparm2);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "dw %d", vdparm3 + 0x8000);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "dw %d", vdparm1 >> 16);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;
	}
	else
	{
		sprintf(tempstring, "dw %d", vdparm2);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;
	
		sprintf(tempstring, "dw %d", vdparm1);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "db %d", vdparm1 >> 16);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "db %d", (vdparm3 + 0x8000) >> 8);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "db %d", vdparm3 + (vdparm2 >> 16));
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

		sprintf(tempstring, "db %d", vdparm1 >> 24);
		temp_instruction = Parseline(tempstring);
		for(loop2=instruction.length; loop2<(instruction.length+temp_instruction.length); loop2++) instruction.i[loop2] = temp_instruction.i[loop2-instruction.length];
		instruction.length += temp_instruction.length;

	}
	return instruction;
}

char isHex(char *string)
{
	char tempstring[255];
	unsigned int loop=0;

	if(!string) return FALSE;
	
	strcpy(tempstring, string);

	if(!gstrnicmp(tempstring, "0x", 2)) { remstring(tempstring, "0x"); remstring(tempstring, "0X"); }
	if(tempstring[strlen(tempstring)-1] == 'h') tempstring[strlen(tempstring)-1] = '\0';

	for(loop=0; tempstring[loop]!='\0'; loop++)
		if(tempstring[loop]>'F' || tempstring[loop]<'A' || tempstring[loop]>'f' || tempstring[loop]<'a' || tempstring[loop]>'9' || tempstring[loop]<'0') return FALSE;

	return TRUE;
}