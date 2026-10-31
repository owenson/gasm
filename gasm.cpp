// GASM Source Code, Gareth Owenson, gareth.owenson@gmail.com
// YOU MUST READ LICENSE.TXT BEFORE USE OF THE SOURCE CODE IS PERMITTED,
// IF YOU DO NOT HAVE LICENSE.TXT THEN EITHER GET A COPY OF
// DELETE THIS SOURCE CODE

/*
GASM Assembler
Copyright (C) 1999  Gareth Owenson

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
#include <stdarg.h>

#include "gasm.h"

typedef long long i64;

#define MAX_LINE		4096
#define MAX_PASSES		1000
#define MAX_INCLUDE_DEPTH	32
#define MAX_OPERANDS		3
#define MAX_OUTPUT		(256LL * 1024 * 1024)	// largest output image

//**************************************
// Global assembler state
//**************************************
t_label labels[MAX_LABELS];
static int label_count = 0;

static unsigned long line_number = 0;	// Current line number of current file
static i64 offset = 0;			// Current address
static i64 line_start = 0;		// Address at the start of the current line ($)
static i64 origin = 0;			// Value set by ORG ($$)
static int BITS = 16;			// Current default operand/address size
static int pass_number = 0;		// Current pass
static int final_pass = FALSE;		// TRUE on the pass that writes output
static int labels_changed = FALSE;	// Did any label move during this pass?
static int include_depth = 0;
static char filename[1024];		// Current source file (for error messages)
static char last_global[LABEL_NAME_MAX];	// Scope for NASM style .local labels

static unsigned char *outbuf = NULL;	// Output image (built on the final pass)
static size_t outlen = 0, outcap = 0;
static i64 emitted = 0;			// bytes generated in this pass

//**************************************
// Error handling
//**************************************
static void fatal_error(const char *fmt, ...)
{
	va_list ap;

	fprintf(stderr, "%s:%lu: error: ", filename, line_number);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(1);
}

//**************************************
// Small string helpers
//**************************************
static int gstricmp(const char *a, const char *b)
{
	if(!a || !b) return 1;
	while(*a && *b) if(tolower((unsigned char)*a++) != tolower((unsigned char)*b++)) return 1;
	return (*a || *b) ? 1 : 0;
}

static char *trim(char *s)
{
	char *e;

	while(*s == ' ' || *s == '\t') s++;
	e = s + strlen(s);
	while(e > s && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\r' || e[-1] == '\n')) *--e = '\0';
	return s;
}

static int is_ident_start(int c) { return isalpha(c) || c == '_' || c == '.' || c == '?' || c == '@'; }
static int is_ident_char(int c) { return isalnum(c) || c == '_' || c == '.' || c == '?' || c == '@' || c == '$' || c == '#' || c == '~'; }
static int is_quote(int c) { return c == '\'' || c == '"' || c == '`'; }

// Splits s in place at top level commas (not inside quotes, brackets or parentheses).
static int split_commas(char *s, char **parts, int max_parts)
{
	int n = 0, depth = 0;
	char quote = 0, *start = s;

	if(*trim(s) == '\0') return 0;
	for(; ; s++)
	{
		if(quote)
		{
			if(*s == quote) quote = 0;
			else if(*s == '\0') fatal_error("unterminated string");
			continue;
		}
		if(is_quote(*s)) { quote = *s; continue; }
		if(*s == '(' || *s == '[') depth++;
		if(*s == ')' || *s == ']') depth--;
		if((*s == ',' && depth == 0) || *s == '\0')
		{
			int end = (*s == '\0');
			if(n >= max_parts) fatal_error("too many operands");
			*s = '\0';
			parts[n++] = trim(start);
			if(parts[n-1][0] == '\0') fatal_error("empty operand");
			if(end) break;
			start = s + 1;
		}
	}
	return n;
}

// Removes a trailing comment, respecting quoted strings.
static void strip_comment(char *s)
{
	char quote = 0;

	for(; *s; s++)
	{
		if(quote) { if(*s == quote) quote = 0; continue; }
		if(is_quote(*s)) quote = *s;
		else if(*s == ';') { *s = '\0'; return; }
	}
}

// If s starts with keyword kw (as a whole word), returns pointer past it, else NULL.
static char *match_keyword(char *s, const char *kw)
{
	size_t n = strlen(kw);
	size_t i;

	for(i = 0; i < n; i++) if(tolower((unsigned char)s[i]) != kw[i]) return NULL;
	if(is_ident_char((unsigned char)s[n])) return NULL;
	return s + n;
}

//**************************************
// Registers
//**************************************
static const char *reg8_names[]  = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh" };
static const char *reg16_names[] = { "ax", "cx", "dx", "bx", "sp", "bp", "si", "di" };
static const char *reg32_names[] = { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi" };
static const char *sreg_names[]  = { "es", "cs", "ss", "ds", "fs", "gs" };
static const unsigned char sreg_prefix[] = { 0x26, 0x2E, 0x36, 0x3E, 0x64, 0x65 };

enum { RK_NONE, RK_GP, RK_SREG, RK_CREG, RK_DREG };

// Looks up a register name. Returns its kind and sets *num and *size.
static int lookup_register(const char *name, int *num, int *size)
{
	int i;

	for(i = 0; i < 8; i++)
	{
		if(!gstricmp(name, reg8_names[i]))  { *num = i; *size = 8;  return RK_GP; }
		if(!gstricmp(name, reg16_names[i])) { *num = i; *size = 16; return RK_GP; }
		if(!gstricmp(name, reg32_names[i])) { *num = i; *size = 32; return RK_GP; }
	}
	for(i = 0; i < 6; i++) if(!gstricmp(name, sreg_names[i])) { *num = i; *size = 16; return RK_SREG; }
	if(tolower((unsigned char)name[0]) == 'c' && tolower((unsigned char)name[1]) == 'r' && name[2] >= '0' && name[2] <= '4' && name[2] != '1' && name[3] == '\0')
		{ *num = name[2] - '0'; *size = 32; return RK_CREG; }
	if(tolower((unsigned char)name[0]) == 'd' && tolower((unsigned char)name[1]) == 'r' && name[2] >= '0' && name[2] <= '7' && name[3] == '\0')
		{ *num = name[2] - '0'; *size = 32; return RK_DREG; }
	return RK_NONE;
}

//**************************************
// Labels
//**************************************

// Expands NASM style local labels (.name) using the last global label.
static void qualify_label(const char *name, char *out)
{
	if(name[0] == '.' && name[1] != '.' && last_global[0])
	{
		if(strlen(last_global) + strlen(name) >= LABEL_NAME_MAX) fatal_error("label name too long");
		sprintf(out, "%s%s", last_global, name);
	}
	else
	{
		if(strlen(name) >= LABEL_NAME_MAX) fatal_error("label name too long");
		strcpy(out, name);
	}
}

static t_label *Findlabel(const char *label_name)
{
	char full[LABEL_NAME_MAX];
	int loop;

	qualify_label(label_name, full);
	for(loop = 0; loop < label_count; loop++)
		if(labels[loop].active && !gstricmp(full, labels[loop].l_name)) return &labels[loop];
	return NULL;
}

static int CheckValidLabel(const char *name)
{
	int num, size;
	const char *p;

	if(!is_ident_start((unsigned char)name[0])) return FALSE;
	for(p = name; *p; p++) if(!is_ident_char((unsigned char)*p)) return FALSE;
	if(lookup_register(name, &num, &size) != RK_NONE) return FALSE;
	return TRUE;
}

static void Addlabel(const char *label_name, i64 value)
{
	char full[LABEL_NAME_MAX];
	t_label *l;

	if(CheckValidLabel(label_name) == FALSE) fatal_error("invalid label name '%s'", label_name);
	qualify_label(label_name, full);
	if(label_name[0] != '.') strcpy(last_global, label_name);

	l = Findlabel(full);
	if(l)
	{
		if(l->pass_number == pass_number) fatal_error("duplicate label '%s'", full);
		if(l->m_offset != value) labels_changed = TRUE;
		l->m_offset = value;
		l->pass_number = pass_number;
		return;
	}
	if(label_count >= MAX_LABELS) fatal_error("too many labels");
	l = &labels[label_count++];
	strcpy(l->l_name, full);
	l->m_offset = value;
	l->active = TRUE;
	l->pass_number = pass_number;
	labels_changed = TRUE;
}

//**************************************
// Expression evaluator
// Precedence (lowest first): |  ^  &  << >>  + -  * / // % %%  unary - + ~ !
//**************************************
typedef struct
{
	const char *p;
	int unresolved;		// an undefined (forward) label was referenced
} t_expr;

static i64 expr_or(t_expr *e);

// Two's complement wrapping arithmetic (signed overflow is undefined behaviour in C++)
typedef unsigned long long u64;
static i64 wrap_neg(i64 a) { return (i64)(0 - (u64)a); }
static i64 wrap_add(i64 a, i64 b) { return (i64)((u64)a + (u64)b); }
static i64 wrap_sub(i64 a, i64 b) { return (i64)((u64)a - (u64)b); }
static i64 wrap_mul(i64 a, i64 b) { return (i64)((u64)a * (u64)b); }

static void skip_ws(t_expr *e) { while(*e->p == ' ' || *e->p == '\t') e->p++; }

static int digit_value(int c)
{
	if(c >= '0' && c <= '9') return c - '0';
	if(c >= 'a' && c <= 'f') return c - 'a' + 10;
	if(c >= 'A' && c <= 'F') return c - 'A' + 10;
	return 99;
}

static int parse_digits(const char *s, int len, int base, i64 *out)
{
	unsigned long long v = 0;
	int i, any = FALSE;

	for(i = 0; i < len; i++)
	{
		int d;
		if(s[i] == '_') continue;
		d = digit_value((unsigned char)s[i]);
		if(d >= base) return FALSE;
		v = v * base + d;
		any = TRUE;
	}
	*out = (i64)v;
	return any;
}

// Numbers: 123, 123d, 0x7F, 7Fh, 0b101, 101b, 17q, 17o
static i64 parse_number(t_expr *e)
{
	char tok[128];
	int len = 0, ok;
	char last;
	i64 v = 0;

	while(isalnum((unsigned char)*e->p) || *e->p == '_')
	{
		if(len >= (int)sizeof(tok) - 1) fatal_error("number too long");
		tok[len++] = *e->p++;
	}
	tok[len] = '\0';
	last = (char)tolower((unsigned char)tok[len-1]);

	if(len > 2 && tok[0] == '0' && (tok[1] == 'x' || tok[1] == 'X')) ok = parse_digits(tok + 2, len - 2, 16, &v);
	else if(last == 'h') ok = parse_digits(tok, len - 1, 16, &v);
	else if(len > 2 && tok[0] == '0' && (tok[1] == 'b' || tok[1] == 'B') && parse_digits(tok + 2, len - 2, 2, &v)) ok = TRUE;
	else if(len > 2 && tok[0] == '0' && (tok[1] == 'o' || tok[1] == 'O' || tok[1] == 'q' || tok[1] == 'Q')) ok = parse_digits(tok + 2, len - 2, 8, &v);
	else if(last == 'b' || last == 'y') ok = parse_digits(tok, len - 1, 2, &v);
	else if(last == 'q' || last == 'o') ok = parse_digits(tok, len - 1, 8, &v);
	else if(last == 'd' || last == 't') ok = parse_digits(tok, len - 1, 10, &v);
	else ok = parse_digits(tok, len, 10, &v);

	if(!ok) fatal_error("invalid number '%s'", tok);
	return v;
}

static i64 expr_primary(t_expr *e)
{
	i64 v = 0;

	skip_ws(e);
	if(*e->p == '(')
	{
		e->p++;
		v = expr_or(e);
		skip_ws(e);
		if(*e->p != ')') fatal_error("missing ')' in expression");
		e->p++;
		return v;
	}
	if(isdigit((unsigned char)*e->p)) return parse_number(e);
	if(is_quote(*e->p))
	{
		// character constant, little endian: 'ab' = 0x6261
		char q = *e->p++;
		int shift = 0;
		while(*e->p && *e->p != q)
		{
			if(shift >= 64) fatal_error("character constant too long");
			v |= (i64)(unsigned char)*e->p++ << shift;
			shift += 8;
		}
		if(*e->p != q) fatal_error("unterminated character constant");
		e->p++;
		return v;
	}
	if(e->p[0] == '$' && e->p[1] == '$') { e->p += 2; return origin; }
	if(e->p[0] == '$' && !is_ident_char((unsigned char)e->p[1])) { e->p++; return line_start; }
	if(is_ident_start((unsigned char)*e->p) || (*e->p == '$' && is_ident_start((unsigned char)e->p[1])))
	{
		char name[LABEL_NAME_MAX];
		int len = 0, num, size;
		t_label *l;

		if(*e->p == '$') e->p++;	// $name forces an identifier
		while(is_ident_char((unsigned char)*e->p))
		{
			if(len >= LABEL_NAME_MAX - 1) fatal_error("identifier too long");
			name[len++] = *e->p++;
		}
		name[len] = '\0';
		if(lookup_register(name, &num, &size) != RK_NONE) fatal_error("register '%s' not allowed in expression", name);
		l = Findlabel(name);
		if(l) return l->m_offset;
		// Every label has been seen once pass 1 is complete
		if(pass_number > 1) fatal_error("undefined label/identifier '%s'", name);
		e->unresolved = TRUE;
		return 0;
	}
	if(*e->p == '\0') fatal_error("expression expected");
	fatal_error("unexpected character '%c' in expression", *e->p);
	return 0;
}

static i64 expr_unary(t_expr *e)
{
	skip_ws(e);
	if(*e->p == '-') { e->p++; return wrap_neg(expr_unary(e)); }
	if(*e->p == '+') { e->p++; return expr_unary(e); }
	if(*e->p == '~') { e->p++; return ~expr_unary(e); }
	if(*e->p == '!') { e->p++; return !expr_unary(e); }
	return expr_primary(e);
}

static i64 expr_mul(t_expr *e)
{
	i64 v = expr_unary(e), r;

	for(;;)
	{
		skip_ws(e);
		if(*e->p == '*') { e->p++; v = wrap_mul(v, expr_unary(e)); }
		else if(*e->p == '/' || *e->p == '%')
		{
			char op = *e->p++;
			int is_signed = (*e->p == op);	// '//' and '%%' are signed
			if(is_signed) e->p++;
			r = expr_unary(e);
			if(r == 0)
			{
				if(e->unresolved) { v = 0; continue; }
				fatal_error("division by zero");
			}
			if(is_signed && r == -1) v = (op == '/') ? wrap_neg(v) : 0;	// avoid INT64_MIN / -1
			else if(is_signed) v = (op == '/') ? v / r : v % r;
			else v = (op == '/') ? (i64)((unsigned long long)v / (unsigned long long)r) : (i64)((unsigned long long)v % (unsigned long long)r);
		}
		else return v;
	}
}

static i64 expr_add(t_expr *e)
{
	i64 v = expr_mul(e);

	for(;;)
	{
		skip_ws(e);
		if(*e->p == '+') { e->p++; v = wrap_add(v, expr_mul(e)); }
		else if(*e->p == '-') { e->p++; v = wrap_sub(v, expr_mul(e)); }
		else return v;
	}
}

static i64 expr_shift(t_expr *e)
{
	i64 v = expr_add(e), r;

	for(;;)
	{
		skip_ws(e);
		if(e->p[0] == '<' && e->p[1] == '<') { e->p += 2; r = expr_add(e); v = (r >= 64 || r < 0) ? 0 : (i64)((unsigned long long)v << r); }
		else if(e->p[0] == '>' && e->p[1] == '>') { e->p += 2; r = expr_add(e); v = (r >= 64 || r < 0) ? 0 : (i64)((unsigned long long)v >> r); }
		else return v;
	}
}

static i64 expr_and(t_expr *e)
{
	i64 v = expr_shift(e);
	for(;;) { skip_ws(e); if(*e->p == '&') { e->p++; v &= expr_shift(e); } else return v; }
}

static i64 expr_xor(t_expr *e)
{
	i64 v = expr_and(e);
	for(;;) { skip_ws(e); if(*e->p == '^') { e->p++; v ^= expr_and(e); } else return v; }
}

static i64 expr_or(t_expr *e)
{
	i64 v = expr_xor(e);
	for(;;) { skip_ws(e); if(*e->p == '|') { e->p++; v |= expr_xor(e); } else return v; }
}

// Evaluates the longest expression prefix of s; *end receives the rest.
static i64 EvaluatePrefix(const char *s, const char **end, int *unresolved)
{
	t_expr e;
	i64 v;

	e.p = s;
	e.unresolved = FALSE;
	v = expr_or(&e);
	skip_ws(&e);
	*end = e.p;
	if(unresolved) *unresolved = e.unresolved;
	return v;
}

// Evaluates a complete expression. *unresolved (optional) is set if a forward reference was used.
static i64 Evaluate(const char *s, int *unresolved)
{
	const char *end;
	i64 v = EvaluatePrefix(s, &end, unresolved);

	if(*end != '\0') fatal_error("unexpected '%s' in expression", end);
	return v;
}

//**************************************
// Output
//**************************************
// Accounts for n more bytes of output, enforcing the size limit (on every pass).
static void count_output(i64 n)
{
	if(n < 0 || n > MAX_OUTPUT - emitted) fatal_error("output exceeds %lld bytes", MAX_OUTPUT);
	emitted += n;
}

static void reserve_output(size_t n)
{
	if(outlen + n > outcap)
	{
		size_t newcap = outcap ? outcap : 4096;
		while(newcap < outlen + n) newcap *= 2;
		outbuf = (unsigned char *)realloc(outbuf, newcap);
		if(!outbuf) fatal_error("out of memory");
		outcap = newcap;
	}
}

static void emit(const unsigned char *bytes, size_t n)
{
	count_output((i64)n);
	if(final_pass)
	{
		reserve_output(n);
		memcpy(outbuf + outlen, bytes, n);
		outlen += n;
	}
	offset += n;
}

static void emit_fill(unsigned char value, i64 count)
{
	if(count <= 0) return;
	count_output(count);
	if(final_pass)
	{
		reserve_output((size_t)count);
		memset(outbuf + outlen, value, (size_t)count);
		outlen += (size_t)count;
	}
	offset += count;
}

// Appends 'count' more copies of the 'size' bytes that start at output position 'start'.
static void emit_repeat(size_t start, i64 size, i64 count)
{
	i64 i;

	if(size <= 0 || count <= 0) return;
	if(count > MAX_OUTPUT / size) fatal_error("output exceeds %lld bytes", MAX_OUTPUT);
	count_output(size * count);
	if(final_pass)
	{
		reserve_output((size_t)(size * count));
		for(i = 0; i < count; i++)
		{
			memcpy(outbuf + outlen, outbuf + start, (size_t)size);
			outlen += (size_t)size;
		}
	}
	offset += size * count;
}

//**************************************
// Operands
//**************************************
enum { OP_NONE, OP_REG, OP_SREG, OP_CREG, OP_DREG, OP_MEM, OP_IMM, OP_FAR };

typedef struct
{
	int kind;
	int size;		// register size, or explicit size keyword (0 = unspecified)
	int reg;		// register number
	int strict;		// 0, 'S'hort, 'N'ear or 'F'ar
	// memory operands
	int base, index, scale;	// base/index are -1 when absent
	int asize;		// address size: 16 or 32
	int seg;		// segment override prefix byte, 0 if none
	// immediate value / memory displacement / far pointer offset
	i64 value;
	int unresolved;
	i64 farseg;		// far pointer segment
	char text[128];		// original operand text, for error messages
} t_operand;

// Reads a small integer constant (a scale factor) from s[0..len).
static int scale_value(const char *s, int len, const t_operand *op)
{
	char tmp[32];

	if(len <= 0 || len >= (int)sizeof(tmp)) fatal_error("invalid effective address '%s'", op->text);
	memcpy(tmp, s, len);
	tmp[len] = '\0';
	return (int)Evaluate(tmp, NULL);
}

// Parses the inside of [ ] into base/index/scale/displacement.
// Register terms must be added (reg, reg*n or n*reg); everything else is the displacement.
static void ParseMemory(char *inner, t_operand *op)
{
	char expr[MAX_LINE];
	int regs[2], scales[2], sizes[2], nregs = 0, num, size, i;
	char *p, *q = expr;
	char *colon;

	op->kind = OP_MEM;
	op->base = op->index = -1;
	op->scale = 1;

	// [seg:...] override
	inner = trim(inner);
	colon = strchr(inner, ':');
	if(colon)
	{
		char segname[8];
		int n = (int)(colon - inner);
		if(n > 4) fatal_error("invalid segment override in '%s'", op->text);
		memcpy(segname, inner, n);
		segname[n] = '\0';
		if(lookup_register(trim(segname), &num, &size) != RK_SREG) fatal_error("invalid segment override in '%s'", op->text);
		if(op->seg) fatal_error("multiple segment overrides");
		op->seg = sreg_prefix[num];
		inner = trim(colon + 1);
	}
	p = inner;

	// Copy the expression, replacing register terms with 0
	while(*p)
	{
		if(is_ident_start((unsigned char)*p) && (p == inner || !is_ident_char((unsigned char)p[-1])))
		{
			char name[LABEL_NAME_MAX];
			int len = 0, scale = 1;
			char *s = p, *b, *after;

			while(is_ident_char((unsigned char)*p) && len < LABEL_NAME_MAX - 1) name[len++] = *p++;
			name[len] = '\0';
			if(lookup_register(name, &num, &size) != RK_GP)
			{
				memcpy(q, s, p - s);	// not a register: copy the identifier
				q += p - s;
				continue;
			}
			if(size == 8) fatal_error("8-bit register not allowed in memory operand");
			if(nregs >= 2) fatal_error("too many registers in memory operand");

			// what precedes the register?
			b = s;
			while(b > inner && (b[-1] == ' ' || b[-1] == '\t')) b--;
			if(b > inner && b[-1] == '*')
			{
				// n*reg
				char *n_end = b - 1, *n_start;
				while(n_end > inner && (n_end[-1] == ' ' || n_end[-1] == '\t')) n_end--;
				n_start = n_end;
				while(n_start > inner && isalnum((unsigned char)n_start[-1])) n_start--;
				scale = scale_value(n_start, (int)(n_end - n_start), op);
				b = n_start;
				while(b > inner && (b[-1] == ' ' || b[-1] == '\t')) b--;
				q -= (s - n_start);	// drop "n*" from the copied expression
				if(q < expr) fatal_error("invalid effective address '%s'", op->text);
			}
			if(b != inner && b[-1] != '+') fatal_error("invalid effective address '%s'", op->text);

			// reg*n
			after = p;
			while(*after == ' ' || *after == '\t') after++;
			if(*after == '*')
			{
				char *n = after + 1, *ne;
				if(scale != 1) fatal_error("invalid effective address '%s'", op->text);
				while(*n == ' ' || *n == '\t') n++;
				ne = n;
				while(isalnum((unsigned char)*ne)) ne++;
				scale = scale_value(n, (int)(ne - n), op);
				p = after = ne;
				while(*after == ' ' || *after == '\t') after++;
			}
			if(*after != '\0' && *after != '+' && *after != '-') fatal_error("invalid effective address '%s'", op->text);
			if(scale != 1 && scale != 2 && scale != 4 && scale != 8) fatal_error("invalid scale factor %d", scale);

			regs[nregs] = num;
			scales[nregs] = scale;
			sizes[nregs] = size;
			nregs++;
			*q++ = '0';
			continue;
		}
		*q++ = *p++;
	}
	*q = '\0';

	if(*trim(expr) == '\0') fatal_error("empty memory operand");
	op->value = Evaluate(trim(expr), &op->unresolved);

	if(nregs == 2 && sizes[0] != sizes[1]) fatal_error("cannot mix 16 and 32-bit registers in memory operand");
	if(nregs > 0) op->asize = sizes[0];
	else
	{
		// absolute address: default address size, unless it cannot fit in 16 bits
		op->asize = BITS;
		if(BITS == 16 && !op->unresolved && (op->value > 0xFFFF || op->value < -0x8000)) op->asize = 32;
	}

	if(op->asize == 16)
	{
		int has_b = -1, has_i = -1;
		for(i = 0; i < nregs; i++)
		{
			if(scales[i] != 1) fatal_error("scaled index not allowed with 16-bit addressing");
			if(regs[i] == 3 || regs[i] == 5) { if(has_b != -1) fatal_error("invalid 16-bit effective address '%s'", op->text); has_b = regs[i]; }
			else if(regs[i] == 6 || regs[i] == 7) { if(has_i != -1) fatal_error("invalid 16-bit effective address '%s'", op->text); has_i = regs[i]; }
			else fatal_error("invalid 16-bit effective address '%s'", op->text);
		}
		op->base = has_b;
		op->index = has_i;
	}
	else
	{
		if(nregs == 1)
		{
			if(scales[0] == 1) op->base = regs[0];
			else if(scales[0] == 2 && regs[0] != 4) { op->base = op->index = regs[0]; }	// [r*2] -> [r+r] (no disp32 needed)
			else { op->index = regs[0]; op->scale = scales[0]; }
		}
		else if(nregs == 2)
		{
			// the scaled register is the index; esp can only be a base
			int bi = 0, ii = 1;
			if(scales[0] != 1 && scales[1] != 1) fatal_error("only one register may be scaled");
			if(scales[0] != 1) { bi = 1; ii = 0; }
			else if(scales[1] == 1 && regs[1] == 4) { bi = 1; ii = 0; }
			op->base = regs[bi];
			op->index = regs[ii];
			op->scale = scales[ii];
		}
		if(op->index == 4) fatal_error("esp cannot be used as an index register");
	}
}

static void ParseOperand(char *text, t_operand *op)
{
	char *p, *k;
	int num, size, kind;

	memset(op, 0, sizeof(*op));
	op->base = op->index = -1;
	op->scale = 1;
	snprintf(op->text, sizeof(op->text), "%s", trim(text));
	p = trim(text);

	// size / distance keywords
	for(;;)
	{
		if((k = match_keyword(p, "byte"))) { op->size = 8; p = trim(k); }
		else if((k = match_keyword(p, "word"))) { op->size = 16; p = trim(k); }
		else if((k = match_keyword(p, "dword"))) { op->size = 32; p = trim(k); }
		else if((k = match_keyword(p, "short"))) { op->strict = 'S'; p = trim(k); }
		else if((k = match_keyword(p, "near"))) { op->strict = 'N'; p = trim(k); }
		else if((k = match_keyword(p, "far"))) { op->strict = 'F'; p = trim(k); }
		else break;
	}
	if(*p == '\0') fatal_error("operand expected");

	// gasm style seg:[...] override
	if(strlen(p) > 3 && p[2] == ':')
	{
		char segname[3] = { p[0], p[1], '\0' };
		if(lookup_register(segname, &num, &size) == RK_SREG)
		{
			char *r = trim(p + 3);
			if(*r != '[') fatal_error("segment override must be followed by a memory operand");
			op->seg = sreg_prefix[num];
			p = r;
		}
	}

	if(*p == '[')
	{
		size_t n = strlen(p);
		if(p[n-1] != ']') fatal_error("missing ']' in '%s'", text);
		p[n-1] = '\0';
		ParseMemory(p + 1, op);
		return;
	}

	kind = lookup_register(p, &num, &size);
	if(kind != RK_NONE)
	{
		if(op->size && op->size != size) fatal_error("register size does not match size keyword");
		op->reg = num;
		op->size = size;
		op->kind = (kind == RK_GP) ? OP_REG : (kind == RK_SREG) ? OP_SREG : (kind == RK_CREG) ? OP_CREG : OP_DREG;
		return;
	}

	// seg:offset far pointer (top level colon outside quotes)
	{
		char quote = 0, *c, *colon = NULL;
		for(c = p; *c; c++)
		{
			if(quote) { if(*c == quote) quote = 0; continue; }
			if(is_quote(*c)) quote = *c;
			else if(*c == ':') { colon = c; break; }
		}
		if(colon)
		{
			int u1 = 0, u2 = 0;
			*colon = '\0';
			op->kind = OP_FAR;
			op->farseg = Evaluate(p, &u1);
			op->value = Evaluate(colon + 1, &u2);
			op->unresolved = u1 || u2;
			return;
		}
	}

	op->kind = OP_IMM;
	op->value = Evaluate(p, &op->unresolved);
}

//**************************************
// Instruction encoding
//**************************************
typedef struct
{
	unsigned char b[32];
	int len;
	int imm_bytes;		// bytes of immediate data (used to prefer sign-extended forms)
} t_code;

static void put(t_code *c, i64 v, int n)
{
	int i;
	for(i = 0; i < n; i++) c->b[c->len++] = (unsigned char)(v >> (8 * i));
}

static int fits_s8(i64 v) { return v >= -128 && v <= 127; }

// Encodes ModRM (+SIB +displacement) for operand op with the given reg field.
static void encode_modrm(t_code *c, const t_operand *op, int regfield)
{
	if(op->kind == OP_REG)
	{
		c->b[c->len++] = (unsigned char)(0xC0 | (regfield << 3) | op->reg);
		return;
	}

	if(op->asize == 16)
	{
		int rm, mod, dsize;
		int b = op->base, i = op->index;

		if(!op->unresolved && (op->value > 0xFFFF || op->value < -0x8000)) fatal_error("displacement out of range for 16-bit addressing");
		if(b == -1 && i == -1)
		{
			c->b[c->len++] = (unsigned char)((regfield << 3) | 6);
			put(c, op->value, 2);
			return;
		}
		if(b == 3 && i == 6) rm = 0;		// [bx+si]
		else if(b == 3 && i == 7) rm = 1;	// [bx+di]
		else if(b == 5 && i == 6) rm = 2;	// [bp+si]
		else if(b == 5 && i == 7) rm = 3;	// [bp+di]
		else if(b == -1 && i == 6) rm = 4;	// [si]
		else if(b == -1 && i == 7) rm = 5;	// [di]
		else if(b == 5 && i == -1) rm = 6;	// [bp] (needs a displacement)
		else rm = 7;				// [bx]

		if(!op->unresolved && op->value == 0 && rm != 6) { mod = 0; dsize = 0; }
		else if(!op->unresolved && fits_s8(op->value)) { mod = 1; dsize = 1; }
		else { mod = 2; dsize = 2; }
		c->b[c->len++] = (unsigned char)((mod << 6) | (regfield << 3) | rm);
		put(c, op->value, dsize);
		return;
	}

	// 32-bit addressing
	{
		int b = op->base, i = op->index, mod, dsize, ss;

		if(!op->unresolved && (op->value > 0xFFFFFFFFLL || op->value < -0x80000000LL)) fatal_error("displacement out of range");
		if(b == -1 && i == -1)
		{
			c->b[c->len++] = (unsigned char)((regfield << 3) | 5);
			put(c, op->value, 4);
			return;
		}
		ss = (op->scale == 8) ? 3 : (op->scale == 4) ? 2 : (op->scale == 2) ? 1 : 0;
		if(b == -1)
		{
			// index only: SIB with no base, always disp32
			c->b[c->len++] = (unsigned char)((regfield << 3) | 4);
			c->b[c->len++] = (unsigned char)((ss << 6) | (i << 3) | 5);
			put(c, op->value, 4);
			return;
		}
		if(!op->unresolved && op->value == 0 && b != 5) { mod = 0; dsize = 0; }	// [ebp] needs a displacement
		else if(!op->unresolved && fits_s8(op->value)) { mod = 1; dsize = 1; }
		else { mod = 2; dsize = 4; }

		if(i == -1 && b != 4)
			c->b[c->len++] = (unsigned char)((mod << 6) | (regfield << 3) | b);
		else
		{
			// SIB needed for an index, or for esp as base
			c->b[c->len++] = (unsigned char)((mod << 6) | (regfield << 3) | 4);
			c->b[c->len++] = (unsigned char)((ss << 6) | ((i == -1 ? 4 : i) << 3) | b);
		}
		put(c, op->value, dsize);
	}
}

static int param_is_imm(unsigned int t) { return t == EVALUATE_IMM8 || t == EVALUATE_IMM16 || t == EVALUATE_IMM32; }
static int param_is_regm(unsigned int t) { return t == EVALUATE_REGM8 || t == EVALUATE_REGM16 || t == EVALUATE_REGM32; }
static int param_is_reg(unsigned int t) { return t == EVALUATE_REG8 || t == EVALUATE_REG16 || t == EVALUATE_REG32; }

static int param_size(unsigned int t)
{
	switch(t)
	{
		case EVALUATE_REG8: case EVALUATE_REGM8: case EVALUATE_IMM8: case EVALUATE_AL: case EVALUATE_CL: return 8;
		case EVALUATE_REG16: case EVALUATE_REGM16: case EVALUATE_IMM16: case EVALUATE_AX: case EVALUATE_DX: return 16;
		case EVALUATE_REG32: case EVALUATE_REGM32: case EVALUATE_IMM32: case EVALUATE_EAX: return 32;
	}
	return 0;
}

static int in_list(const char *m, const char *const *list)
{
	for(; *list; list++) if(!gstricmp(m, *list)) return TRUE;
	return FALSE;
}

static const char *const no_opsize_list[] = { "lldt", "ltr", "verr", "verw", "lmsw", "arpl", NULL };
static const char *const imm16_fixed_list[] = { "ret", "retf", "enter", NULL };
static const char *const mem16_only_list[] = { "sldt", "smsw", "str", NULL };	// memory form is always 16-bit

// Operand size implied by a dynamic table entry: 0 (none), 16 or 32.
static int entry_opsize(const t_dynamic_instruction *d)
{
	unsigned int p[2];
	int i, size = 0;

	p[0] = d->parm1; p[1] = d->parm2;
	if(d->format & FORMAT_REL32) return 32;
	if(d->format & FORMAT_REL16) return 16;
	for(i = 0; i < 2; i++)
		if(p[i] == EVALUATE_SREG || p[i] == EVALUATE_CREG || p[i] == EVALUATE_DREG) return 0;
	if(in_list(d->instruction, no_opsize_list)) return 0;
	for(i = 0; i < 2; i++)
	{
		switch(p[i])
		{
			case EVALUATE_REG32: case EVALUATE_REGM32: case EVALUATE_EAX: case EVALUATE_PTR16_PTR32: case EVALUATE_IMM32:
				return 32;
			case EVALUATE_REG16: case EVALUATE_REGM16: case EVALUATE_AX: case EVALUATE_PTR16_PTR16:
				size = 16;
				break;
			case EVALUATE_IMM16:
				if(!in_list(d->instruction, imm16_fixed_list)) size = 16;
				break;
		}
	}
	return size;
}

// Is this IMM8 slot sign-extended to the operand size?
static int imm8_sign_extended(const t_dynamic_instruction *d)
{
	return d->opcode_length == 1 && (d->c1 == 0x83 || d->c1 == 0x6A || d->c1 == 0x6B);
}

// Does operand op fit parameter type t of entry d? *uncertain is set when it only fits
// because the value is not known yet.
static int match_param(const t_dynamic_instruction *d, unsigned int t, const t_operand *op, int *uncertain)
{
	int opsize = entry_opsize(d);
	int eff = opsize ? opsize : BITS;

	switch(t)
	{
		case EVALUATE_NONE: return op->kind == OP_NONE;
		case EVALUATE_REG8: case EVALUATE_REG16: case EVALUATE_REG32:
			return op->kind == OP_REG && op->size == param_size(t);
		case EVALUATE_AL: return op->kind == OP_REG && op->size == 8 && op->reg == 0;
		case EVALUATE_CL: return op->kind == OP_REG && op->size == 8 && op->reg == 1;
		case EVALUATE_AX: return op->kind == OP_REG && op->size == 16 && op->reg == 0;
		case EVALUATE_EAX: return op->kind == OP_REG && op->size == 32 && op->reg == 0;
		case EVALUATE_DX: return op->kind == OP_REG && op->size == 16 && op->reg == 2;
		case EVALUATE_SREG: return op->kind == OP_SREG;
		case EVALUATE_CREG: return op->kind == OP_CREG;
		case EVALUATE_DREG: return op->kind == OP_DREG;
		case EVALUATE_REGM8: case EVALUATE_REGM16: case EVALUATE_REGM32:
			if(op->kind == OP_REG) return op->size == param_size(t);
			if(op->kind == OP_MEM) return op->size == 0 || op->size == param_size(t);
			return FALSE;
		case EVALUATE_PTR16_PTR16: case EVALUATE_PTR16_PTR32:
			if(op->kind != OP_FAR) return FALSE;
			if(op->size && op->size != (t == EVALUATE_PTR16_PTR32 ? 32 : 16)) return FALSE;
			if(t == EVALUATE_PTR16_PTR16 && !op->unresolved && (op->value > 0xFFFF || op->value < 0)) return FALSE;
			return TRUE;
		case EVALUATE_IMM8: case EVALUATE_IMM16: case EVALUATE_IMM32:
		{
			i64 v = op->value;
			if(op->kind != OP_IMM) return FALSE;
			if(d->format & (FORMAT_REL8 | FORMAT_REL16 | FORMAT_REL32))
			{
				if(op->size && op->size != eff) return FALSE;
				if(op->strict == 'S' && !(d->format & FORMAT_REL8)) return FALSE;
				if(op->strict == 'N' && (d->format & FORMAT_REL8)) return FALSE;
				return TRUE;	// range is checked once the instruction length is known
			}
			if(op->strict) return FALSE;
			if(op->size)
			{
				if(op->size == 8 && t != EVALUATE_IMM8) return FALSE;
				if(op->size != 8 && eff != op->size) return FALSE;
			}
			if(t == EVALUATE_IMM8)
			{
				if(imm8_sign_extended(d))
				{
					i64 m = (eff == 32) ? 0xFFFFFFFFLL : 0xFFFF;
					if(op->unresolved) { *uncertain = TRUE; return TRUE; }
					if(v < -(m / 2 + 1) || v > m) return FALSE;
					return (v & m) == ((i64)(signed char)(v & 0xFF) & m);
				}
				if(op->unresolved) return TRUE;
				return v >= -128 && v <= 255;
			}
			if(op->unresolved) return TRUE;
			if(t == EVALUATE_IMM16) return v >= -32768 && v <= 65535;
			return v >= -2147483648LL && v <= 4294967295LL;
		}
	}
	return FALSE;
}

// Encodes entry d with operands o[0], o[1] located at address 'here'.
// Returns FALSE if a relative branch target is out of range for this form.
static int encode_entry(const t_dynamic_instruction *d, const t_operand *o, i64 here, t_code *c)
{
	unsigned int p[2];
	int opsize = entry_opsize(d);
	int i, rel_pos = -1, rel_size = 0, mem = -1;
	int short_shift = FALSE;
	unsigned char op1 = d->c1;

	p[0] = d->parm1; p[1] = d->parm2;
	c->len = 0;
	c->imm_bytes = 0;

	for(i = 0; i < 2; i++) if(o[i].kind == OP_MEM) mem = i;
	if(mem >= 0 && in_list(d->instruction, mem16_only_list)) opsize = 0;

	// prefixes
	if(mem >= 0 && o[mem].seg) c->b[c->len++] = (unsigned char)o[mem].seg;
	if(opsize && opsize != BITS) c->b[c->len++] = 0x66;
	else if(p[1] == EVALUATE_SREG && o[0].kind == OP_REG && o[0].size != BITS) c->b[c->len++] = 0x66;	// mov r16, sreg
	if(mem >= 0 && o[mem].asize != BITS) c->b[c->len++] = 0x67;
	if(!gstricmp(d->instruction, "jcxz") && BITS != 16) c->b[c->len++] = 0x67;
	if(!gstricmp(d->instruction, "jecxz") && BITS != 32) c->b[c->len++] = 0x67;

	// shift/rotate by an immediate 1 uses the shorter (and 8086 compatible) D0/D1 form
	if((d->c1 == 0xC0 || d->c1 == 0xC1) && d->opcode_length == 1 && o[1].kind == OP_IMM && !o[1].unresolved && o[1].value == 1)
	{
		short_shift = TRUE;
		op1 = (d->c1 == 0xC0) ? 0xD0 : 0xD1;
	}

	// opcode
	c->b[c->len++] = op1;
	if(d->opcode_length >= 2) c->b[c->len++] = d->c2;
	if(d->opcode_length >= 3) c->b[c->len++] = d->c3;

	if(d->format & FORMAT_REGPLUSOP)
		for(i = 0; i < 2; i++) if(param_is_reg(p[i])) c->b[c->len-1] += o[i].reg;

	if(d->format & FORMAT_MODRM)
	{
		int regfield = d->initial_modrm >> 3, rm = -1;
		for(i = 0; i < 2; i++)
		{
			if(param_is_reg(p[i]) || p[i] == EVALUATE_SREG || p[i] == EVALUATE_CREG || p[i] == EVALUATE_DREG) regfield = o[i].reg;
			else if(param_is_regm(p[i])) rm = i;
		}
		if(rm < 0) fatal_error("internal error: no r/m operand for '%s'", d->instruction);
		encode_modrm(c, &o[rm], regfield);
	}

	// immediates / relative targets / far pointers
	for(i = 0; i < 2; i++)
	{
		if(param_is_imm(p[i]))
		{
			int n = param_size(p[i]) / 8;
			if(d->format & (FORMAT_REL8 | FORMAT_REL16 | FORMAT_REL32))
			{
				rel_pos = c->len;
				rel_size = n;
				c->len += n;
				continue;
			}
			if(short_shift && i == 1) continue;
			put(c, o[i].value, n);
			c->imm_bytes += n;
		}
		else if(p[i] == EVALUATE_PTR16_PTR16 || p[i] == EVALUATE_PTR16_PTR32)
		{
			put(c, o[i].value, p[i] == EVALUATE_PTR16_PTR32 ? 4 : 2);
			put(c, o[i].farseg, 2);
		}
	}

	if(rel_pos >= 0)
	{
		// relative to the end of the instruction
		i64 rel = wrap_sub(o[0].value, here + c->len);
		int save = c->len;
		// Forward references (first pass only) optimistically assume a short branch fits;
		// later passes grow it if needed, converging on the shortest encoding.
		if(o[0].unresolved) {}
		else if(rel_size == 1 && !fits_s8(rel)) return FALSE;
		else if(rel_size == 2 && (rel < -65536 || rel > 65535)) return FALSE;
		c->len = rel_pos;
		put(c, rel, rel_size);
		c->len = save;
	}
	return TRUE;
}

static int dynamic_mnemonic(const char *m)
{
	int i;
	for(i = 0; dynamic_lookup[i].opcode_length; i++) if(!gstricmp(m, dynamic_lookup[i].instruction)) return TRUE;
	return FALSE;
}

static const t_fixed_instruction *fixed_lookup(const char *m)
{
	int i;
	for(i = 0; fixed_instruction_lookup[i].length; i++) if(!gstricmp(m, fixed_instruction_lookup[i].i_name)) return &fixed_instruction_lookup[i];
	return NULL;
}

static int is_instruction(const char *m)
{
	return fixed_lookup(m) != NULL || dynamic_mnemonic(m);
}

// push/pop of segment registers
static void encode_seg_pushpop(const char *m, const t_operand *o, t_code *c)
{
	static const unsigned char push_op[] = { 0x06, 0x0E, 0x16, 0x1E };
	static const unsigned char pop_op[]  = { 0x07, 0x00, 0x17, 0x1F };
	int is_push = !gstricmp(m, "push");

	c->len = 0;
	if(o->reg < 4)
	{
		if(!is_push && o->reg == 1) fatal_error("pop cs is not a valid instruction");
		c->b[c->len++] = is_push ? push_op[o->reg] : pop_op[o->reg];
	}
	else
	{
		c->b[c->len++] = 0x0F;
		c->b[c->len++] = (unsigned char)((o->reg == 4 ? 0xA0 : 0xA8) + (is_push ? 0 : 1));
	}
}

// imul with an immediate: imul r, r/m, imm  and  imul r, imm
static void encode_imul_imm(const t_operand *dst, const t_operand *src, const t_operand *imm, t_code *c)
{
	int size = dst->size;
	int use8;

	if(dst->kind != OP_REG || (size != 16 && size != 32)) fatal_error("invalid combination of opcode and operands");
	if(src->kind != OP_REG && src->kind != OP_MEM) fatal_error("invalid combination of opcode and operands");
	if((src->kind == OP_REG || src->size) && src->size != size) fatal_error("operand size mismatch");
	if(imm->kind != OP_IMM || imm->strict) fatal_error("invalid combination of opcode and operands");
	if(imm->size && imm->size != 8 && imm->size != size) fatal_error("operand size mismatch");
	if(!imm->unresolved && (imm->value < (size == 16 ? -32768 : -2147483648LL) || imm->value > (size == 16 ? 0xFFFF : 0xFFFFFFFFLL)))
		fatal_error("immediate value out of range");

	if(imm->size == 8) use8 = TRUE;
	else if(imm->unresolved || imm->size) use8 = FALSE;
	else
	{
		i64 m = (size == 32) ? 0xFFFFFFFFLL : 0xFFFF;
		use8 = (imm->value & m) == ((i64)(signed char)(imm->value & 0xFF) & m);
	}
	c->len = 0;
	if(src->kind == OP_MEM && src->seg) c->b[c->len++] = (unsigned char)src->seg;
	if(size != BITS) c->b[c->len++] = 0x66;
	if(src->kind == OP_MEM && src->asize != BITS) c->b[c->len++] = 0x67;
	c->b[c->len++] = use8 ? 0x6B : 0x69;
	encode_modrm(c, src, dst->reg);
	put(c, imm->value, use8 ? 1 : size / 8);
}

// Selects the best table entry for the operands and encodes it.
// Preference: no unneeded operand size prefix, unsized memory at the default size,
// shortest encoding, fewest immediate bytes, table order.
static int select_encoding(const char *mnemonic, const t_operand *o, i64 here, t_code *out)
{
	int attempt, k, found = FALSE;
	int best_pen = 0, best_sz = 0, best_len = 0, best_imm = 0;
	t_code code;

	// First try without relying on unresolved (forward referenced) values fitting a
	// short form; only if nothing else matches accept such a form.
	for(attempt = 0; attempt < 2 && !found; attempt++)
	{
		for(k = 0; dynamic_lookup[k].opcode_length; k++)
		{
			const t_dynamic_instruction *d = &dynamic_lookup[k];
			int unc = FALSE, pen, sz_pen = 0, j, swap;
			t_operand ops[2];

			if(gstricmp(mnemonic, d->instruction)) continue;

			for(swap = 0; swap < 2; swap++)
			{
				// test is commutative: also accept "test reg, r/m"
				if(swap && gstricmp(mnemonic, "test")) break;
				ops[0] = o[swap]; ops[1] = o[1 - swap];
				unc = FALSE;
				if(match_param(d, d->parm1, &ops[0], &unc) && match_param(d, d->parm2, &ops[1], &unc)) break;
			}
			if(swap == 2 || (swap == 1 && gstricmp(mnemonic, "test"))) continue;
			if(attempt == 0 && unc) continue;

			if(!encode_entry(d, ops, here, &code)) continue;

			for(j = 0; j < 2; j++)
				if(ops[j].kind == OP_MEM && ops[j].size == 0 && param_size(j == 0 ? d->parm1 : d->parm2) != BITS) sz_pen = 1;

			pen = (entry_opsize(d) && entry_opsize(d) != BITS) ? 1 : 0;
			if(!found || pen < best_pen || (pen == best_pen && (sz_pen < best_sz || (sz_pen == best_sz &&
				(code.len < best_len || (code.len == best_len && code.imm_bytes < best_imm))))))
			{
				found = TRUE;
				best_pen = pen;
				best_sz = sz_pen;
				best_len = code.len;
				best_imm = code.imm_bytes;
				*out = code;
			}
		}
	}
	return found;
}

// Assembles an instruction (mnemonic + operand text) located at address 'here'.
static void AssembleInstruction(const char *mnemonic, char *rest, i64 here, t_code *out)
{
	char *parts[MAX_OPERANDS];
	t_operand o[MAX_OPERANDS];
	int nops, i, k;

	nops = split_commas(rest, parts, MAX_OPERANDS);
	for(i = 0; i < MAX_OPERANDS; i++) { memset(&o[i], 0, sizeof(o[i])); o[i].kind = OP_NONE; }
	for(i = 0; i < nops; i++) ParseOperand(parts[i], &o[i]);

	if(nops == 0)
	{
		const t_fixed_instruction *f = fixed_lookup(mnemonic);
		if(!f) fatal_error("'%s' requires operands", mnemonic);
		out->len = 0;
		if(f->bits && f->bits != BITS) out->b[out->len++] = 0x66;
		out->b[out->len++] = f->c1;
		if(f->length >= 2) out->b[out->len++] = f->c2;
		if(f->length >= 3) out->b[out->len++] = f->c3;
		return;
	}

	if(!gstricmp(mnemonic, "imul") && (nops == 3 || (nops == 2 && o[1].kind == OP_IMM)))
	{
		if(nops == 3) encode_imul_imm(&o[0], &o[1], &o[2], out);
		else encode_imul_imm(&o[0], &o[0], &o[1], out);
		return;
	}
	if(nops > 2) fatal_error("too many operands for '%s'", mnemonic);

	if((!gstricmp(mnemonic, "push") || !gstricmp(mnemonic, "pop")) && nops == 1 && o[0].kind == OP_SREG)
	{
		encode_seg_pushpop(mnemonic, &o[0], out);
		return;
	}

	// indirect far jmp/call: FF /5, FF /3
	if((!gstricmp(mnemonic, "jmp") || !gstricmp(mnemonic, "call")) && nops == 1 && o[0].strict == 'F' && o[0].kind == OP_MEM)
	{
		out->len = 0;
		if(o[0].seg) out->b[out->len++] = (unsigned char)o[0].seg;
		if(o[0].size && o[0].size != 16 && o[0].size != 32) fatal_error("invalid size for far pointer");
		if(o[0].size && o[0].size != BITS) out->b[out->len++] = 0x66;
		if(o[0].asize != BITS) out->b[out->len++] = 0x67;
		out->b[out->len++] = 0xFF;
		encode_modrm(out, &o[0], !gstricmp(mnemonic, "jmp") ? 5 : 3);
		return;
	}
	for(i = 0; i < nops; i++)
		if(o[i].strict == 'F' && o[i].kind != OP_FAR) fatal_error("invalid use of 'far'");

	// mov between the accumulator and an absolute address: short A0-A3 (moffs) forms
	if(!gstricmp(mnemonic, "mov") && nops == 2)
		for(i = 0; i < 2; i++)
		{
			const t_operand *acc = &o[i], *mem = &o[1 - i];
			if(acc->kind == OP_REG && acc->reg == 0 && mem->kind == OP_MEM && mem->base == -1 && mem->index == -1 && (mem->size == 0 || mem->size == acc->size))
			{
				out->len = 0;
				if(mem->seg) out->b[out->len++] = (unsigned char)mem->seg;
				if(acc->size != 8 && acc->size != BITS) out->b[out->len++] = 0x66;
				if(mem->asize != BITS) out->b[out->len++] = 0x67;
				out->b[out->len++] = (unsigned char)((i == 0 ? 0xA0 : 0xA2) + (acc->size != 8));
				put(out, mem->value, mem->asize / 8);
				return;
			}
		}

	if(select_encoding(mnemonic, o, here, out)) return;

	// Work out a helpful error message
	if(o[0].kind == OP_IMM && !o[0].unresolved)
		for(k = 0; dynamic_lookup[k].opcode_length; k++)
			if(!gstricmp(mnemonic, dynamic_lookup[k].instruction) && (dynamic_lookup[k].format & (FORMAT_REL8 | FORMAT_REL16 | FORMAT_REL32)))
				fatal_error(o[0].strict == 'S' ? "short jump out of range" : "jump target out of range");
	for(i = 0; i < 2; i++)
		if(o[i].kind == OP_IMM && !o[i].unresolved)
		{
			t_operand probe[2];
			probe[0] = o[0]; probe[1] = o[1];
			probe[i].unresolved = TRUE;	// would any value fit?
			if(select_encoding(mnemonic, probe, here, out)) fatal_error("immediate value out of range");
		}
	fatal_error("invalid combination of opcode and operands");
}

//**************************************
// Directives
//**************************************

// Is a (trimmed) data item a single complete string literal?
static int is_string_literal(const char *a)
{
	size_t len = strlen(a);
	return len >= 2 && is_quote(a[0]) && strchr(a + 1, a[0]) == a + len - 1;
}

static void DataDirective(int unit, char *args)
{
	char *parts[MAX_LINE / 2];
	int n, i, k;

	n = split_commas(args, parts, MAX_LINE / 2);
	if(n == 0) fatal_error("data expected");
	for(i = 0; i < n; i++)
	{
		char *a = parts[i];
		size_t len = strlen(a);
		if(is_string_literal(a) && (unit == 1 || len - 2 > (size_t)unit))
		{
			// string: emitted byte by byte, zero padded to a multiple of the unit size
			size_t slen = len - 2;
			emit((unsigned char *)a + 1, slen);
			if(slen % unit) emit_fill(0, unit - slen % unit);
		}
		else
		{
			unsigned char b[4];
			i64 v = Evaluate(a, NULL);
			for(k = 0; k < unit; k++) b[k] = (unsigned char)(v >> (8 * k));
			emit(b, unit);
		}
	}
}

static void ReserveDirective(int unit, char *args)
{
	int unres = 0;
	i64 n = Evaluate(args, &unres);

	if(n < 0 && !unres) fatal_error("negative reserve count");
	if(n < 0) n = 0;
	if(n > MAX_OUTPUT) fatal_error("output exceeds %lld bytes", MAX_OUTPUT);
	emit_fill(0, n * unit);
}

// desc base, limit, flags  -- builds an 8 byte segment or gate descriptor
static void DescDirective(char *args)
{
	char *parts[3];
	i64 base, limit, flags;
	unsigned char d[8];

	if(split_commas(args, parts, 3) != 3) fatal_error("desc requires 3 operands: base, limit, flags");
	base = Evaluate(parts[0], NULL);
	limit = Evaluate(parts[1], NULL);
	flags = Evaluate(parts[2], NULL);

	if(flags & ((~flags) >> 2) & 0x400)
	{
		// gate: offset 0..15, selector, flags + present, offset 16..31
		i64 f = flags + 0x8000;
		d[0] = (unsigned char)base; d[1] = (unsigned char)(base >> 8);
		d[2] = (unsigned char)limit; d[3] = (unsigned char)(limit >> 8);
		d[4] = (unsigned char)f; d[5] = (unsigned char)(f >> 8);
		d[6] = (unsigned char)(base >> 16); d[7] = (unsigned char)(base >> 24);
	}
	else
	{
		// segment: limit 0..15, base 0..23, access, flags + limit 16..19, base 24..31
		d[0] = (unsigned char)limit; d[1] = (unsigned char)(limit >> 8);
		d[2] = (unsigned char)base; d[3] = (unsigned char)(base >> 8);
		d[4] = (unsigned char)(base >> 16);
		d[5] = (unsigned char)((flags + 0x8000) >> 8);
		d[6] = (unsigned char)(flags + ((limit >> 16) & 0x0F));
		d[7] = (unsigned char)(base >> 24);
	}
	emit(d, 8);
}

// label tss  -- reserves a 104 byte TSS and defines label.field for each field
static void TssDirective(const char *label)
{
	static const struct { const char *name; int off; } fields[] = {
		{"back", 0}, {"esp0", 4}, {"ss0", 8}, {"esp1", 12}, {"ss1", 16}, {"esp2", 20}, {"ss2", 24},
		{"cr3", 28}, {"eip", 32}, {"eflags", 36}, {"eax", 40}, {"ecx", 44}, {"edx", 48}, {"ebx", 52},
		{"esp", 56}, {"ebp", 60}, {"esi", 64}, {"edi", 68}, {"es", 72}, {"cs", 76}, {"ss", 80},
		{"ds", 84}, {"fs", 88}, {"gs", 92}, {"ldt", 96}, {"trap", 100}, {"io", 102}
	};
	char name[LABEL_NAME_MAX * 2], scope[LABEL_NAME_MAX];
	size_t i;

	Addlabel(label, offset);
	strcpy(scope, last_global);	// the field labels must not change the .local label scope
	for(i = 0; i < sizeof(fields) / sizeof(fields[0]); i++)
	{
		snprintf(name, sizeof(name), "%s.%s", label, fields[i].name);
		if(strlen(name) >= LABEL_NAME_MAX) fatal_error("label name too long");
		Addlabel(name, offset + fields[i].off);
	}
	strcpy(last_global, scope);
	emit_fill(0, 104);
}

static void Parsefile(const char *fname);

static void SetBits(char *arg)
{
	i64 v = Evaluate(arg, NULL);

	if(v == 16) BITS = 16;
	else if(v == 32) BITS = 32;
	else fatal_error("invalid BITS operand, valid operands are 16 and 32 only");
}

static void SetOrg(char *arg)
{
	int unres = 0;
	i64 v = Evaluate(arg, &unres);

	if(unres) fatal_error("ORG value must not use forward references");
	if(v < 0 || v > 0xFFFFFFFFLL) fatal_error("ORG value out of range");
	offset += v;	// gasm behaviour: ORG adds to the current address
	origin = v;
}

static void Command(char *line)
{
	char *end = strrchr(line, ']');
	char *cmd, *args;

	if(!end) fatal_error("missing ']'");
	*end = '\0';
	if(*trim(end + 1) != '\0') fatal_error("unexpected text after ']'");
	cmd = trim(line + 1);
	for(args = cmd; *args && *args != ' ' && *args != '\t'; args++);
	if(*args) *args++ = '\0';
	args = trim(args);

	if(!gstricmp(cmd, "org")) SetOrg(args);
	else if(!gstricmp(cmd, "bits")) SetBits(args);
	else if(!gstricmp(cmd, "section") || !gstricmp(cmd, "segment") || !gstricmp(cmd, "global") || !gstricmp(cmd, "extern") || !gstricmp(cmd, "cpu")) {}
	else fatal_error("unknown directive '[%s]'", cmd);
}

static void Preprocessor(char *line)
{
	char *cmd = line, *args;

	for(args = cmd; *args && *args != ' ' && *args != '\t'; args++);
	if(*args) *args++ = '\0';
	args = trim(args);

	if(!gstricmp(cmd, "%include"))
	{
		char path[1024], saved_file[1024];
		unsigned long saved_line = line_number;
		size_t n = strlen(args);
		FILE *f;

		if(n >= 2 && (args[0] == '"' || args[0] == '\'' || args[0] == '<') && (args[n-1] == '"' || args[n-1] == '\'' || args[n-1] == '>'))
		{
			args[n-1] = '\0';
			args++;
		}
		if(*args == '\0') fatal_error("%%include requires a file name");
		if(strlen(args) >= sizeof(path)) fatal_error("include path too long");
		strcpy(path, args);
		// Paths are relative to the current directory, falling back to the including file's directory
		if((f = fopen(path, "rb")) != NULL) fclose(f);
		else if(args[0] != '/')
		{
			const char *slash = strrchr(filename, '/');
			if(slash && (size_t)(slash - filename) + 1 + strlen(args) < sizeof(path))
			{
				char alt[1024];
				memcpy(alt, filename, slash - filename + 1);
				strcpy(alt + (slash - filename + 1), args);
				if((f = fopen(alt, "rb")) != NULL) { fclose(f); strcpy(path, alt); }
			}
		}
		if(include_depth >= MAX_INCLUDE_DEPTH) fatal_error("includes nested too deeply");

		strcpy(saved_file, filename);
		include_depth++;
		Parsefile(path);
		include_depth--;
		strcpy(filename, saved_file);
		line_number = saved_line;
		return;
	}
	fatal_error("unknown preprocessor directive '%s'", cmd);
}

static const char *const data_directives[] = { "db", "dw", "dd", "resb", "resw", "resd", "desc", "times", "align", "alignb", NULL };
static const char *const prefix_names[] = { "rep", "repe", "repz", "repne", "repnz", "lock", NULL };
static const unsigned char prefix_bytes[] = { 0xF3, 0xF3, 0xF3, 0xF2, 0xF2, 0xF0 };

// Reads the leading identifier of s into word; returns its length.
static int read_word(const char *s, char *word, int max)
{
	int len = 0;
	while(is_ident_char((unsigned char)s[len]))
	{
		if(len >= max - 1) fatal_error("identifier too long");
		word[len] = s[len];
		len++;
	}
	word[len] = '\0';
	return len;
}

// Assembles a statement: an instruction or directive, possibly preceded by a colonless label.
static void AssembleStatement(char *line)
{
	char word[LABEL_NAME_MAX], *rest;
	int len, i;
	i64 here = offset;
	t_code code;

	line = trim(line);
	if(*line == '\0') return;

	len = read_word(line, word, sizeof(word));
	rest = trim(line + len);

	if(!gstricmp(word, "db")) { DataDirective(1, rest); return; }
	if(!gstricmp(word, "dw")) { DataDirective(2, rest); return; }
	if(!gstricmp(word, "dd")) { DataDirective(4, rest); return; }
	if(!gstricmp(word, "resb")) { ReserveDirective(1, rest); return; }
	if(!gstricmp(word, "resw")) { ReserveDirective(2, rest); return; }
	if(!gstricmp(word, "resd")) { ReserveDirective(4, rest); return; }
	if(!gstricmp(word, "desc")) { DescDirective(rest); return; }
	if(!gstricmp(word, "bits")) { SetBits(rest); return; }
	if(!gstricmp(word, "org")) { SetOrg(rest); return; }
	if(!gstricmp(word, "align") || !gstricmp(word, "alignb"))
	{
		char *parts[2];
		int n = split_commas(rest, parts, 2);
		i64 a, fill = gstricmp(word, "align") ? 0 : 0x90;	// align pads with NOPs, alignb with zeros
		if(n < 1) fatal_error("align requires an operand");
		a = Evaluate(parts[0], NULL);
		if(a <= 0 || (a & (a - 1))) fatal_error("alignment must be a power of two");
		if(n == 2)
		{
			char *f = parts[1], *k2;
			if((k2 = match_keyword(f, "db"))) fill = Evaluate(trim(k2), NULL);
			else if(match_keyword(f, "nop") && *trim(f + 3) == '\0') fill = 0x90;
			else fill = Evaluate(f, NULL);
		}
		emit_fill((unsigned char)fill, (a - (offset % a)) % a);
		return;
	}
	if(!gstricmp(word, "times"))
	{
		const char *after;
		int unres = 0;
		i64 count, loop;
		char copy[MAX_LINE];

		count = EvaluatePrefix(rest, &after, &unres);
		if(count < 0)
		{
			if(!unres) fatal_error("TIMES count is negative");
			count = 0;
		}
		if(*after == '\0') fatal_error("TIMES requires an instruction or data");
		if(count > MAX_OUTPUT) fatal_error("TIMES count too large");
		read_word(after, copy, LABEL_NAME_MAX);
		if(count > 1 && in_list(copy, data_directives) && gstricmp(copy, "times") && gstricmp(copy, "align") && gstricmp(copy, "alignb"))
		{
			// data does not depend on its position ($ is the start of the line): assemble once and replicate
			i64 start = offset;
			size_t start_len = outlen;
			strcpy(copy, after);
			AssembleStatement(copy);
			emit_repeat(start_len, offset - start, count - 1);
			return;
		}
		for(loop = 0; loop < count; loop++)
		{
			strcpy(copy, after);	// relative branches depend on each repetition's position
			AssembleStatement(copy);
		}
		return;
	}

	for(i = 0; prefix_names[i]; i++)
		if(!gstricmp(word, prefix_names[i]) && *rest)
		{
			emit(&prefix_bytes[i], 1);
			AssembleStatement(rest);
			return;
		}

	if(is_instruction(word))
	{
		AssembleInstruction(word, rest, here, &code);
		emit(code.b, code.len);
		return;
	}

	// gasm accepts labels without a colon, but only before an instruction or directive
	if(len > 0 && CheckValidLabel(word) && (line[len] == '\0' || line[len] == ' ' || line[len] == '\t'))
	{
		char next[LABEL_NAME_MAX];
		read_word(rest, next, sizeof(next));
		if(*rest == '\0' || is_instruction(next) || in_list(next, data_directives) || in_list(next, prefix_names))
		{
			Addlabel(word, offset);
			AssembleStatement(rest);
			return;
		}
		fatal_error("unknown instruction '%s'", word);
	}
	fatal_error("syntax error: '%s'", line);
}

static void AssembleLine(char *line)
{
	char word[LABEL_NAME_MAX], *p, *k;
	int len;

	strip_comment(line);
	line = trim(line);
	if(*line == '\0') return;
	line_start = offset;

	if(line[0] == '[') { Command(line); return; }
	if(line[0] == '%') { Preprocessor(line); return; }

	len = read_word(line, word, sizeof(word));
	p = trim(line + len);
	k = (*p == ':') ? trim(p + 1) : p;

	// name equ value
	if(len > 0 && !is_instruction(word) && (k = match_keyword(k, "equ")))
	{
		k = trim(k);
		if(*k == '\0') fatal_error("EQU requires a value");
		Addlabel(word, Evaluate(k, NULL));
		return;
	}
	// name tss
	k = (*p == ':') ? trim(p + 1) : p;
	if(len > 0 && !is_instruction(word) && (k = match_keyword(k, "tss")))
	{
		if(*trim(k) != '\0') fatal_error("unexpected text after 'tss'");
		TssDirective(word);
		return;
	}
	// name:
	if(len > 0 && *p == ':')
	{
		Addlabel(word, offset);
		AssembleStatement(p + 1);
		return;
	}
	AssembleStatement(line);
}

static void Parsefile(const char *fname)
{
	char line[MAX_LINE + 2];
	FILE *in;

	if(strlen(fname) >= sizeof(filename)) fatal_error("file name too long");
	if((in = fopen(fname, "rb")) == NULL)
	{
		if(filename[0]) fatal_error("unable to open '%s'", fname);
		fprintf(stderr, "error: unable to open '%s'\n", fname);
		exit(1);
	}
	strcpy(filename, fname);
	line_number = 0;

	while(fgets(line, sizeof(line), in))
	{
		size_t n = strlen(line);
		line_number++;
		if(n == sizeof(line) - 1 && line[n-1] != '\n') fatal_error("line too long (maximum %d characters)", MAX_LINE);
		AssembleLine(line);
	}
	fclose(in);
}

static void RunPass(const char *source)
{
	offset = 0;
	origin = 0;
	BITS = 16;
	labels_changed = FALSE;
	last_global[0] = '\0';
	filename[0] = '\0';
	line_number = 0;
	include_depth = 0;
	outlen = 0;
	emitted = 0;
	Parsefile(source);
}

// Output file: source name with its extension replaced by .bin
static void OutputName(const char *src, char *out, size_t outsize)
{
	const char *slash = strrchr(src, '/');
	const char *dot = strrchr(src, '.');
	size_t n = (dot && dot != src && (!slash || dot > slash + 1)) ? (size_t)(dot - src) : strlen(src);

	if(n + 5 > outsize) { fprintf(stderr, "error: file name too long\n"); exit(1); }
	memcpy(out, src, n);
	strcpy(out + n, ".bin");
}

//**************************************
// MAIN FUNCTION -=- PROGRAM ENTRY POINT
//**************************************
int main(int argc, char **argv)
{
	char outname[1024];
	const char *source;
	FILE *out;
	int quiet = FALSE, argi = 1;
	size_t i;

	if(argi < argc && !strcmp(argv[argi], "-q")) { quiet = TRUE; argi++; }

	if(!quiet)
	{
		printf("GASM  --- Gaz's Assembler v0.56, by Gareth Owen.\n(C)Copyright 1998 by Gareth Owen. All rights reserved\n");
		printf("http://gasm.cjb.net -=- Contact Gareth Owen at gaz@athene.co.uk\n");
	}

	if(argc - argi < 1 || argc - argi > 2)
	{
		printf("\n\nUsage: gasm [-q] <source.asm> [output.bin]\n");
		exit(1);
	}
	source = argv[argi];
	if(argc - argi == 2)
	{
		if(strlen(argv[argi+1]) >= sizeof(outname)) { fprintf(stderr, "error: file name too long\n"); exit(1); }
		strcpy(outname, argv[argi+1]);
	}
	else OutputName(source, outname, sizeof(outname));

	// Predefined labels (descriptor constants)
	for(i = 0; i < sizeof(predefines) / sizeof(predefines[0]); i++) labels[label_count++] = predefines[i];

	// Assemble until every label address is stable, then do a final pass that writes the output
	for(pass_number = 1; ; pass_number++)
	{
		if(pass_number > MAX_PASSES) { fprintf(stderr, "%s: error: label addresses did not converge after %d passes\n", source, MAX_PASSES); exit(1); }
		RunPass(source);
		if(pass_number > 1 && !labels_changed) break;
	}
	pass_number++;
	final_pass = TRUE;
	RunPass(source);
	if(labels_changed) { fprintf(stderr, "%s: error: label addresses changed during the final pass\n", source); exit(1); }

	if((out = fopen(outname, "wb")) == NULL) { perror(outname); exit(1); }
	if(outlen && fwrite(outbuf, 1, outlen, out) != outlen) { perror(outname); exit(1); }
	if(fclose(out) != 0) { perror(outname); exit(1); }
	free(outbuf);
	return 0;
}
