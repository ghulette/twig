%module sexp
%{
#include "sexp.h"
%}

sexp_errcode_t set_parser_buffer_params(size_t ss, size_t gs);
sexp_t *sexp_t_allocate(void);
void sexp_t_deallocate(sexp_t *s);
void sexp_cleanup(void);
int print_sexp(char *loc, size_t size, const sexp_t *e);
int print_sexp_cstr(CSTRING **s, const sexp_t *e, size_t ss);
sexp_t *new_sexp_list(sexp_t *l);
sexp_t *new_sexp_atom(const char *buf, size_t bs, atom_t aty);
pcont_t *init_continuation(char *str);
void destroy_continuation (pcont_t * pc);
sexp_iowrap_t *init_iowrap(int fd);
void destroy_iowrap(sexp_iowrap_t *iow);
sexp_t *read_one_sexp(sexp_iowrap_t *iow);
sexp_t *parse_sexp(char *s, size_t len);
sexp_t *iparse_sexp(char *s, size_t len, pcont_t *cc);
pcont_t *cparse_sexp(char *s, size_t len, pcont_t *pc);
void destroy_sexp(sexp_t *s);
void reset_sexp_errno();
