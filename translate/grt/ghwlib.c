/*  GHDL Wavefile reader library.
    Copyright (C) 2005 Tristan Gingold

    GHDL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING.  If not, write to the Free
    Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "ghwlib.h"

int
ghw_open (struct ghw_handler *h, const char *filename)
{
  char hdr[16];

  h->stream = fopen (filename, "rb");
  if (h->stream == NULL)
    return -1;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;
  /* Check magic.  */
  if (memcmp (hdr, "GHDLwave\n", 9) != 0)
    return -2;
  /* Check version.  */
  if (hdr[9] != 16
      || hdr[10] != 0)
    return -2;
  h->version = hdr[11];
  if (h->version > 1)
    return -3;
  if (hdr[12] == 1)
    h->word_be = 0;
  else if (hdr[12] == 2)
    h->word_be = 1;
  else
    return -4;
#if 0
  /* Endianness.  */
  {
    int endian;
    union { unsigned char b[4]; uint32_t i;} v;
    v.i = 0x11223344;
    if (v.b[0] == 0x11)
      endian = 2;
    else if (v.b[0] == 0x44)
      endian = 1;
    else
      return -3;

    if (hdr[12] != 1 && hdr[12] != 2)
      return -3;
    if (hdr[12] != endian)
      h->swap_word = 1;
    else
      h->swap_word = 0;
  }
#endif
  h->word_len = hdr[13];
  h->off_len = hdr[14];

  if (hdr[15] != 0)
    return -5;

  h->hie = NULL;
  return 0;
}

int32_t
ghw_get_i32 (struct ghw_handler *h, unsigned char *b)
{
  if (h->word_be)
    return (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | (b[3] << 0);
  else
    return (b[3] << 24) | (b[2] << 16) | (b[1] << 8) | (b[0] << 0);
}

int64_t
ghw_get_i64 (struct ghw_handler *ghw_h, unsigned char *b)
{
  int l, h;

  if (ghw_h->word_be)
    {
      h = (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | (b[3] << 0);
      l = (b[4] << 24) | (b[5] << 16) | (b[6] << 8) | (b[7] << 0);
    }
  else
    {
      l = (b[3] << 24) | (b[2] << 16) | (b[1] << 8) | (b[0] << 0);
      h = (b[7] << 24) | (b[6] << 16) | (b[5] << 8) | (b[4] << 0);
    }
  return (((int64_t)h) << 32) | l;
}

int
ghw_read_byte (struct ghw_handler *h, unsigned char *res)
{
  int v;

  v = fgetc (h->stream);
  if (v == EOF)
    return -1;
  *res = v;
  return 0;
}

int
ghw_read_uleb128 (struct ghw_handler *h, uint32_t *res)
{
  unsigned int r = 0;
  unsigned int off = 0;

  while (1)
    {
      int v = fgetc (h->stream);
      if (v == EOF)
	return -1;
      r |= (v & 0x7f) << off;
      if ((v & 0x80) == 0)
	break;
      off += 7;
    }
  *res = r;
  return 0;
}

int
ghw_read_sleb128 (struct ghw_handler *h, int32_t *res)
{
  int32_t r = 0;
  unsigned int off = 0;

  while (1)
    {
      int v = fgetc (h->stream);
      if (v == EOF)
	return -1;
      r |= ((int32_t)(v & 0x7f)) << off;
      off += 7;
      if ((v & 0x80) == 0)
	{
	  if ((v & 0x40) && off < 32)
	    r |= -1 << off;
	  break;
	}
    }
  *res = r;
  return 0;
}

int
ghw_read_lsleb128 (struct ghw_handler *h, int64_t *res)
{
  static const int64_t r_mask = -1;
  int64_t r = 0;
  unsigned int off = 0;

  while (1)
    {
      int v = fgetc (h->stream);
      if (v == EOF)
	return -1;
      r |= ((int64_t)(v & 0x7f)) << off;
      off += 7;
      if ((v & 0x80) == 0)
	{
	  if ((v & 0x40) && off < 64)
	    r |= r_mask << off;
	  break;
	}
    }
  *res = r;
  return 0;
}

int
ghw_read_f64 (struct ghw_handler *h, double *res)
{
  /* FIXME: handle byte order.  */
  if (fread (res, sizeof (*res), 1, h->stream) != 1)
    return -1;
  return 0;
}

const char *
ghw_read_strid (struct ghw_handler *h)
{
  unsigned int id;
  if (ghw_read_uleb128 (h, &id) != 0)
    return NULL;
  return h->str_table[id];
}

union ghw_type *
ghw_read_typeid (struct ghw_handler *h)
{
  unsigned int id;
  if (ghw_read_uleb128 (h, &id) != 0)
    return NULL;
  return h->types[id - 1];
}

union ghw_range *
ghw_read_range (struct ghw_handler *h)
{
  int t = fgetc (h->stream);
  if (t == EOF)
    return NULL;
  switch (t & 0x7f)
    {
    case ghdl_rtik_type_b2:
      {
	struct ghw_range_b2 *r;
	r = malloc (sizeof (struct ghw_range_b2));
	r->kind = t & 0x7f;
	r->dir = (t & 0x80) != 0;
	if (ghw_read_byte (h, &r->left) != 0)
	  return NULL;
	if (ghw_read_byte (h, &r->right) != 0)
	  return NULL;
	return (union ghw_range *)r;
      }
    case ghdl_rtik_type_e8:
      {
	struct ghw_range_e8 *r;
	r = malloc (sizeof (struct ghw_range_e8));
	r->kind = t & 0x7f;
	r->dir = (t & 0x80) != 0;
	if (ghw_read_byte (h, &r->left) != 0)
	  return NULL;
	if (ghw_read_byte (h, &r->right) != 0)
	  return NULL;
	return (union ghw_range *)r;
      }
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_p32:
      {
	struct ghw_range_i32 *r;
	r = malloc (sizeof (struct ghw_range_i32));
	r->kind = t & 0x7f;
	r->dir = (t & 0x80) != 0;
	if (ghw_read_sleb128 (h, &r->left) != 0)
	  return NULL;
	if (ghw_read_sleb128 (h, &r->right) != 0)
	  return NULL;
	return (union ghw_range *)r;
      }
    case ghdl_rtik_type_i64:
    case ghdl_rtik_type_p64:
      {
	struct ghw_range_i64 *r;
	r = malloc (sizeof (struct ghw_range_i64));
	r->kind = t & 0x7f;
	r->dir = (t & 0x80) != 0;
	if (ghw_read_lsleb128 (h, &r->left) != 0)
	  return NULL;
	if (ghw_read_lsleb128 (h, &r->right) != 0)
	  return NULL;
	return (union ghw_range *)r;
      }
    case ghdl_rtik_type_f64:
      {
	struct ghw_range_f64 *r;
	r = malloc (sizeof (struct ghw_range_f64));
	r->kind = t & 0x7f;
	r->dir = (t & 0x80) != 0;
	if (ghw_read_f64 (h, &r->left) != 0)
	  return NULL;
	if (ghw_read_f64 (h, &r->right) != 0)
	  return NULL;
	return (union ghw_range *)r;
      }
    default:
      fprintf (stderr, "ghw_read_range: type %d unhandled\n", t & 0x7f);
      return NULL;
    }
}

int
ghw_read_str (struct ghw_handler *h)
{
  unsigned char hdr[12];
  int i;
  char *p;
  int prev_len;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  if (hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0)
    return -1;
  h->nbr_str = ghw_get_i32 (h, &hdr[4]);
  h->nbr_str++;
  h->str_size = ghw_get_i32 (h, &hdr[8]);
  h->str_table = (char **)malloc ((h->nbr_str + 1) * sizeof (char *));
  h->str_content = (char *)malloc (h->str_size + h->nbr_str + 1);

  if (h->flag_verbose)
    {
      printf ("Number of strings: %d\n", h->nbr_str - 1);
      printf ("String table size: %d\n", h->str_size);
    }

  h->str_table[0] = "<anon>";
  p = h->str_content;
  prev_len = 0;
  for (i = 1; i < h->nbr_str; i++)
    {
      int j;
      int c;
      char *prev;
      int sh;

      h->str_table[i] = p;
      prev = h->str_table[i - 1];
      for (j = 0; j < prev_len; j++)
	*p++ = prev[j];

      while (1)
	{
	  c = fgetc (h->stream);
	  if (c == EOF)
	    return -1;
	  if ((c >= 0 && c <= 31)
	      || (c >= 128 && c <= 159))
	    break;
	  *p++ = c;
	}
      *p++ = 0;

      if (h->flag_verbose > 1)
	printf (" string %d (pl=%d): %s\n", i, prev_len, h->str_table[i]);

      prev_len = c & 0x1f;
      sh = 5;
      while (c >= 128)
	{
	  c = fgetc (h->stream);
	  if (c == EOF)
	    return -1;
	  prev_len |= (c & 0x1f) << sh;
	  sh += 5;
	}
    }
  if (fread (hdr, 4, 1, h->stream) != 1)
    return -1;
  if (memcmp (hdr, "EOS", 4) != 0)
    return -1;
  return 0;
}

union ghw_type *
ghw_get_base_type (union ghw_type *t)
{
  switch (t->kind)
    {
    case ghdl_rtik_type_b2:
    case ghdl_rtik_type_e8:
    case ghdl_rtik_type_e32:
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_i64:
    case ghdl_rtik_type_f64:
    case ghdl_rtik_type_p32:
    case ghdl_rtik_type_p64:
      return t;
    case ghdl_rtik_subtype_scalar:
      return t->ss.base;
    case ghdl_rtik_subtype_array:
      return (union ghw_type*)(t->sa.base);
    default:
      fprintf (stderr, "ghw_get_base_type: cannot handle type %d\n", t->kind);
      abort ();
    }
}

int
get_nbr_elements (union ghw_type *t)
{
  switch (t->kind)
    {
    case ghdl_rtik_type_b2:
    case ghdl_rtik_type_e8:
    case ghdl_rtik_type_e32:
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_i64:
    case ghdl_rtik_type_f64:
    case ghdl_rtik_type_p32:
    case ghdl_rtik_type_p64:
    case ghdl_rtik_subtype_scalar:
      return 1;
    case ghdl_rtik_subtype_array:
    case ghdl_rtik_subtype_array_ptr:
      return t->sa.nbr_el;
    case ghdl_rtik_type_record:
      return t->rec.nbr_el;
    default:
      fprintf (stderr, "get_nbr_elements: unhandled type %d\n", t->kind);
      abort ();
    }
}

int
get_range_length (union ghw_range *rng)
{
  switch (rng->kind)
    {
    case ghdl_rtik_type_i32:
      if (rng->i32.dir)
	return (rng->i32.left - rng->i32.right + 1);
      else
	return (rng->i32.right - rng->i32.left + 1);
    default:
      fprintf (stderr, "get_range_length: unhandled kind %d\n", rng->kind);
      abort ();
    }
}

int
ghw_read_type (struct ghw_handler *h)
{
  unsigned char hdr[8];
  int i;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  if (hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0)
    return -1;
  h->nbr_types = ghw_get_i32 (h, &hdr[4]);
  h->types = (union ghw_type **)
    malloc (h->nbr_types * sizeof (union ghw_type *));

  for (i = 0; i < h->nbr_types; i++)
    {
      int t;

      t = fgetc (h->stream);
      if (t == EOF)
	return -1;
      /* printf ("type[%d]= %d\n", i, t); */
      switch (t)
	{
	case ghdl_rtik_type_b2:
	case ghdl_rtik_type_e8:
	  {
	    struct ghw_type_enum *e;
	    int j;

	    e = malloc (sizeof (struct ghw_type_enum));
	    e->kind = t;
	    e->wkt = ghw_wkt_unknown;
	    e->name = ghw_read_strid (h);
	    if (ghw_read_uleb128 (h, &e->nbr) != 0)
	      return -1;
	    e->lits = (const char **) malloc (e->nbr * sizeof (char *));
	    if (h->flag_verbose > 1)
	      printf ("enum %s:", e->name);
	    for (j = 0; j < e->nbr; j++)
	      {
		e->lits[j] = ghw_read_strid (h);
		if (h->flag_verbose > 1)
		  printf (" %s", e->lits[j]);
	      }
	    if (h->flag_verbose > 1)
	      printf ("\n");
	    h->types[i] = (union ghw_type *)e;
	  }
	  break;
	case ghdl_rtik_type_i32:
	case ghdl_rtik_type_i64:
	case ghdl_rtik_type_f64:
	  {
	    struct ghw_type_scalar *sc;

	    sc = malloc (sizeof (struct ghw_type_scalar));
	    sc->kind = t;
	    sc->name = ghw_read_strid (h);
	    if (h->flag_verbose > 1)
	      printf ("scalar: %s\n", sc->name);
	    h->types[i] = (union ghw_type *)sc;
	  }
	  break;
	case ghdl_rtik_type_p32:
	case ghdl_rtik_type_p64:
	  {
	    struct ghw_type_physical *ph;

	    ph = malloc (sizeof (struct ghw_type_physical));
	    ph->kind = t;
	    ph->name = ghw_read_strid (h);
	    if (h->version == 0)
	      ph->nbr_units = 0;
	    else
	      {
		int i;

		if (ghw_read_uleb128 (h, &ph->nbr_units) != 0)
		  return -1;
		ph->units = malloc (ph->nbr_units * sizeof (struct ghw_unit));
		for (i = 0; i < ph->nbr_units; i++)
		  {
		    ph->units[i].name = ghw_read_strid (h);
		    if (ghw_read_lsleb128 (h, &ph->units[i].val) < 0)
		      return -1;
		  }
	      }
	    if (h->flag_verbose > 1)
	      printf ("physical: %s\n", ph->name);
	    h->types[i] = (union ghw_type *)ph;
	  }
	  break;
	case ghdl_rtik_subtype_scalar:
	  {
	    struct ghw_subtype_scalar *ss;

	    ss = malloc (sizeof (struct ghw_subtype_scalar));
	    ss->kind = t;
	    ss->name = ghw_read_strid (h);
	    ss->base = ghw_read_typeid (h);
	    ss->rng = ghw_read_range (h);
	    if (h->flag_verbose > 1)
	      printf ("subtype scalar: %s\n", ss->name);
	    h->types[i] = (union ghw_type *)ss;
	  }
	  break;
	case ghdl_rtik_type_array:
	  {
	    struct ghw_type_array *arr;
	    int j;

	    arr = malloc (sizeof (struct ghw_type_array));
	    arr->kind = t;
	    arr->name = ghw_read_strid (h);
	    arr->el = ghw_read_typeid (h);
	    if (ghw_read_uleb128 (h, &arr->nbr_dim) != 0)
	      return -1;
	    arr->dims = (union ghw_type **)
	      malloc (arr->nbr_dim * sizeof (union ghw_type *));
	    for (j = 0; j < arr->nbr_dim; j++)
	      arr->dims[j] = ghw_read_typeid (h);
	    if (h->flag_verbose > 1)
	      printf ("array: %s\n", arr->name);
	    h->types[i] = (union ghw_type *)arr;
	  }
	  break;
	case ghdl_rtik_subtype_array:
	case ghdl_rtik_subtype_array_ptr:
	  {
	    struct ghw_subtype_array *sa;
	    int j;
	    int nbr_el;

	    sa = malloc (sizeof (struct ghw_subtype_array));
	    sa->kind = t;
	    sa->name = ghw_read_strid (h);
	    sa->base = (struct ghw_type_array *)ghw_read_typeid (h);
	    nbr_el = get_nbr_elements (sa->base->el);
	    sa->rngs = malloc (sa->base->nbr_dim * sizeof (union ghw_range *));
	    for (j = 0; j < sa->base->nbr_dim; j++)
	      {
		sa->rngs[j] = ghw_read_range (h);
		nbr_el *= get_range_length (sa->rngs[j]);
	      }
	    sa->nbr_el = nbr_el;
	    if (h->flag_verbose > 1)
	      printf ("subtype array: %s (nbr_el=%d)\n", sa->name, sa->nbr_el);
	    h->types[i] = (union ghw_type *)sa;
	  }
	  break;
	case ghdl_rtik_type_record:
	  {
	    struct ghw_type_record *rec;
	    int j;
	    int nbr_el;

	    rec = malloc (sizeof (struct ghw_type_record));
	    rec->kind = t;
	    rec->name = ghw_read_strid (h);
	    if (ghw_read_uleb128 (h, &rec->nbr_fields) != 0)
	      return -1;
	    rec->el = malloc
	      (rec->nbr_fields * sizeof (struct ghw_record_element));
	    nbr_el = 0;
	    for (j = 0; j < rec->nbr_fields; j++)
	      {
		rec->el[j].name = ghw_read_strid (h);
		rec->el[j].type = ghw_read_typeid (h);
		nbr_el += get_nbr_elements (rec->el[j].type);
	      }
	    rec->nbr_el = nbr_el;
	    if (h->flag_verbose > 1)
	      printf ("record type: %s (nbr_el=%d)\n", rec->name, rec->nbr_el);
	    h->types[i] = (union ghw_type *)rec;
	  }
	  break;
	default:
	  fprintf (stderr, "ghw_read_type: unknown type %d\n", t);
	  return -1;
	}
    }
  if (fgetc (h->stream) != 0)
    return -1;
  return 0;
}

int
ghw_read_wk_types (struct ghw_handler *h)
{
  char hdr[4];

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  if (hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0)
    return -1;

  while (1)
    {
      int t;
      union ghw_type *tid;

      t = fgetc (h->stream);
      if (t == EOF)
	return -1;
      else if (t == 0)
	break;

      tid = ghw_read_typeid (h);
      if (tid->kind == ghdl_rtik_type_b2
	  || tid->kind == ghdl_rtik_type_e8)
	{
	  if (h->flag_verbose > 0)
	    printf ("%s: wkt=%d\n", tid->en.name, t);
	  tid->en.wkt = t;
	}
    }
  return 0;
}

void
ghw_disp_typename (struct ghw_handler *h, union ghw_type *t)
{
  printf ("%s", t->common.name);
}

/* Read a signal composed of severals elements.  */
int
ghw_read_signal (struct ghw_handler *h, unsigned int *sigs, union ghw_type *t)
{
  switch (t->kind)
    {
    case ghdl_rtik_type_b2:
    case ghdl_rtik_type_e8:
    case ghdl_rtik_type_e32:
    case ghdl_rtik_subtype_scalar:
      {
	unsigned int sig_el;

	if (ghw_read_uleb128 (h, &sig_el) < 0)
	  return -1;
	*sigs = sig_el;
	if (sig_el >= h->nbr_sigs)
	  abort ();
	if (h->sigs[sig_el].type == NULL)
	  h->sigs[sig_el].type = ghw_get_base_type (t);
      }
      return 0;
    case ghdl_rtik_subtype_array:
    case ghdl_rtik_subtype_array_ptr:
      {
	int i;
	int stride;
	int len;

	len = t->sa.nbr_el;
	stride = get_nbr_elements (t->sa.base->el);

	for (i = 0; i < len; i += stride)
	  if (ghw_read_signal (h, &sigs[i], t->sa.base->el) < 0)
	    return -1;
      }
      return 0;
    case ghdl_rtik_type_record:
      {
	int i;
	int off;
	
	off = 0;
	for (i = 0; i < t->rec.nbr_fields; i++)
	  {
	    if (ghw_read_signal (h, &sigs[off], t->rec.el[i].type) < 0)
	      return -1;
	    off += get_nbr_elements (t->rec.el[i].type);
	  }
      }
      return 0;
    default:
      fprintf (stderr, "ghw_read_signal: type kind %d unhandled\n", t->kind);
      abort ();
    }
}


int
ghw_read_value (struct ghw_handler *h,
		union ghw_val *val, union ghw_type *type)
{
  switch (ghw_get_base_type (type)->kind)
    {
    case ghdl_rtik_type_b2:
      {
	int v;
	v = fgetc (h->stream);
	if (v == EOF)
	  return -1;
	val->b2 = v;
      }
      break;
    case ghdl_rtik_type_e8:
      {
	int v;
	v = fgetc (h->stream);
	if (v == EOF)
	  return -1;
	val->e8 = v;
      }
      break;
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_p32:
      {
	int32_t v;
	if (ghw_read_sleb128 (h, &v) < 0)
	  return -1;
	val->i32 = v;
      }
      break;
    case ghdl_rtik_type_f64:
      {
	double v;
	if (ghw_read_f64 (h, &v) < 0)
	  return -1;
	val->f64 = v;
      }
      break;
    case ghdl_rtik_type_p64:
      {
	int64_t v;
	if (ghw_read_lsleb128 (h, &v) < 0)
	  return -1;
	val->i64 = v;
      }
      break;
    default:
      fprintf (stderr, "read_value: cannot handle format %d\n", type->kind);
      abort ();
    }
  return 0;
}

int
ghw_read_hie (struct ghw_handler *h)
{
  unsigned char hdr[16];
  int nbr_scopes;
  int nbr_sigs;
  int i;
  struct ghw_hie *blk;
  struct ghw_hie **last;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  if (hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0)
    return -1;
  nbr_scopes = ghw_get_i32 (h, &hdr[4]);
  /* Number of declared signals (which may be composite).  */
  nbr_sigs = ghw_get_i32 (h, &hdr[8]);
  /* Number of basic signals.  */
  h->nbr_sigs = ghw_get_i32 (h, &hdr[12]);

  if (h->flag_verbose)
    printf ("%d scopes, %d signals, %d signal elements\n",
	    nbr_scopes, nbr_sigs, h->nbr_sigs);

  blk = (struct ghw_hie *)malloc (sizeof (struct ghw_hie));
  blk->kind = ghw_hie_design;
  blk->name = NULL;
  blk->parent = NULL;
  blk->brother = NULL;
  blk->u.blk.child = NULL;

  last = &blk->u.blk.child;
  h->hie = blk;

  h->nbr_sigs++;
  h->sigs = (struct ghw_sig *) malloc (h->nbr_sigs * sizeof (struct ghw_sig));
  memset (h->sigs, 0, h->nbr_sigs * sizeof (struct ghw_sig));

  while (1)
    {
      int t;
      struct ghw_hie *el;
      unsigned int str;

      t = fgetc (h->stream);
      if (t == EOF)
	return -1;
      if (t == 0)
	break;

      if (t == ghw_hie_eos)
	{
	  blk = blk->parent;
	  if (blk->u.blk.child == NULL)
	    last = &blk->u.blk.child;
	  else
	    {
	      struct ghw_hie *l = blk->u.blk.child;
	      while (l->brother != NULL)
		l = l->brother;
	      last = &l->brother;
	    }

	  continue;
	}

      el = (struct ghw_hie *) malloc (sizeof (struct ghw_hie));
      el->kind = t;
      el->parent = blk;
      el->brother = NULL;

      /* Link.  */
      *last = el;
      last = &el->brother;

      /* Read name.  */
      if (ghw_read_uleb128 (h, &str) != 0)
	return -1;
      el->name = h->str_table[str];

      switch (t)
	{
	case ghw_hie_eoh:
	case ghw_hie_design:
	case ghw_hie_eos:
	  /* Should not be here.  */
	  abort ();
	case ghw_hie_process:
	  break;
	case ghw_hie_block:
	case ghw_hie_generate_if:
	case ghw_hie_generate_for:
	case ghw_hie_instance:
	case ghw_hie_generic:
	case ghw_hie_package:
	  /* Create a block.  */
	  el->u.blk.child = NULL;

	  if (t == ghw_hie_generate_for)
	    {
	      el->u.blk.iter_type = ghw_read_typeid (h);
	      el->u.blk.iter_value = malloc (sizeof (union ghw_val));
	      if (ghw_read_value (h, el->u.blk.iter_value,
				  el->u.blk.iter_type) < 0)
		return -1;
	    }
	  blk = el;
	  last = &el->u.blk.child;
	  break;
	case ghw_hie_signal:
	case ghw_hie_port_in:
	case ghw_hie_port_out:
	case ghw_hie_port_inout:
	case ghw_hie_port_buffer:
	case ghw_hie_port_linkage:
	  /* For a signal, read type.  */
	  {
	    int nbr_el;
	    unsigned int *sigs;

	    el->u.sig.type = ghw_read_typeid (h);
	    nbr_el = get_nbr_elements (el->u.sig.type);
	    sigs = (unsigned int *) malloc
	      ((nbr_el + 1) * sizeof (unsigned int));
	    el->u.sig.sigs = sigs;
	    /* Last element is NULL.  */
	    sigs[nbr_el] = 0;

	    if (h->flag_verbose > 1)
	      printf ("signal %s: %d el [", el->name, nbr_el);
	    if (ghw_read_signal (h, sigs, el->u.sig.type) < 0)
	      return -1;
	    if (h->flag_verbose > 1)
	      {
		int i;
		for (i = 0; i < nbr_el; i++)
		  printf (" #%u", sigs[i]);
		printf ("]\n");
	      }
	  }
	  break;
	default:
	  fprintf (stderr, "ghw_read_hie: unhandled kind %d\n", t);
	  abort ();
	}
    }

  /* Allocate values.  */
  for (i = 0; i < h->nbr_sigs; i++)
    if (h->sigs[i].type != NULL)
      h->sigs[i].val = (union ghw_val *) malloc (sizeof (union ghw_val));
  return 0;
}

const char *
ghw_get_hie_name (struct ghw_hie *h)
{
  switch (h->kind)
    {
    case ghw_hie_eoh:
      return "eoh";
    case ghw_hie_design:
      return "design";
    case ghw_hie_block:
      return "block";
    case ghw_hie_generate_if:
      return "generate-if";
    case ghw_hie_generate_for:
      return "generate-for";
    case ghw_hie_instance:
      return "instance";
    case ghw_hie_package:
      return "package";
    case ghw_hie_process:
      return "process";
    case ghw_hie_generic:
      return "generic";
    case ghw_hie_eos:
      return "eos";
    case ghw_hie_signal:
      return "signal";
    case ghw_hie_port_in:
      return "port-in";
    case ghw_hie_port_out:
      return "port-out";
    case ghw_hie_port_inout:
      return "port-inout";
    case ghw_hie_port_buffer:
      return "port-buffer";
    case ghw_hie_port_linkage:
      return "port-linkage";
    default:
      return "??";
    }
}

void
ghw_disp_value (union ghw_val *val, union ghw_type *type);

void
ghw_disp_hie (struct ghw_handler *h, struct ghw_hie *top)
{
  int i;
  int indent;
  struct ghw_hie *hie;
  struct ghw_hie *n;

  hie = top;
  indent = 0;
  
  while (1)
    {
      for (i = 0; i < indent; i++)
	fputc (' ', stdout);
      printf ("%s", ghw_get_hie_name (hie));

      switch (hie->kind)
	{
	case ghw_hie_design:
	case ghw_hie_block:
	case ghw_hie_generate_if:
	case ghw_hie_generate_for:
	case ghw_hie_instance:
	case ghw_hie_process:
	case ghw_hie_package:
	  if (hie->name)
	    printf (" %s", hie->name);
	  if (hie->kind == ghw_hie_generate_for)
	    {
	      printf ("(");
	      ghw_disp_value (hie->u.blk.iter_value, hie->u.blk.iter_type);
	      printf (")");
	    }
	  n = hie->u.blk.child;
	  if (n == NULL)
	    n = hie->brother;
	  else
	    indent++;
	  break;
	case ghw_hie_generic:
	case ghw_hie_eos:
	  abort ();
	case ghw_hie_signal:
	case ghw_hie_port_in:
	case ghw_hie_port_out:
	case ghw_hie_port_inout:
	case ghw_hie_port_buffer:
	case ghw_hie_port_linkage:
	  {
	    unsigned int *sigs;

	    printf (" %s: ", hie->name);
	    ghw_disp_typename (h, hie->u.sig.type);
	    for (sigs = hie->u.sig.sigs; *sigs != 0; sigs++)
	      printf (" #%u", *sigs);
	    n = hie->brother;
	  }
	  break;
	default:
	  abort ();
	}
      printf ("\n");

      while (n == NULL)
	{
	  if (hie->parent == NULL)
	    return;
	  hie = hie->parent;
	  indent--;
	  n = hie->brother;
	}
      hie = n;
    }
}

int
ghw_read_eoh (struct ghw_handler *h)
{
  return 0;
}


int
ghw_read_base (struct ghw_handler *h)
{
  unsigned char hdr[4];
  int res;

  while (1)
    {
      if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
	return -1;
      if (memcmp (hdr, "STR", 4) == 0)
	res = ghw_read_str (h);
      else if (memcmp (hdr, "HIE", 4) == 0)
	res = ghw_read_hie (h);
      else if (memcmp (hdr, "TYP", 4) == 0)
	res = ghw_read_type (h);
      else if (memcmp (hdr, "WKT", 4) == 0)
	res = ghw_read_wk_types (h);
      else if (memcmp (hdr, "EOH", 4) == 0)
	return 0;
      else
	{
	  fprintf (stderr, "ghw_read_base: unknown GHW section %c%c%c%c\n",
		   hdr[0], hdr[1], hdr[2], hdr[3]);
	  return -1;
	}
      if (res != 0)
	{
	  fprintf (stderr, "ghw_read_base: error in section %s\n", hdr);
	  return res;
	}
    }
}

int
ghw_read_signal_value (struct ghw_handler *h, struct ghw_sig *s)
{
  return ghw_read_value (h, s->val, s->type);
}

int
ghw_read_snapshot (struct ghw_handler *h)
{
  unsigned char hdr[12];
  int i;
  struct ghw_sig *s;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  if (hdr[0] != 0 || hdr[1] != 0 || hdr[2] != 0 || hdr[3] != 0)
    return -1;
  h->snap_time = ghw_get_i64 (h, &hdr[4]);
  if (h->flag_verbose > 1)
    printf ("Time is %lld fs\n", h->snap_time);

  for (i = 0; i < h->nbr_sigs; i++)
    {
      s = &h->sigs[i];
      if (s->type != NULL)
	{
	  if (h->flag_verbose > 1)
	    printf ("read type %d for sig %d\n", s->type->kind, i);
	  if (ghw_read_signal_value (h, s) < 0)
	    return -1;
	}
    }
  if (fread (hdr, 4, 1, h->stream) != 1)
    return -1;

  if (memcmp (hdr, "ESN", 4))
    return -1;

  return 0;
}

void ghw_disp_values (struct ghw_handler *h);

int
ghw_read_cycle_start (struct ghw_handler *h)
{
  unsigned char hdr[8];

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  h->snap_time = ghw_get_i64 (h, hdr);
  return 0;
}

int
ghw_read_cycle_cont (struct ghw_handler *h, int *list)
{
  int i;
  int *list_p;

  i = 0;
  list_p = list;
  while (1)
    {
      uint32_t d;
      
      /* Read delta to next signal.  */
      if (ghw_read_uleb128 (h, &d) < 0)
	return -1;
      if (d == 0)
	{
	  /* Last signal reached.  */
	  break;
	}

      /* Find next signal.  */
      while (d > 0)
	{
	  i++;
	  if (h->sigs[i].type != NULL)
	    d--;
	}
      
      if (ghw_read_signal_value (h, &h->sigs[i]) < 0)
	return -1;
      if (list_p)
	*list_p++ = i;
    }
  
  if (list_p)
    *list_p = 0;
  return 0;
}

int
ghw_read_cycle_next (struct ghw_handler *h)
{
  int64_t d_time;

  if (ghw_read_lsleb128 (h, &d_time) < 0)
    return -1;
  if (d_time == -1)
    return 0;
  h->snap_time += d_time;
  return 1;
}


int
ghw_read_cycle_end (struct ghw_handler *h)
{
  char hdr[4];

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;
  if (memcmp (hdr, "ECY", 4))
    return -1;

  return 0;
}

static const char *
ghw_get_lit (union ghw_type *type, int e)
{
  if (e >= type->en.nbr || e < 0)
    return "??";
  else
    return type->en.lits[e];
}

static void
ghw_disp_lit (union ghw_type *type, int e)
{
  printf ("%s (%d)", ghw_get_lit (type, e), e);
}

void
ghw_disp_value (union ghw_val *val, union ghw_type *type)
{
  switch (ghw_get_base_type (type)->kind)
    {
    case ghdl_rtik_type_b2:
      ghw_disp_lit (type, val->b2);
      break;
    case ghdl_rtik_type_e8:
      ghw_disp_lit (type, val->e8);
      break;
    case ghdl_rtik_type_i32:
      printf ("%d", val->i32);
      break;
    case ghdl_rtik_type_p64:
      printf ("%lld", val->i64);
      break;
    case ghdl_rtik_type_f64:
      printf ("%g", val->f64);
      break;
    default:
      fprintf (stderr, "ghw_disp_value: cannot handle type %d\n",
	       type->kind);
      abort ();
    }
}

/* Put the ASCII representation of VAL into BUF, whose size if LEN.
   A NUL is always written to BUF.
*/
void
ghw_get_value (char *buf, int len, union ghw_val *val, union ghw_type *type)
{
  switch (ghw_get_base_type (type)->kind)
    {
    case ghdl_rtik_type_b2:
      if (val->b2 <= 1)
	{
	  strncpy (buf, type->en.lits[val->b2], len - 1);
	  buf[len - 1] = 0;
	}
      else
	{
	  snprintf (buf, len, "?%d", val->b2);
	}
      break;
    case ghdl_rtik_type_e8:
      if (val->b2 <= type->en.nbr)
	{
	  strncpy (buf, type->en.lits[val->e8], len - 1);
	  buf[len - 1] = 0;
	}
      else
	{
	  snprintf (buf, len, "?%d", val->e8);
	}
      break;
    case ghdl_rtik_type_i32:
      snprintf (buf, len, "%d", val->i32);
      break;
    case ghdl_rtik_type_p64:
      snprintf (buf, len, "%lld", val->i64);
      break;
    case ghdl_rtik_type_f64:
      snprintf (buf, len, "%g", val->f64);
      break;
    default:
      snprintf (buf, len, "?bad type %d?", type->kind);
    }
}

void
ghw_disp_values (struct ghw_handler *h)
{
  int i;

  for (i = 0; i < h->nbr_sigs; i++)
    {
      struct ghw_sig *s = &h->sigs[i];
      if (s->type != NULL)
	{
	  printf ("#%d: ", i);
	  ghw_disp_value (s->val, s->type);
	  printf ("\n");
	}
    }
}

int
ghw_read_directory (struct ghw_handler *h)
{
  unsigned char hdr[8];
  int nbr_entries;
  int i;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  nbr_entries = ghw_get_i32 (h, &hdr[4]);
  
  if (h->flag_verbose)
    printf ("Directory (%d entries):\n", nbr_entries);

  for (i = 0; i < nbr_entries; i++)
    {
      unsigned char ent[8];
      int pos;

      if (fread (ent, sizeof (ent), 1, h->stream) != 1)
	return -1;

      pos = ghw_get_i32 (h, &ent[4]);
      if (h->flag_verbose)
	printf (" %s at %d\n", ent, pos);
    }

  if (fread (hdr, 4, 1, h->stream) != 1)
    return -1;
  if (memcmp (hdr, "EOD", 4))
    return -1;
  return 0;
}

int
ghw_read_tailer (struct ghw_handler *h)
{
  unsigned char hdr[8];
  int pos;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    return -1;

  pos = ghw_get_i32 (h, &hdr[4]);
  
  if (h->flag_verbose)
    printf ("Tailer: directory at %d\n", pos);
  return 0;
}

enum ghw_res
ghw_read_sm_hdr (struct ghw_handler *h, int *list)
{
  unsigned char hdr[4];
  int res;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    {
      if (feof (h->stream))
	return ghw_res_eof;
      else
	return ghw_res_error;
    }
  if (memcmp (hdr, "SNP", 4) == 0)
    {
      res = ghw_read_snapshot (h);
      if (res < 0)
	return res;
      return ghw_res_snapshot;
    }
  else if (memcmp (hdr, "CYC", 4) == 0)
    {
      res = ghw_read_cycle_start (h);
      if (res < 0)
	return res;
      res = ghw_read_cycle_cont (h, list);
      if (res < 0)
	return res;
      
      return ghw_res_cycle;
    }
  else if (memcmp (hdr, "DIR", 4) == 0)
    {
      res = ghw_read_directory (h);
    }
  else if (memcmp (hdr, "TAI", 4) == 0)
    {
      res = ghw_read_tailer (h);
    }
  else 
    {
      fprintf (stderr, "unknown GHW section %c%c%c%c\n",
	       hdr[0], hdr[1], hdr[2], hdr[3]);
      return -1;
    }
  if (res != 0)
    return res;
  return ghw_res_other;
}

int
ghw_read_sm (struct ghw_handler *h, enum ghw_sm_type *sm)
{
  int res;

  while (1)
    {
      /* printf ("sm: state = %d\n", *sm); */
      switch (*sm)
	{
	case ghw_sm_init:
	case ghw_sm_sect:
	  res = ghw_read_sm_hdr (h, NULL);
	  switch (res)
	    {
	    case ghw_res_other:
	      break;
	    case ghw_res_snapshot:
	      *sm = ghw_sm_sect;
	      return res;
	    case ghw_res_cycle:
	      *sm = ghw_sm_cycle;
	      return res;
	    default:
	      return res;
	    }
	  break;
	case ghw_sm_cycle:
	  if (0)
	    printf ("Time is %lld fs\n", h->snap_time);
	  if (0)
	    ghw_disp_values (h);
	  
	  res = ghw_read_cycle_next (h);
	  if (res < 0)
	    return res;
	  if (res == 1)
	    {
	      res = ghw_read_cycle_cont (h, NULL);
	      if (res < 0)
		return res;
	      return ghw_res_cycle;
	    }
	  res = ghw_read_cycle_end (h);
	  if (res < 0)
	    return res;
	  *sm = ghw_sm_sect;
	  break;
	}
    }
}

int
ghw_read_cycle (struct ghw_handler *h)
{
  int res;

  res = ghw_read_cycle_start (h);
  if (res < 0)
    return res;
  while (1)
    {
      res = ghw_read_cycle_cont (h, NULL);
      if (res < 0)
	return res;
      
      if (0)
	printf ("Time is %lld fs\n", h->snap_time);
      if (0)
	ghw_disp_values (h);
      
	      
      res = ghw_read_cycle_next (h);
      if (res < 0)
	return res;
      if (res == 0)
	break;
    }
  res = ghw_read_cycle_end (h);
  return res;
}

int
ghw_read_dump (struct ghw_handler *h)
{
  unsigned char hdr[4];
  int res;

  while (1)
    {
      if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
	{
	  if (feof (h->stream))
	    return 0;
	  else
	    return -1;
	}
      if (memcmp (hdr, "SNP", 4) == 0)
	{
	  res = ghw_read_snapshot (h);
	  if (0 && res >= 0)
	    ghw_disp_values (h);
	}
      else if (memcmp (hdr, "CYC", 4) == 0)
	{
	  res = ghw_read_cycle (h);
	}
      else if (memcmp (hdr, "DIR", 4) == 0)
	{
	  res = ghw_read_directory (h);
	}
      else if (memcmp (hdr, "TAI", 4) == 0)
	{
	  res = ghw_read_tailer (h);
	}
      else 
	{
	  fprintf (stderr, "unknown GHW section %c%c%c%c\n",
		   hdr[0], hdr[1], hdr[2], hdr[3]);
	  return -1;
	}
      if (res != 0)
	return res;
    }
}

struct ghw_section ghw_sections[] = {
  { "\0\0\0", NULL },
  { "STR", ghw_read_str },
  { "HIE", ghw_read_hie },
  { "TYP", ghw_read_type },
  { "WKT", ghw_read_wk_types },
  { "EOH", ghw_read_eoh },
  { "SNP", ghw_read_snapshot },
  { "CYC", ghw_read_cycle },
  { "DIR", ghw_read_directory },
  { "TAI", ghw_read_tailer }
};

int
ghw_read_section (struct ghw_handler *h)
{
  unsigned char hdr[4];
  int i;

  if (fread (hdr, sizeof (hdr), 1, h->stream) != 1)
    {
      if (feof (h->stream))
	return -2;
      else
	return -1;
    }
  
  for (i = 1; i < sizeof (ghw_sections) / sizeof (*ghw_sections); i++)
    if (memcmp (hdr, ghw_sections[i].name, 4) == 0)
      return i;

  fprintf (stderr, "ghw_read_section: unknown GHW section %c%c%c%c\n",
	   hdr[0], hdr[1], hdr[2], hdr[3]);
  return 0;
}

void
ghw_close (struct ghw_handler *h)
{
  if (h->stream)
    {
      fclose (h->stream);
      h->stream = NULL;
    }
}

const char *
ghw_get_dir (int is_downto)
{
  return is_downto ? "downto" : "to";
}

void
ghw_disp_range (union ghw_type *type, union ghw_range *rng)
{
  switch (rng->kind)
    {
    case ghdl_rtik_type_e8:
      printf ("%s %s %s", ghw_get_lit (type, rng->e8.left),
	      ghw_get_dir (rng->e8.dir), ghw_get_lit (type, rng->e8.right));
      break;
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_p32:
      printf ("%d %s %d",
	      rng->i32.left, ghw_get_dir (rng->i32.dir), rng->i32.right);
      break;
    case ghdl_rtik_type_i64:
    case ghdl_rtik_type_p64:
      printf ("%lld %s %lld",
	      rng->i64.left, ghw_get_dir (rng->i64.dir), rng->i64.right);
      break;
    case ghdl_rtik_type_f64:
      printf ("%g %s %g",
	      rng->f64.left, ghw_get_dir (rng->f64.dir), rng->f64.right);
      break;
    default:
      printf ("?(%d)", rng->kind);
    }
}

void
ghw_disp_type (struct ghw_handler *h, union ghw_type *t)
{
  switch (t->kind)
    {
    case ghdl_rtik_type_b2:
    case ghdl_rtik_type_e8:
      {
	struct ghw_type_enum *e = &t->en;
	int i;

	printf ("type %s is (", e->name);
	for (i = 0; i < e->nbr; i++)
	  {
	    if (i != 0)
	      printf (", ");
	    printf ("%s", e->lits[i]);
	  }
	printf (");");
	if (e->wkt != ghw_wkt_unknown)
	  printf ("  -- WKT:%d", e->wkt);
	printf ("\n");
      }
      break;
    case ghdl_rtik_type_i32:
    case ghdl_rtik_type_f64:
      {
	struct ghw_type_scalar *s = &t->sc;
	printf ("type %s is range <>;\n", s->name);
      }
      break;
    case ghdl_rtik_type_p32:
    case ghdl_rtik_type_p64:
      {
	int i;

	struct ghw_type_physical *p = &t->ph;
	printf ("type %s is range <> units\n", p->name);
	for (i = 0; i < p->nbr_units; i++)
	  {
	    struct ghw_unit *u = &p->units[i];
	    printf ("  %s = %lld %s;\n", u->name, u->val, p->units[0].name);
	  }
	printf ("end units\n");
      }
      break;
    case ghdl_rtik_subtype_scalar:
      {
	struct ghw_subtype_scalar *s = &t->ss;
	printf ("subtype %s is ", s->name);
	ghw_disp_typename (h, s->base);
	printf (" range ");
	ghw_disp_range (s->base, s->rng);
	printf (";\n");
      }
      break;
    case ghdl_rtik_type_array:
      {
	struct ghw_type_array *a = &t->ar;
	int i;

	printf ("type %s is array (", a->name);
	for (i = 0; i < a->nbr_dim; i++)
	  {
	    if (i != 0)
	      printf (", ");
	    ghw_disp_typename (h, a->dims[i]);
	    printf (" range <>");
	  }
	printf (") of ");
	ghw_disp_typename (h, a->el);
	printf (";\n");
      }
      break;
    case ghdl_rtik_subtype_array:
    case ghdl_rtik_subtype_array_ptr:
      {
	struct ghw_subtype_array *a = &t->sa;
	int i;

	printf ("subtype %s is ", a->name);
	ghw_disp_typename (h, (union ghw_type *)a->base);
	printf (" (");
	for (i = 0; i < a->base->nbr_dim; i++)
	  {
	    if (i != 0)
	      printf (", ");
	    ghw_disp_range ((union ghw_type *)a->base, a->rngs[i]);
	  }
	printf (");\n");
      }
      break;
    case ghdl_rtik_type_record:
      {
	struct ghw_type_record *r = &t->rec;
	int i;

	printf ("type %s is record\n", r->name);
	for (i = 0; i < r->nbr_fields; i++)
	  {
	    printf ("  %s: ", r->el[i].name);
	    ghw_disp_typename (h, r->el[i].type);
	    printf ("\n");
	  }
	printf ("end record;\n");
      }
      break;
    default:
      printf ("ghw_disp_type: unhandled type kind %d\n", t->kind);
    }
}

void
ghw_disp_types (struct ghw_handler *h)
{
  int i;

  for (i = 0; i < h->nbr_types; i++)
    ghw_disp_type (h, h->types[i]);
}
