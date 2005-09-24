/*  Ada bindings for GCC internals - generate Ada files.
    Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "real.h"
#include "options.h"
#undef abort

static const char *progname;

/* Taken from tree.h.  */


#define XSTR(X) #X
#define STR(X) XSTR(X)
static const char *treecode_sym[] =
{
#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   #SYM,
#include "tree.def"
#undef DEFTREECODE
  NULL
};

static const char *treecode_string[] =
{
#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   STRING,
#include "tree.def"
#undef DEFTREECODE
  NULL
};

void
gen_tree_code (void)
{
  int i, j;
  size_t len;
  const size_t indent = 24;

  for (i = 0; treecode_sym[i] != NULL; i++)
    {
      len = strlen (treecode_sym[i]);
      printf ("      %s, ", treecode_sym[i]);
      for (j = len; j < indent; j++)
        putchar (' ');
      printf ("-- %s\n", treecode_string[i]);
    }
  printf ("      LAST_AND_UNUSED_TREE_CODE\n");
}

static const char *built_in_function_sym[] =
{
#if 0
#define DEF_BUILTIN(x) #x,
#else
#define DEF_BUILTIN(ENUM, N, C, T, LT, B, F, NA, ATTR, IMP) #ENUM,
#endif
#include "builtins.def"
#undef DEF_BUILTIN
  NULL
};

static void
print_underscore (const char *sym)
{
  for (; *sym != 0; sym++)
    {
      if (sym[0] == '_' && (sym[1] == '_' || sym[1] == 0))
	fputs ("_u", stdout);
      else
	fputc (sym[0], stdout);
    }
}

void
gen_built_in_function (void)
{
  int i;
  
  for (i = 0; built_in_function_sym[i] != NULL; i++)
    {
      fputs ("      ", stdout);
      print_underscore (built_in_function_sym[i]);
      fputs (",\n", stdout);
    }

  printf ("      END_BUILTINS\n");
}

#if 0
static const char *machmode_sym[] =
{
#if 0
#define DEF_MACHMODE(SYM, NAME, TYPE, BITSIZE, SIZE, UNIT, WIDER)  #SYM,
#else
#define DEF_MACHMODE(SYM, NAME, TYPE, BITSIZE, SIZE, UNIT, WIDER, INNER) \
  #SYM,
#endif
#include "machmode.def"
#undef DEF_MACHMODE
  NULL
};
#endif

static void
gen_machmode (void)
{
  int i;
  char line[128];
  FILE *f;
  int do_emit;
  char *p;

  f = fopen ("insn-modes.h", "r");
  if (f == NULL)
    {
      fprintf (stderr, "cannot open insn-modes\n");
      exit (1);
    }

  do_emit = 0;
  while (1)
    {
      if (fgets (line, sizeof (line), f) == NULL)
	break;
      if (!do_emit)
	{
	  if (strncmp (line, "enum machine_mode", 17) == 0)
	    do_emit = 1;
	}
      else if (memcmp (line, "  MAX_MACHINE_MODE,", 19) == 0)
	{
	  fclose (f);
	  break;
	}
      else	  
	{
	  /* Search for "  [A-Z0-9_]*mode,".  */
	  p = line;
	  if (p[0] != ' ' || p[1] != ' ')
	    continue;
	  p += 2;
	  while ((*p >= 'A' && *p <= 'Z')
		 || (*p >= '0' && *p <= '9')
		 || (*p == '_'))
	    p++;
	  if (memcmp (p, "mode,", 5) == 0)
	    {
	      p[4] = 0;
	      printf ("      %s,\n", line + 2);
	    }
	}

    }
  printf ("      MAX_MACHINE_MODE\n");
}

static void
gen_options_CL (void)
{
  printf ("   CL_C : constant Integer := %d;\n", CL_C);
  printf ("   CL_vhdl : constant Integer := %d;\n", CL_vhdl);
}

static void
gen_options_OPTs (void)
{
  char line[128];
  FILE *f;
  int do_emit;
  char *p;

  f = fopen ("options.h", "r");
  if (f == NULL)
    {
      fprintf (stderr, "cannot open options.h\n");
      exit (1);
    }

  do_emit = 0;
  while (1)
    {
      if (fgets (line, sizeof (line), f) == NULL)
	break;
      if (!do_emit)
	{
	  if (strncmp (line, "enum opt_code", 13) == 0)
	    do_emit = 1;
	}
      else if (memcmp (line, "  N_OPTS", 9) == 0)
	{
	  fclose (f);
	  break;
	}
      else	  
	{
	  /* Search for "  [A-Z0-9]*mode,".  */
	  p = line;
	  if (memcmp (p, "  OPT_", 6) != 0)
	    continue;
	  printf ("      OPT");
	  for (p = line + 5; *p != ','; p++)
	    {
	      if (p[0] == '_' && (p[1] == ',' || p[1] == '_'))
		fputs ("_U", stdout);
	      else
		{
		  if (p[0] >= 'A' && p[0] <= 'Z')
		    putchar ('U');
		  putchar (p[0]);
		}
	    }
	  printf (",\n");
	}

    }
  printf ("       N_OPTS\n");
}

struct xtab_t
{
  int val;
  const char *name;
};

void
gen_enumeration (const struct xtab_t *xtab, int max, const char *max_name)
{
  int i;

  for (i = 0; i < max; i++)
    {
      const struct xtab_t *t;

      for (t = xtab; t->name; t++)
        if (t->val == i)
          break;

      if (t->name == NULL)
        {
          fprintf (stderr, "gen_enumeration: kind %d unknown (max is %s)\n",
		   i, max_name);
          exit (1);
        }

      printf ("       %s,\n", t->name);
    }
  printf ("      %s\n", max_name);
}

const struct xtab_t size_type_names[] =
{
  { SIZETYPE, "TK_SIZETYPE" },
  { SSIZETYPE, "TK_SSIZETYPE" },
  { USIZETYPE, "TK_USIZETYPE" },
  { BITSIZETYPE, "TK_BITSIZETYPE" },
  { SBITSIZETYPE, "TK_SBITSIZETYPE" },
  { UBITSIZETYPE, "TK_UBITSIZETYPE" },
  { 0, NULL}
};

static void
gen_size_type (void)
{
  gen_enumeration (size_type_names, TYPE_KIND_LAST, "TYPE_KIND_LAST");
}


const struct xtab_t type_qual_tab[] =
{
  { TYPE_UNQUALIFIED, "TYPE_UNQUALIFIED" },
  { TYPE_QUAL_CONST, "TYPE_QUAL_CONST" },
  { TYPE_QUAL_VOLATILE, "TYPE_QUAL_VOLATILE" },
  { TYPE_QUAL_RESTRICT, "TYPE_QUAL_RESTRICT" },
  { 0, NULL}
};

void
gen_type_qual (void)
{
  const struct xtab_t *t;
  for (t = type_qual_tab; t->name; t++)
    printf ("   %s : constant Type_Qual_Type := %d;\n", t->name, t->val);
}

const struct xtab_t tree_index_tab[] =
{
  /* Defined in tree.h  */
  { TI_ERROR_MARK, "TI_ERROR_MARK" },
  { TI_INTQI_TYPE, "TI_INTQI_TYPE" },
  { TI_INTHI_TYPE, "TI_INTHI_TYPE" },
  { TI_INTSI_TYPE, "TI_INTSI_TYPE" },
  { TI_INTDI_TYPE, "TI_INTDI_TYPE" },
  { TI_INTTI_TYPE, "TI_INTTI_TYPE" },

  { TI_UINTQI_TYPE, "TI_UINTQI_TYPE" },
  { TI_UINTHI_TYPE, "TI_UINTHI_TYPE" },
  { TI_UINTSI_TYPE, "TI_UINTSI_TYPE" },
  { TI_UINTDI_TYPE, "TI_UINTDI_TYPE" },
  { TI_UINTTI_TYPE, "TI_UINTTI_TYPE" },

  { TI_INTEGER_ZERO, "TI_INTEGER_ZERO" },
  { TI_INTEGER_ONE, "TI_INTEGER_ONE" },
  { TI_INTEGER_MINUS_ONE, "TI_INTEGER_MINUS_ONE" },
  { TI_NULL_POINTER, "TI_NULL_POINTER" },

  { TI_SIZE_ZERO, "TI_SIZE_ZERO" },
  { TI_SIZE_ONE, "TI_SIZE_ONE" },

  { TI_BITSIZE_ZERO, "TI_BITSIZE_ZERO" },
  { TI_BITSIZE_ONE, "TI_BITSIZE_ONE" },
  { TI_BITSIZE_UNIT, "TI_BITSIZE_UNIT" },

  { TI_PUBLIC, "TI_PUBLIC" },
  { TI_PROTECTED, "TI_PROTECTED" },
  { TI_PRIVATE, "TI_PRIVATE" },

  { TI_BOOLEAN_FALSE, "TI_BOOLEAN_FALSE" },
  { TI_BOOLEAN_TRUE, "TI_BOOLEAN_TRUE" },

  { TI_COMPLEX_INTEGER_TYPE, "TI_COMPLEX_INTEGER_TYPE" },
  { TI_COMPLEX_FLOAT_TYPE, "TI_COMPLEX_FLOAT_TYPE" },
  { TI_COMPLEX_DOUBLE_TYPE, "TI_COMPLEX_DOUBLE_TYPE" },
  { TI_COMPLEX_LONG_DOUBLE_TYPE, "TI_COMPLEX_LONG_DOUBLE_TYPE" },

  { TI_FLOAT_TYPE, "TI_FLOAT_TYPE" },
  { TI_DOUBLE_TYPE, "TI_DOUBLE_TYPE" },
  { TI_LONG_DOUBLE_TYPE, "TI_LONG_DOUBLE_TYPE" },

  { TI_FLOAT_PTR_TYPE, "TI_FLOAT_PTR_TYPE" },
  { TI_DOUBLE_PTR_TYPE, "TI_DOUBLE_PTR_TYPE" },
  { TI_LONG_DOUBLE_PTR_TYPE, "TI_LONG_DOUBLE_PTR_TYPE" },
  { TI_INTEGER_PTR_TYPE, "TI_INTEGER_PTR_TYPE" },

  { TI_VOID_TYPE, "TI_VOID_TYPE" },
  { TI_PTR_TYPE, "TI_PTR_TYPE" },
  { TI_CONST_PTR_TYPE, "TI_CONST_PTR_TYPE" },
  { TI_SIZE_TYPE, "TI_SIZE_TYPE" },
  { TI_PTRDIFF_TYPE, "TI_PTRDIFF_TYPE" },
  { TI_VA_LIST_TYPE, "TI_VA_LIST_TYPE" },
  { TI_BOOLEAN_TYPE, "TI_BOOLEAN_TYPE" },

  { TI_VOID_LIST_NODE, "TI_VOID_LIST_NODE" },

  { TI_UV4SF_TYPE, "TI_UV4SF_TYPE" },
  { TI_UV4SI_TYPE, "TI_UV4SI_TYPE" },
  { TI_UV8HI_TYPE, "TI_UV8HI_TYPE" },
  { TI_UV8QI_TYPE, "TI_UV8QI_TYPE" },
  { TI_UV4HI_TYPE, "TI_UV4HI_TYPE" },
  { TI_UV2HI_TYPE, "TI_UV2HI_TYPE" },
  { TI_UV2SI_TYPE, "TI_UV2SI_TYPE" },
  { TI_UV2SF_TYPE, "TI_UV2SF_TYPE" },
  { TI_UV2DI_TYPE, "TI_UV2DI_TYPE" },
  { TI_UV1DI_TYPE, "TI_UV1DI_TYPE" },
  { TI_UV16QI_TYPE, "TI_UV16QI_TYPE" },

  { TI_V4SF_TYPE, "TI_V4SF_TYPE" },
  { TI_V16SF_TYPE, "TI_V16SF_TYPE" },
  { TI_V4SI_TYPE, "TI_V4SI_TYPE" },
  { TI_V8HI_TYPE, "TI_V8HI_TYPE" },
  { TI_V8QI_TYPE, "TI_V8QI_TYPE" },
  { TI_V4HI_TYPE, "TI_V4HI_TYPE" },
  { TI_V2HI_TYPE, "TI_V2HI_TYPE" },
  { TI_V2SI_TYPE, "TI_V2SI_TYPE" },
  { TI_V2SF_TYPE, "TI_V2SF_TYPE" },
  { TI_V2DF_TYPE, "TI_V2DF_TYPE" },
  { TI_V2DI_TYPE, "TI_V2DI_TYPE" },
  { TI_V1DI_TYPE, "TI_V1DI_TYPE" },
  { TI_V16QI_TYPE, "TI_V16QI_TYPE" },
  { TI_V4DF_TYPE, "TI_V4DF_TYPE" },

  { TI_MAIN_IDENTIFIER, "TI_MAIN_IDENTIFIER" },

  { 0, NULL }
};

const struct xtab_t integer_types_tab[] =
{
  { itk_char, "itk_char" },
  { itk_signed_char, "itk_signed_char" },
  { itk_unsigned_char, "itk_unsigned_char" },
  { itk_short, "itk_short" },
  { itk_unsigned_short, "itk_unsigned_short" },
  { itk_int, "itk_int" },
  { itk_unsigned_int, "itk_unsigned_int" },
  { itk_long, "itk_long" },
  { itk_unsigned_long, "itk_unsigned_long" },
  { itk_long_long, "itk_long_long" },
  { itk_unsigned_long_long, "itk_unsigned_long_long" },
  { 0, NULL }
};


void
gen_tree_index (void)
{
  gen_enumeration (tree_index_tab, TI_MAX, "TI_MAX");
}

void
gen_integer_types (void)
{
  gen_enumeration (integer_types_tab, itk_none, "itk_none");
}

static void
gen_host_wide_int_decl (void)
{
  int l;
  switch (sizeof (HOST_WIDE_INT))
    {
    case 4:
      l = 32;
      break;
    case 8:
      l = 64;
      break;
    default:
      fprintf (stderr, "%s: cannot handle sizeof (HOST_WIDE_INT) %d\n",
	       progname, sizeof (HOST_WIDE_INT));
      exit (1);
    }
  printf ("   type HOST_WIDE_INT is new Interfaces.Integer_%d;\n", l);
  printf ("   type UNSIGNED_HOST_WIDE_INT is new Interfaces.Unsigned_%d;\n",
	  l);
}

static void
gen_host_big_endian (void)
{
#ifdef HOST_WORDS_BIG_ENDIAN
  printf ("   HOST_WORDS_BIG_ENDIAN : constant Boolean := True;\n");
#else
  printf ("   HOST_WORDS_BIG_ENDIAN : constant Boolean := False;\n");
#endif
}

static void
gen_real (void)
{
  printf ("   type Real_Value_Type_Arr is array (0 .. %d) of HOST_WIDE_INT;\n",
	  (sizeof (REAL_VALUE_TYPE) / sizeof (HOST_WIDE_INT)) - 1);
  printf ("   type REAL_VALUE_TYPE is record\n"
	  "      r : Real_Value_Type_Arr;\n"
	  "   end record;\n");
}

static void
gen_tm (void)
{
#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif
  /* This is a constant.  */
  printf ("   MAX_BITS_PER_WORD : constant Natural := %d;\n",
	  MAX_BITS_PER_WORD);
}

int
main (int argc, char *argv[])
{
  FILE *infile;
  char line[2048];
  const char *filename;
  int c;

  progname = argv[0];

  while ((c = getopt (argc, argv, "C:")) != -1)
    switch (c)
      {
      case 'C':
	chdir (optarg);
	break;
      case '?':
	fprintf (stderr, "%s: unknown option '%s'\n", progname, optopt);
	exit (1);
      default:
	abort ();
      }

  if (argc - optind != 1)
    {
      fprintf (stderr, "usage: %s FILENAME\n", progname);
      exit (1);
    }
  filename = argv[optind];
  if (strcmp (filename, "-") == 0)
    infile = stdin;
  else
    infile = fopen (filename, "r");
  if (infile == NULL)
    {
      fprintf (stderr, "%s: cannot open %s (%s)\n", progname, filename,
               strerror (errno));
      exit (1);
    }
#if 0
#ifdef REAL_IS_NOT_DOUBLE
  printf ("-- REAL_IS_NOT_DOUBLE is not yet implemented\n");
  printf ("You loose\n");
  return 1;
#endif
#endif
  printf ("--  Automatically generated by %s\n", progname);
  printf ("--    from %s\n", filename);
  printf ("--  DO NOT EDIT THIS FILE\n");

  while (fgets (line, sizeof (line), infile) != NULL)
    {
      if (line[0] != '@')
        fputs (line, stdout);
      else
        {
          char *p;

          for (p = line + 1; isalpha (*p) || *p == '_'; p++)
            ;
          *p = 0;

          if (!strcmp (line, "@tree_code"))
            gen_tree_code ();
	  else if (!strcmp (line, "@built_in_function"))
	    gen_built_in_function ();
          else if (!strcmp (line, "@size_type_kind"))
            gen_size_type ();
          else if (!strcmp (line, "@type_qual"))
            gen_type_qual ();
          else if (!strcmp (line, "@host_wide_int"))
            gen_host_wide_int_decl ();
          else if (!strcmp (line, "@tree_index"))
            gen_tree_index ();
          else if (!strcmp (line, "@integer_types"))
            gen_integer_types ();
          else if (!strcmp (line, "@host_big_endian"))
	    gen_host_big_endian ();
          else if (!strcmp (line, "@real"))
	    gen_real ();
          else if (!strcmp (line, "@machmode"))
	    gen_machmode ();
          else if (!strcmp (line, "@tm"))
	    gen_tm ();
	  else if (!strcmp (line, "@options_CL"))
	    gen_options_CL ();
	  else if (!strcmp (line, "@options_OPTs"))
	    gen_options_OPTs ();
          else
            {
              fprintf (stderr, "unknown code `%s'\n", line);
              exit (1);
            }
        }
    }
  return 0;
}
